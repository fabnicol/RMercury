%% File: test0.m Purpose: An R interface for Mercury: proof of concept.
%% Copyright Fabrice Nicol <fabnicol@users.sourceforge.net>, 2021 The latest
%% version can be found at http://github.com/fabnicol

%% This program is free software; you can redistribute it and/or modify it under
%% the terms of the GNU General Public License as published by the Free Software
%% Foundation; either version 3 of the License, or (at your option) any later
%% version.

%% This program is distributed in the hope that it will be useful, but WITHOUT
%% ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
%% FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
%% details.

%% You should have received a copy of the GNU General Public License along with
%% this program; if not, write to the Free Software Foundation, Inc., 675 Mass
%% Ave, Cambridge, MA 02139, USA.

:- module test0.

:- interface.

:- import_module io.

:- impure pred main(io::di, io::uo) is det.

:- type float_buffer.
:- type int_buffer.

%----------------------------------------------%

:- implementation.

:- import_module int.
:- import_module bool.
:- import_module char.
:- import_module float.
:- import_module string.
:- import_module univ.
:- import_module math.
:- import_module list.
:- import_module array.

        % Predicates for sourcing and evaluating R code.  Predicates
        % performing C I/O, semipure or impure (pending evaluation).

:- pragma foreign_decl("C", "#include <RInside_C.h>").

:- pragma foreign_decl("C", "#include <Rinternals.h>").

:- pragma foreign_decl("C", "int start = 1;").

:- impure pred start_r is det.

:- pragma foreign_proc("C",
                       start_r,
                       [will_not_call_mercury],
                       "
                         if (start) setupRinC();
                         start = 0;
                       ").

:- pred stop_r is det.

    % Call this predicate at the end of a session.
    % For ill-understood reasons, this RInside function (01.21)
    % does not reset setup status to zero, so start_r
    % should not be called as would be expected

:- pragma foreign_proc("C",
                       stop_r,
                       [promise_pure, will_not_call_mercury],
                       "teardownRinC();").

:- impure pred source(string::in) is det.

% Mercury tweak: in embedded C code, double-quote double-quotes
% and escape escape characters. Easy to remember!

:- pragma foreign_proc("C",
                       source(X::in),
                       [will_not_call_mercury],
                       "
                         if (start) setupRinC();
                         start = 0;
                         evalQuietlyInR(X);
                       ").

:- impure pred source_echo(string::in) is det.

:- pragma foreign_proc("C",
                       source_echo(X::in),
                       [will_not_call_mercury],
                       "
                         if (start) setupRinC();
                         start = 0;
                         Rf_PrintValue(evalInR(X));
                       ").

    % Predicates implementated as semipure (promised pure
    % or hopefully so). Starting with one-dimensional
    % real or integer output.
    % Predicates of arity one flagged impure
    % pending further evaluation.

%% Mercury type           C type
%% int                    MR_Integer
%% int8                   int8_t
%% int16                  int16_t
%% int32                  int32_t
%% int64                  int64_t
%% uint                   MR_Unsigned
%% uint8                  uint8_t
%% uint16                 int16_t
%% uint32                 uint32_t
%% uint64                 uint64_t
%% float                  MR_Float
%% char                   MR_Char
%% string                 MR_String

:- typeclass r_eval(T) where [
       pred eval(string::in, T::out) is det
   ].

:- instance r_eval(string) where [
       pred(eval/2) is eval_string
   ].

:- instance r_eval(int) where [
       pred(eval/2) is eval_int
   ].

:- instance r_eval(bool) where [
       pred(eval/2) is eval_bool
   ].

:- instance r_eval(float) where [
       pred(eval/2) is eval_float
   ].

    % Predicates instanciating r_eval may be promised pure,
    % but do not rely on them for invoking internal state
    % variables across calls.  Output is crushed.
    % R variables are threaded to Mercury
    % using the output mode variables.

:- pred eval_string(string::in, string::out) is det.

:- pragma foreign_proc("C", eval_string(X::in, Y::out),
                    [promise_pure, will_not_call_mercury],
    "
         if (start) setupRinC();
         start = 0;
         int S = strlen(X);
         char buf[S + 100];
         memset(buf, 0, 100 + S);
         sprintf(buf, ""%s%s%s"",
                      ""z<-try({"",X,""});\
if (inherits(z, 'try-error')) E <- 'R string error' else E <- z"");

         // Important note: use a buffer here.
         // writing Y = evalInRtoString(buf).string will NOT do.

         R_StringResult res = evalInRToString(buf);

         // More care & control of memory allocation than  with
         // just strdup

         MR_String a = MR_GC_NEW_ARRAY(char, res.length + 1);
         memcpy(a, res.string, res.length);
         Y = a;
    ").


:- pred eval_int(string::in, int::out) is det.

:- pragma foreign_proc("C",
                       eval_int(X::in, Z::out),
                       [promise_pure, will_not_call_mercury],
                       "
                         if (start) setupRinC();
                         start = 0;
                         MR_Integer a = evalInRToInt(X);
                         Z = (MR_Integer) a;
                       ").


:- pragma foreign_decl("C", "

#include ""mercury_float.h""    /* For MR_FLT_FMT. */
#include ""mercury_memory.h""
#include ""mercury_string.h""

#include <stdio.h>  /* For sscanf. */

typedef struct {
    MR_Integer  size;
    MR_Float   *contents;
} FLOAT_BUFFER;

typedef struct {
    MR_Integer  size;
    MR_Integer  *contents;
} INT_BUFFER;

").

:- pragma foreign_type("C", int_buffer, "INT_BUFFER *",
                       [can_pass_as_mercury_type]).

:- pragma foreign_type("C", float_buffer, "FLOAT_BUFFER *",
                       [can_pass_as_mercury_type]).

    % eval_int_vect(R_Code, Array(float)).
    % Parse R code into a Mercury float array of length Size.

:- pred float_vect(string::in, float_buffer::out) is det.

:- pragma foreign_proc("C",
                       float_vect(X::in, Buffer::out),
                       [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
                        does_not_affect_liveness],
                       "
                         if (start) setupRinC();
                         start = 0;
                         SEXP V = evalInR(X);
                         MR_Float *Items = (MR_Float *) REAL(V);
                         Buffer = MR_GC_NEW(FLOAT_BUFFER);
                         Buffer->size = LENGTH(V);
                         Buffer->contents = Items;
                       ").

:- pred int_vect(string::in, int_buffer::out) is det.

:- pragma foreign_proc("C",
                       int_vect(X::in, Buffer::out),
                       [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
                        does_not_affect_liveness],
                       "
                         if (start) setupRinC();
                         start = 0;
                         SEXP V = evalInR(X);
                         Buffer = MR_GC_NEW(INT_BUFFER);
                         MR_Integer S = LENGTH(V);
                         MR_Float *V1 = REAL(V);
                         Buffer->size = S;
                         Buffer->contents = MR_GC_malloc(sizeof(MR_Integer) * S);
                         for (int i = 0; i < S; ++i) Buffer->contents[i] = floor(V1[i]);
                       ").

:- pred lookup_int(int::in, int_buffer::in, int::out) is det.
:- pred lookup_int_size(int_buffer::in, int::out) is det.
:- pred lookup_float_size(float_buffer::in, int::out) is det.
:- pred lookup_float(int::in, float_buffer::in, float::out) is det.

:- pragma foreign_proc("C",
                       lookup_int_size(Buffer::in, Value::out),
                       [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
                        does_not_affect_liveness
                       ],
                       "
                             Value=(MR_Integer) Buffer->size;
                       ").

:- pragma foreign_proc("C",
                       lookup_float_size(Buffer::in, Value::out),
                       [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
                        does_not_affect_liveness
                       ],
                       "
                             Value=(MR_Integer) Buffer->size;
                       ").

:- pragma foreign_proc("C",
                       lookup_float(Index::in, Buffer::in, Value::out),
                       [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
                        does_not_affect_liveness
                       ],
                       "
                             Value=(MR_Float) Buffer->contents[Index];
                       ").

:- pragma foreign_proc("C",
                       lookup_int(Index::in, Buffer::in, Value::out),
                       [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
                        does_not_affect_liveness
                       ],
                       "
                             Value=(MR_Integer) Buffer->contents[Index];
                       ").



:- pred eval_int_vect(string::in, int::out, array(int)::array_uo) is det.

:- pragma foreign_proc("C",
                       eval_int_vect(X::in, S::out, Z::array_uo),
                       [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
                        does_not_affect_liveness
                       ],
                       "
                         if (start) setupRinC();
                         start = 0;
                         SEXP V = evalInR(X);
                         PROTECT(V);
                         S = (int) LENGTH(V);
                         MR_Integer *Items = (MR_Integer *) INTEGER(V);
                         MR_Integer *Items2 = MR_GC_NEW_ARRAY(int,S);
                         for (int i = 0; i < S; ++i) Items2[i]=Items[i];
                         Z = Items2;
                         UNPROTECT(1);
                       ").

:- pred eval_float(string::in, float::out) is det.

:- pragma foreign_proc("C",
                       eval_float(X::in, Z::out),
                       [promise_pure, will_not_call_mercury],
                       "
                         if (start) setupRinC();
                         start = 0;
                         float a = REAL(evalInR(X))[0];
                         Z = (MR_Float) a;
                       ").

:- pred eval_bool(string::in, bool::out) is det.

:- pragma foreign_proc("C",
                       eval_bool(X::in, Z::out),
                       [promise_pure, will_not_call_mercury],
                       "
                         if (start) setupRinC();
                         start = 0;
                         MR_Bool a = evalInRToBool(X) ?
                                       MR_YES : MR_NO ;
                         Z = (MR_Bool) a;
                       ").

:- pred test_array(list(int)::in, array(int)::out) is det.

test_array(L, A) :- A = array.from_list(L).


%-----------------------------------------%


% Testbed: playing with R I/O and retrieving R computations
% in Mercury variables through the C foreign code interface.

main(!IO) :-
             test_array([1, 2, 3, 4], A),
             io.write_int(lookup(A, 0), !IO),io.nl(!IO),
             float_vect("c(1.0,2.0,3.0,4.0,5.0)", B),
             copy_to_array(B, Array),
             io.write_float(array.lookup(Array, 2), !IO), io.nl(!IO),
             int_vect("c(1,2,3,4)", C),
%             eval_int_vect("as.integer(c(1,2,3,4))", U, C),
%             io.write_int(S, !IO), io.nl(!IO),
             lookup_float(3, B, V),
             lookup_int(2, C, W),
             lookup_int_size(C, H),
             lookup_float_size(B, Z),
             io.write_int(W, !IO), io.nl(!IO),
             io.write_float(V, !IO), io.nl(!IO),
             io.write_int(H, !IO), io.nl(!IO),
             io.write_int(Z, !IO), io.nl(!IO),
%             io.write_int(lookup(C,1), !IO), io.nl(!IO),
             stop_r .

%% Raw R-managed output (can one funnel it through Mercury IO module?):
%% V1 V2
%% 1:  3  4

%% V1 V2 V3
%% 1:  1  2  3
%% 2:  4  5  6
%%     3:  7  8  9

%% V1 V2
%% 1:  3  4

%% V1 V2
%% 1:  2  3

%% [1] "abcd"
%% [1] 9
%% eval z:  9
%% abcd
%% 11.5
%% 2.0
%% yes
%% 9
%% 9
%% 0.8414709568023682
%% 58

%% %% real	0m0,371s
%% user	0m0,334s
%% sys  0m0,037s

% TODO: expand this POC to vectorized data structures,
% which will request setting precise equivalence relations
% between C, R (via the SEXP C-interface) and Mercury data types.
% This does not look as intractable.
