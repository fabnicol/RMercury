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

:- type float_buffer.
:- type int_buffer.

:- impure pred source(string::in) is det.

:- impure pred source_echo(string::in) is det.

:- pred float_vect(string::in, float_buffer::out) is det.

:- pred int_vect(string::in, int_buffer::out) is det.

:- impure pred main(io::di, io::uo) is det.

%----------------------------------------------%

:- implementation.

:- import_module int.
:- import_module bool.
:- import_module char.
:- import_module float.
:- import_module string.
:- import_module math.

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

%-----------------------------------------------------------------------------%
% Sourcing R code
%

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
%% vector                 Possibly MR_ArrayPtr. Currently replaced by int_buffer/float_buffer.

%-----------------------------------------------------------------------------%
% Basic types
%

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

:- pred write_bool(bool::in, io::di, io::uo) is det.

write_bool(Value, !IO) :- (Value = yes -> write_string("TRUE", !IO)
                          ;  write_string("FALSE", !IO)).

% ---------------------------------------------------------------------------%
% Vectors
%

  % Note that C-structures shoud not contain pointers
  % beyond the first word, otherwise the GC will lose track.
  % A first-rank Integer field is OK.

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

    % Mercury types corresponding to R integer() and numeric()
    % with respective types int_buffer and float_buffer

:- pragma foreign_type("C", int_buffer, "INT_BUFFER *",
                       [can_pass_as_mercury_type]).

:- pragma foreign_type("C", float_buffer, "FLOAT_BUFFER *",
                       [can_pass_as_mercury_type]).

    % float_vect(R_Code, float_buffer)).
    % Parse R code into a Mercury float_buffer.


:- pragma foreign_proc("C",
                       float_vect(X::in, Buffer::out),
                       [will_not_call_mercury, promise_pure],
                       "
                         if (start) setupRinC();
                         start = 0;
                         SEXP V = evalInR(X);
                         MR_Float *Items = (MR_Float *) REAL(V);
                         Buffer = MR_GC_NEW(FLOAT_BUFFER);
                         Buffer->size = LENGTH(V);

         /* TODO:
         *  1. Does the GC effectively collect the R-internally
         *     allocated chunk when float_buffer is freed?
         *  2. Is it necessary to call R internals to free the chunk?
         *  3. Possible double-free is using 'MR_GC_register_finalizer'?
         */

                         Buffer->contents = Items;
                       ").

:- pragma foreign_proc("C",
                       int_vect(X::in, Buffer::out),
                       [will_not_call_mercury, promise_pure],
                       "
                         if (start) setupRinC();
                         start = 0;
                         SEXP V = evalInR(X);
                         Buffer = MR_GC_NEW(INT_BUFFER);
                         MR_Integer S = LENGTH(V);
                         MR_Float *V1 = REAL(V);
                         Buffer->size = S;

                         /* TODO:
                         *
                         * RInside currently implements 'evalInR' by returning double vectors.
                         * It would be more efficient to reemplement this with interger vectors
                         * using an ad-hoc 'evalIntegerVectorInR', therby avoiding the extra
                         * MR_GC_malloc + floor below.
                         * The above questions now concern the C-local SEXP V, not the int_buffer,
                         * which is entirely allocated on the Mercury GC'd heap.
                         */

                         Buffer->contents = MR_GC_malloc(sizeof(MR_Integer) * S);
                         for (int i = 0; i < S; ++i)
                            Buffer->contents[i] = floor(V1[i]);
                       ").

:- pred lookup_int(int::in, int_buffer::in, int::out) is det.
:- pred lookup_int_size(int_buffer::in, int::out) is det.
:- pred lookup_float_size(float_buffer::in, int::out) is det.
:- pred lookup_float(int::in, float_buffer::in, float::out) is det.

:- pragma foreign_proc("C",
                       lookup_int_size(Buffer::in, Value::out),
                       [will_not_call_mercury, promise_pure],
                       "
                             Value=(MR_Integer) Buffer->size;
                       ").

:- pragma foreign_proc("C",
                       lookup_float_size(Buffer::in, Value::out),
                       [will_not_call_mercury, promise_pure],
                       "
                             Value=(MR_Integer) Buffer->size;
                       ").

:- pragma foreign_proc("C",
                       lookup_float(Index::in, Buffer::in, Value::out),
                       [will_not_call_mercury, promise_pure],
                       "
                             Value=(MR_Float) Buffer->contents[Index];
                       ").

:- pragma foreign_proc("C",
                       lookup_int(Index::in, Buffer::in, Value::out),
                       [will_not_call_mercury, promise_pure],
                       "
                             Value=(MR_Integer) Buffer->contents[Index];
                       ").

% It should be easy to convert int_buffer or float_buffer into arrays or lists,
% using item setters and iterating.
% Yet ideally it would be nice to allow the 'elements' C-array
% to take ownership of int/float_buffer 'contents' C-array
% already allocated on the GC heap (int_buffer) or by R internals (float_buffer)
% without a further copy operation.

%----------------------------------------------------------------------------%
% Testbed: playing with R I/O and retrieving R computations
% in Mercury variables through the C foreign code interface.
%


main(!IO) :-
             impure source("
                 library(data.table)
                 y <- data.table(c(1,2,3), c(2,3,4))
                 z <- data.table(c(1,2,3), c(2,3,11))
             "),
             impure source_echo("y[V1 == 3]"),
             impure source_echo("
                 library(data.table)
                 fread(file = 'a.csv')
             "),
             impure source_echo("z[V1 == 2]"),
             eval("y <- 12.0; z <- 11.5", X0),
             eval_string("y <- ""abcd""", X1),
             impure source_echo("E"),
             eval_int("z <- 9", X2),
             eval_int("cat('eval z: ', z, '\\n'); z", X3),
             impure source_echo("z"),
             eval_bool("u <- TRUE", X4),
             eval_float("y <- 2.0", X5),
             eval_float("sin(1.0)", X7),
             eval_int("8 + 50", X8),
             io.write_float(X0,!IO), io.nl(!IO),
             io.write_string(X1, !IO), io.nl(!IO),
             io.write_int(X2,  !IO), io.nl(!IO),
             io.write(X3,  !IO), io.nl(!IO),
             write_bool(X4, !IO), io.nl(!IO),
             io.write_float(X5,  !IO), io.nl(!IO),
             io.write_float(X7,!IO), io.nl(!IO),
             io.write_int(X8,  !IO), io.nl(!IO),
             float_vect("c(1.0,2.0,3.0,4.0,5.0)", X9),
             lookup_float(3, X9, X12),
             lookup_float_size(X9, X14),
             int_vect("c(1,2,3,4)", X10),
             lookup_int(2, X10, X11),
             lookup_int_size(X10, X13),
             io.write_int(X11, !IO), io.nl(!IO),
             io.write_float(X12, !IO), io.nl(!IO),
             io.write_int(X13, !IO), io.nl(!IO),
             io.write_int(X14, !IO), io.nl(!IO),
             int_vect("invisible({
                data.table::fwrite(list(1:1E6), 'a.csv')
                  # note: we could avoid casting to numeric
                  # if evalInR alloaxed to return as integer vector.
                  # to be developed.
                as.numeric(data.table::fread('a.csv')[[1]])})", Column),
             lookup_int_size(Column, X15),io.nl(!IO),
             io.write_int(X15, !IO),io.nl(!IO),
             lookup_int(500000, Column, X16),
             io.write_int(X16, !IO),io.nl(!IO),
             stop_r .

%% %% Test Output

%% Select a line in a data.table
%%
%%     V1 V2
%% 1:  3  4

%% Read an R data.table, 1M lines

%% V1
%% 1:       1
%% 2:       2
%% 3:       3
%% 4:       4
%% 5:       5
%% ---
%% 999996:  999996
%% 999997:  999997
%% 999998:  999998
%% 999999:  999999
%% 1000000: 1000000

%% Select again
%%     V1 V2
%% 1:  2  3

%% Raw (impure) R Output

%% [1] "abcd"
%% eval z:  9
%% [1] 9

%% Mercury-managed output
%% 11.5
%% 9
%% 9
%% TRUE
%% 2.0
%% 0.8414709568023682
%% 58
%% 3
%% 4.0
%% 4
%% 5

%% R-Vector processing

%% 1000000
%% 500001

%% real	0m0,627s
%% user	0m0,381s
%% sys	0m0,054s

%% % TODO: expand this POC to other vectorized data structures.
