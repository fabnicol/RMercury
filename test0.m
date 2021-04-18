% File: test0.m Purpose: An R interface for Mercury: proof of concept.
% Copyright Fabrice Nicol <fabnicol@users.sourceforge.net>, 2021 The latest
% version can be found at http://github.com/fabnicol

% This program is free software; you can redistribute it and/or modify it under
% the terms of the GNU General Public License as published by the Free Software
% Foundation; either version 3 of the License, or (at your option) any later
% version.

% This program is distributed in the hope that it will be useful, but WITHOUT
% ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
% FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
% details.

% You should have received a copy of the GNU General Public License along with
% this program; if not, write to the Free Software Foundation, Inc., 675 Mass
% Ave, Cambridge, MA 02139, USA.

:- module test0.
:- interface.

:- import_module io.
:- import_module bool.

    % Mercury representation types for R vectors.

:- type float_buffer.
:- type string_buffer.
:- type int_buffer.

    % Catch-all Mercury type for any type of R vector.

:- type buffer.

    % Catch-all Mercury type for any type of R element.

:- type buffer_item.

    % Numeric behabior

:- type allow
    --->    allow(inf :: bool, nan :: bool).

    % More general behavior fields

:- type behavior
    --->    behavior(numeric :: allow). % to be augmented

% source_echo(RCode).
    % source(RCode) : tries to suppress output.
    %
    % Impure predicates for sourcing R scripts.
    % Should not be backtracked upon.
    % No Mercury output, just R-managed IO.

:- impure pred source(string::in) is det.
:- impure pred source_echo(string::in) is det.

    % eval(RCode, T), where T is a basic type.
    %
    % Evaluation of R 'scalars' (1-dimensional vectors)
    % TODO: typeclass-constraints on T?

:- typeclass r_eval(T) where [
    pred eval(string::in, T::out, io::di, io::uo) is det
].

    % <type>_vect(RCode, Buffer)
    %
    % Predicates for marshalling R script return values
    % into Mercury code using the 'buffer' catch-all type.
    % Reminder: no scalars in R, only vectors.
    % Original R types are quite unsafe as R is dynamically
    % typed. Using explicit type coercion prefixed to the
    % names of the vector-catch predicates:

:- pred float_vect(string::in, buffer::out) is det.
:- pred int_vect(string::in, buffer::out) is det.
:- pred string_vect(string::in, buffer::out) is det.

    % is_<type>_buffer(Buffer)
    %
    % Helper predicates: boolean type identification of Buffer.

:- pred is_int_buffer(buffer::in) is semidet.
:- pred is_float_buffer(buffer::in) is semidet.
:- pred is_string_buffer(buffer::in) is semidet.

    % <type>_buffer(Buffer) = Type_buffer
    %
    % Pseudo-cast of Buffer to its underlying type.

:- func int_buffer(buffer) = int_buffer is semidet.
:- func float_buffer(buffer) = float_buffer is semidet.
:- func string_buffer(buffer) = string_buffer is semidet.

    % <type>_buffer(Type_buffer, Buffer)
    %
    % Pseudo-cast of a typed buffer to an encapsulating 'buffer' type.

:- pred int_buffer(int_buffer::in, buffer::out) is det.
:- pred float_buffer(float_buffer::in, buffer::out) is det.
:- pred string_buffer(string_buffer::in, buffer::out) is det.

    % Object length accessor typeclass.

:- typeclass length(T).

    % Currently only implemented for type 'buffer'.

:- instance length(buffer).

    % lookup_<type>_vect(Typed_buffer, Index, T)
    %
    % Mercury typed buffer lookup predicates: get value of buffer
    % at 0-based index.
    % On error:
    % For numeric types, return 0 if buffer is not accessible, or index
    % is out of bounds [0, Size-1].
    % For strings, return NA_BUFFER for null C buffer, NA_BUFFER_CONTENTS
    % for null C buffer underlying data array, NA_BUFFER_SIZE for empty buffer
    % and NA_BUFFER_INDEX for index out of bounds [0, Size-1].
    % Note: More efficient for large data bases.
    % Warning: Currently empty R vectors are causing an error when accessed
    % in Mercury. This behavior should be changed.

:- pred lookup_int_vect(int_buffer::in, int::in, int::out) is det.
:- pred lookup_float_vect(float_buffer::in, int::in, float::out) is det.
:- pred lookup_string_vect(string_buffer::in, int::in, string::out) is det.

    % lookup(Buffer, Index, Buffer_Item)
    %
    % Same as the above using encapsulating 'buffer' and buffer_item' types.
    % If parsing large data chunks, preferably use the typed versions above.

:- pred lookup(buffer::in, int::in, buffer_item::out) is det.

    % Marshalling classes from R object representation to standard data types.
    %

   % Printer of booleans in R-type format (TRUE/FALSE)

:- pred write_bool(bool::in, io::di, io::uo) is det.

   % Printer helper for encapsulating type 'buffer_item'.

:- pred write_item(buffer_item::in, io::di, io::uo) is det.

:- impure pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module exception.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module math.
:- import_module string.

    % Predicates for sourcing and evaluating R code.  Predicates
    % performing C I/O, semipure or impure (pending evaluation).

:- pragma foreign_decl("C", "#include <RInside_C.h>").

:- pragma foreign_decl("C", "#include <Rinternals.h>").

%-----------------------------------------------------------------------------%
% Sourcing R code
%

:- pragma foreign_proc("C",
    source(X::in),
    [will_not_call_mercury],
"
setupRinC();
evalQuietlyInR(X);
teardownRinC();
").

:- pragma foreign_proc("C",
    source_echo(X::in),
    [will_not_call_mercury],
"
setupRinC();
Rf_PrintValue(evalInR(X));
teardownRinC();
").

%-----------------------------------------------------------------------------%
% Exception handling (bound check)
%

:- pred check_finite(string, pred(string, buffer_item), behavior, buffer_item, io, io).
:- mode check_finite(in, in, in, out, di, uo) is det.

check_finite(Code, Predicate, Behavior, Result, !IO) :-
    ( try [io(!IO)] (
        Predicate(Code, Ret)
    )
    then
        ( if  Ret = float_base(F)
        then
            (
                is_inf(F),
                Behavior^numeric^inf = no
            ;
                is_nan(F),
                Behavior^numeric^nan = no
            ),
            throw(domain_error("Returned +Infinity or NAN yet \
                +/- Infinity or NAN are not allowed."))
            %%
            % Here possibly add in other cases
            %
        else
           Result = Ret
        )
    catch_any Excp ->
            io.format("Returned: EXCP (%s)\n", [s(string(Excp))], !IO)
    ).

%-----------------------------------------------------------------------------%
% Evaluating basic types for one-dimensional R vectors.
%

:- instance r_eval(string) where [
    pred(eval/4) is eval_string
].

:- instance r_eval(int) where [
    pred(eval/4) is eval_int
].

:- instance r_eval(bool) where [
    pred(eval/4) is eval_bool
].

:- instance r_eval(float) where [
    pred(eval/4) is eval_float
].

     % r_eval instances use the standard numeric behavior
     % currently only relevant for floats.

:- pred eval_string(string::in, string::out, io::di, io::uo) is det.

eval_string(Code, Result, !IO) :-
    eval_string(Code, behavior(allow(yes, yes)), Ret, !IO),
    Ret = string_base(Result).

:- pred eval_int(string::in, int::out, io::di, io::uo) is det.

eval_int(Code, Result, !IO) :-
    eval_int(Code, behavior(allow(yes, yes)), Ret, !IO),
    Ret = int_base(Result).

:- pred eval_bool(string::in, bool::out, io::di, io::uo) is det.

eval_bool(Code, Result, !IO) :-
    eval_bool(Code, behavior(allow(yes, yes)), Ret, !IO),
    Ret = bool_base(Result).

:- pred eval_float(string::in, float::out, io::di, io::uo) is det.

eval_float(Code, Result, !IO) :-
    eval_float(Code, behavior(allow(yes, yes)), Ret, !IO),
    Ret = float_base(Result).

    % The encapsulating versions of eval_<type> with behavior and buffer_item
    % arguments will be exported when finalized.

:- pred eval_string(string::in, behavior::in,  buffer_item::out,
    io::di, io::uo) is det.

eval_string(Code, Behavior, Result, !IO) :-
    check_finite(Code,
        (pred(S::in, T::out) is det :-
            eval_string0(S, U), T = string_base(U)), Behavior, Result, !IO).

:- pred eval_string0(string::in, string::out) is det.

% RInside evalInR returns a SEXP, possibly NULL, or crashes.
% When NULL is returned, we substitute a NAN for floats or
% a 0 value for integral types, a FALSE/no for booleans, or
% an empty string/character for string/character types.


:- pragma foreign_proc("C", eval_string0(X::in, Y::out),
    [promise_pure, will_not_call_mercury],
"
setupRinC();
int S = strlen(X);
char buf[S + 100];
memset(buf, 0, 100 + S);
sprintf(buf, ""%s%s%s"",
    ""z<-try({"",X,""});\
if (inherits(z, 'try-error')) E <- 'R string error' else E <- z"");

SEXP res = evalInR(buf);
if (res == NULL)
    Y = "";
else
    Y = (MR_String) CHAR(STRING_PTR(res)[0]);
teardownRinC();
").

:- pred eval_int(string::in, behavior::in,  buffer_item::out,
    io::di, io::uo) is det.

eval_int(Code, Behavior, Result, !IO) :-
    check_finite(Code,
        (pred(S::in, T::out) is det :-
            eval_int0(S, U), T = int_base(U)), Behavior, Result, !IO).

:- pred eval_int0(string::in, int::out) is det.

:- pragma foreign_proc("C",
    eval_int0(X::in, Z::out),
    [promise_pure, will_not_call_mercury],
"
setupRinC();
SEXP res = evalInR(X);
if (res == NULL) {
  Z = 0;
  teardownRinC();
  return;
}

MR_Integer a = (MR_Integer) floor(REAL(res)[0]);
Z = (MR_Integer) a;
teardownRinC();
").

:- pred eval_float(string::in, behavior::in, buffer_item::out,
    io::di, io::uo) is det.
:- pred eval_float0(string::in, float::out) is det.

eval_float(Code, Behavior, Result, !IO) :-
    check_finite(Code,
        (pred(S::in, T::out) is det :-
            eval_float0(S, U), T = float_base(U)), Behavior, Result, !IO).

:- pragma foreign_proc("C",
    eval_float0(X::in, Z::out),
    [promise_pure, will_not_call_mercury],
"
// How to return NAN in C? There must be a Mercury-defined constant.
// like MR_NAN etc.
MR_Float a = REAL(evalInR(X))[0];
Z = (MR_Float) a;
teardownRinC();
").

:- pred eval_bool(string::in, behavior::in, buffer_item::out,
    io::di, io::uo) is det.
:- pred eval_bool0(string::in, bool::out) is det.

eval_bool(Code, Behavior, Result, !IO) :-
    check_finite(Code,
        (pred(S::in, T::out) is det :-
            eval_bool0(S, U), T = bool_base(U)), Behavior, Result, !IO).

:- pragma foreign_proc("C",
    eval_bool0(X::in, Z::out),
    [promise_pure, will_not_call_mercury],
"
SEXP res = evalInR(X);
if (res == NULL) {
  Z = MR_NO;
  teardownRinC();
  return;
}
bool a = LOGICAL(res)[0];
Z = a ? MR_YES : MR_NO;
teardownRinC();
").

% ------------------------------------------------------------------------%
% Mercury processing of R Vectors into foreign C-typed buffers.
%

    %-------------------------------------------------------------------------%
    % A note on R-to-Mercury type casting
    %
    % R is a dynamically-typed language whose returns are not type-safe.
    % Sometimes integer vectors will be returned as double vectors,
    % sometimes as integer vectors. Booleans and factors are often cast.
    % We adopt an coertion approach: what counts is the target type fixed
    % by the prefix [type_] and we regard Boolean, Real and Integer vectors
    % in R as castable on return to the target Mercury type,
    % using Rf_coerceVector.
    %
    % Warning: while this is true for numeric types, it is not always so
    % when the R source returns a string vector.
    % TODO: implement exception processing.
    %%

    % Catch-all Mercury representation types for R vectors and vector elements

    % Encapsulating 'buffer' type for vectors.

:- type buffer
    --->    int(int_buffer)
    ;       float(float_buffer)
    ;       string(string_buffer).  % TODO: to be augmented.

    % Encapsulating 'buffer_item' for vector elements.

:- type buffer_item
    --->    bool_base(bool)
    ;       float_base(float)
    ;       int_base(int)
    ;       string_base(string).    % TODO: to be augmented.

    % Foreign C code
    % Note that C-structures shoud not contain pointers
    % beyond the first word, otherwise the GC will lose track.
    % A first-rank Integer field is OK.

:- pragma foreign_decl("C",
"
#include ""mercury_float.h""    /* For MR_FLT_FMT. */
#include ""mercury_memory.h""
#include ""mercury_string.h""
#include ""Rdefines.h""

#include <stdio.h>  /* For sscanf. */

typedef struct {
    MR_Integer  size;
    MR_Integer  *contents;
} INT_BUFFER;

typedef struct {
    MR_Integer  size;
    MR_Float   *contents;
} FLOAT_BUFFER;

typedef struct {
    MR_Integer  size;
    MR_String   *contents;
} STRING_BUFFER;

#define ASSIGN_SIZE(Value, Buffer) \
do  {  if (Buffer == NULL) \
    Value = 0;  \
else  \
    Value = (MR_Integer) Buffer->size; } while(0)
").

    % Mercury types corresponding to R integer(), numeric() and character().
    % with respective types int_buffer, float_buffer and string_buffer.

:- pragma foreign_type("C", int_buffer, "INT_BUFFER *",
    [can_pass_as_mercury_type]).

:- pragma foreign_type("C", float_buffer, "FLOAT_BUFFER *",
    [can_pass_as_mercury_type]).

:- pragma foreign_type("C", string_buffer, "STRING_BUFFER *",
    [can_pass_as_mercury_type]).

    % Boolean helper predicates to test 'buffer' underlying sub-type

is_int_buffer(int(_)).

is_float_buffer(float(_)).

is_string_buffer(string(_)).

    % Pseudo-cast to underlying type of 'buffer'

int_buffer(Buffer) = Value :- int(Value) = Buffer.

float_buffer(Buffer) = Value :- float(Value) = Buffer.

string_buffer(Buffer) = Value :- string(Value) = Buffer.

   % Pseudo-cast to 'buffer' from underlying sub-type

int_buffer(Value, Buffer) :- int(Value) = Buffer.

float_buffer(Value, Buffer) :- float(Value) = Buffer.

string_buffer(Value, Buffer) :- string(Value) = Buffer.

    % c_int_vect(R_Code, int_buffer).
    %
    % Parse R code into a Mercury int_buffer.
    %
    % Coerce logical, string and integer vectors into MR_Integer*.
    % Set Buffer as NULL on error.
    %
    % TODO: implement Mercury correlates for error/exception and NULL.
    % WARNING: ill-undertood issue if not casting R returns to double,
    %          even when vector is integer in R code.

:- pred c_int_vect(string::in, int_buffer::out) is det.

int_vect(Code, Buffer) :-
    c_int_vect(Code, Buffer0),
    int_buffer(Buffer0, Buffer).

:- pragma foreign_proc("C",
    c_int_vect(X::in, Buffer::out),
    [will_not_call_mercury, promise_pure],
"
setupRinC();
SEXP V = evalInR(X);
if (! Rf_isVector(V)) {
    Buffer = NULL;   /* Cannot coerce */
    return;
}

/* See above comments wrt PROTECT/UNPROTECT */

/* For poorly understoof reasons, if V is an INTSXP,
   or any other type than REALSXP, it will not survive
   garbage collection.
   This might have been to do with the interplay of Mercury
   and R-allocated memory.
   So we cast to REALSXP before casting again to MR_Integer.
*  TODO: try to fix this costly recast.
*/

if (Rf_isLogical(V) || Rf_isInteger(V) || Rf_isString(V)) {
   V = Rf_coerceVector(V, REALSXP);

   /* PROTECT(V = Rf_coerceVector(V, REALSXP)); */
}

if (! Rf_isReal(V)) {
    /* UNPROTECT(1); */
    Buffer = NULL;   /* Cannot coerce */
    return;
}

Buffer = MR_GC_NEW(INT_BUFFER);
MR_Integer S = LENGTH(V);
Buffer->size = S;

MR_Float *V1 = REAL(V);
Buffer->contents = MR_GC_malloc(sizeof(MR_Integer) * S);
for (int i = 0; i < S; ++i)
Buffer->contents[i] = floor(V1[i]);

/* UNPROTECT(1); */
teardownRinC();
").

    % c_float_vect(R_Code, float_buffer).
    %
    % Parse R code into a Mercury float_buffer.
    %
    % Coerce logical, string and integer vectors into MR_Float*.
    % Set Buffer as NULL on error.
    %
    % TODO: implement Mercury correlates for error/exception and NULL.

:- pred c_float_vect(string::in, float_buffer::out) is det.

float_vect(Code, Buffer) :-
    c_float_vect(Code, Buffer0),
    float_buffer(Buffer0, Buffer).

:- pragma foreign_proc("C",
    c_float_vect(X::in, Buffer::out),
    [will_not_call_mercury, promise_pure],
"
setupRinC();
SEXP V;
V = evalInR(X);
if (! Rf_isVector(V)) {
    Buffer = NULL;   /* Cannot coerce */
    return;
}

if (Rf_isLogical(V) || Rf_isInteger(V) || Rf_isString(V)) {
H    V = Rf_coerceVector(V, REALSXP);

    /* PROTECT should be a non-op as the R runtime
    * is not fired, so the R GC is not either.
    * Using PROTECT in case code chunk
    * is ported back to R C-library code. */
    /* PROTECT(V = Rf_coerceVector(V, REALSXP)); */

} else if (! Rf_isReal(V)) {
    Buffer = NULL;   /* Cannot coerce */
    return;
}

/* REAL returns a double.
*  We are casting to MR_Float yet this should be
*  investigated a bit further */

MR_Float *Items = (MR_Float *) REAL(V);
Buffer = MR_GC_NEW(FLOAT_BUFFER);
Buffer->size = LENGTH(V);

/* TODO:
*  1. Does the GC effectively collect the R-internally
*     allocated chunk when float_buffer is freed?
*  2. Using 'MR_GC_register_finalizer'?
*/

Buffer->contents = Items;

/* Normally a non-op in Mercury C-code.
* To be used if ported back */

/* UNPROTECT(1); */
teardownRinC();
").

    % c_string_vect(R_Code, string_buffer).
    %
    % Parse R code into a Mercury string_buffer.
    %
    % Coerce logical, integer and real vectors into MR_String*.
    % Set Buffer as NULL on error.
    % TODO: implement Mercury correlates for error/exception and NULL.

:- pred c_string_vect(string::in, string_buffer::out) is det.

string_vect(Code, Buffer) :- c_string_vect(Code, Buffer0),
string_buffer(Buffer0, Buffer).

:- pragma foreign_proc("C",
    c_string_vect(X::in, Buffer::out),
    [will_not_call_mercury, promise_pure],
"
setupRinC();
SEXP V = evalInR(X);
if (V == NULL || ! Rf_isVector(V)) {
    Buffer = NULL;   /* Cannot coerce */
    return;
}

/* See above comments wrt PROTECT/UNPROTECT */

if (! Rf_isString(V)) {
    if (Rf_isLogical(V) || Rf_isInteger(V) || Rf_isReal(V)) {
        V = Rf_coerceVector(V, STRSXP);
       /* PROTECT(V = Rf_coerceVector(V, STRSXP)); */
    } else {
        /* UNPROTECT(1); */
        Buffer = NULL;   /* Cannot coerce */
        return;
    }
}

if (V == NULL || ! Rf_isString(V)) {
    Buffer = NULL;   /* Cannot coerce */
    return;
}

Buffer = MR_GC_NEW(STRING_BUFFER);
MR_Integer S = LENGTH(V);
Buffer->size = S;
SEXP *ptr = STRING_PTR(V);
Buffer->contents = MR_GC_malloc(sizeof(MR_String) * S);
for (int i = 0; i < S; ++i) {
    Buffer->contents[i] = CHAR(ptr[i]);
}
/* UNPROTECT(1); */
teardownRinC();
").

    %-------------------------------------------------------------------------%
    % Mercury accessors to buffer-type representation of R vectors.
    %

    % Size accessors
    %

    % lookup_<type>_vect_size(Typed_buffer, Size)
    % lookup_<type>_vect_size(Type_buffer) = Size
    %
    % Vector size getter predicates and functions, typed version.

:- pred lookup_int_vect_size(int_buffer::in, int::out) is det.
:- func lookup_int_vect_size(int_buffer) = int.
:- pred lookup_float_vect_size(float_buffer::in, int::out) is det.
:- func lookup_float_vect_size(float_buffer) = int.
:- pred lookup_string_vect_size(string_buffer::in, int::out) is det.
:- func lookup_string_vect_size(string_buffer) = int.

lookup_int_vect_size(Buffer) = Size :-
    lookup_int_vect_size(Buffer, Size).

lookup_float_vect_size(Buffer) = Size :-
    lookup_float_vect_size(Buffer, Size).

lookup_string_vect_size(Buffer) = Size :-
    lookup_string_vect_size(Buffer, Size).

    % Implementation C code

:- pragma foreign_proc("C",
    lookup_int_vect_size(Buffer::in, Value::out),
    [will_not_call_mercury, promise_pure],
" ASSIGN_SIZE(Value, Buffer);").

:- pragma foreign_proc("C",
    lookup_float_vect_size(Buffer::in, Value::out),
    [will_not_call_mercury, promise_pure],
" ASSIGN_SIZE(Value, Buffer);").

:- pragma foreign_proc("C",
    lookup_string_vect_size(Buffer::in, Value::out),
    [will_not_call_mercury, promise_pure],
" ASSIGN_SIZE(Value, Buffer);").

    % lookup_buffer_vect_size(Buffer, Size)
    %
    % Same as above for the encapsulating 'buffer' type.

:- pred lookup_buffer_vect_size(buffer::in, int::out) is det.
:- func lookup_buffer_vect_size(buffer) = int.

lookup_buffer_vect_size(Buffer, Value) :-
    ( if
        is_float_buffer(Buffer)
    then
        ( if
            lookup_float_vect_size(float_buffer(Buffer), X)
        then
            Value = X
        else
            Value = 0
        )
    else if
        is_int_buffer(Buffer)
    then
        ( if
            lookup_int_vect_size(int_buffer(Buffer), X)
        then
            Value = X
        else
            Value = 0
        )
    else if
        is_string_buffer(Buffer)
    then
        ( if
            lookup_string_vect_size(string_buffer(Buffer), X)
         then
              Value = X
         else
              Value = 0
         )
    else
        Value = 0
    ).

lookup_buffer_vect_size(Buffer) = Value :-
    lookup_buffer_vect_size(Buffer, Value).

    % Using typeclass 'length' for a more polymorphic interface.
    % Later to be expanded with other types than 'buffer'.

:- typeclass length(T) where [
       pred length(T, int),
       mode length(in, out) is det,
       func length(T) = int
].

:- instance length(buffer) where [
       pred(length/2) is lookup_buffer_vect_size,
       func(length/1) is lookup_buffer_vect_size
].

    %-------------------------------------------------------------------------%
    % Vector item lookup accessors.
    %

    % C code implementation

:- pragma foreign_proc("C",
    lookup_int_vect(Index::in, Buffer::in, Value::out),
    [will_not_call_mercury, promise_pure],
"
if (Buffer == NULL
      || Buffer->contents == NULL
      || Buffer->size <= 0
      || Index < 0)

    Value = 0;
else
    Value=(MR_Integer) Buffer->contents[Index];
").

:- pragma foreign_proc("C",
    lookup_float_vect(Index::in, Buffer::in, Value::out),
    [will_not_call_mercury, promise_pure],
"
if (Buffer == NULL
      || Buffer->contents == NULL
      || Buffer->size <= 0
      || Index < 0)

    Value = 0;
else
    Value = (MR_Float) Buffer->contents[Index];
").

:- pragma foreign_proc("C",
    lookup_string_vect(Index::in, Buffer::in, Value::out),
    [will_not_call_mercury, promise_pure],
"
if (Buffer == NULL)
    Value = ""NA_BUFFER"";
else if (Buffer->contents == NULL)
    Value = ""NA_BUFFER_CONTENTS"";
else if (Buffer->size <= 0)
    Value = ""NA_BUFFER_SIZE"";
else if (Index < 0 || Index > Buffer->size - 1)
    Value = ""NA_BUFFER_INDEX"";
else
    Value = (MR_String) Buffer->contents[Index];
").

    % Vector item lookup predicate for 'buffer'-type vectors
    %
    % Index is zero-based.

lookup(Buffer, Index, Item) :-
    ( if
        is_int_buffer(Buffer)
    then
        ( if
            lookup_int_vect(int_buffer(Buffer), Index, Value)
        then
            Item = int_base(Value)
        else
            Item = int_base(0)
        )
    else if
        is_float_buffer(Buffer)
    then
        ( if
            lookup_float_vect(float_buffer(Buffer), Index, Value)
        then
            Item = float_base(Value)
        else
            Item = float_base(0.0)
        )
    else if
        is_string_buffer(Buffer)
    then
        ( if
            lookup_string_vect(string_buffer(Buffer), Index, Value)
        then
            Item = string_base(Value)
        else
            Item = string_base("")
        )
    else
        Item = string_base("")
    ).

%-----------------------------------------------------------------------------%
% Marshalling to Mercury data types
%

    %%
    % To list
    %

:- pred marshall_vect_to_list(int::in, int::in, buffer::in,
                              list(buffer_item)::out) is det.

marshall_vect_to_list(Start, End, Buffer, L) :-
    S = length(Buffer),
    ( if
        End < S, End >= 0, Start >= 0, Start =< End
    then
        marshall_helper(Start, End, Buffer, length(Buffer), [], L)
    else
        L = []
    ).

:- pred marshall_helper(int::in, int::in, buffer::in, int::in,
                        list(buffer_item)::in, list(buffer_item)::out) is det.

marshall_helper(Start, Index, Buffer, Size, L0, L1) :-
    lookup(Buffer, Index, Value),
    L = [ Value | L0],
    ( if
        Index = Start
    then
        L1 = L
    else
        marshall_helper(Start, Index - 1, Buffer, Size, L, L1)
    ).

:- func marshall_vect_to_list(buffer) = list(buffer_item).

marshall_vect_to_list(Buffer) = List :-
    marshall_vect_to_list(0, length(Buffer) - 1, Buffer, List).

:- typeclass to_list(U, V) where [
    pred to_list(int::in, int::in, U::in, list(V)::out) is det,
    func to_list(U) = list(V)
].

    % Currently to_list is  only instantiated for 'buffer' types
    % This should change when moving on to dates, events etc.

:- instance to_list(buffer, buffer_item) where [
    pred(to_list/4) is marshall_vect_to_list,
    func(to_list/1) is marshall_vect_to_list
].


%-----------------------------------------------------------------------------%
% Print helpers

    % Print helper for R booleans in R output format.
    % Is this really useful? Hum.

write_bool(Value, !IO) :-
    ( if
        Value = yes
    then
        write_string("TRUE", !IO)
    else
        write_string("FALSE", !IO)
    ).

    % Print helper for catch-all type 'buffer_item'

write_item(Item, !IO) :-
    ( if
        Item = int_base(Value)
    then
        io.write_int(Value, !IO)
    else if
        Item = float_base(Value)
    then
        io.write_float(Value, !IO)
    else if
        Item = string_base(Value)
    then
        io.write_string(Value, !IO)
    else
        io.nl(!IO)
    ).
%-----------------------------------------------------------------------------%
% It should be easy to convert int_buffer or float_buffer into arrays or lists,
% using item setters and iterating.
% Yet ideally it would be nice to allow the 'elements' C-array
% to take ownership of int/float_buffer 'contents' C-array
% without a further copy operation.

%-----------------------------------------------------------------------------%

%-----------------------------------------------------------------------------%
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
    eval("y <- 12.0; z <- 11.5", X0, !IO),
    eval_string("y <- ""abcd""", X1, !IO),
    impure source_echo("E"),
    eval_int("z <- 9", X2, !IO),
    eval_int("cat('eval z: ', z, '\\n'); z", X3, !IO),
    impure source_echo("z"),
    eval_bool("u <- TRUE", X4, !IO),
    eval_float("y <- 2.0", X5, !IO),
    eval_float("sin(1.0)", X7, !IO),
    eval_int("8 + 50", X8, !IO),
    io.write_float(X0,!IO), io.nl(!IO),
    io.write_string(X1, !IO), io.nl(!IO),
    io.write_int(X2,  !IO), io.nl(!IO),
    io.write(X3,  !IO), io.nl(!IO),
    write_bool(X4, !IO), io.nl(!IO),
    io.write_float(X5,  !IO), io.nl(!IO),
    io.write_float(X7,!IO), io.nl(!IO),
    io.write_int(X8,  !IO), io.nl(!IO),
    float_vect("c(1.0,2.0,3.0,4.0,5.0)", X9),
    lookup(X9, 2, X12),
    length(X9, X14),
    int_vect("c(1,2,3,4)", X10),
    lookup(X10, 2, X11),
    length(X10, X13),
    write_item(X11, !IO), io.nl(!IO),
    write_item(X12, !IO), io.nl(!IO),
    write_int(X13, !IO), io.nl(!IO),
    write_int(X14, !IO), io.nl(!IO),
    %% note: testing the R internals type coertion
    %% primitive: integer -> double -> MR_Float -> 'float'
    %% primitive: logical -> double -> MR_Float -> 'float'
    float_vect("invisible({
        data.table::fwrite(list(c(rep(FALSE,1E6), rep(TRUE,1E6))), 'a.csv')
        as.numeric(data.table::fread('a.csv')[[1]])})", Column),
    length(Column, X15),io.nl(!IO),
    write_int(X15, !IO),io.nl(!IO),
    lookup(Column, 15000, X16),
    write_item(X16, !IO),io.nl(!IO),
    string_vect("c(""abcd"", ""efgh"")", X17),
    lookup(X17, 1, X18),
    write_item(X18, !IO),
    length(X17, X19),io.nl(!IO),
    write_int(X19, !IO),io.nl(!IO),
    int_vect("c(1234, 5678)", X20), % type coertion from int to string.
    lookup(X20, 0, X21),
    write_item(X21, !IO),
    length(X20, X22),io.nl(!IO),
    write_int(X22, !IO),io.nl(!IO),
    % cast R string vectors to list(string).
    write_item(det_index0(to_list(X20), 0), !IO), io.nl(!IO),
    float_vect("c(""1234"", ""5678.0"",""4.5"")", X23),
    lookup(X23, 0, X24),
    write_item(X24, !IO), io.nl(!IO),
    write_item(det_index0(to_list(X23), 0), !IO),
    io.nl(!IO).

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
