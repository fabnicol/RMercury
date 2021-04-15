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
:- import_module bool.

    % Mercury representation types for R vectors.

:- type float_buffer.
:- type string_buffer.
:- type int_buffer.

    % Catch-all Mercury type for any type of R vector.

:- type buffer.

    % Catch-all Mercury type for any type of R element.

:- type buffer_item.

    % Impure predicates for sourcing R scripts.
    % Should not be backtracked upon.
    % No Mercury output, just R-managed IO.
    %   source(RCode) : tries to suppress output.
    %   source_echo(RCode).

:- impure pred source(string::in) is det.
:- impure pred source_echo(string::in) is det.

    % Evaluation of R 'scalars' (1-dimensional vectors)
    % eval(RCode, T), where T is a basic type.
    % TODO: typeclass-constraints on T?

:- typeclass r_eval(T) where [
    pred eval(string::in, T::out) is det
].
    % Printer of booleans in R-type format (TRUE/FALSE)

:- pred write_bool(bool::in, io::di, io::uo) is det.

    % Predicates for marshalling R script return values
    % into Mercury code using the 'buffer' catch-all type.
    % Reminder: no scalars in R, only vectors.
    % Original R types are quite unsafe as R is dynamically
    % typed. Using explicit type coercion prefixed to the
    % names of the vector-catch predicates:
    %   <type>_vect(RCode, Buffer).

:- pred float_vect(string::in, buffer::out) is det.
:- pred int_vect(string::in, buffer::out) is det.
:- pred string_vect(string::in, buffer::out) is det.

    % Catch-all Mercury representation type

:- type buffer.

    % Helper predicates: boolean type identification, getters and setters
    % to and from catch-all type 'buffer' and its type-explicit instanciations.

    % Boolean predicates

:- pred is_int_buffer(buffer::in) is semidet.
:- pred is_float_buffer(buffer::in) is semidet.
:- pred is_string_buffer(buffer::in) is semidet.

    % Setter functions

:- func int_buffer(buffer) = int_buffer is semidet.
:- func float_buffer(buffer) = float_buffer is semidet.
:- func string_buffer(buffer) = string_buffer is semidet.

    % Getter predicates

:- pred int_buffer(int_buffer::in, buffer::out) is det.
:- pred float_buffer(float_buffer::in, buffer::out) is det.
:- pred string_buffer(string_buffer::in, buffer::out) is det.

:- impure pred main(io::di, io::uo) is det.

%----------------------------------------------%

:- implementation.

:- import_module char.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module math.
:- import_module string.

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
%% vector                 Possibly MR_ArrayPtr.
%%                        Currently replaced by type 'buffer'.

%-----------------------------------------------------------------------------%
% Evaluationg basic types for one-dimensional R vectors.
%

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
    % variables across calls.

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

SEXP res = evalInR(buf);
Y = (MR_String) CHAR(STRING_PTR(res)[0]);
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
MR_Float a = REAL(evalInR(X))[0];
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

write_bool(Value, !IO) :-
    ( if Value = yes
    then
        write_string("TRUE", !IO)
    else
        write_string("FALSE", !IO)
    ).

% ------------------------------------------------------------------------%
% Mercury processing of R Vectors
%

%-------------------------------------------------------------------------%

    % Catch-all Mercury representation types for R vectors and vector elements

    % Universal representation of vectors

:- type buffer
    --->     int(int_buffer)
    ;        float(float_buffer)
    ;        string(string_buffer).  % TODO: to be augmented.

    % Universal representation of vector elements

:- type buffer_item
    --->    float_base(float)
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

    % Mercury types corresponding to R integer(),  numeric() and character()
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

    % Setters to underlying 'buffer' sub-type

int_buffer(Buffer) = Value :- int(Value) = Buffer.

float_buffer(Buffer) = Value :- float(Value) = Buffer.

string_buffer(Buffer) = Value :- string(Value) = Buffer.

   % Getters to 'buffer' from underlying sub-type

int_buffer(Value, Buffer) :- int(Value) = Buffer.

float_buffer(Value, Buffer) :- float(Value) = Buffer.

string_buffer(Value, Buffer) :- string(Value) = Buffer.

%------------------------------------------------------------------------- %
% Mercury type representation for R vector types
%
% R is a dynamically-typed language whose returns are not type-safe.
% Sometimes integer vectors will be returned as double vectors,
% sometimes as integer vectors. Booleans and factors are often cast.
% We adopt an coertion approach: what counts is the target type fixed
% by the prefix [type_] and we regard Boolean, Real and Integer vectors
% in R as castable on return to the target Mercury type.
% Warning: while this is true for nnumeric types, it is not always so
% when the R source returns a string vector.
% TODO: implement exception processing.
%%

:- pred c_int_vect(string::in, int_buffer::out) is det.

int_vect(Code, Buffer) :-
    c_int_vect(Code, Buffer0),
    int_buffer(Buffer0, Buffer).

:- pragma foreign_proc("C",
    c_int_vect(X::in, Buffer::out),
    [will_not_call_mercury, promise_pure],
"
if (start) setupRinC();
start = 0;
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
").

    % c_float_vect(R_Code, float_buffer).
    %
    % Parse R code into a Mercury float_buffer.
    % Coerce logical and integer vectors into MR_Float.
    % Set Buffer as NULL on error.
    % TODO: implement Mercury correlates for error/exception and NULL.

:- pred c_float_vect(string::in, float_buffer::out) is det.

float_vect(Code, Buffer) :-
    c_float_vect(Code, Buffer0),
    float_buffer(Buffer0, Buffer).

:- pragma foreign_proc("C",
    c_float_vect(X::in, Buffer::out),
    [will_not_call_mercury, promise_pure],
"
if (start) setupRinC();
start = 0;
SEXP V;
V = evalInR(X);
if (! Rf_isVector(V)) {
    Buffer = NULL;   /* Cannot coerce */
    return;
}

if (Rf_isLogical(V) || Rf_isInteger(V) || Rf_isString(V)) {
    V = Rf_coerceVector(V, REALSXP);

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
").

    % c_string_vect(R_Code, string_buffer).
    %
    % Parse R code into a Mercury float_buffer.
    % Coerce logical and integer vectors into MR_Float.
    % Set Buffer as NULL on error.
    % TODO: implement Mercury correlates for error/exception and NULL.

:- pred c_string_vect(string::in, string_buffer::out) is det.

string_vect(Code, Buffer) :- c_string_vect(Code, Buffer0),
string_buffer(Buffer0, Buffer).

:- pragma foreign_proc("C",
    c_string_vect(X::in, Buffer::out),
    [will_not_call_mercury, promise_pure],
"
if (start) setupRinC();
start = 0;
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
").

:- pred lookup_int_vect_size(int_buffer::in, int::out) is det.

:- func lookup_int_vect_size(int_buffer) = int.

lookup_int_vect_size(Buffer) = Size :- lookup_int_vect_size(Buffer, Size).

:- pred lookup_float_vect_size(float_buffer::in, int::out) is det.

:- func lookup_float_vect_size(float_buffer) = int.

lookup_float_vect_size(Buffer) = Size :- lookup_float_vect_size(Buffer, Size).

:- pred lookup_string_vect_size(string_buffer::in, int::out) is det.

:- func lookup_string_vect_size(string_buffer) = int.

lookup_string_vect_size(Buffer) = Size :- lookup_string_vect_size(Buffer, Size).

:- pred lookup(int::in, buffer::in, buffer_item::out) is det.

:- pred lookup_int_vect(int::in, int_buffer::in, int::out) is det.

:- pred lookup_float_vect(int::in, float_buffer::in, float::out) is det.

:- pred lookup_string_vect(int::in, string_buffer::in, string::out) is det.

lookup(Index, Buffer, Item) :-
    (
        is_int_buffer(Buffer) ->
        (
            lookup_int_vect(Index, int_buffer(Buffer), Value)
            ->
            Item = int_base(Value)
        ;
            Item = int_base(0)
        )
    ;
        is_float_buffer(Buffer) ->
        (
            lookup_float_vect(Index, float_buffer(Buffer), Value)
            ->
            Item = float_base(Value)
        ;
            Item = float_base(0.0)
        )
    ;
        is_string_buffer(Buffer) ->
        (
            lookup_string_vect(Index, string_buffer(Buffer), Value)
            ->
            Item = string_base(Value)
        ;
            Item = string_base("")
        )
    ;
        Item = string_base("")
    ).

:- pred marshall_vect_to_list(int::in,
    int::in,
    buffer::in,
    list(buffer_item)::out) is det.

marshall_vect_to_list(Start, End, Buffer, L) :-
    S = length(Buffer),
    (  End < S, End >= 0, Start >= 0, Start =< End ->
        marshall_helper(Start, End, Buffer, length(Buffer), [], L)
    ;
        L = []
    ).

:- pred marshall_helper(int::in,
    int::in,
    buffer::in,
    int::in,
    list(buffer_item)::in,
    list(buffer_item)::out) is det.

marshall_helper(Start, Index, Buffer, Size, L0, L1) :-
    lookup(Index, Buffer, Value),
    L = [ Value | L0],
    (
        Index = Start -> L1 = L
    ;
        marshall_helper(Start, Index - 1, Buffer, Size, L, L1)
    ).

:- func marshall_vect_to_list(buffer) = list(buffer_item).

marshall_vect_to_list(Buffer) = List :-
    marshall_vect_to_list(0, length(Buffer) - 1, Buffer, List).

:- typeclass length(T)
where [
pred length(T, int),
mode length(in, out) is det,
func length(T) = int
].

:- typeclass to_list(U)
where [
pred to_list(int::in, int::in, buffer::in, list(U)::out) is det,
func to_list(buffer) = list(U)
].

:- instance to_list(buffer_item) where [
pred(to_list/4) is marshall_vect_to_list,
func(to_list/1) is marshall_vect_to_list
].

:- instance length(buffer) where [
pred(length/2) is lookup_buffer_vect_size,
func(length/1) is lookup_buffer_vect_size
].

:- pred write_item(buffer_item::in, io::di, io::uo) is det.

write_item(Item, !IO) :-
    (
        Item = int_base(Value) ->
        io.write_int(Value, !IO)
    ;
        Item = float_base(Value) ->
        io.write_float(Value, !IO)
    ;
        Item = string_base(Value) ->
        io.write_string(Value, !IO)
    ;
        io.nl(!IO)
    ).

:- pred lookup_buffer_vect_size(buffer::in, int::out) is det.

:- func lookup_buffer_vect_size(buffer) = int.

lookup_buffer_vect_size(Buffer, Value) :-
    (
        if is_float_buffer(Buffer)
        then
            (
                if  lookup_float_vect_size(float_buffer(Buffer), X)
                then
                    Value = X
                else
                    Value = 0
                )
            else if is_int_buffer(Buffer)
            then
                (
                    if  lookup_int_vect_size(int_buffer(Buffer), X)
                    then
                        Value = X
                    else
                        Value = 0
                    )
                else if is_string_buffer(Buffer)
                then
                    (
                        if lookup_string_vect_size(string_buffer(Buffer), X)
                        then
                            Value = X
                        else
                            Value = 0
                        )
                    else
                        Value = 0
                    ).

lookup_buffer_vect_size(Buffer) = Value :- lookup_buffer_vect_size(Buffer, Value).

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
    lookup(3, X9, X12),
    length(X9, X14),
    int_vect("c(1,2,3,4)", X10),
    lookup(2, X10, X11),
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
    lookup(1500000, Column, X16),
    write_item(X16, !IO),io.nl(!IO),
    string_vect("c(""abcd"", ""efgh"")", X17),
    lookup(1, X17, X18),
    write_item(X18, !IO),
    length(X17, X19),io.nl(!IO),
    write_int(X19, !IO),io.nl(!IO),
    int_vect("c(1234, 5678)", X20), % type coertion from int to string.
    lookup(0, X20, X21),
    write_item(X21, !IO),
    length(X20, X22),io.nl(!IO),
    write_int(X22, !IO),io.nl(!IO),
    % cast R string vectors to list(string).
    write_item(det_index0(to_list(X20), 0), !IO), io.nl(!IO),
    float_vect("c(""1234"", ""5678.0"",""4.5"")", X23),
    lookup(0, X23, X24),
    write_item(X24, !IO), io.nl(!IO),
    write_item(det_index0(to_list(X23), 0), !IO),
    io.nl(!IO),
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
