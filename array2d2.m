%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2003, 2005-2007, 2011-2012 The University of Melbourne.
% Copyright (C) 2013-2018 The Mercury team.
% Copyright (C) 2022 Fabrice Nicol (adaptations from array2d.m).
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: array2d2.m.
% Author: Ralph Becket <rafe@cs.mu.oz.au>
% Adapted by Fabrice Nicol<fabrnicol@gmail.com> (2022).
% Stability: medium-low.
%
% Two-dimensional rectangular array (i.e. not ragged) in column-major order.
%
% XXX The same caveats re: uniqueness of arrays apply to array2d2s.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module array2d2.
:- interface.

:- import_module array.
:- import_module array2d.
:- import_module list.

%---------------------------------------------------------------------------%

    % An array2d2 is a two-dimensional array stored in column-major order
    % (that is, the elements of the first column in left-to-right
    % order, followed by the elements of the second column and so forth.)
    %
:- type array2d2(T).

:- inst array2d2 for array2d2/1
    --->    array2d2(ground, ground, array).

    % XXX These are work-arounds until we get nested uniqueness working.
    %
:- mode array2d2_di == di(array2d2).
:- mode array2d2_ui == in(array2d2).
:- mode array2d2_uo == out(array2d2).

    % init(NumRows, NumColumns, Elem):
    % Creates a 2d array with the given numbers of rows and columns
    % whose every element is set to Elem.
    %
    % Throws an exception if either NumRows or NumColumns is negative.
    %
:- func init(int, int, T) = array2d2(T).
:- mode init(in, in, in) = array2d2_uo is det.

    % array2d2([[X11, ..., XM1], ..., [X1N, ..., XMN]]) constructs an
    % array2d2 of size M * N, with M number of lines and N number of
    % columns, with the special case that bounds(array2d2([]), 0, 0).
    %
    % In other words, the elements of the top level list each represent
    % one column, and each column is itself a list of the values in the
    % rows of that columns.
    %
    % Throws an exception unless all columns have the same number of rows.
    %
:- func array2d2(list(list(T))) = array2d2(T).
:- mode array2d2(in) = array2d2_uo is det.

    % A synonym for the array2d2 function above.
    %
:- func from_lists(list(list(T))) = array2d2(T).
:- mode from_lists(in) = array2d2_uo is det.

    % from_array(NumRows, NumColumns, Array) constructs an array2d2
    % of size NumRows * NumColumns where the elements are taken from Array
    % in column-major order, i.e. the element at row R column C is taken from
    % Array at index (C * NumRows + R). Indices start from zero.
    %
    % Throws an exception if NumRows < 0 or NumColumns < 0, or if
    % the number of elements in Array does not equal NumRows * NumColumns.
    %
:- func from_array(int, int, array(T)) = array2d2(T).
:- mode from_array(in, in, array_di) = array2d2_uo is det.

    % from_array2d(Array) constructs an array2d2 from an array2d.
    %

:- func from_array2d(array2d(T)) = array2d2(T).
:- mode from_array2d(array2d_di) = array2d2_uo is det.

   % to_array2d(Array) constructs an array2d from an array2d2.
   %
   % For every array2d2 Array, from_array2d(to_array2d(Array)) = Array.
   % For every array2d  Array, to_array2d(from_array2d(Array)) = Array.

:- func to_array2d(array2d2(T)) = array2d(T).
:- mode to_array2d(array2d2_di) = array2d_uo is det.

    % is_empty(Array):
    % True iff Array contains zero elements.
    %
:- pred is_empty(array2d2(T)).
% :- mode is_empty(array2d2_ui) is semidet.
:- mode is_empty(in) is semidet.

    % bounds(Array, NumRows, NumColumns):
    %
    % Returns the number of rows and columns in the given 2d array.
    %
:- pred bounds(array2d2(T), int, int).
% :- mode bounds(array2d2_ui, out, out) is det.
:- mode bounds(in, out, out) is det.

    % in_bounds(Array, R, C):
    %
    % Succeeds if and only if 0 =< C < NumRows, 0 =< C < NumColumns.
    %
:- pred in_bounds(array2d2(T), int, int).
% :- mode in_bounds(array2d2_ui, in, in) is semidet.
:- mode in_bounds(in, in, in) is semidet.

    % lookup(Array, R, C):
    %
    % Given a 2d array Array with NumRows rows and NumColumns columns,
    % return the element at row R and column C. Indices start at zero.
    %
    % This function requires 0 =< R < NumRows and 0 =< C < NumColumns.
    % If this requirement is not satisfied, this function will throw
    % an exception.
    %
:- func lookup(array2d2(T), int, int) = T.
% :- mode lookup(array2d2_ui, in, in) = out is det.
:- mode lookup(in, in, in) = out is det.
:- pred lookup(array2d2(T), int, int, T).
% :- mode lookup(array2d2_ui, in, in, out) is det.
:- mode lookup(in, in, in, out) is det.
:- func array2d2(T) ^ elem(int, int) = T.
% :- mode array2d2_ui ^ elem(in, in) = out is det.
:- mode in ^ elem(in, in) = out is det.

    % unsafe_lookup(Array, R, C):
    %
    % Given a 2d array Array with NumRows rows and NumColumns columns,
    % return the element at row R and column C. Indices start at zero.
    %
    % This function requires 0 =< R < NumRows and 0 =< C < NumColumns.
    % If this requirement is not satisfied, the behavior of this function
    % is undefined.
    %
:- func unsafe_lookup(array2d2(T), int, int) = T.
% :- mode unsafe_lookup(array2d2_ui, in, in) = out is det.
:- mode unsafe_lookup(in, in, in) = out is det.
:- pred unsafe_lookup(array2d2(T), int, int, T).
% :- mode unsafe_lookup(array2d2_ui, in, in, out) is det.
:- mode unsafe_lookup(in, in, in, out) is det.
:- func array2d2(T) ^ unsafe_elem(int, int) = T.
% :- mode array2d2_ui ^ unsafe_elem(in, in) = out is det.
:- mode in ^ unsafe_elem(in, in) = out is det.

    % set(R, C, NewElem, Array0, Array):
    %
    % Return Array, which differs from Array0 only in that
    % the value at row R and column C is NewElem.
    %
    % Throws an exception unless 0 =< R < NumRows, 0 =< C < NumColumns.
    %
:- pred set(int, int, T, array2d2(T), array2d2(T)).
:- mode set(in, in, in, array2d2_di, array2d2_uo) is det.
:- func (array2d2(T) ^ elem(int, int) := T) = array2d2(T).
:- mode (array2d2_di ^ elem(in, in) := in) = array2d2_uo is det.

    % unsafe_set(R, C, NewElem, Array0, Array):
    %
    % Return Array, which differs from Array0 only in that
    % the value at row R and column C is NewElem.
    %
    % The behavior is defined only if 0 =< R < NumRows, 0 =< C < NumColumns.
    %
:- pred unsafe_set(int, int, T, array2d2(T), array2d2(T)).
:- mode unsafe_set(in, in, in, array2d2_di, array2d2_uo) is det.
:- func (array2d2(T) ^ unsafe_elem(int, int) := T ) = array2d2(T).
:- mode (array2d2_di ^ unsafe_elem(in, in) := in) = array2d2_uo is det.


    % lists(Array):
    %
    % Return the contents of the given 2d array as a list of columns,
    % with each column containing the values in its rows.
    %
    % This function is the converse of from_lists.
    % For every Array, from_lists(lists(Array) = Array,
    % and for every Lists such as from_lists(Lists) does not throw
    % an exception, lists(from_lists(Lists)) = Lists.
    %
:- func lists(array2d2(T)) = list(list(T)).
% :- mode lists(array2d2_ui) = out is det.
:- mode lists(in) = out is det.

    % fill(Item, !Array):
    % Sets every element of the array to Item.
    %
:- pred fill(T::in, array2d2(T)::array2d2_di, array2d2(T)::array2d2_uo) is det.

% ---- Array transpose utility ---------------------------------------------%

    % transpose_array2d(Array):
    % 
    % Return the transposed given 2d array. 
    % If Array of dimension (M, N) has the standard list representation:
    %
    %    [[X11, ..., X1N], ..., [XM1, ..., XMN]]
    %
    % then the returned array has the same list representation
    % in column-major order (if mapped as a 2d2 array).
    %

:- func transpose_array2d(array2d(T)) = array2d(T).

    % transpose_array2d2(Array):
    % 
    % Return the transposed given 2d2 column-major array. 
    % If Array of dimension (M, N) has the standard list representation:
    %
    %    [[X11, ..., X1M], ..., [XN1, ..., XNM]]
    %
    % then the returned array has the same list representation
    % in row-major order (if mapped as a 2d array).
    %
    
:- func transpose_array2d2(array2d2(T)) = array2d2(T).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module require.
:- import_module int.

:- interface.

    % This should be abstract, but needs to be exported for insts.
    %
:- type array2d2(T)
    --->    array2d2(
                % The number of rows.
                int,

                % The number of columns.
                int,

                % The contents of the 2d array, flattened out.
                % It stores the element at row R and column C at
                % index (C * NumRows) + R in the flattened array.
                array(T)
            ).

:- implementation.


%---------------------------------------------------------------------------%

init(NumRows, NumColumns, Elem) =
    ( if NumRows >= 0, NumColumns >= 0 then
        array2d2(NumRows, NumColumns, array.init(NumRows * NumColumns, Elem))
    else
        func_error($pred, "bounds must be non-negative")
    ).

%---------------------------------------------------------------------------%

array2d2(Rows) = from_lists(Rows).

from_lists([]) = array2d2(0, 0, make_empty_array).
from_lists(Columns @ [FirstColumn | _]) = Array :-
    NumColumns = list.length(Columns),
    NumRows = list.length(FirstColumn),
    ( if
        all [Column] (
            list.member(Column, Columns)
        =>
            list.length(Column) = NumRows
        )
    then
        A = array(list.condense(Columns)),
        Array = array2d2(NumRows, NumColumns, A)
    else
        error($pred, "non-rectangular list of lists")
    ).

from_array(NumRows, NumColumns, Array) = Array2d :-
    ( if
        NumRows >= 0,
        NumColumns >= 0
    then
        array.size(Array, Size),
        compare(Result, Size, NumRows * NumColumns),
        (
            Result = (=),
            Array2d = array2d2(NumRows, NumColumns, Array)
        ;
            Result = (>),
            error($pred, "too many elements")
        ;
            Result = (<),
            error($pred, "too few elements")
        )
    else
        error($pred, " bounds must be non-negative")
    ).

%--- Conversions between array2d and array2d2 ------------------------------%
    
from_array2d(Array2d) = array2d2(lists(transpose_array2d(Array2d))).

to_array2d(Array2d2) = array2d(lists(transpose_array2d2(Array2d2))).

%--- Array transpose utility -----------------------------------------------%
 
% For column-major arrays, use NumRows as N in the inverse transpose
% Cate & Twigg formula.    

transpose_array2d2(Array) = TransposedArray :-
    Array = array2d2(NumRows, NumCols, A),
    transpose_array(A, NumRows) = TA,
    TransposedArray = array2d2(NumCols, NumRows, TA).

% For row-major arrays, use NumCols as N in the inverse transpose
% Cate & Twigg formula.    

transpose_array2d(Array) = TransposedArray :-
    Array = array2d(NumRows, NumCols, A),
    transpose_array(A, NumCols) = TA,
    TransposedArray = array2d(NumCols, NumRows, TA).
    
:- func transpose_array(array(T), int) = array(T).

% Note: The implementation is inefficient as it does not use cycle 
% properties or in-cache memory optimization. 
% We are using the simple Cate & Twigg (inverse transpose) formula.

transpose_array(Array, N) = TransposedArray :-
    Size = size(Array),
    array(map(lookup(Array),
          map(transpose_index(N, Size), 0 `..` (Size - 1))))
        = TransposedArray.

:- func transpose_index(int, int, int) = int.

transpose_index(N, Size, Index) = Result :-
    ( if Index = Size -1 then
        Result = Size - 1
    else
        Result = (N * Index) mod (Size -1)
    ).

%---------------------------------------------------------------------------%

is_empty(array2d2(_, _, A)) :-
    array.is_empty(A).

%---------------------------------------------------------------------------%

bounds(array2d2(NumRows, NumColumns, _A), NumRows, NumColumns).

%---------------------------------------------------------------------------%

in_bounds(array2d2(NumRows, NumColumns, _A), R, C) :-
    0 =< R, R < NumRows,
    0 =< C, C < NumColumns.

%---------------------------------------------------------------------------%

lookup(Array, R, C) = Elem :-
    ( if in_bounds(Array, R, C) then
        Elem = unsafe_lookup(Array, R, C)
    else
        error($pred, "indices out of bounds")
    ).

lookup(Array, R, C, Elem) :-
    Elem = lookup(Array, R, C).

Array ^ elem(R, C) =
    lookup(Array, R, C).

%---------------------------------------------------------------------------%

unsafe_lookup(Array, R, C) = Elem :-
    Array = array2d2(NumRows, _, A),
    array.unsafe_lookup(A, (C * NumRows) + R, Elem).

unsafe_lookup(Array, R, C, Elem) :-
    Elem = unsafe_lookup(Array, R, C).

Array ^ unsafe_elem(R, C) =
    unsafe_lookup(Array, R, C).

%---------------------------------------------------------------------------%

set(R, C, Value, !Array) :-
    ( if in_bounds(!.Array, R, C) then
        unsafe_set(R, C, Value, !Array)
    else
        error($pred, "indices out of bounds")
    ).

( Array0 ^ elem(R, C) := Value ) = Array :-
    set(R, C, Value, Array0, Array).

%---------------------------------------------------------------------------%

unsafe_set(R, C, Value, !Array) :-
    !.Array = array2d2(NumRows, NumColumns, A0),
    array.unsafe_set((C * NumRows) + R, Value, A0, A),
    !:Array = array2d2(NumRows, NumColumns, A).

( Array0 ^ unsafe_elem(R, C) := Value ) = Array :-
    unsafe_set(R, C, Value, Array0, Array).

%---------------------------------------------------------------------------%

lists(array2d2(NumRows, NumColumns, A)) = Columns :-
    get_columns(NumRows, NumColumns - 1, A, [], Columns).

:- pred get_columns(int, int, array(T), list(list(T)), list(list(T))).
% :- mode get_columns(in, in, array_ui, in, out) is det.
:- mode get_columns(in, in, in, in, out) is det.

get_columns(NumRows, ColumnNum, A, !Columns) :-
    ( if ColumnNum >= 0 then
        get_rows(NumRows - 1, ColumnNum, NumRows, A, [], Rows),
        !:Columns = [Rows | !.Columns],
        get_columns(NumRows, ColumnNum - 1, A, !Columns)
    else
        true
    ).

:- pred get_rows(int, int, int, array(T), list(T), list(T)).
% :- mode get_rows(in, in, in, array_ui, in, out) is det.
:- mode get_rows(in, in, in, in, in, out) is det.

get_rows(RowNum, ColumnNum, NumRows, A, !Rows) :-
    ( if RowNum >= 0 then
        array.unsafe_lookup(A, (ColumnNum * NumRows) + RowNum, Elem),
        !:Rows = [Elem | !.Rows],
        get_rows(RowNum - 1, ColumnNum, NumRows, A, !Rows)
    else
        true
    ).

%---------------------------------------------------------------------------%

fill(Item, Array0, Array) :-
    Array0 = array2d2(NumRows, NumColumns, A0),
    array.fill(Item, A0, A),
    Array = array2d2(NumRows, NumColumns, A).

%---------------------------------------------------------------------------%
:- end_module array2d2.
%---------------------------------------------------------------------------%
