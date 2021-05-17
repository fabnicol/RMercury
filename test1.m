% File: test1.m Purpose: An R interface for Mercury: test of library.
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

%-----------------------------------------------------------------------------%
%
% Build command line:
%
% rm test1; mmc --search-lib-files-dir $PWD/local/lib/mercury/lib/hlc.gc/ \
% --init-file $PWD/local/lib/mercury/modules/hlc.gc/ri.init \
% --link-object $PWD/local/lib/mercury/lib/hlc.gc/libri.a \
% --ld-flags "-lR -lRblas" --make test1
%
%-----------------------------------------------------------------------------%
%
% Exec command line:
% R_HOME=/usr/lib64/R  LD_LIBRARY_PATH=/usr/lib64/R/lib ./test1
% Replace R_HOME with your platform value.
%-----------------------------------------------------------------------------%

:- module test1.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module require.
:- import_module string.
:- import_module ri.
:- import_module bool.
:- import_module univ.
:- import_module array, array2d.

main(!IO) :-
    eval_int("a<-8", E, !IO),
    write_int(E, !IO),nl(!IO),
    eval_bool("a<-FALSE", E1 : bool, !IO),
    writeln_rbool(E1, !IO),
    eval_float("a<-0.2", E2, !IO),
    write_float(E2, !IO),nl(!IO),
    eval_string("a<-""abcs""", E3, !IO),
    write_string(E3, !IO), nl(!IO),
    string_vect("c(""abcd"", ""efgh"")", E4,!IO),
    lookup(E4, 0, S),
    writeln_item(S, !IO),
    int_vect("c(1234, 5678)", E5,!IO),
    lookup(E5, 0, S1),
    writeln_item(S1, !IO),
    float_vect("c(1234.2, 5678.0)", E6,!IO),
    lookup(E6, 0, S2),
    writeln_item(S2, !IO),
    bool_vect("c(FALSE, TRUE)", E7,!IO),
    % lookup_bool_vect(bool_buffer_det(E7), 1, S3), : OK
    lookup(E7, 1, S3),
    writeln_item(S3, !IO),
    lookup_bool_vect_size(bool_buffer_det(E7), S4),
    write_int(S4, !IO),nl(!IO),
    lookup_buffer_vect_size(E7, S5),
    write_int(S5, !IO),nl(!IO),
    create_string_buffer_det(3, ["aze", "zzz", "bcf"], StringBuffer),
    lookup_string_vect(StringBuffer, 2, S6),
    write_string(S6, !IO),nl(!IO),
    source_string_echo("data.table::fread(""a.csv"")[[1]]", Sexp, Res, !IO),
    write_int(Res, !IO),nl(!IO),
    to_bool_buffer_det(Sexp, BoolBuffer),
    lookup_bool_vect(BoolBuffer, 2, S10),
    write("Now bool:", !IO), nl(!IO),
    writeln_rbool(S10, !IO),
    eval("a<-""b""", S11, !IO),
    io.format("this string: %s\n", [s(S11)], !IO),
    eval("a <- 10000", S11b, !IO),
    io.format("this int: %d\n", [i(S11b)], !IO),
    to_sexp_det("This is an example", S12),
    to_string_det(S12, S13),
    write_string(S13, !IO),io.nl(!IO),
    to_sexp_det(33, S14),
    to_int_det(S14, S15),
    write_int(S15, !IO),io.nl(!IO),
    to_sexp_det(34.0, S16),
    to_float_det(S16, S17),
    write_float(S17, !IO),io.nl(!IO),
    to_sexp_det(yes, S18),
    to_bool_det(S18, S19),
    write_rbool(S19, !IO),io.nl(!IO),
    bool_to_sexp_det(not(S19), S20),
    to_bool_buffer_det(S20, S21),
    lookup_bool_vect(S21, 0, S22),
    writeln_rbool(S22, !IO),
    float_to_sexp_det(56.0, S23),
    to_float_buffer_det(S23, S24),
    lookup_float_vect(S24, 0, S25),
    write_float(S25, !IO),nl(!IO),
    int_to_sexp_det(842, S26),
    to_int_buffer_det(S26, S27),
    lookup_int_vect(S27, 0, S28),
    write_int(S28, !IO),nl(!IO),
    string_to_sexp_det("Now again a string!", S29),
    to_string_buffer_det(S29, S30),
    lookup_string_vect(S30, 0, S31),
    write_string(S31, !IO), nl(!IO),
    to_buffer_det(S29, S29b),
    write_int(ri.length(S29b), !IO), nl(!IO),
    ( if is_string_buffer(S29b) then
      io.format("%s is a string buffer\n", [s(S31)], !IO)
    else
         unexpected($pred, "String buffer test failed.")
    ),
    buffer_to_sexp_det(S30, S32),
    write_string(to_string_det(S32), !IO), nl(!IO),
    buffer_to_sexp_det(S21, S33),
    write_rbool(to_bool_det(S33), !IO), nl(!IO),
    create_float_buffer_det(3, [2.0,1.0], S34),
    write_float(lookup_float_vect(S34, 0), !IO),nl(!IO),
    write_float(lookup_float_vect(S34, 1), !IO),nl(!IO),
    write_float(lookup_float_vect(S34, 2), !IO),nl(!IO),
    buffer_to_sexp_det(S34, S35),
    % to_float_buffer_det(S35, S36), : OK
    sexp_to_buffer_det(S35, S36),
    lookup_float_vect(S36, 0, S36a),
    lookup_float_vect(S36, 1, S36b),
    lookup_float_vect(S36, 2, S36c),
    write_float(S36a, !IO), nl(!IO),
    write_float(S36b, !IO), nl(!IO),
    write_float(S36c, !IO), nl(!IO),
    create_string_buffer_det(3, ["abc","def"], S37),
    write_string(lookup_string_vect(S37, 0), !IO),nl(!IO),
    write_string(lookup_string_vect(S37, 1), !IO),nl(!IO),
    write_string(lookup_string_vect(S37, 2), !IO),nl(!IO),
    buffer_to_sexp_det(S37, S38),
    % to_string_buffer_det(S38, S39), : OK
    sexp_to_buffer_det(S38, S39),
    lookup_string_vect(S39, 0, S39a),
    lookup_string_vect(S39, 1, S39b),
    lookup_string_vect(S39, 2, S39c),
    write_string(S39a, !IO), nl(!IO),
    write_string(S39b, !IO), nl(!IO),
    write_string(S39c, !IO), nl(!IO),
    create_int_buffer_det(3, [10,20], S40),
    write_int(lookup_int_vect(S40, 0), !IO),nl(!IO),
    write_int(lookup_int_vect(S40, 1), !IO),nl(!IO),
    write_int(lookup_int_vect(S40, 2), !IO),nl(!IO),
    buffer_to_sexp_det(S40, S41),
    % to_int_buffer_det(S41, S42), : OK
    sexp_to_buffer_det(S41, S42),
    lookup_int_vect(S42, 0, S42a),
    lookup_int_vect(S42, 1, S42b),
    lookup_int_vect(S42, 2, S42c),
    write_int(S42a, !IO), nl(!IO),
    write_int(S42b, !IO), nl(!IO),
    write_int(S42c, !IO), nl(!IO),
    create_bool_buffer_det(3, [yes,no], S43),
    writeln_rbool(lookup_bool_vect(S43, 0), !IO),
    writeln_rbool(lookup_bool_vect(S43, 1), !IO),
    writeln_rbool(lookup_bool_vect(S43, 2), !IO),
    buffer_to_sexp_det(S43, S44),
    % to_bool_buffer_det(S44, S45), : OK
    %sexp_to_buffer_det(S44, S45), :OK
    sexp_to_univ_buffer_det(S44, S45),
    % lookup_bool_vect(S45, 0, S45a), : OK
    %lookup(from_bool_buffer(S45), 0, S45a), :OK
    % lookup(univ_buffer(S45), 0, S45a), : OK
    lookup(S45, 0, S45a),
    lookup(S45, 1, S45b),
    lookup(S45, 2, S45c),
    writeln_item(S45a, !IO),
    writeln_item(S45b, !IO),
    writeln_item(S45c, !IO),
    apply_to_int_array("sum", array([1,2,3,4]),yes, S46, Errorcode, !IO),
    write_int(Errorcode, !IO), nl(!IO),
    write_int(to_int_det(S46), !IO),nl(!IO),

    apply_to_string_array("cat", array(["abc", "def"]), yes, _, _, !IO),nl(!IO),
    apply_to_bool_array("print", array([yes, no]), yes, _, _, !IO),
    apply_to_float_array("print", array([1.15, 3.2]), yes, _, _, !IO),
    apply_to_int_array("print", array([1, 2]), yes, _, _, !IO),

    apply_to_string("cat", "abc", yes, _, _, !IO),nl(!IO),
    apply_to_bool("print", yes, yes,  _, _, !IO),
    apply_to_float("print",1.15, yes, _, _, !IO),
    apply_to_int("print", 1, yes, _, _, !IO),
    % For graphics, there is still to connect to X
    %apply_to_float("plot", array([0.0,1.0,2.0,3.0,5.5]), no, _, !.IO, !:IO) = _,
    % same with source
    source_string("print(sum(1:50))", !IO),
    apply_to_string2d("unlist", array2d([["cat", "miaow"], ["dog", "waow"]]),
                      yes, S47, _, !IO),

    % We would like to fit in R options, like collapse = " "
    apply_to_sexp("paste0", S47, no, S48, _, !IO),
    write_string(to_string_det(S48), !IO),
    apply_to_bool2d("print", array2d([[no, yes], [yes, yes]]),
                      yes, _, _, !IO),
    apply_to_int2d("print", array2d([[1, 3], [2, 4]]),
                   yes, _, _, !IO),
    % This fails. We have to teach the R C FFI to load libraries. How?
    % apply_to_float2d("data.table::data.table", array2d([[1.15, 3.15], [2.15, 4.15]]),
    %               yes, _, !.IO, !:IO) = _,
    compose_to_float2d(["log", "sum", "unlist"],
                       array2d([[1.15, 3.15],
                                [2.15, 4.15]]),
                     yes, S50, _, !IO),
    apply_to_sexp("print", S50, no, S51, _, !IO), nl(!IO),
    compose_to_sexp(["print", "exp", "exp"], S50, no, S52, _, !IO), nl(!IO),
    write_float(to_float_det(S52), !IO), nl(!IO),
    compose_to_float2d(["print", "data.frame"],
                       array2d([[1.15, 3.15],
                                [2.15, 4.15]]),
                       yes, _, _, !IO),
    compose_to_float2d(["print", "sum", "colSums", "data.frame"],
                       array2d([[1.15, 3.15],
                                [2.15, 4.15]]),
                       yes, S53, _, !IO),
    % Summing a numeric column table
    % Using R base:
    apply_to_string("read.csv", "a.csv", yes, S55, _, !IO),
    compose_to_sexp(["print", "sum", "unlist"],
                    S55, yes, S56, _ , !IO),
    % Using data.table. Currently source_ must be used, as apply does not load
    % libraries yet:
    source_string("data.table::fread(""a.csv"")[, .(sum(x))] [[1]]", S57, !IO),
    apply_to_sexp("print", S57, yes, _, _, !IO),
    apply_to_string2d("unlist", array2d([["cat", "miaow"],
                                         ["dog", "waow"]]),
                  yes, S58, _, !IO),
    apply_to_sexp("print", S58, no, _, _, !IO), % [1] "cat" "dog" "miaow" "waow"
    apply_to_string2d("unlist", transpose_array(
                                    array2d([["cat", "miaow"],
                                             ["dog", "waow"]])),
                      yes, S59, _, !IO),
    apply_to_sexp("print", S59, no, _, _, !IO). % [1] "cat" "miaow" "dog" "waow"
    % Note: Mercury array2d is row-major while R is column-major in data.frames.
    % This may be confusing. TODO: consider some sort of transpose.
    % The predicate below needs some doing: one would need to export univ_value
    % to C for it to work.
    % apply_to_univ2d("print",
    %                array2d([[univ(1), univ("a"), univ(3.0)],
    %                         [univ(2), univ("bc"), univ(4.2)]]),
    %                no, _, _, !IO).


:- initialise start_R/2.

:- finalise shutdown_R/2.  %  or end_R/2

% Output:
%
% 8
% FALSE
% 0.2
% abcs
% abcd
% 1234
% 1234.2
% TRUE
% 2
% 2
% bcf
% 0
% "Now bool:"
% TRUE
% this string: b
% this int: 10000
% This is an example
% 33
% 34.0
% TRUE
% FALSE
% 56.0
% 842
% Now again a string!
% 1
% Now again a string! is a string buffer
% Now again a string!
% FALSE
% 2.0
% 1.0
% 0.0
% 2.0
% 1.0
% 0.0
% abc
% def
%
% abc
% def
%
% 10
% 20
% 0
% 10
% 20
% 0
% TRUE
% FALSE
% FALSE
% TRUE
% FALSE
% FALSE
% 0
% 10
% abc def
% [1]  TRUE FALSE
% [1] 1.15 3.20
% [1] 1 2
% abc
% [1] TRUE
% [1] 1.15
% [1] 1
% [1] 1275
% cat[[1]]
% [1] FALSE  TRUE
%
% [[2]]
% [1] TRUE TRUE
%
% [[1]]
% [1] 1 2
%
% [[2]]
% [1] 3 4
%
% [1] 2.360854
%
% [1] 40134.84
%
% 40134.83743087578
% c.1.15..2.15. c.3.15..4.15.
% 1          1.15          3.15
% 2          2.15          4.15
% [1] 10.6
% [1] 9993949
% [1] 9993949
% [1] "cat"   "dog"   "miaow" "waow"
% [1] "cat"   "miaow" "dog"   "waow"
%
% real	0m0,559s
% user	0m0,497s
% sys	0m0,057s
%
% Compilation finished at Mon May 17 05:52:19
%
