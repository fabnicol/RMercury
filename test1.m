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
% export R_HOME=/usr/lib64/R && LD_LIBRARY_PATH=/usr/lib64/R/lib ./test1
%
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
:- import_module array.

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
    lookup_bool_vect(bool_buffer_det(E7), 1, S3),
    % issue with lookup(E7, 1, S3)
    writeln_rbool(S3, !IO),
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
    ).



:- initialise start_R/2.

:- finalise shutdown_R/2.  %  or end_R/2

% A = array(["R", "--no-save", "--gui=none", "--silent"]),
% start_R(no, Exitcode, !IO),
% write(Exitcode, !IO),
% end_R(no, no, Exitcode2, !IO),


%% Test Output:
%  *** Mercury runtime: caught segmentation violation ***
% cause: unknown
% PC at signal: 94262267084616 (55bb24f1ff48)
% address involved: (nil)
% This may have been caused by a stack overflow, due to unbounded recursion.
% exiting from signal handler

% Debugging process (in progress)
% -------------------------------
% Note:this is a first test (1st May 2021).

% Second test: OK: correct start_R (do not allocate string on the stack)
% and eval_int (double R init + cannot presume Rf_isType from FFI MR_Word.

% Third test:
% + tested: start_R, end_R, eval_<type> (alone).
% Found oddly buggy: shutdown_R, unexpected unterminated " issue.
% Issue also found with RInside (Rcpp): end_R does not reset initialization
% of R server to zero: "R is already initialized". TODO: find a way to.

% Test #4
% Fixed eval_<type>. Fixed " issue. Add finalise/initialise decls with
% start_R/2, end_R/2 or shutdown_R/2.
