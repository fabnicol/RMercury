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
    eval_bool("a<-FALSE", E1, !IO),
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
    write_int(Res, !IO),
    to_bool_buffer_det(Sexp, BoolBuffer),
    %lookup_bool_vect(BoolBuffer, 2, S10),
    %write_rbool(S10, !IO)
    io.nl(!IO).

:- initialise start_R/2.

:- finalise shutdown_R/2. % or end_R/2

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
