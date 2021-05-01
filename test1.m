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
    %A = array(["R", "--no-save", "--gui=none", "--silent"]),
    %start_R(no, Exitcode, !IO),
    %write(Exitcode, !IO),
    %end_R(no, no, Exitcode2, !IO),
    eval_int("a<-8", E, !IO),
    write_int(E, !IO),
    eval_bool("a<-FALSE", E1, !IO),
    write_bool(E1, !IO),
    io.nl(!IO).

%% Build command line:

% rm test1; mmc --search-lib-files-dir $PWD/local/lib/mercury/lib/hlc.gc/ \
% --init-file $PWD/local/lib/mercury/modules/hlc.gc/ri.init \
% --link-object $PWD/local/lib/mercury/lib/hlc.gc/libri.a \
% --ld-flags "-lR -lRblas" --make test1

%% Exec command line:
% export R_HOME=/usr/lib64/R && LD_LIBRARY_PATH=/usr/lib64/R/lib ./test1

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
