%% File:   test0.m
%% Purpose:  An R interface for Mercury: proof of concept.
%% Copyright Fabrice Nicol <fabnicol@users.sourceforge.net>, 2021
%% The latest version can be found at http://github.com/fabnicol

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License as
%% published by the Free Software Foundation; either version 3 of the
%% License, or (at your option) any later version.

%% This program is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%% General Public License for more details.

%% You should have received a copy of the GNU General Public License
%% along with this program; if not, write to the Free Software
%% Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


:- module test0.

:- interface.
:- import_module io.

:- impure pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int, float.
:- import_module math.

% Predicates for sourcing and evaluating R code. Impure predicates
% performing C I/O.
% TODO: see if module io could be used somehow to
% reduce impurity

:- impure pred source(string::in, string::in, int::in) is det.
:- pragma foreign_decl("C", "#include <RInside_C.h>").
:- pragma foreign_code("C", "int start = 1;").

:- pragma foreign_proc("C",
                       source(X::in, Y::in, Z::in),
                       [will_not_call_mercury],
                       "
                         if (start) setupRinC();
                         start = 0;
                         evalQuietlyInR(X);
                         Rf_PrintValue(evalInR(Y));
                         if (Z) teardownRinC();
                       ").

:- impure pred source(string::in, int::in) is det.

:- pragma foreign_proc("C",
                       source(X::in, Z::in),
                       [will_not_call_mercury],
                       "
                         if (start) setupRinC();
                         start = 0;
                         Rf_PrintValue(evalInR(X));
                         if (Z) teardownRinC();
                       ").

% Predicates implemented as semipure that should be regarded as pure
% in Mercury code (promised pure or hopefully so). Starting with
% one-dimensional real or integer output

:- pred eval_float(string::in, string::in, int::in, float::out) is det.

:- pragma foreign_proc("C",
                       eval_float(X::in, Y::in, T::in, Z::out),
                       [promise_pure, will_not_call_mercury],
                       "
                         if (start) setupRinC();
                         evalQuietlyInR(X);
                         double a = REAL(evalInR(Y))[0];
                         Z = (double) a; // is it double or
                                         // long double ?
                         if (T) teardownRinC();
                       ").


:- pred eval_float(string::in, int::in, float::out) is det.
:- pragma foreign_proc("C",
                       eval_float(X::in, T::in, Z::out),
                       [promise_pure, will_not_call_mercury],
                       "
                         if (start) setupRinC();
                         double a = REAL(evalInR(X))[0];
                         Z = (double) a; // idem?
                         if (T) teardownRinC();
                       ").

:- pred eval_int(string::in, string::in, int::in, int::out) is det.

:- pragma foreign_proc("C",
                       eval_int(X::in, Y::in, T::in, Z::out),
                       [promise_pure, will_not_call_mercury],
                       "
                         if (start) setupRinC();
                         evalQuietlyInR(X);
                         long int a = REAL(evalInR(Y))[0];
                         Z = (long int) a; // is it int ot long int
                                           // or long long?
                         if (T) teardownRinC();
                       ").

:- pred eval_int(string::in, int::in, int::out) is det.

:- pragma foreign_proc("C",
                       eval_int(X::in, T::in, Z::out),
                       [promise_pure, will_not_call_mercury],
                       "
                         if (start) setupRinC();
                         long int a = REAL(evalInR(X))[0];
                         Z = (long int) a; // idem?
                         if (T) teardownRinC();
                       ").

% Testbed: playing with R I/O and retrieving R computations in Mercury
% variables through the C foreign code interface.

main(!IO) :- impure source("
                 library(data.table)
                 y <- data.table(c(1,2,3), c(2,3,4))
               ",
               "y[V1 == 3]",
               0),
             impure source("
                 library(data.table)
                 fread(file = 'a.csv')
               ",
               0),
             eval_float("y <- 8.0
                         z <- 9.0",
			 "y + z",
			 0,
			 X),
             eval_int("y <- 8; z <- 9", "y + z", 0, Y),
             eval_float("sin(1.0)", 0, Z),
             eval_int("8 + 50", 1, T),
             io.write_float(X, !IO), io.nl(!IO),
             io.write_int(Y, !IO), io.nl(!IO),
             io.write_float(Z, !IO), io.nl(!IO),
             io.write_int(T, !IO), io.nl(!IO).

%% Raw R-managed output (can one funnel it through Mercury IO module?):

%% V1 V2
%% 1:  3  4

%% V1 V2 V3
%% 1:  1  2  3
%% 2:  4  5  6
%% 3:  7  8  9

%% Mercury variables computed and assigned by R:
%% 17.0
%% 17
%% 0.8414709848078965
%% 58

%% real	0m0,371s
%% user	0m0,334s
%% sys  0m0,037s

% TODO: expand this POC to vectorized data structures, which will
% request setting precise equivalence relations between C, R (via the
% SEXP C-interface) and Mercury data types.  This does not look as
% intractable.

% TODO: ideally code introspection could allow to avoid some of the
% impurities: run setupRinC() on the first R interface predicate and
% not until the last one; run teardownRinC() only in the last call.
% Whether or not this can be realistically achieved, I do not know. In
% the meantime int::in flags will remain to this effect.
