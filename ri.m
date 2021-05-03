% File: ri.m Purpose: An R interface library for Mercury.
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

% Version R >= 2.4.0. This is untested, but 2.4 has been obsolete for long now.

%-----------------------------------------------------------------------------%
%
% Coding standards
%
% This file follows the Mercury coding standards with a few departures:
%  - no vim headers when emacs is used;
%  - types formatted as in type 'allow';
%  - a comment begins with an uppercase letter if it contains a verb or is
%    a section header. A section header has no final dot. A sentence ends in
%    a dot. Otherwise a comment begins with a lowercase letter and does not end
%    in dot;
%  - inlined comments are separated from code with just 4 spaces, unless they
%    are right-aligned vertically for clarity;
%  - last arguments and determinism of predicates and functions may be
%    right-aligned as follows:
%
% :- pred lookup_bool_vect_size(bool_buffer::in,     int::out) is det.
% :- pred lookup_int_vect_size(int_buffer::in,       int::out) is det.
%
%    The amount of white space added is such that determinism is vertically
%    aligned;
%  - closely-related declarations may be grouped together under common
%    comment heading and paragraphs if all patterns are documented;
%  - C code uses brackets formatted as follows:
%
%      if (condition) {
%          ...
%      } else {
%          ...
%      }
%  - there may subsections separated as follows:
%  %---- Header ----% [78 characters exactly]
%
%  - pointers to external APIs should be referenced as follows e.g.:
%    Reference: R API, path/to/file.extension | path to Doxygen index.html.
%
%-----------------------------------------------------------------------------%

:- module ri.
:- interface.

:- import_module array.
:- import_module array2d.
:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module type_desc.
:- import_module univ.

%-----------------------------------------------------------------------------%
%
% Representation types, item setters, getters and printers
%
    % Mercury representation types for R vectors

:- type bool_buffer.
:- type float_buffer.
:- type int_buffer.
:- type string_buffer.

    % numeric behavior

:- type allow
    --->    allow(
                inf :: bool,    % Allow +/- Infinity.
                nan :: bool     % Allow NaN (not a number).
            ).

    % more general behavior fields

:- type behavior
    --->    behavior(numeric :: allow).    % To be augmented.

    %-------------------------------------------------------------------------%
    % A note on R-to-Mercury type casting
    %
    % R is a dynamically-typed language whose returns are not type-safe.
    % Sometimes integer vectors will be returned as double vectors,
    % sometimes as integer vectors. Booleans and factors are often cast.
    % We adopt a coertion approach: what counts is the target type fixed
    % by the prefix [type_] and we regard R vectors as castable on return
    % to the target Mercury type, using Rf_coerceVector.
    %
    % Warning: while this is true for numeric types, it is not always so
    % when the R source returns a string vector.
    % TODO: implement exception processing.
    %

    % Catch-all Mercury representation types for R vectors and vector elements
    % Encapsulating 'buffer' type for vectors.

:- type buffer
    --->    int(int_buffer)         % Constructs buffer from int_buffer.
    ;       bool(bool_buffer)       % Constructs buffer from bool_buffer.
    ;       float(float_buffer)     % Constructs buffer from float_buffer.
    ;       string(string_buffer).  % Constructs buffer from string_buffer.

    % TODO: to be augmented.

    % Encapsulating 'buffer_item' for vector elements.

:- type buffer_item
    --->    bool_base(bool)        % Constructs buffer_item from bool.
    ;       float_base(float)      % Constructs buffer_item from float.
    ;       int_base(int)          % Constructs buffer_item from int.
    ;       string_base(string)    % Constructs buffer_item from bool.
    ;       nil.                   % Untyped default item like C NULL.

    % Implemented as foreign enum ParseStatus in R_ext/Parse.h.
    % Describes result of parsing R code by R server.

:- type ri.parse_status
    --->    r_parse_null           % See C definitions of PARSE_STATUS.
    ;       r_parse_ok
    ;       r_parse_incomplete
    ;       r_parse_error
    ;       r_parse_eof.

    % Implemented as foreign enum R_MR_ERROR.
    % Describes error code returned by foreign code calls.

:- type ri.error
    --->      call_error          % Predicate/function was called with error.
    ;         cast_error          % Argument type is not correct.
    ;         file_error          % file path or opening/close error
    ;         source_yes          % reserved
    ;         source_no           % reserved
    ;         sprintf_error       % reserved
    ;         null_string         % Error was caused by a wrong NULL string.
    ;         null_args           % Error was caused by a wrong NULL argument.
    ;         parse_error         % R code could not be parsed.
    ;         max_vect_error      % Maximum length for R vector was exceeded.
    ;         size_vect2d_alloc_error.    % Maximum length for R matrix/data
                                          % frame was exceeded.

    % Mercury representation of R SEXP type.

:- type sexp.

:- pragma foreign_type("C", sexp, "SEXP").

%-----------------------------------------------------------------------------%
%
% Typed buffer creation
%

    % create_<type>_buffer(Value, Buffer)
    %
    % Create <type>_buffer containing only one element of type <type>
    % and value Value. Buffer is nil in case of an allocation issue.
    % 
    % create_<type>_buffer_det(Value, Buffer)
    %
    % Det version of the above, with default values in the (rare) event
    % of a buffer memory allocation error. Defaults are: no (bool), 0.0 (float)
    % 0 (int), "" (string).
 

:- pred create_bool_buffer(bool::in,     bool_buffer::out)   is semidet.
:- pred create_bool_buffer_det(bool::in, bool_buffer::out)   is det.

:- pred create_float_buffer(float::in,   float_buffer::out)  is semidet.
:- pred create_float_buffer_det(float::in,   float_buffer::out)  is det.

:- pred create_int_buffer(int::in,   int_buffer::out)  is semidet.
:- pred create_int_buffer_det(int::in,   int_buffer::out)  is det.

:- pred create_string_buffer(string::in,   string_buffer::out)  is semidet.
:- pred create_string_buffer_det(string::in,   string_buffer::out)  is det.


    % create_<type>_buffer(Size, List, Buffer)
    % 
    % Create <type>_buffer containing Size elements of type <type> out of List.
    % Buffer is nil in case of an allocation issue.
    %
    % Det version of the above, with default values in the (rare) event
    % of a buffer memory allocation error. Defaults are: no (bool), 0.0 (float)
    % 0 (int), "" (string).


:- pred create_bool_buffer(int::in, list(bool)::in,
    bool_buffer::out)   is semidet.
:- pred create_bool_buffer_det(int::in, list(bool)::in,
    bool_buffer::out)   is det.

:- pred create_float_buffer(int::in, list(float)::in,
    float_buffer::out)   is semidet.
:- pred create_float_buffer_det(int::in, list(float)::in,
    float_buffer::out)   is det.

:- pred create_int_buffer(int::in, list(int)::in,
    int_buffer::out)   is semidet.
:- pred create_int_buffer_det(int::in, list(int)::in,
    int_buffer::out)   is det.

:- pred create_string_buffer(int::in, list(string)::in,
    string_buffer::out)   is semidet.
:- pred create_string_buffer_det(int::in, list(string)::in,
    string_buffer::out)   is det.

    % is_<type>_buffer(Buffer)
    %
    % Helper predicates: boolean type identification of Buffer.
    % Reads: Buffer is of underlying type <type>.
    %

:- pred is_bool_buffer(buffer::in)   is semidet.
:- pred is_int_buffer(buffer::in)    is semidet.
:- pred is_float_buffer(buffer::in)  is semidet.
:- pred is_string_buffer(buffer::in) is semidet.

    % <type>_buffer(Buffer) = Type_buffer
    %
    % Pseudo-cast of Buffer to its underlying type.
    % If Buffer has underlying type <type>, output <type>_buffer,
    % with same size data characteristics, otherwise fail.

:- func bool_buffer(buffer)   = bool_buffer   is semidet.
:- func int_buffer(buffer)    = int_buffer    is semidet.
:- func float_buffer(buffer)  = float_buffer  is semidet.
:- func string_buffer(buffer) = string_buffer is semidet.

    % <type>_buffer_det(Buffer) = Type_buffer
    %
    % Pseudo-cast of Buffer to its underlying type.
    % If Buffer has underlying type <type>, output <type>_buffer,
    % with same size data characteristics, otherwise output a
    % default value (no for bool, 0 for int, 0.0 for float, "" for string).

:- func bool_buffer_det(buffer)   = bool_buffer   is det.
:- func int_buffer_det(buffer)    = int_buffer    is det.
:- func float_buffer_det(buffer)  = float_buffer  is det.
:- func string_buffer_det(buffer) = string_buffer is det.

    % <type>_buffer(Type_buffer, Buffer)
    %
    % Pseudo-cast of a typed buffer to an encapsulating 'buffer' type.
    % Construct and output univ-type Buffer associated with Type_buffer.
    % Reverse of the above.

:- pred from_bool_buffer(bool_buffer::in,     buffer::out) is det.
:- pred from_int_buffer(int_buffer::in,       buffer::out) is det.
:- pred from_float_buffer(float_buffer::in,   buffer::out) is det.
:- pred from_string_buffer(string_buffer::in, buffer::out) is det.

    % Object length accessor typeclass.

:- typeclass length(T).

    % Currently only implemented for type 'buffer'.

:- instance length(buffer).

    % lookup_<type>_vect(<type>_buffer, Index, <type>)
    %
    % Mercury typed buffer lookup predicates: get value of buffer
    % at 0-based index.
    % On error:
    % - for numeric types, return 0 if buffer is not accessible, or index
    % is out of bounds [0, Size-1];
    % - for strings, return NA_BUFFER for null C buffer, NA_BUFFER_CONTENTS
    % for null C buffer underlying data array, NA_BUFFER_SIZE for empty buffer
    % and NA_BUFFER_INDEX for index out of bounds [0, Size-1].
    % Note: More efficient for large data bases.
    % Warning: Currently empty R vectors are causing an error when accessed
    % in Mercury. This behavior should be changed.

:- pred lookup_bool_vect(bool_buffer::in, int::in,     bool::out)   is det.
:- pred lookup_float_vect(float_buffer::in, int::in,   float::out)  is det.
:- pred lookup_int_vect(int_buffer::in, int::in,       int::out)    is det.
:- pred lookup_string_vect(string_buffer::in, int::in, string::out) is det.

    % lookup(Buffer, Index, Buffer_Item)
    %
    % Same as the above using encapsulating 'buffer' and buffer_item' types.
    % If parsing large data chunks, preferably use the typed versions above.

:- pred lookup(buffer::in, int::in, buffer_item::out) is det.

    % Marshalling classes from R object representation to standard data types.


    % write_bool(Bool, !IO)
    %
    % Printer of booleans in R-type format (TRUE/FALSE)

:- pred write_rbool(bool::in, io::di, io::uo) is det.

    % Same as 'write_rbool', followed by newline.

:- pred writeln_rbool(bool::in, io::di, io::uo) is det.

    % write_ritem(Item, !IO)
    %
    % Printer helper for encapsulating type 'buffer_item'.

:- pred write_item(buffer_item::in, io::di, io::uo) is det.

    % writeln_item(Item, !IO)
    %
    % Same as 'write_item', followed by newline.

:- pred writeln_item(buffer_item::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Initialization and Finalization/Cleanup on R server stop.
%

    %  start_R(Argv, Silent, Exitcode, !IO)
    %
    %  Start R with command line Argv.
    %  Silent = yes should suppress IO.
    %  Return errno as Exitcode.

:- pred start_R(array(string), bool, int, io, io).
:- mode start_R(array_di, in, out, di, uo) is det.

    %  start_R_semidet(Argv, Exitcode)
    %
    %  Semi-deterministic version of the above (can fail).
    %  Note: As not attached to IO, should be used on startup
    %  in non-commutative semantics or marked impure.

:- pred start_R_semidet(array(string), int).
:- mode start_R_semidet(array_di, out) is semidet.

    %  start_R(Silent, Exitcode, !IO)
    %
    %  Start R with default command line.
    %  Silent = yes should suppress IO.
    %  Return errno as Exitcode.

:- pred start_R(bool, int, io, io).
:- mode start_R(in, out, di, uo) is det.

   % This curtailed version is for 'initialise'.
:- pred start_R(io, io).
:- mode start_R(di, uo) is det.

:- pred start_R_echo(io, io).
:- mode start_R_echo(di, uo) is det.

    %  start_R_semidet(Exitcode)
    %
    %  Semi-deterministic version of the above (can fail).

:- pred start_R_semidet(int).
:- mode start_R_semidet(out) is semidet.

    %  end_R(Silent, Fatal, Exitcode, !IO)
    %
    %  Clean up R memory after completion of R session.
    %  Use Fatal = yes for emergency bail out.
    %  Silent = yes should suppress IO.
    %  Return errno as exit code.

:- pred end_R(bool::in, bool::in, int::out, io::di, io::uo) is det.

    % end_R(!IO)
    %
    % Curtailed version for :- finalise.

:- pred end_R(io::di, io::uo) is det.

    %  end_R_semidet(Silent, Fatal, Exitcode, !IO)
    %
    %  Semi-deterministic version of the above (can fail).

:- pred end_R_semidet(bool::in, int::out) is semidet.

    %  shutdown_R(!IO)
    %
    %  Clean up R memory after completion of R session.
    %  Like end_R but when R_PreserveObjects is used.
    %  No error returns.
    %  Reference:

:- pred shutdown_R(io::di, io::uo) is det.

    %  shutdown_R_semidet(!IO)
    %
    %  Semi-deterministic version of the above (can fail).

:- pred shutdown_R_semidet is semidet.

%-----------------------------------------------------------------------------%
%
% Functions and predicates for sourcing R code.
%
    %  source(Path, Save_on_quit, Silent, Result, Exitcode, !IO)
    %  source(Path, Save_on_quit, Silent, Result, !IO) = Exitcode
    %  source(Path, Result, !IO)
    %  source(Path, Result, !IO) = Exitcode
    %  source_echo(Path, Result, Exutcode, !IO)
    %  source_echo(Path, Result, !IO) = Exitcode
    %  source(Path, Result, !IO)
    %  source(Path, !IO) = Result
    %  source_echo(Path, Result, !IO)
    %  source_echo(Path, !IO) = Result
    %  source(Path, !IO)
    %  source_echo(Path, !IO)
    %
    %  Source file with path Path into an S expression pointer.
    %  If Save_on_quit =  0, shuts down R server without saving state.
    %  If Save_on_quit =  1, shuts down R server and save state.
    %  if Save_on_quit = -1, do not shut down R server.
    %  Silent = yes should suppress IO, so this case could be reimplemented
    %  without !IO.
    %  Return result of R execution in Result.
    %  Return the error code of R_tryEval (3rd arg) in Exitcode.
    %  Predicates and functions of lesser arity imply Silent = yes,
    %  unless they are postfixed with '_echo'.
    %  Predicates and functions of lesser arity all imply Save_on_quit = 0.
    %  References: Embedding/Rerror.c,Rpostscript.c
    %  src/main/context.c, R_tryEval, R_tryEvalSilent.

:- pred source(string, int, bool, sexp, int, io, io).
:- mode source(in, in, in, out, out, di, uo) is det.

:- func source(string, int, bool, sexp, io, io) = int.
:- mode source(in, in, in, out, di, uo) = out is det.

:- pred source(string, sexp, int, io, io).
:- mode source(in, out, out, di, uo) is det.

:- func source(string, sexp, io, io) = int.
:- mode source(in, out, di, uo) = out is det.

:- pred source_echo(string, sexp, int, io, io).
:- mode source_echo(in, out, out, di, uo) is det.

:- func source_echo(string, sexp, io, io) = int.
:- mode source_echo(in, out, di, uo) = out is det.

:- pred source(string, sexp, io, io).
:- mode source(in, out, di, uo) is det.

:- func source(string, io, io) = sexp.
:- mode source(in, di, uo) = out is det.

:- pred source_echo(string, sexp, io, io).
:- mode source_echo(in, out, di, uo) is det.

:- func source_echo(string, io, io) = sexp.
:- mode source_echo(in, di, uo) = out is det.

:- pred source(string::in, io::di, io::uo) is det.

:- pred source_echo(string::in, io::di, io::uo) is det.

    %  source_string(Path, Save_on_quit, Silent, Result, Status, Exitcode, !IO)
    %  source_string(Path, Save_on_quit, Silent, Result, !IO) = Exitcode
    %  source_string(Path, Result, Exitcode, !IO)
    %  source_string(Path, Result, !IO) = Exitcode
    %  source_string_echo(Path, Result, Exitcode, !IO)
    %  source_string_echo(Path, Result, !IO) = Exitcode
    %  source_string(Path, Result, !IO)
    %  source_string(Path, !IO) = Result
    %  source_string_echo(Path, Result, !IO)
    %  source_string_echo(Path, !IO) = Result
    %  source_string(Path, !IO)
    %  source_string_echo(Path, !IO)
    %
    %  Source R Command into an S expression pointer Result.
    %  Like the above source predicates/functions, but with a command
    %  string instead of a path.
    %  The first pair of predicate/function also returns a parse_status value.
    %  Reference: R API, Examples/RParseEval.c

:- pred source_string(string, int, bool, sexp, parse_status, int, io, io).
:- mode source_string(in, in, in, out, out, out, di, uo) is det.

:- func source_string(string, int, bool, sexp, parse_status, io, io) = int.
:- mode source_string(in, in, in, out, out, di, uo) = out is det.

:- pred source_string(string, sexp, int, io, io).
:- mode source_string(in, out, out, di, uo) is det.

:- func source_string(string, sexp, io, io) = int.
:- mode source_string(in, out, di, uo) = out is det.

:- pred source_string_echo(string, sexp, int, io, io).
:- mode source_string_echo(in, out, out, di, uo) is det.

:- func source_string_echo(string, sexp, io, io) = int.
:- mode source_string_echo(in, out, di, uo) = out is det.

:- pred source_string(string, sexp, io, io).
:- mode source_string(in, out, di, uo) is det.

:- func source_string(string, io, io) = sexp.
:- mode source_string(in, di, uo) = out is det.

:- pred source_string_echo(string, sexp, io, io).
:- mode source_string_echo(in, out, di, uo) is det.

:- func source_string_echo(string, io, io) = sexp.
:- mode source_string_echo(in, di, uo) = out is det.

:- pred source_string(string::in, io::di, io::uo) is det.

:- pred source_string_echo(string::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Predicates for sourcing R code to built-in Mecury types (limited
% implementation).
%

    % Typeclass eval(T)
    %
    % Following eval_<type> predicates instantiate this typeclass.
    % Also used to constrain T in other predicates.

:- typeclass eval(T) where [
    pred eval(string::in, T::out, io::di, io::uo) is cc_multi
].

    %
    % eval_<type>(Code, Scalar/String, !IO), type = bool, float, int, string.

    % eval_float(Code, Behavior, Item, !IO).
    %
    % Like the simple case, but with control of NaN and Inf cases through
    % type behavior(allow(inf, nan)).
    %

:- pred eval_bool(string::in, bool::out, io::di,   io::uo)   is det.
:- pred eval_float(string::in, float::out, io::di, io::uo)   is cc_multi.
:- pred eval_float(string::in, behavior::in, buffer_item::out,
    io::di, io::uo) is cc_multi.

:- pred eval_int(string::in, int::out, io::di, io::uo)       is det.
:- pred eval_string(string::in, string::out, io::di, io::uo) is cc_multi.

%-----------------------------------------------------------------------------%
%
% Predicates for sourcing R code to R vectors of general dimension
% (Mercury type 'buffer').
%

    % <type>_vect(R_code, Buffer)
    %
    % Predicates for marshalling R script return values
    % into Mercury code using the 'buffer' catch-all type.
    % Reminder: no scalars in R, only vectors.
    % Original R types are quite unsafe as R is dynamically
    % typed. Using explicit type coercion prefixed to the
    % names of the vector-catch predicates:

:- pred bool_vect(string::in, buffer::out, io::di,   io::uo) is det.
:- pred float_vect(string::in, buffer::out, io::di,  io::uo) is det.
:- pred int_vect(string::in, buffer::out, io::di,    io::uo) is det.
:- pred string_vect(string::in, buffer::out, io::di, io::uo) is det.

%-------------------------------------------------------------------------%
%
% Mercury accessors to buffer-type representation of R vectors.
%
    % Size accessors
    %
    % lookup_<type>_vect_size(Typed_buffer, Size)
    % lookup_<type>_vect_size(Type_buffer) = Size
    %
    % Vector size getter predicates and functions, typed version.

:- pred lookup_bool_vect_size(bool_buffer::in,     int::out) is det.
:- pred lookup_int_vect_size(int_buffer::in,       int::out) is det.
:- pred lookup_float_vect_size(float_buffer::in,   int::out) is det.
:- pred lookup_string_vect_size(string_buffer::in, int::out) is det.

:- func lookup_bool_vect_size(bool_buffer)     = int.
:- func lookup_int_vect_size(int_buffer)       = int.
:- func lookup_float_vect_size(float_buffer)   = int.
:- func lookup_string_vect_size(string_buffer) = int.

    % lookup_buffer_vect_size(Buffer, Size)
    %
    % Same as above for the univ-like 'buffer' type.

:- pred lookup_buffer_vect_size(buffer::in, int::out) is det.
:- func lookup_buffer_vect_size(buffer) = int.

%-----------------------------------------------------------------------------%
%
% Type 'casts' between built-in types or buffered types and sexps.
%

    % get_sexp_type(S, T)
    %
    % Recovers the underlying type descriptor T of S sexp by calling R.

:- pred get_sexp_type(sexp::in, type_desc::out) is semidet.

:- pred is_bool(sexp::in)   is semidet.
:- pred is_float(sexp::in)  is semidet.
:- pred is_int(sexp::in)    is semidet.
:- pred is_string(sexp::in) is semidet.

    % Sexp to built-in type.

:- func to_bool(sexp)   = bool   is semidet. % fails if not bool.
:- func to_float(sexp)  = float  is semidet. % fails if not float
:- func to_int(sexp)    = int    is semidet. % fails if not int.
:- func to_string(sexp) = string is semidet. % fails if not string.

:- func to_bool_det(sexp)   = bool.   % returns FALSE if not bool.
:- func to_float_det(sexp)  = float.  % returns 0.0 if not float
:- func to_int_det(sexp)    = int.    % returns 0 if not int.
:- func to_string_det(sexp) = string. % returns "" if not string.

    % Sexp to typed buffer.

:- pred to_bool_buffer(sexp::in,     bool_buffer::out)   is semidet.
:- pred to_float_buffer(sexp::in,    float_buffer::out)  is semidet.
:- pred to_int_buffer(sexp::in,      int_buffer::out)    is semidet.
:- pred to_string_buffer(sexp::in,   string_buffer::out) is semidet.

    % Sexp to univ-like buffer type.

:- pred to_buffer(sexp::in, buffer::out) is semidet.

    % Built-in type to sexp.

:- pred bool_to_sexp(bool::in,     sexp::out) is semidet.
:- pred float_to_sexp(float::in,   sexp::out) is semidet.
:- pred int_to_sexp(int::in,       sexp::out) is semidet.
:- pred string_to_sexp(string::in, sexp::out) is semidet.

    % Typed buffer to sexp.

:- pred bool_buffer_to_sexp(bool_buffer::in,     sexp::out) is semidet.
:- pred float_buffer_to_sexp(float_buffer::in,   sexp::out) is semidet.
:- pred int_buffer_to_sexp(int_buffer::in,       sexp::out) is semidet.
:- pred string_buffer_to_sexp(string_buffer::in, sexp::out) is semidet.

:- func nil_sexp = sexp.
:- pred nil_sexp(sexp::out) is det.

    % Univ-like buffer to sexp.

:- pred buffer_to_sexp(buffer::in, sexp::out) is semidet.

:- func buffer_to_sexp(buffer) = sexp is semidet.

%-----------------------------------------------------------------------------%
%
% Functions/Predicates for evaluating functions applied to arrays
% (R vectors of dim 1).
%
% The following functions are boilerplate-analogous to the first one.
% These functions are a bit redundant with source-type ones, which are
% more general. Yet they should have a noticeably lower startup penalty
% and in some cases more appropriate interfaces.
%
    %  apply_to_bool(Function, Arg, Silent, Result, !IO)
    %      = Exitcode
    %
    %  Apply R Function to logical vector Arg into an S expression Result.
    %  Reference: R API, Embedding/RParseEval.c, embeddedRCall.c

:- func apply_to_bool(string, array(bool), bool, sexp, io, io) = int.
:- mode apply_to_bool(in, array_di, in, out, di, uo) = out is det.

    %  apply_to_float(Function, Arg, Silent, Result, !IO)
    %     = Exitcode
    %
    %  Apply R Function to numeric vector Arg into an S expression Result.
    %  'float' is used to conform to the corresponding Mercury type, which
    %  is actually a double in most cases.
    %  Reference: R API, Embedding/RParseEval.c,embeddedRCall.c

:- func apply_to_float(string, array(float), bool, sexp, io, io) = int.
:- mode apply_to_float(in, array_di, in, out, di, uo) = out is det.

    %  apply_to_int(Function, Arg, Silent, Result, !IO)
    %      = Exitcode
    %
    %  Apply R Function to integral vector Arg into an S expression Result.
    %  Reference: R API, Embedding/RParseEval.c, embeddedRCall.c

:- func apply_to_int(string, array(int), bool, sexp, io, io) = int.
:- mode apply_to_int(in, array_di, in, out, di, uo) = out is det.

    %  apply_to_string(Function, Arg, Silent, Status, Result, !IO)
    %     = Exitcode
    %
    %  Apply R Function to string vector Arg into an S expression Result.
    %  Reference: R API, Embedding/RParseEval.c,embeddedRCall.c

:- func apply_to_string(string, array(string), bool, sexp, io, io) = int.
:- mode apply_to_string(in, array_di, in, out, di, uo) = out is det.

%-----------------------------------------------------------------------------%
%
% Functions for evaluating functions applied to an array2d (R data frame with
% uniform types or matrices).
%
    %  apply_to_<type>2d(Function, Args2d, Silent, Result, !IO)
    %     = Exitcode
    %
    %  Apply R Function to array2d Args2d of type <type>
    %  into an S expression Result. Args2d represents an R list of vectors,
    %  or data frame (or data table).
    %  R vectors are of type VECSXP and may represent any type like
    %  Mercury's array(T).
    %  They also represent tuples as each component may be of any type.
    %  Here we restrict the pattern to a collection of vectors
    %  of same type <type>.
    %  They do not represent Lisp/Prolog/Mercury linked lists
    %  (which are LISTSXP).
    %  LISTSXP are only marginally used in R and will not be supported
    %  for now.
    %  Reference: R API, Embedding/RParseEval.c,embeddedRCall.c
    %

:- func apply_to_bool2d(string, array2d(bool), int, bool, sexp, io, io) = int.
:- mode apply_to_bool2d(in, array_di, in, in, out, di, uo) = out is det.
:- func apply_to_float2d(string, array2d(float), int, bool, sexp, io, io) = int.
:- mode apply_to_float2d(in, array_di, in, in, out, di, uo) = out is det.
:- func apply_to_int2d(string, array2d(int), int, bool, sexp, io, io) = int.
:- mode apply_to_int2d(in, array_di, in, in, out, di, uo) = out is det.
:- func apply_to_string2d(string, array2d(string), int, bool,
                          sexp, io, io) = int.
:- mode apply_to_string2d(in, array_di, in, in, out, di, uo) = out is det.

    %  apply_to_univ2d(Function, Args, Silent, Result, !IO)
    %      = Exitcode
    %
    %  Source R Function to "matrix" Args into an S expression Result.
    %  Vector elements may be same-length vectors of different types.
    %  Silent governs console output and Exitcode is the error exit code
    %  or 0 on success.
    %  Allowed vector types are:

    %  Mercury     R        Encoding in Types
    %  --------------------------------------
    %  int         INTSXP   ""int""
    %  float       REALSXP  ""float""
    %  bool        LGLSXP  ""bool""
    %  string      STRSXP   ""string""
    %
    %  These built-in types will be encapsuled in a univ array2d.
    %  Reference: R API, Embedding/RParseEval.c,embeddedRCall.c
    %

:- func apply_to_univ2d(string, array2d(univ), bool, sexp, io, io) = int.

:- mode apply_to_univ2d(in, array_di, in, out, di, uo) = out is det.

    %  apply_to_univ_list_arrays(Function, Args, Silent, Status, Result, !IO)
    %      = Exitcode
    %
    %  This function is like the above function but with a list of
    %  1-dimensional arrays of possibly different dimensions for Args.
    %

:- func apply_to_univ_list_arrays(string, list(array(univ)), bool,
     sexp, io, io) = int.

:- mode apply_to_univ_list_arrays(in, array_di, in,
     out, di, uo) = out is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module exception.
:- import_module float.
:- import_module int.
:- import_module math.
:- import_module require.
:- import_module string.

:- pragma require_feature_set([conservative_gc, double_prec_float]).

:- type sexp_container
--->    some [T] sexp_container(T) => eval(T).

%-----------------------------------------------------------------------------%

% ParseStatus is defined in R_ext/Parse.h.

:- pragma foreign_decl("C",
"
#include <R.h>
#include <Rinternals.h>
#include <Rembedded.h>
#include <R_ext/Parse.h>
#include <R_ext/RStartup.h>
#include <Rdefines.h>
#include <config.h>
#include <Defn.h>
#include <Internal.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#define NUM_OLD_GENERATIONS 2
").

:- pragma foreign_enum("C", ri.parse_status/0,
    [
      r_parse_null       - "PARSE_NULL",
      r_parse_ok         - "PARSE_OK",
      r_parse_incomplete - "PARSE_INCOMPLETE",
      r_parse_error      - "PARSE_ERROR",
      r_parse_eof        - "PARSE_EOF"
    ]).

:- pragma foreign_enum("C", ri.error/0,
    [
      call_error         - "R_MR_CALL_ERROR",
      cast_error         - "R_MR_CAST_ERROR",
      file_error         - "R_MR_FILE_ERROR",
      source_yes         - "R_MR_SOURCE_YES",
      source_no          - "R_MR_SOURCE_NO",
      sprintf_error      - "R_MR_SPRINTF_ERROR",
      null_string        - "R_MR_NULL_STRING",
      null_args          - "R_MR_NULL_ARGS",
      parse_error        - "R_MR_PARSE_ERROR",
      max_vect_error     - "R_MR_MAX_VECT_ERROR",
      size_vect2d_alloc_error - "R_MR_SIZE_VECT2D_ALLOC_ERROR"
    ]).

%-----------------------------------------------------------------------------%
%
% Mercury representation of R Vectors with foreign C buffers.
% Note that C-structures shoud not contain pointers
% beyond the first word, otherwise the GC will lose track.
% A first-rank Integer field is OK.
%

:- pragma foreign_decl("C",
"
#include ""mercury_float.h""    /* For MR_FLT_FMT. */
#include ""mercury_memory.h""
#include ""mercury_string.h""
#include ""mercury_library_types.h""    // for MR_ArrayPtr

#include <stdio.h>  /* For sscanf. */

enum {
    R_MR_CALL_ERROR,
    R_MR_CAST_ERROR,
    R_MR_FILE_ERROR,
    R_MR_SOURCE_YES,
    R_MR_SOURCE_NO,
    R_MR_SPRINTF_ERROR,
    R_MR_NULL_STRING,
    R_MR_NULL_ARGS,
    R_MR_PARSE_ERROR,
    R_MR_MAX_VECT_ERROR,
    R_MR_SIZE_VECT2D_ALLOC_ERROR
};

typedef struct {
    MR_Integer  size;
    MR_Bool  *contents;
} BOOL_BUFFER;

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

    % Mercury types corresponding to R logical(), integer(), numeric() and
    % character().
    % with respective types bool_buffer, int_buffer, float_buffer and
    % string_buffer.

:- pragma foreign_type("C", bool_buffer,   "BOOL_BUFFER *",
    [can_pass_as_mercury_type]).

:- pragma foreign_type("C", int_buffer,    "INT_BUFFER *",
    [can_pass_as_mercury_type]).

:- pragma foreign_type("C", float_buffer,  "FLOAT_BUFFER *",
    [can_pass_as_mercury_type]).

:- pragma foreign_type("C", string_buffer, "STRING_BUFFER *",
    [can_pass_as_mercury_type]).

    % Create <type>_buffer with one element or a list of elements.

create_bool_buffer(Value,   Buffer) :-
    create_bool_buffer(1,   [Value], Buffer).
create_bool_buffer_det(Value,  Buffer) :-
    create_bool_buffer_det(1,  [Value], Buffer).

create_float_buffer(Value,  Buffer) :-
    create_float_buffer(1,  [Value], Buffer).
create_float_buffer_det(Value,  Buffer) :-
    create_float_buffer_det(1,  [Value], Buffer).

create_int_buffer(Value,    Buffer) :-
    create_int_buffer(1,    [Value], Buffer).
create_int_buffer_det(Value, Buffer) :-
    create_int_buffer_det(1, [Value], Buffer).

create_string_buffer(Value,    Buffer) :-
    create_string_buffer(1,    [Value], Buffer).
create_string_buffer_det(Value, Buffer) :-
    create_string_buffer_det(1, [Value], Buffer).

    % Note: in case of an allocation failure,
    % <type>_buffer is nil <--> <TYPE>_BUFFER* is NULL
    % So nil should be allowed as a legitimate <type>_buffer value.

:- pragma foreign_proc("C",
    create_bool_buffer(Size::in, List::in, BoolBuffer::out),
    [promise_pure, will_not_call_mercury],
"    
    BoolBuffer = MR_GC_NEW(BOOL_BUFFER);
    if (BoolBuffer != NULL) {
        BoolBuffer->size = Size;
        BoolBuffer->contents = MR_GC_malloc(sizeof(MR_Bool) * Size);
        if (BoolBuffer->contents != NULL) {
	    SUCCESS_INDICATOR = TRUE;
	    
	    for (int i = 0; i < Size && ! MR_list_is_empty(List); ++i) {
	        MR_Bool L_i = MR_list_head(List);
	        List = MR_list_tail(List);
	        BoolBuffer->contents[i] = L_i;
	    }
	    
	} else SUCCESS_INDICATOR = FALSE;
    } else SUCCESS_INDICATOR = FALSE;
    	    
").

:- pragma foreign_proc("C",
    create_float_buffer(Size::in, List::in, FloatBuffer::out),
    [promise_pure, will_not_call_mercury],
"    
    FloatBuffer = MR_GC_NEW(FLOAT_BUFFER);
    if (FloatBuffer != NULL) {
        FloatBuffer->size = Size;
        FloatBuffer->contents = MR_GC_malloc(sizeof(MR_Float) * Size);
        if (FloatBuffer->contents != NULL) {
	    SUCCESS_INDICATOR = TRUE;
	    
	    for (int i = 0; i < Size && ! MR_list_is_empty(List); ++i) {
	        MR_Float L_i = MR_word_to_float(MR_list_head(List));
	        List = MR_list_tail(List);
	        FloatBuffer->contents[i] = L_i;
	    }
	    
	} else SUCCESS_INDICATOR = FALSE;
    } else SUCCESS_INDICATOR = FALSE;
    	    
").

:- pragma foreign_proc("C",
    create_int_buffer(Size::in, List::in, IntBuffer::out),
    [promise_pure, will_not_call_mercury],
"    
    IntBuffer = MR_GC_NEW(INT_BUFFER);
    if (IntBuffer != NULL) {
        IntBuffer->size = Size;
        IntBuffer->contents = MR_GC_malloc(sizeof(MR_Integer) * Size);
        if (IntBuffer->contents != NULL) {
	    SUCCESS_INDICATOR = TRUE;
		
	    for (int i = 0; i < Size && ! MR_list_is_empty(List); ++i) {
	        MR_Integer L_i = MR_list_head(List);
	        List = MR_list_tail(List);
	        IntBuffer->contents[i] = L_i;
	    }
	    
	} else SUCCESS_INDICATOR = FALSE;
    } else SUCCESS_INDICATOR = FALSE;
    	    
").

:- pragma foreign_proc("C",
    create_string_buffer(Size::in, List::in, StringBuffer::out),
    [promise_pure, will_not_call_mercury],
"    
    StringBuffer = MR_GC_NEW(STRING_BUFFER);
    if (StringBuffer != NULL) {
        StringBuffer->size = Size;
        StringBuffer->contents = MR_GC_malloc(sizeof(MR_String) * Size);
        if (StringBuffer->contents != NULL) {
	    SUCCESS_INDICATOR = TRUE;
		
	    for (int i = 0; i < Size && ! MR_list_is_empty(List); ++i) {
	        MR_String L_i = (MR_String) MR_list_head(List);
	        List = MR_list_tail(List);
	        StringBuffer->contents[i] = L_i;
	    }
	    
	} else SUCCESS_INDICATOR = FALSE;
    } else SUCCESS_INDICATOR = FALSE;
    	    
").

create_bool_buffer_det(Size, List, BoolBuffer) :-
    ( if create_bool_buffer(Size, List, X) then
	BoolBuffer = X
    else
	unexpected($pred, "Could not create bool buffer.")
    ).

create_float_buffer_det(Size, List, BoolBuffer) :-
    ( if create_float_buffer(Size, List, X) then
	BoolBuffer = X
    else
	unexpected($pred, "Could not create float buffer.")
    ).

create_int_buffer_det(Size, List, BoolBuffer) :-
    ( if create_int_buffer(Size, List, X) then
	BoolBuffer = X
    else
	unexpected($pred, "Could not create int buffer.")
    ).

create_string_buffer_det(Size, List, BoolBuffer) :-
    ( if create_string_buffer(Size, List, X) then
	BoolBuffer = X
    else
	unexpected($pred, "Could not create string buffer.")
    ).

    % Boolean helper predicates to test 'buffer' underlying sub-type.

is_bool_buffer(bool(_)).

is_int_buffer(int(_)).

is_float_buffer(float(_)).

is_string_buffer(string(_)).

    % pseudo-cast to underlying type of 'buffer'

bool_buffer(Buffer) = Value :- bool(Value) = Buffer.

int_buffer(Buffer) = Value :- int(Value) = Buffer.

float_buffer(Buffer) = Value :- float(Value) = Buffer.

string_buffer(Buffer) = Value :- string(Value) = Buffer.

    % det version of the above

bool_buffer_det(Buffer) = Value :-
     ( if bool(X) = Buffer then
 	Value = X
     else
 	create_bool_buffer_det(no, Value)
     ).

int_buffer_det(Buffer) = Value :-
    ( if int(X) = Buffer then
	Value = X
    else
	create_int_buffer_det(0, Value)
    ).
   
float_buffer_det(Buffer) = Value :-
    ( if float(X) = Buffer then
	Value = X
    else
	create_float_buffer_det(0.0, Value)
    ).

string_buffer_det(Buffer) = Value :-
    ( if string(X) = Buffer then
	Value = X
    else
	create_string_buffer_det("", Value)
    ).

from_bool_buffer(Value, Buffer) :- bool(Value) = Buffer.

from_int_buffer(Value, Buffer) :- int(Value) = Buffer.

from_float_buffer(Value, Buffer) :- float(Value) = Buffer.

from_string_buffer(Value, Buffer) :- string(Value) = Buffer.

:- pragma foreign_code("C",
"
int oldshow;
int num_old_gens_to_collect = 0;
int R_ShowErrorMessages = 1;
").

:- pragma foreign_decl("C",
"
/* For debugging purposes. To limit potential buffer overflow
*  consequences in the development period. To be reset to INT_MAX later on.
*  Not exported. */

#define R_MR_MAX_VECT_SIZE  10000

#define SET_SILENT(Silent)  do {\
    oldshow = R_ShowErrorMessages; \
    R_ShowErrorMessages = (Silent == MR_YES ? FALSE : TRUE); } while(0)

#define RESTORE_VERBOSITY do { R_ShowErrorMessages = oldshow; } while(0)
").

%-----------------------------------------------------------------------------%
%
% Initialization and Finalization of R server
%

% Note: the following predicates and functions related to startup/shutdown and
% sourcing are inherently sequential.  Even if no 'real' console output
% is performed, attaching to IO is necessary to block backtracking or
% reordering in a declarative programming model and promise purity.
% The following 3 predicates are actually impure predicates
% that we shall attach to IO to avoid impurity declarations.
% Hopefully this is enough for them to be promised pure (this point should be
% taken care of).

% Note on Foreign procs: The Mercury FFI does not allow 'return' in C code.
% This accounts for code patterns that could be optimized out if this were not
% the case.

:- pragma foreign_proc("C",
    start_R(Array::array_di, Silent::in, Ret::out, IO0::di, IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io,
     does_not_affect_liveness],
"
errno = 0;

SET_SILENT(Silent);
MR_ArrayPtr array = (MR_ArrayPtr) Array;
int size = array->size;
char **elements = (char**) array->elements;
Rf_initEmbeddedR(size, elements);

Ret = errno;
RESTORE_VERBOSITY;
errno = 0;
").

:- pragma foreign_proc("C",
    start_R_semidet(Array::array_di, Ret::out),
    [promise_pure, will_not_call_mercury, tabled_for_io,
    does_not_affect_liveness],
"
errno = 0;

SET_SILENT(TRUE);
MR_ArrayPtr array = (MR_ArrayPtr) Array;
int size = array->size;
char **elements = (char**) array->elements;
Rf_initEmbeddedR(size, elements);
Ret = errno;

SUCCESS_INDICATOR = (errno == 0);

RESTORE_VERBOSITY;
errno = 0;
").

start_R(Silent, Exitcode, !IO) :-
    start_R(array(["R", "--no-save", "--gui=none", "--silent"]), Silent, Exitcode, !IO).

start_R_semidet(Exitcode) :-
    start_R_semidet(array(["R", "--no-save", "--gui=none", "--silent"]), Exitcode).

start_R(!IO) :- start_R(yes, _, !IO).

start_R_echo(!IO) :- start_R(no, _, !IO).

% Standard R server stop.

:- pragma foreign_proc("C",
    end_R(Silent::in, Fatal::in, Ret::out, IO0::di, IO::uo),
    [promise_pure, will_not_call_mercury],
"
errno = 0;

SET_SILENT(Silent);

/* does not return */
Rf_endEmbeddedR(Fatal == MR_NO);
UNPROTECT(2);
Ret = errno;

RESTORE_VERBOSITY;
errno = 0;
").

end_R(!IO) :- end_R(yes, no, _, !IO).

:- pragma foreign_proc("C",
    end_R_semidet(Fatal::in, Ret::out),
    [promise_pure, will_not_call_mercury],
"
errno = 0;
SET_SILENT(TRUE);

/* does not return */
Rf_endEmbeddedR(Fatal == MR_NO);
UNPROTECT(2);
Ret = errno;

SUCCESS_INDICATOR = (errno == 0);
RESTORE_VERBOSITY;
errno = 0;
").

% R server stop when R_PreserveObjects is used

:- pragma foreign_proc("C",
    shutdown_R(IO0::di, IO::uo),
    [promise_pure, will_not_call_mercury],
"
R_dot_Last();
R_RunExitFinalizers();
CleanEd();
KillAllDevices();
num_old_gens_to_collect = NUM_OLD_GENERATIONS;
R_gc();
errno = 0;
").

:- pragma foreign_proc("C",
    shutdown_R_semidet,
    [promise_pure, will_not_call_mercury],
"
errno = 0;
SET_SILENT(TRUE);
R_dot_Last();
R_RunExitFinalizers();
CleanEd();
KillAllDevices();
num_old_gens_to_collect = NUM_OLD_GENERATIONS;
R_gc();
SUCCESS_INDICATOR = (errno == 0);
RESTORE_VERBOSITY;
errno = 0;
").

%-----------------------------------------------------------------------------%
%
% Sourcing R scripts
%

source(Path, Quit, Silent, Result, Exitcode, !.IO, !:IO) :-
    source(Path, Quit, Silent, Result, !.IO, !:IO) = Exitcode.

:- pragma foreign_proc("C",
    source(Path::in, Quit::in, Silent::in, Result::out,
        IO0::di, IO::uo) = (Exitcode::out),
    [promise_pure, will_not_call_mercury, tabled_for_io,
     does_not_affect_liveness],
"
errno = 0;
SET_SILENT(Silent);
SEXP e, tmp;

PROTECT(tmp = lang2(install(""source""), mkString(Path)));
int exitcode = 0;
Result = R_tryEval(tmp, R_GlobalEnv, &exitcode);
Exitcode = (MR_Integer) exitcode;  /* int -> long int */
UNPROTECT(1);

/* q() */

if (Quit) {
  const char* quit = (Quit ? ""yes"" : ""no"");
  R_ShowErrorMessages = FALSE;
  PROTECT(e = lang2(install(""q""), mkString(quit)));
  eval(e, R_GlobalEnv);
  UNPROTECT(1);
}
errno = 0;
RESTORE_VERBOSITY;
").

% 'Source' shorthand helpers.

source(Path, Result, Exitcode, !IO) :-
    % start_R(yes, Error_start, !IO),
    % ( if Error_start = 0 then
    %     true
    % else
    %     io.set_exit_status(Error_start, !IO)
    % ),
    source(Path, 0, yes, Result, Exitcode, !IO).
    %end_R(yes, no, Error_end, !IO),
    %io.set_exit_status(Error_end, !IO).

source(Path, Result, !IO) = Exitcode :-
    source(Path, Result, Exitcode, !IO).

source(Path, Result, !IO) :- source(Path, Result, _, !IO).

source(Path, !IO) = Result :- source(Path, Result, _, !IO).

source_echo(Path, Result, Exitcode, !IO) :-
    % start_R(no, Error_start, !IO),
    % ( if Error_start = 0
    % then
    %     true
    % else
    %     io.set_exit_status(Error_start, !IO)
    % ),
    source(Path, 0, no, Result, Exitcode, !IO).
   % end_R(no, yes, Error_end, !IO),
   % io.set_exit_status(Error_end, !IO).

source_echo(Path, Result, !IO) = Exitcode :-
    source(Path, Result, Exitcode, !IO).

source_echo(Path, Result, !IO) :- source_echo(Path, Result, _, !IO).

source_echo(Path, !IO) = Result :- source_echo(Path, Result, _, !IO).

source(Path, !IO) :- source(Path, _, !IO).

source_echo(Path, !IO) :- source_echo(Path, _, !IO).

%---- Source R string  into an S expression pointer Result -------------------%

source_string(Command, Quit, Silent, Result, Status, Exitcode, !.IO, !:IO) :-
   source_string(Command, Quit, Silent, Result, Status, !.IO, !:IO) = Exitcode.

:- pragma foreign_proc("C",
    source_string(Command::in, Quit::in, Silent::in, Result::out, Status::out,
        IO0::di, IO::uo) = (Errorcode::out),
    [promise_pure, will_not_call_mercury, tabled_for_io,
     does_not_affect_liveness],
"
SEXP tmp = NULL;
SEXP e;
errno = 0;

SET_SILENT(Silent);

/* It is safer to enclose R code in a block */

int newlength = strlen(Command) + 2;
char code[newlength + 1];
int ret = sprintf(code, ""{%s}"", Command);
if (ret != newlength) {
    Errorcode = R_MR_SPRINTF_ERROR;
    Status =  PARSE_ERROR;
    Result = NULL;
}

PROTECT(tmp = mkString(code));

if (tmp == NULL) {
    Errorcode = R_MR_NULL_STRING;
    Status = PARSE_ERROR;
    Result = NULL;
} else {
    ParseStatus S;
    PROTECT(e = R_ParseVector(tmp, -1, &S, R_NilValue));
    Status = (MR_Integer) S;
    if (Status != PARSE_OK || TYPEOF(e) != EXPRSXP) {
        UNPROTECT(2);
        Errorcode = R_MR_PARSE_ERROR;
        Status = PARSE_ERROR;
        Result = NULL;
    }
    int errorcode = 0;
    Result = R_tryEval(VECTOR_ELT(e, 0), R_GlobalEnv, &errorcode);
    Errorcode = errorcode; /* int -> long int */
}

errno = 0;
RESTORE_VERBOSITY;
").

source_string(Command, Result, Exitcode, !IO) :-
    source_string(Command, 0, yes, Result, _, Exitcode, !IO).

source_string(Command, Result, !IO) = Exitcode :-
    source_string(Command, Result, Exitcode, !IO).

source_string(Command, Result, !IO) :-
    source_string(Command, Result, _, !IO).

source_string(Command, !IO) = Result :-
    source_string(Command, Result, _, !IO).

source_string_echo(Command, Result, Exitcode, !IO) :-
    source_string(Command, 0, no, Result, _, Exitcode, !IO).

source_string_echo(Command, Result, !IO) = Exitcode :-
    source_string(Command, Result, Exitcode, !IO).

source_string_echo(Command, Result, !IO) :-
    source_string_echo(Command, Result, _, !IO).

source_string_echo(Command, !IO) = Result :-
    source_string_echo(Command, Result, _, !IO).

source_string(Path, !IO) :- source_string(Path, _, !IO).

source_string_echo(Path, !IO) :- source_string_echo(Path, _, !IO).

%-----------------------------------------------------------------------------%
%
% Sourcing R code string into scalar types (one-dimensional R vectors)
% and vector types (dim > 1).
%


%---- Abstraction typeclass for eval predicates ------------------------------%

:- instance eval(string) where [
    pred(eval/4) is eval_string
].

:- instance eval(int) where [
    pred(eval/4) is eval_int
].

:- instance eval(bool) where [
    pred(eval/4) is eval_bool
].

:- instance eval(float) where [
    pred(eval/4) is eval_float
].

%---- Sourcing into scalars --------------------------------------------------%

eval_float(Code, Result, !IO) :-
    eval_float(Code, behavior(allow(yes, yes)), Ret, !IO),
    (Ret = float_base(X) -> Result = X ; Result = 0.0).

eval_float(Code, Behavior, Result, !IO) :-
    check_finite(
        Code,
        (pred(C::in, FloatElement::out, !.IO::di, !:IO::uo) is det :-
            eval_F(C, to_float_det, Float, !.IO, !:IO),
            FloatElement = float_base(Float)),
        Behavior,
        Result, !IO).

eval_string(Code, Result, !IO) :-
    (
        eval_F(Code, to_string_det, Result, !IO)
    ;
        Result = ""
    ).


eval_int(Code, Result, !IO) :-
    eval_F(Code, to_int_det, Result, !IO).

eval_bool(Code, Result, !IO) :-
    eval_F(Code, to_bool_det, Result, !IO).

:- pred eval_F(string, (func(sexp) =  T), T, io, io).

:- mode eval_F(in, func(in) = out is det, out, di, uo) is det.

eval_F(Code, Func, Out, !IO) :-
    source_string(Code, E, Errorcode, !IO),
    ( if Errorcode = 0 then
        true
    else
        io.set_exit_status(Errorcode, !IO)
    ),
    Func(E) = Out.

%---- Sourcing R code into R vectors and Mercury 'buffer' type ---------------%

int_vect(Code, Buffer, !IO) :-
    source_string(Code, Sexp, Errorcode, !IO),
    ( if Errorcode = 0 then
        true
    else
        unexpected($pred,
               "An error occurred while sourcing the R command."),
        io.set_exit_status(Errorcode, !IO)
    ),
    (if to_int_buffer(Sexp, X) then
        IntBuffer = X
    else
        unexpected($pred, "SEXP expression could not be converted \
            to int buffer."),
        io.set_exit_status(Errorcode, !IO)
    ),
    from_int_buffer(IntBuffer, Buffer).


    % Source R code into buffer array-like structure.

bool_vect(Code, Buffer, !IO) :-
    source_string(Code, Sexp, Errorcode, !IO),
    ( if Errorcode = 0 then
        true
    else
        unexpected($pred,
            "An error occurred while sourcing the R command."),
            io.set_exit_status(Errorcode, !IO)
    ),
    ( if to_bool_buffer(Sexp, X) then
        BoolBuffer = X
    else
        unexpected($pred, "SEXP expression could not be converted \
            to bool buffer."),
        io.set_exit_status(Errorcode, !IO)
    ),
    from_bool_buffer(BoolBuffer, Buffer).


    % Source R code into buffer array-like structure.

float_vect(Code, Buffer, !IO) :-
    source_string(Code, Sexp, Errorcode, !IO),
    ( if Errorcode = 0 then
        true
    else
        unexpected($pred,
            "An error occurred while sourcing the R command."),
            io.set_exit_status(Errorcode, !IO)
    ),
    ( if to_float_buffer(Sexp, X) then
        FloatBuffer = X
    else
        unexpected($pred, "SEXP expression could not be converted \
            to float buffer."),
        io.set_exit_status(Errorcode, !IO)
    ),
    from_float_buffer(FloatBuffer, Buffer).

    % Source R code into buffer array-like structure.

string_vect(Code, Buffer, !IO) :-
    source_string(Code, Sexp, Errorcode, !IO),
    ( if Errorcode = 0 then
        true
    else
        unexpected($pred,
            "An error occurred while sourcing the R command."),
        io.set_exit_status(Errorcode, !IO)
    ),
    ( if to_string_buffer(Sexp, X) then
        StringBuffer = X
    else
        unexpected($pred, "SEXP expression could not be converted \
            to float buffer."),
        io.set_exit_status(Errorcode, !IO)
    ),
    from_string_buffer(StringBuffer, Buffer).

%-----------------------------------------------------------------------------%
%
% Operations ont sexp/SEXP (universal R type and Mercury counterpart).
% Setters/getters. Type 'casts' to and from built-in types and types sexp,
% <type>_buffers and 'buffer'
%

%---- Type getter ------------------------------------------------------------%

get_sexp_type(Sexp, TypeDesc) :-
    ( if is_bool(Sexp) then
        TypeDesc = type_of(bool.yes)
    else if is_float(Sexp) then
        TypeDesc = type_of(0.0)
    else if is_int(Sexp) then
        TypeDesc = type_of(0)
    else if is_string(Sexp) then
        TypeDesc = type_of("")
    else
        fail
    ).

%---- Type tests -------------------------------------------------------------%

:- pragma foreign_proc("C",
    is_bool(Sexp::in),
    [will_not_call_mercury, promise_pure, tabled_for_io,
     does_not_affect_liveness],
"
SUCCESS_INDICATOR = (Rf_isLogical(Sexp));
"
).

:- pragma foreign_proc("C",
    is_float(Sexp::in),
    [will_not_call_mercury, promise_pure, tabled_for_io,
    does_not_affect_liveness],
"
SUCCESS_INDICATOR = (Rf_isReal(Sexp));
").

:- pragma foreign_proc("C",
    is_int(Sexp::in),
    [will_not_call_mercury, promise_pure, tabled_for_io,
    does_not_affect_liveness],
"
SUCCESS_INDICATOR = (Rf_isInteger(Sexp));
"
).

:- pragma foreign_proc("C",
    is_string(Sexp::in),
    [will_not_call_mercury, promise_pure, tabled_for_io,
    does_not_affect_liveness],
"
SUCCESS_INDICATOR = (Rf_isString(Sexp));
").

%---- 'Cast'from sexp to built-in type (bool, float, int, string) ------------%

:- pragma foreign_proc("C",
    to_bool(Sexp::in) = (Value::out),
    [will_not_call_mercury, promise_pure, tabled_for_io,
     does_not_affect_liveness],
"
SUCCESS_INDICATOR = (Rf_isLogical(Sexp) && LOGICAL(Sexp)[0] != NA_LOGICAL);
Value = Rf_asLogical(Sexp);
").

:- pragma foreign_proc("C",
    to_bool_det(Sexp::in) = (Value::out),
    [will_not_call_mercury, promise_pure, tabled_for_io,
    does_not_affect_liveness],
"
if (! Rf_isLogical(Sexp))
    Value = FALSE;
else {
    Value = Rf_asLogical(Sexp);
}
").

:- pragma foreign_proc("C",
    to_float(Sexp::in) = (Value::out),
    [will_not_call_mercury, promise_pure, tabled_for_io,
     does_not_affect_liveness],
"
SUCCESS_INDICATOR = (Rf_isReal(Sexp) && R_finite(REAL(Sexp)[0]));
Value = Rf_asReal(Sexp);
").

:- pragma foreign_proc("C",
    to_float_det(Sexp::in) = (Value::out),
    [will_not_call_mercury, promise_pure, tabled_for_io,
    does_not_affect_liveness],
"
if (! Rf_isReal(Sexp) || ! R_finite(REAL(Sexp)[0]))
    Value = 0;
else {
    Value = Rf_asReal(Sexp);
}
").


:- pragma foreign_proc("C",
    to_int(Sexp::in) = (Value::out),
    [will_not_call_mercury, promise_pure, tabled_for_io,
     does_not_affect_liveness],
"
errno = 0;
Value = Rf_asInteger(Sexp);
SUCCESS_INDICATOR = (errno != 0 && Value != NA_INTEGER);
errno = 0;
").

:- pragma foreign_proc("C",
    to_int_det(Sexp::in) = (Value::out),
    [will_not_call_mercury, promise_pure, tabled_for_io,
     does_not_affect_liveness],
"
errno = 0;
Value = Rf_asInteger(Sexp);
if (errno) {
    Value = 0;
}
errno = 0;
").

:- pragma foreign_proc("C",
    to_string(Sexp::in) =  (Value::out),
    [will_not_call_mercury, promise_pure, tabled_for_io,
    does_not_affect_liveness],
"
SUCCESS_INDICATOR = (Rf_isString(Sexp));
if (SUCCESS_INDICATOR == TRUE) {
  Value = (MR_String) CHAR(STRING_ELT(Sexp, 0));
  SUCCESS_INDICATOR = (Value != (MR_String) NA_STRING);
}
").

:- pragma foreign_proc("C",
    to_string_det(Sexp::in) =  (Value::out),
    [will_not_call_mercury, promise_pure, tabled_for_io,
    does_not_affect_liveness],
"
if (! Rf_isString(Sexp))
    Value = (MR_String) """";
else {
    Value = (MR_String) CHAR(STRING_ELT(Sexp, 0));
}
").

%---- From sexp to <type>_buffer ---------------------------------------------%

:- pragma foreign_proc("C",
    to_bool_buffer(Sexp::in, Buffer::out),
    [promise_pure, will_not_call_mercury, tabled_for_io,
     does_not_affect_liveness],
"
SEXP S = (SEXP) Sexp; /* From MR_Word to SEXP */            
if (Rf_isInteger(S) || Rf_isReal(S) || Rf_isString(S)) {
    S = Rf_coerceVector(S, LGLSXP);
}

Buffer = MR_GC_NEW(BOOL_BUFFER);
if (Buffer == NULL || ! Rf_isLogical(S))
    SUCCESS_INDICATOR = FALSE;
else {
    MR_Integer size = LENGTH(S);
    Buffer->size = size;
    if (size == 0) {
        SUCCESS_INDICATOR = FALSE;
    } else {
        Rboolean *V = (Rboolean*) LOGICAL(S);
        /* Rboolean is an int in R yet MR_Bool is MR_Word i.e.
           unsigned pointer size. On a 64-platform, this is 8 bytes,
           while int is 4 bytes. memcpy will not do. */
        Buffer->contents = MR_GC_malloc(sizeof(MR_Bool) * size);
        if (Buffer->contents == NULL)
          SUCCESS_INDICATOR = FALSE;
        else {
          /* memcpy(Buffer->contents, V, size); ? */
          for (int i = 0; i < size; ++i)
            Buffer->contents[i] = (MR_Bool) V[i];
          SUCCESS_INDICATOR = TRUE;
        }
    }
    #ifdef FINALIZE_BUFFER
            MR_GC_register_finalizer(Buffer, R_MR_finalize_Buffer, 0);
    #endif
}
").

:- pragma foreign_proc("C",
    to_float_buffer(Sexp::in, Buffer::out),
    [promise_pure, will_not_call_mercury, tabled_for_io,
     does_not_affect_liveness],
"
SEXP S = (SEXP) Sexp; /* From MR_Word to SEXP */        
if (Rf_isLogical(S) || Rf_isInteger(S) || Rf_isString(S)) {
    S = Rf_coerceVector(S, REALSXP);
}

Buffer = MR_GC_NEW(FLOAT_BUFFER);
if (Buffer == NULL || ! Rf_isReal(S))
    SUCCESS_INDICATOR = FALSE;
else {
    MR_Integer size = LENGTH(S);
    Buffer->size = size;
    if (size == 0) {
        SUCCESS_INDICATOR = FALSE;
    } else {
        MR_Float *V1 = REAL(S);
        Buffer->contents = MR_GC_malloc(sizeof(MR_Float) * size);
        if (Buffer->contents == NULL)
            SUCCESS_INDICATOR = FALSE;
        else {
	 /* memcpy is possible as R 'numeric' vectors are of item
	 *  size double, which is MR_Float.
         *  Cautionary note: if definition of MR_Float should change,
	 *  e.g. move to 'long double', code would be broken. */
		
            memcpy(Buffer->contents, V1, size * sizeof(MR_Float));
            SUCCESS_INDICATOR = TRUE;
        }
    }
}
").

:- pragma foreign_proc("C",
  to_int_buffer(Sexp::in, Buffer::out),
  [promise_pure, will_not_call_mercury, tabled_for_io,
   does_not_affect_liveness],
"
SEXP S = (SEXP) Sexp; /* From MR_Word to SEXP */    
Buffer = MR_GC_NEW(INT_BUFFER);
if (Rf_isLogical(S) || Rf_isReal(S) || Rf_isString(S)) {
    S = Rf_coerceVector(S, INTSXP);
}

if (Buffer == NULL || ! Rf_isInteger(S))
    SUCCESS_INDICATOR = FALSE;
else {
    MR_Integer size = LENGTH(S);
    Buffer->size = size;
    if (size == 0) {
        SUCCESS_INDICATOR = FALSE;
    } else {
        int *V = INTEGER(S);
        /* INTEGER creates an array of ints in R
           yet MR_INTEGER is void pointer size.
           On a 64-platform, this is 8 bytes,
           while int is 4 bytes. memcpy will not do. */

        Buffer->contents = MR_GC_malloc(sizeof(MR_Integer) * size);
        if (Buffer->contents == NULL)
            SUCCESS_INDICATOR = FALSE;
        else {
            /* memcpy(Buffer->contents, V,
		size * sizeof(MR_Integer)); --> This is *not* possible */
            for (int i = 0; i < size; ++i)
                Buffer->contents[i] = (MR_Integer) V[i];

            SUCCESS_INDICATOR = TRUE;
        }
    }
}
").

:- pragma foreign_proc("C",
    to_string_buffer(Sexp::in, Buffer::out),
    [promise_pure, will_not_call_mercury, tabled_for_io,
     does_not_affect_liveness],
"
SEXP S = (SEXP) Sexp; /* From MR_Word to SEXP */
    
if (! Rf_isString(S)) {
    if (Rf_isLogical(S) || Rf_isInteger(S) || Rf_isReal(S)) {
        S = Rf_coerceVector(S, STRSXP);
	
    }
}
	
Buffer = MR_GC_NEW(STRING_BUFFER);
if (Buffer == NULL || ! Rf_isString(S)) {
    SUCCESS_INDICATOR = FALSE;
} else {
    MR_Integer size = LENGTH(S);
    Buffer->size = size;
    if (size == 0) {
        SUCCESS_INDICATOR = FALSE;
    } else {
        Buffer->contents = MR_GC_malloc(sizeof(MR_String) * size);
        if (Buffer->contents == NULL)
            SUCCESS_INDICATOR = FALSE;
        else {
            for (int j = 0; j < size; ++j) {
                MR_String Sj = (MR_String) CHAR(STRING_ELT(S, j));
                MR_Integer Lj = strlen(Sj);
                Buffer->contents[j] = MR_GC_malloc(Lj + 1);
                if (Buffer->contents[j] == NULL)
                    SUCCESS_INDICATOR = FALSE;
                else {
                    memcpy(Buffer->contents[j], Sj, Lj + 1);
                }
            }
            SUCCESS_INDICATOR = TRUE;
        }
    }
}
").

%---- 'Cast' from sexp to univ-like type 'buffer' ----------------------------%

to_buffer(Sexp, Buffer) :-
    ( if is_bool(Sexp) then
        to_bool_buffer(Sexp, BoolBuffer),
        from_bool_buffer(BoolBuffer, Buffer)
    else if is_float(Sexp) then
        to_float_buffer(Sexp, FloatBuffer),
        from_float_buffer(FloatBuffer, Buffer)
    else if is_int(Sexp) then
        to_int_buffer(Sexp, IntBuffer),
        from_int_buffer(IntBuffer, Buffer)
    else if is_string(Sexp) then
        to_string_buffer(Sexp, StringBuffer),
        from_string_buffer(StringBuffer, Buffer)
    else
        fail
    ).

%---- 'Cast' from built-in type (bool, float, int, string) to sexp -----------%

:- pragma foreign_proc("C",
    bool_to_sexp(Value::in, Sexp::out),
    [promise_pure, will_not_call_mercury, does_not_affect_liveness],
"
PROTECT(Sexp = Rf_ScalarLogical((int) Value));
if (Sexp == NULL) {
    SUCCESS_INDICATOR = FALSE;
} else if (Rf_isLogical(Sexp)) {
        SUCCESS_INDICATOR = TRUE;
} else {
    SUCCESS_INDICATOR = FALSE;
}
").

:- pragma foreign_proc("C",
    float_to_sexp(Value::in, Sexp::out),
    [promise_pure, will_not_call_mercury, does_not_affect_liveness],
"
PROTECT(Sexp = Rf_ScalarReal((double) Value));
if (Sexp == NULL) {
    SUCCESS_INDICATOR = FALSE;
} else if (Rf_isReal(Sexp)) {
    SUCCESS_INDICATOR = TRUE;
} else {
    SUCCESS_INDICATOR = FALSE;
}
").

:- pragma foreign_proc("C",
    int_to_sexp(Value::in, Sexp::out),
    [promise_pure, will_not_call_mercury, does_not_affect_liveness],
"
PROTECT(Sexp = Rf_ScalarInteger((int) Value));
if (Sexp == NULL) {
    SUCCESS_INDICATOR = FALSE;
} else  if (Rf_isInteger(Sexp)) {
    SUCCESS_INDICATOR = TRUE;
} else {
    SUCCESS_INDICATOR = FALSE;
}
").

:- pragma foreign_proc("C",
    string_to_sexp(Value::in, Sexp::out),
    [promise_pure, will_not_call_mercury, does_not_affect_liveness],
"
PROTECT(Sexp = Rf_mkString((const char*) Value));
if (Sexp == NULL) {
    SUCCESS_INDICATOR = FALSE;
} else if (Rf_isString(Sexp)) {
    SUCCESS_INDICATOR = TRUE;
} else {
    SUCCESS_INDICATOR = FALSE;
}
").

%---- 'Cast' from <type>_buffer to sexp --------------------------------------%

:- pragma foreign_proc("C",
    bool_buffer_to_sexp(Value::in, Sexp::out),
    [promise_pure, will_not_call_mercury, does_not_affect_liveness],
"
BOOL_BUFFER *value = (BOOL_BUFFER*) Value;
int size = (int) value->size; /* long int -> int */
MR_Bool *contents = value->contents; /* MR_Bool <-> MR_Word */

PROTECT(Sexp = Rf_allocVector(LGLSXP, size));
if (Sexp == NULL) {
    SUCCESS_INDICATOR = FALSE;
} else {
    errno = 0;
    Rboolean *p = (Rboolean*) LOGICAL(Sexp);  /* Rboolean <-> int, no memcpy */
    // memcpy(p, contents, size);
    for (int i = 0; i < size; ++i) p[i] = (int) contents[i];
    SUCCESS_INDICATOR = errno ? FALSE : TRUE;
    errno = 0;
}
").

:- pragma foreign_proc("C",
    float_buffer_to_sexp(Value::in, Sexp::out),
    [promise_pure, will_not_call_mercury, does_not_affect_liveness],
"
FLOAT_BUFFER *value = (FLOAT_BUFFER*) Value;
int size = (int) value->size;
MR_Float *contents = (MR_Float*) value->contents;
/* ISSUE here.
   With 64-platforms this should be OK.
   With other platforms, where sizeof(MR_Float) != sizeof(MR_Word)
   the MR_word_to_float/float_to_word macros should be used yet
   I do not see how. This reminder is a note. */

PROTECT(Sexp = Rf_allocVector(REALSXP, size));
if (Sexp == NULL) {
    SUCCESS_INDICATOR = FALSE;
} else {
    errno = 0;
    double *p = REAL(Sexp);

    /* memcpy should work for 64-bit platforms, not necessarily for others.*/

    memcpy(p, Value->contents, Value->size);
    SUCCESS_INDICATOR = errno ? FALSE : TRUE;
    errno = 0;
}
"
).

:- pragma foreign_proc("C",
    int_buffer_to_sexp(Value::in, Sexp::out),
    [promise_pure, will_not_call_mercury, does_not_affect_liveness],
"
INT_BUFFER *value = (INT_BUFFER*) Value;
int size = (int) value->size;
MR_Integer *contents = (MR_Integer*) value->contents;

PROTECT(Sexp = Rf_allocVector(INTSXP, 1));
if (Sexp == NULL) {
    SUCCESS_INDICATOR = FALSE;
} else {
    errno = 0;
    int *p = INTEGER(Sexp);
    /* This should be OK.
       Unless mistake, sizeof(MR_Word) = sizeof(MR_Integer) */
    memcpy(p, contents, size);
    SUCCESS_INDICATOR = errno ? FALSE : TRUE;
}
").

:- pragma foreign_proc("C",
    string_buffer_to_sexp(Value::in, Sexp::out),
    [promise_pure, will_not_call_mercury, does_not_affect_liveness],
"
STRING_BUFFER *value = (STRING_BUFFER*) Value;
int size = (int) value->size;
MR_String *contents = (MR_String*) value->contents;

PROTECT(Sexp = Rf_allocVector(STRSXP, 1));
if (Sexp == NULL) {
    SUCCESS_INDICATOR = FALSE;
} else {
    errno = 0;
    SUCCESS_INDICATOR = TRUE;
    for (int j = 0; j < size; ++j)
    SET_STRING_ELT(Sexp, j, Rf_mkString((const char*) contents[j]));
    SUCCESS_INDICATOR = errno ? FALSE : TRUE;
}
").

nil_sexp = X :- nil_sexp(X).

:- pragma foreign_proc("C",
    nil_sexp(Sexp::out),
    [promise_pure, will_not_call_mercury, does_not_affect_liveness],
"
   Sexp = NILSXP;
").

%---- 'Cast' from type 'buffer' type to sexp ---------------------------------%

buffer_to_sexp(Buffer, Sexp) :-
    (
        Buffer = bool(BoolBuffer), bool_buffer_to_sexp(BoolBuffer, Sexp)
    ;
        Buffer = float(FloatBuffer), float_buffer_to_sexp(FloatBuffer, Sexp)
    ;
        Buffer = int(IntBuffer), int_buffer_to_sexp(IntBuffer, Sexp)
    ;
        Buffer = string(StringBuffer), string_buffer_to_sexp(StringBuffer, Sexp)
    ).

buffer_to_sexp(Buffer) =  Sexp :- buffer_to_sexp(Buffer, Sexp).

%-----------------------------------------------------------------------------%
%
% Buffer/Vector size accessor
%

lookup_bool_vect_size(Buffer) = Size :- lookup_bool_vect_size(Buffer, Size).

lookup_int_vect_size(Buffer) = Size :- lookup_int_vect_size(Buffer, Size).

lookup_float_vect_size(Buffer) = Size :- lookup_float_vect_size(Buffer, Size).

lookup_string_vect_size(Buffer) = Size :- lookup_string_vect_size(Buffer, Size).

    % Implementation C code

:- pragma foreign_proc("C",
    lookup_bool_vect_size(Buffer::in, Value::out),
    [will_not_call_mercury, promise_pure],
" ASSIGN_SIZE(Value, Buffer);").

:- pragma foreign_proc("C",
    lookup_int_vect_size(Buffer::in, Value::out),
    [will_not_call_mercury, promise_pure],
" ASSIGN_SIZE(Value, Buffer);").

:- pragma foreign_proc("C",
    lookup_float_vect_size(Buffer::in, Value::out),
    [promise_pure, will_not_call_mercury, tabled_for_io,
     does_not_affect_liveness],
" ASSIGN_SIZE(Value, Buffer);").

:- pragma foreign_proc("C",
    lookup_string_vect_size(Buffer::in, Value::out),
    [promise_pure, will_not_call_mercury, tabled_for_io,
     does_not_affect_liveness],
" ASSIGN_SIZE(Value, Buffer);").

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
        is_bool_buffer(Buffer)
    then
        ( if
            lookup_bool_vect_size(bool_buffer(Buffer), X)
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

%-----------------------------------------------------------------------------%
%
% Buffer item lookup
%

:- pragma foreign_proc("C",
    lookup_bool_vect(Buffer::in, Index::in, Value::out),
    [promise_pure, will_not_call_mercury, tabled_for_io,
     does_not_affect_liveness],
"
if (Buffer == NULL
    || Buffer->contents == NULL
    || Buffer->size <= 0
    || Index < 0)

    Value = MR_NO;
else
    Value=(MR_Bool) Buffer->contents[Index];
").

:- pragma foreign_proc("C",
    lookup_int_vect(Buffer::in, Index::in, Value::out),
    [promise_pure, will_not_call_mercury, tabled_for_io,
     does_not_affect_liveness],
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
    lookup_float_vect(Buffer::in, Index::in, Value::out),
    [promise_pure, will_not_call_mercury, tabled_for_io,
     does_not_affect_liveness],
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
    lookup_string_vect(Buffer::in, Index::in, Value::out),
    [promise_pure, will_not_call_mercury, tabled_for_io,
     does_not_affect_liveness],
"
if (Buffer == NULL)
    Value = (MR_String) ""NA_BUFFER"";
else if (Buffer->contents == NULL)
    Value = (MR_String) ""NA_BUFFER_CONTENTS"";
else if (Buffer->size <= 0)
    Value = (MR_String) ""NA_BUFFER_SIZE"";
else if (Index < 0 || Index > Buffer->size - 1)
    Value = (MR_String) ""NA_BUFFER_INDEX"";
else
    Value = (MR_String) Buffer->contents[Index];
").

    % Vector item lookup predicate for 'buffer'-type vectors
    %
    % Index is zero-based.

lookup(Buffer, Index, Item) :-
    ( if is_int_buffer(Buffer) then
        ( if lookup_int_vect(int_buffer(Buffer), Index, Value) then
            Item = int_base(Value)
        else
            Item = int_base(0)
        )
    else if is_float_buffer(Buffer) then
        ( if lookup_float_vect(float_buffer(Buffer), Index, Value) then
            Item = float_base(Value)
        else
            Item = float_base(0.0)
        )
    else if is_bool_buffer(Buffer)
    then
        ( if lookup_bool_vect(bool_buffer(Buffer), Index, Value) then
            Item = bool_base(Value)
        else
            Item = bool_base(no)
        )
    else if is_string_buffer(Buffer) then
        ( if lookup_string_vect(string_buffer(Buffer), Index, Value) then
            Item = string_base(Value)
        else
            Item = string_base("")
        )
    else
        Item = string_base("")
    ).

%-----------------------------------------------------------------------------%
%
% Sourcing R functions
%

%----  Apply R Function to vector Arg into an S expression Result ------------%

%  Apply R Function to logical vector Arg into an S expression Result.

:- pragma foreign_proc("C",
    apply_to_bool(Function::in, Array::array_di, Silent::in,
        Sexp::out, IO0::di, IO::uo) = (Exitcode::out),
    [promise_pure, will_not_call_mercury, tabled_for_io,
     does_not_affect_liveness],
"
SEXP tmp;
MR_ArrayPtr array = (MR_ArrayPtr) Array;
int size = array->size;
MR_Bool *elements = array->elements;

if (size == 0) {
    Exitcode = R_MR_NULL_ARGS;
    Sexp = NULL;
} else {
    SEXP  arg;
    SET_SILENT(Silent);
    PROTECT(arg = allocVector(LGLSXP, size));
    Rboolean *V = (Rboolean*) LOGICAL(arg);
    for (int i = 0; i < size; ++i)
        V[i] = (Rboolean) elements[i]; /* MR_Word -> int, lossy cast. */

    PROTECT(tmp = lang2(install(Function), arg));
    int exitcode = 0;
    Sexp = R_tryEval(tmp, R_GlobalEnv, &exitcode);
    Exitcode = (MR_Integer) exitcode;
}

RESTORE_VERBOSITY;
").

%  Apply R Function to numeric vector Arg into an S expression Result.

:- pragma foreign_proc("C",
    apply_to_float(Function::in, Array::array_di, Silent::in,
        Sexp::out, IO0::di, IO::uo) = (Exitcode::out),
    [promise_pure, will_not_call_mercury, tabled_for_io,
     does_not_affect_liveness],
"
MR_ArrayPtr array = (MR_ArrayPtr) Array;
int size = array->size;
MR_Float *elements = (MR_Float*) array->elements;

/*  Here again caveat: works only iff:
    sizeof(MR_Word) = sizeof(MR_Float) = sizeof(double).
    Otherwise:
    MR_Word *elements__ = (MR_Word*) array->elements;
    doubme elements[size];
    for (int i = 0; i < size; ++i)
        elements[i] = MR_word_to_float(elements__[i]);  */

if (size == 0) {
     Exitcode = R_MR_NULL_ARGS;
     Sexp = NULL;
} else {
     SEXP tmp, arg;

     SET_SILENT(Silent);

     PROTECT(arg = allocVector(REALSXP, size));
     double *V = REAL(arg);
     memcpy(V, elements, size); /* Here it is OK to use memcpy */

     PROTECT(tmp = lang2(install(Function), arg));
     int exitcode = 0;
     Sexp = R_tryEval(tmp, R_GlobalEnv, &exitcode);
     Exitcode = (MR_Integer) exitcode; /* int -> MR_Integer, long signed int */
}

RESTORE_VERBOSITY;
").

%  Apply R Function to integer vector Arg into an S expression Result.

:- pragma foreign_proc("C",
    apply_to_int(Function::in, Array::array_di, Silent::in,
        Sexp::out, IO0::di, IO::uo) = (Exitcode::out),
    [promise_pure, will_not_call_mercury, tabled_for_io,
     does_not_affect_liveness],
"
MR_ArrayPtr array = (MR_ArrayPtr) Array;
int size = array->size;
MR_Integer *elements = (MR_Integer *) array->elements;

/* Should be OK as sizeof of (MR_Integer) = sizeof(MR_Word).
   The only difference is signedness, which is irrelevant. */

if (size == 0) {
     Exitcode = R_MR_NULL_ARGS;
     Sexp = NULL;
} else {
     SEXP tmp, arg;

     SET_SILENT(Silent);

     PROTECT(arg = allocVector(INTSXP, size));
     int *V = INTEGER(arg);
     for (int i = 0; i < size; ++i)
        V[i] = (int) elements[i];   /* MR_Integer -> int, lossy cast,
                                       no memcpy */

     PROTECT(tmp = lang2(install(Function), arg));
     int exitcode = 0;
     Sexp = R_tryEval(tmp, R_GlobalEnv, &exitcode);
     Exitcode = exitcode;
}

RESTORE_VERBOSITY;
").

%  Apply R Function to string vector Arg into an S expression Result.

:- pragma foreign_proc("C",
    apply_to_string(Function::in, Array::array_di, Silent::in,
        Sexp::out, IO0::di, IO::uo) = (Exitcode::out),
    [promise_pure, will_not_call_mercury, tabled_for_io,
     does_not_affect_liveness],
"
MR_ArrayPtr array = (MR_ArrayPtr) Array;
int size = array->size;
MR_String *elements = (MR_String*) array->elements;

/* Should be OK as sizeof(char*) = sizeof(void*) = sizeof(MR_Word) */

if (size == 0) {
    Exitcode = R_MR_NULL_ARGS;
    Sexp = NULL;
} else {
    SEXP tmp, arg;

    SET_SILENT(Silent);

    PROTECT(arg = allocVector(STRSXP, size));
    for (int i = 0; i < size; ++i)
        SET_STRING_ELT(arg, i, Rf_mkString((const char*) elements[i]));

    PROTECT(tmp = lang2(install(Function), arg));
    int exitcode = 0;
    Sexp = R_tryEval(tmp, R_GlobalEnv, &exitcode);
    Exitcode = exitcode;
}

RESTORE_VERBOSITY;
").

%---- Apply R Function to R data frame Arg into an S expression --------------%

% Apply R Function to R logical data frame Arg into an S expression

% The helpers below might be redefined using macros in a more general way,
% to minimize boilerplate. For now, sticking to standard functions.

:- pragma foreign_decl("C",
"
void
R_MR_APPLY_HELPER_LOGICAL(MR_ArrayPtr, MR_Integer, MR_Integer, SEXP);
void
R_MR_APPLY_HELPER_INTEGER(MR_ArrayPtr, MR_Integer, MR_Integer, SEXP);
void
R_MR_APPLY_HELPER_REAL(MR_ArrayPtr, MR_Integer, MR_Integer, SEXP);
void
R_MR_APPLY_HELPER_STRING(MR_ArrayPtr, MR_Integer, MR_Integer, SEXP);
void
R_MR_APPLY_HELPER_VECTOR(MR_ArrayPtr, MR_Integer, MR_Integer, SEXP);
").

:- pragma foreign_proc("C",
    apply_to_bool2d(Function::in, Array::array_di, NumCols::in, Silent::in,
        Sexp::out, IO0::di, IO::uo) = (Exitcode::out),
    [promise_pure, will_not_call_mercury, tabled_for_io,
     does_not_affect_liveness],
"
Exitcode = R_MR_APPLY_HELPER(Function, (MR_ArrayPtr) Array, NumCols, Silent,
    R_MR_APPLY_HELPER_LOGICAL, LGLSXP, Sexp);
").

% Apply R Function of R integral data frame Arg into an S expression Result.

:- pragma foreign_proc("C",
    apply_to_int2d(Function::in, Array::array_di, NumCols::in, Silent::in,
        Sexp::out, IO0::di, IO::uo) = (Exitcode::out),
    [promise_pure, will_not_call_mercury, tabled_for_io],
"
Exitcode = R_MR_APPLY_HELPER(Function, (MR_ArrayPtr) Array, NumCols, Silent,
    R_MR_APPLY_HELPER_INTEGER, INTSXP, Sexp);
").

% Apply R Function of R numeric data frame Arg into an S expression Result.

:- pragma foreign_proc("C",
    apply_to_float2d(Function::in, Array::array_di, NumCols::in, Silent::in,
        Sexp::out, IO0::di, IO::uo) = (Exitcode::out),
    [promise_pure, will_not_call_mercury, tabled_for_io,
     does_not_affect_liveness],
"
Exitcode = R_MR_APPLY_HELPER(Function, (MR_ArrayPtr) Array, NumCols, Silent,
    R_MR_APPLY_HELPER_REAL, REALSXP, Sexp);
").

%  Apply R Function of R character (string) data frame  Arg
%  into an S expression Result.

:- pragma foreign_proc("C",
    apply_to_string2d(Function::in, Array::array_di, NumCols::in, Silent::in,
        Sexp::out, IO0::di, IO::uo) = (Exitcode::out),
    [promise_pure, will_not_call_mercury, tabled_for_io,
     does_not_affect_liveness],
"
Exitcode = R_MR_APPLY_HELPER(Function, (MR_ArrayPtr) Array, NumCols, Silent,
    R_MR_APPLY_HELPER_STRING, STRSXP, Sexp);
").

:- pragma foreign_code("C",
"
inline MR_Integer
R_MR_APPLY_HELPER(MR_String function, MR_ArrayPtr array, MR_Integer numcols,
    MR_Bool silent, void (*f)(MR_ArrayPtr, MR_Integer, MR_Integer, SEXP),
    int sexptype, SEXP s)
{
    SET_SILENT(silent);
    int exitcode;
    SEXP tmp;

    if (numcols == 0) {
        exitcode = R_MR_NULL_ARGS;
        s = NULL;
    } else {
        SEXP arg = PROTECT(allocVector(VECSXP, numcols));
        if (arg == NULL) {
            return R_MR_SIZE_VECT2D_ALLOC_ERROR;
        }
        int numrows = array->size / numcols;
        for (int i = 0; i < numcols; ++i) {
            SEXP tmp_i = PROTECT(allocVector(sexptype, numrows));
            f(array, numrows, i * numcols, tmp_i);
            SET_VECTOR_ELT(arg, i, tmp_i);
        }
        PROTECT(tmp = lang2(install(function), arg));
        // TODO: test s != NULL

        s = R_tryEval(tmp, R_GlobalEnv, &exitcode);
        UNPROTECT(1);
    }
    RESTORE_VERBOSITY;
    return exitcode;
}
").

:- pragma foreign_code("C",
"
inline void
R_MR_APPLY_HELPER_LOGICAL(MR_ArrayPtr array, MR_Integer nrows,
    MR_Integer index, SEXP vect)
{
    Rboolean *L = (Rboolean*) LOGICAL(vect);
    for (int j = 0; j < nrows; ++j)
       L[j]  = (Rboolean) array->elements[index + j];
    /* MR_YES = O = FALSE = (Rboolean) FALSE */
}
").

:- pragma foreign_code("C",
"
inline void
R_MR_APPLY_HELPER_INTEGER(MR_ArrayPtr array, MR_Integer nrows,
    MR_Integer index, SEXP vect)
{
    int *I = INTEGER(vect);
    for (int j = 0; j < nrows; ++j)
        I[j]  = array->elements[index + j];
}
").

:- pragma foreign_code("C",
"
inline void
R_MR_APPLY_HELPER_REAL(MR_ArrayPtr array, MR_Integer nrows,
    MR_Integer index, SEXP vect)
{
    MR_Float *R = REAL(vect);
    for (int j = 0; j < nrows; ++j)
        R[j]  = array->elements[index + j];
}
").

:- pragma foreign_code("C",
"
inline void
R_MR_APPLY_HELPER_STRING(MR_ArrayPtr array, MR_Integer nrows,
    MR_Integer index, SEXP vect)
{
    for (int j = 0; j < nrows; ++j)
        SET_STRING_ELT(vect, j,
            Rf_mkString((MR_String) array->elements[index + j]));
}
").

%  Apply R Function of R data frame Arg with column types either logical,
%  integral, numeric or character (string) into an S expression Result.

apply_to_univ2d(Code, Argv, Silent, Result, !.IO, !:IO) = Exitcode :-
    univ_to_type_name(Argv, NumRows, NumCols, Types),
    Argv = array2d(_, _, Array),
    apply_to_univ2d_helper(Code, NumRows, NumCols, Types, Array, Silent,
        Result, !.IO, !:IO) = Exitcode.

% Getting a 1-row array of type names.

:- pred univ_to_type_name(array2d(univ)::in, int::out, int::out,
            array(string)::array_uo) is det.

univ_to_type_name(A, NumRows, NumCols, Typenames) :-
    bounds(A, NumRows, NumCols),
    Typenames =  array(map(to_type_name(A), 0 `..` (NumCols - 1))).

:- func to_type_name(array2d(univ), int) = string.

:- mode to_type_name(in, in) = out is det.

to_type_name(A, J) = Typename :-
    Typename = type_name(univ_type(A ^ elem(0, J))).

:- func apply_to_univ2d_helper(string, int, int,
    array(string), array(univ), bool,
    sexp, io, io) = int.

:- mode apply_to_univ2d_helper(in, in, in,
    array_di, array_di, in,
    out, di, uo) = out is det.

:- pragma foreign_proc("C",
    apply_to_univ2d_helper(Function::in, NumRows::in, NumCols::in,
        Types::array_di, Array::array_di, Silent::in,
        E::out, IO0::di, IO::uo) = (Exitcode::out),
    [promise_pure, will_not_call_mercury, tabled_for_io,
     does_not_affect_liveness],
"
SET_SILENT(Silent);
SEXP arg;
SEXP tmp;

arg = PROTECT(allocVector(VECSXP, NumCols));
if (arg == NULL) {
    Exitcode = R_MR_SIZE_VECT2D_ALLOC_ERROR;
} else {
    MR_ArrayPtr Types_ptr = (MR_ArrayPtr) Types;
    MR_Integer size = Types_ptr->size;
    const char **elements = (const char**) Types_ptr->elements;
    MR_ArrayPtr Array_i_ptr = (MR_ArrayPtr) Array;

    for (int i = 0; i < NumCols; ++i) {
        if (strcmp(elements[i], ""string"")) {
            SEXP Sexp_i = PROTECT(allocVector(STRSXP, NumRows));
            if (Sexp_i == NULL)
                Exitcode = R_MR_SIZE_VECT2D_ALLOC_ERROR;
            else
                R_MR_APPLY_HELPER_STRING(Array_i_ptr, NumRows,
                    i * NumCols, Sexp_i);
        } else if (strcmp(elements[i], ""int"")) {
            SEXP Sexp_i = PROTECT(allocVector(INTSXP, NumRows));
            if (Sexp_i == NULL)
                Exitcode = R_MR_SIZE_VECT2D_ALLOC_ERROR;
             else
                 R_MR_APPLY_HELPER_INTEGER(Array_i_ptr, NumRows,
                     i * NumCols, Sexp_i);
        } else if (strcmp(elements[i], ""float"")) {
            SEXP Sexp_i = PROTECT(allocVector(REALSXP, NumRows));
            if (Sexp_i == NULL)
                Exitcode = R_MR_SIZE_VECT2D_ALLOC_ERROR;
            else
                R_MR_APPLY_HELPER_REAL(Array_i_ptr, NumRows,
                    i * NumCols, Sexp_i);
        } else if (strcmp(elements[i], ""bool"")) {
            SEXP Sexp_i = PROTECT(allocVector(LGLSXP, NumRows));
            if (Sexp_i == NULL)
                Exitcode = R_MR_SIZE_VECT2D_ALLOC_ERROR;
            else
                R_MR_APPLY_HELPER_LOGICAL(Array_i_ptr, NumRows,
                    i * NumCols, Sexp_i);
        } else {
                Exitcode = R_MR_CALL_ERROR;
        }
    }
}

if (Exitcode != R_MR_CALL_ERROR
       && Exitcode != R_MR_SIZE_VECT2D_ALLOC_ERROR) {
    PROTECT(tmp = lang2(install(Function), arg));
    int exitcode = 0;
    E = R_tryEval(tmp, R_GlobalEnv, &exitcode);
    Exitcode = exitcode;
    UNPROTECT(1);          /* tmp */
    UNPROTECT(NumCols);    /* column vectors */
    UNPROTECT(1);          /* arg */
}
RESTORE_VERBOSITY;
").

%  Apply R Function to list of vectors with types either logical,
%  integral, numeric or character (string) into an S expression Result.

apply_to_univ_list_arrays(Code, L, Silent, Result, !IO) = Exitcode :-
    univ_to_type_name_list(L, NumRows, NumCols, Types),
    apply_to_univ_list_arrays_helper(Code, NumRows, NumCols, Types, L, Silent,
        Result, Exitcode, !.IO, !:IO).

    % univ_to_type_name_list(List, Sizes, NumCols, Typenames)
    %
    % Given a list of arrays of univ elements, output an array giving
    % the size of each univ array, the length of the list,
    % and an array of the corresponding typenames.

:- pred univ_to_type_name_list(list(array(univ))::in,
    list(int)::out, int::out, list(string)::out) is det.

univ_to_type_name_list(L, NumRows, NumCols, Typenames) :-
    NumCols = list.length(L),
    NumRows = map(array.size, L),
    Typenames = map(to_type_name_list, L).

:- func to_type_name_list(array(univ)) = string.

to_type_name_list(A) = Typename :-
    Typename = type_name(univ_type(A ^ elem(0))).

:- pred apply_to_univ_list_arrays_helper(string, list(int),
    int, list(string), list(array(univ)),
    bool, sexp, int, io, io).

:- mode apply_to_univ_list_arrays_helper(in, in,
    in, in, array_di,
    in, out, out, di, uo) is det.

:- pragma foreign_decl("C",
"void
R_MR_APPLY_HELPER_2D(MR_Word L, MR_Integer nrows, MR_Integer sum_nrows,
    void (*f)(MR_ArrayPtr, MR_Integer, MR_Integer, SEXP),
    int sexptype, MR_Integer *Exitcode, SEXP col);
").

:- pragma foreign_proc("C",
    apply_to_univ_list_arrays_helper(Function::in, NumRows::in,
        NumCols::in, Types::in, L::array_di,
        Silent::in, E::out, Exitcode::out, IO0::di, IO::uo),
    [promise_pure, will_not_call_mercury, tabled_for_io,
     does_not_affect_liveness],
"
errno = 0;
int exitcode = 0;
Exitcode = 0;
SET_SILENT(Silent);
SEXP arg = allocVector(VECSXP, NumCols);
R_PreserveObject(arg);
SEXP col[NumCols];
SEXP tmp;
MR_Integer sum_nrows = 0;

if (arg == NULL) {
    Exitcode = R_MR_SIZE_VECT2D_ALLOC_ERROR;
    E = NULL;
} else {

    for (int i = 0; i < NumCols && ! MR_list_is_empty(L); ++i) {

        MR_String type_i = (MR_String) MR_list_head(Types);
        Types = MR_list_tail(Types);
        MR_Integer nrows_i = (MR_Integer) MR_list_head(NumRows);
        sum_nrows += nrows_i;
        NumRows = MR_list_tail(NumRows);

        if (strcmp(type_i, ""string"")) {
            R_MR_APPLY_HELPER_2D(L, nrows_i, sum_nrows,
                R_MR_APPLY_HELPER_STRING, STRSXP, &Exitcode, col[i]);
        } else if (strcmp(type_i, ""int"")) {
            R_MR_APPLY_HELPER_2D(L, nrows_i, sum_nrows,
                R_MR_APPLY_HELPER_INTEGER, INTSXP, &Exitcode, col[i]);
        } else if (strcmp(type_i, ""float"")) {
            R_MR_APPLY_HELPER_2D(L, nrows_i, sum_nrows,
                R_MR_APPLY_HELPER_REAL, REALSXP, &Exitcode, col[i]);
        } else if (strcmp(type_i, ""bool"")) {
            R_MR_APPLY_HELPER_2D(L, nrows_i, sum_nrows,
                R_MR_APPLY_HELPER_LOGICAL, LGLSXP, &Exitcode, col[i]);
        } else {
            Exitcode = R_MR_CALL_ERROR;
        }

        if (Exitcode != R_MR_CALL_ERROR && Exitcode != R_MR_NULL_ARGS)
            SET_VECTOR_ELT(arg, i, col[i]);
    }

    if (Exitcode != R_MR_CALL_ERROR && Exitcode != R_MR_NULL_ARGS) {
        tmp = lang2(install(Function), arg);
        R_PreserveObject(tmp);
        E = R_tryEval(tmp, R_GlobalEnv, &exitcode);
        Exitcode = (MR_Integer) exitcode; /* int -> long int */
        R_ReleaseObject(tmp);
        for (int i = 0; i < NumCols; ++i) R_ReleaseObject(col[i]);
        R_ReleaseObject(arg);
    } else {
        for (int i = 0; i < NumCols; ++i) R_ReleaseObject(col[i]);
        R_ReleaseObject(arg);

        /* There is a remote chance of errno catching some error
           uncaught by Exitcode. Allowing this. */
        if (exitcode == 0 && errno) Exitcode = errno;
    }
}
RESTORE_VERBOSITY;
errno = 0;
").

:- pragma foreign_code("C",
"
inline void
R_MR_APPLY_HELPER_2D(MR_Word L, MR_Integer nrows, MR_Integer sum_nrows,
    void (*f)(MR_ArrayPtr, MR_Integer, MR_Integer, SEXP),
    int sexptype, MR_Integer *Exitcode, SEXP col)
{
    col = allocVector(sexptype, (int) nrows);
    R_PreserveObject(col);
    MR_ArrayPtr array = (MR_ArrayPtr) MR_list_head(L);
    f(array, nrows, sum_nrows, col);
    L = MR_list_tail(L);
}
").

%-----------------------------------------------------------------------------%
%
% Exception handling (bound check)
% Only implemented for numeric type
% (Mercury: MR_Float / R: numeric() / C: double).

:- func to_float_base(buffer_item) = float.

to_float_base(B) = F :- (B = float_base(X) -> F = X ; F = 0.0).

:- pred check_finite(string, pred(string, buffer_item, io, io), behavior,
    buffer_item, io, io).
:- mode check_finite(in, pred(in, out, di, uo) is det, in,
    out, di, uo) is cc_multi.

check_finite(Code, Predicate, Behavior, Result, !IO) :-
    ( try [io(!IO)] (
        Predicate(Code, Ret, !.IO, !:IO)
    )
    then
        F = to_float_base(Ret),
            ( if
                is_inf(F)
            then
                ( if  Behavior ^ numeric ^ inf = no
                then
                    unexpected($pred,
                        "Returned Infinity yet Infinity is not allowed.")
                else
                    Result = Ret
                )
            else
                ( if is_nan(F)
                then
                    ( if Behavior ^ numeric ^ nan = no
                    then
                        unexpected($pred,
                            "Returned NAN yet NAN is not allowed.")
                    else
                        Result = Ret
                    )
                else
                    Result = Ret
                )
            )
            %%
            % Here possibly add in other cases
            %
    catch_any Excp ->
            io.format("Returned: EXCP (%s)\n", [s(string(Excp))], !IO),
            Result = nil
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

write_rbool(Value, !IO) :-
    ( if
        Value = yes
    then
        write_string("TRUE", !IO)
    else
        write_string("FALSE", !IO)
    ).

writeln_rbool(Value, !IO) :- write_rbool(Value, !IO), io.nl(!IO).

    % Print helper for catch-all type 'buffer_item'

write_item(Item, !IO) :-
    ( if Item = int_base(Value) then
        io.write_int(Value, !IO)
    else if Item = float_base(Value) then
        io.write_float(Value, !IO)
    else if Item = string_base(Value) then
        io.write_string(Value, !IO)
    else if Item = bool_base(Value) then
	ri.write_rbool(Value, !IO)
    else
        io.nl(!IO)
    ).

writeln_item(Item, !IO) :- write_item(Item, !IO), io.nl(!IO).

% --------------------------------------------------------------------------- %
:- end_module ri.
% --------------------------------------------------------------------------- %
