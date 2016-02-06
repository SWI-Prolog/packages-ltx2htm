/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2008-2013, University of Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Run latex2html without installing it.  Usage:

	% pl -s path/to/run.pl -g main -- file
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

setup :-
	prolog_load_context(directory, Dir),
	file_directory_name(Dir, Parent),
	asserta(user:file_search_path(ltx2html, Dir)),
	asserta(user:file_search_path(foreign, Dir)),
	asserta(user:file_search_path(library, Parent)), % find library(pldoc)
	asserta(user:file_search_path(library, Dir)).

:- setup.
:- load_files(latex2html, [silent(true)]).

main :-
	current_prolog_flag(argv, [File]), !,
	(   process(File)
	->  halt
	;   halt(1)
	).
main :-
	format(user_error, 'Usage: script options -- file~n', []),
	halt(1).

process(File) :-
	exists_file(File),
	file_name_extension(Base, tex, File), !,
	latex2html(Base).
process(Base) :-
	file_name_extension(Base, tex, File),
	exists_file(File), !,
	latex2html(Base).



