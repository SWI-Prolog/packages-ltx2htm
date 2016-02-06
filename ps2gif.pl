/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1997-2013, University of Amsterdam
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

:- module(ps2gif,
	  [ ps2gif/2,			% +In, +Out
	    ps2gif/3			% +In, +Out, +Options
	  ]).

option(gs,	gs).
option(res,	72).
option(device,	ppmraw).
option(tmp,	Tmp) :-
	tmp_file(ps2gif, Tmp).

ps2gif(In, Out) :-
	ps2gif(In, Out, []).

ps2gif(In, Out, Options) :-
	get_option(Options, tmp(Tmp)),
	get_option(Options, res(Res0)),
	absolute_file_name(In, [ access(read),
				 extensions([ps, eps]),
				 file_errors(fail)
			       ],
			   InFile),
	get_ps_parameters(InFile, EPS, bb(X1,Y1,X2,Y2)),
	(   get_option(Options, width(W))
	->  ScaleX is W/((X2-X1)/72)
	;   ScaleX is 1
	),
	(   get_option(Options, height(H))
	->  ScaleY is H/((Y2-Y1)/72)
	;   ScaleY is 1
	),
	ResX is Res0 * ScaleX,
	ResY is Res0 * ScaleY,
	(   ResX =:= ResY
	->  Res = ResX
	;   sformat(Res, '~wx~w', [ResX, ResY])
	),
	BBX is -X1,
	BBY is -Y1,
	BBW0 = X2 - X1,
	BBH0 = Y2 - Y1,
	BBW is round(BBW0 * ResX / 72),
	BBH is round(BBH0 * ResY / 72),
	gs_command([size(BBW,BBH),tmp(Tmp),res(Res)|Options], Cmd),
	setup_call_cleanup(open(pipe(Cmd), write, Pipe),
			   (   format(Pipe, '~w ~w translate ', [BBX, BBY]),
			       format(Pipe, '(~w) run ', InFile),
			       (   EPS == eps
			       ->  format(Pipe, 'showpage ', [])
			       ;   true
			       ),
			       format(Pipe, 'quit~n', [])
			   ),
			   close(Pipe)),
	(   exists_file(Tmp)
	->  ppm2gif(Tmp, Out, Options),
	    delete_file(Tmp)
	;   EPS == ps,
	    format(user_error,
		   'No output from ~w, Trying again with showpage~n',
		   [InFile]),
	    setup_call_cleanup(open(pipe(Cmd), write, Pipe),
			       (   format(Pipe, '~w ~w translate ', [BBX, BBY]),
				   format(Pipe, '(~w) run ', InFile),
				   format(Pipe, 'showpage ', []),
				   format(Pipe, 'quit~n', [])
			       ),
			       close(Pipe)),
	    ppm2gif(Tmp, Out, Options)
	).


ppm2gif(Tmp, Out, Options) :-
	(   get_option(Options, margin(B))
	->  aformat(Cmd,
		    'pnmcrop < ~w | pnmmargin ~w | pnmmargin -black 1 | ppmquant 192 | ppmtogif > ~w',
		    [Tmp, B, Out])
	;   aformat(Cmd, 'pnmcrop < ~w | ppmquant 192 | ppmtogif > ~w',
		    [Tmp, Out])
	),
	shell(Cmd).

gs_command(Options, Cmd) :-
	get_option(Options, gs(GS)),
	get_option(Options, res(Res)),
	get_option(Options, device(Dev)),
	get_option(Options, tmp(Tmp)),
	(   get_option(Options, size(W, H))
	->  sformat(SCmd, '-g~wx~w', [W, H])
	;   SCmd = ''
	),
	aformat(Cmd,
		'~w -q -dNOPAUSE -sDEVICE=~w ~w -r~w -sOutputFile=~w',
		[GS, Dev, SCmd, Res, Tmp]).


get_option(List, Term) :-
	memberchk(Term, List), !.
get_option(_, Term) :-
	functor(Term, Name, _),
	option(Name, Def), !,
	arg(1, Term, Def).

aformat(Atom, Fmt, Args) :-
	format(atom(Atom), Fmt, Args).
