/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1997-2011, University of Amsterdam
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

#include <SWI-Prolog.h>
#include <stdio.h>
#include <string.h>

#define MAXLINE 10240

static int
substr(const char *in, const char *sub)
{ int ls = strlen(sub);
  int l = strlen(in) - ls;

  for( ; l >= 0; in++, l-- )
  { if ( strncmp(in, sub, ls) == 0 )
      return 1;
  }

  return 0;
}


static foreign_t
pl_get_ps_parameters(term_t file, term_t iseps, term_t bb)
{ char *fname;
  FILE *fd;

  if ( !PL_get_chars(file, &fname, CVT_ALL) )
    return PL_warning("get_ps_parameters/3: invalid filename");

  if ( (fd = fopen(fname, "r")) )
  { char buf[MAXLINE];
    char *s;

    if ( (s=fgets(buf, sizeof(buf), fd)) )
    { int rc;

      if ( substr(s, "EPSF") )
	rc = PL_unify_atom_chars(iseps, "eps");
      else
	rc = PL_unify_atom_chars(iseps, "ps");

      if ( !rc )
	return rc;
    }

    do
    { double a1, a2, a3, a4;

      if ( sscanf(buf, "%%%%BoundingBox: %lf %lf %lf %lf", &a1, &a2, &a3, &a4) == 4 )
      { fclose(fd);
	return PL_unify_term(bb,
			     PL_FUNCTOR, PL_new_functor(PL_new_atom("bb"), 4),
			     PL_FLOAT, a1,
			     PL_FLOAT, a2,
			     PL_FLOAT, a3,
			     PL_FLOAT, a4);
      }
    } while( (s=fgets(buf, sizeof(buf), fd)) );

    fclose(fd);
    PL_warning("get_ps_parameters/3: could not find %%%%BoundingBox in %s",
	       fname);

    PL_fail;
  }

  PL_warning("get_ps_parameters/3: could not open %s", fname);
  PL_fail;
}


void
install_ps()
{ PL_register_foreign("get_ps_parameters", 3, pl_get_ps_parameters, 0);
}

