/*
 * "getline" - portable getline reimplementation
 * Copyright (c) 2000	James Kehl <ecks@optusnet.com.au>
 * Modified 2008, Justin J. Meza < justin dot meza at gmail dot com >
 * - Changed `ssize_t' return types to `unsigned int' primarily for Windows
 *   compliance without the use of typedefs.
 * - Changed type of `i' from `int' to `unsigned int' for same above reason
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Library General Public License as published
 * by the Free Software Foundation; version 2 of the License, with the
 * added restriction that it may only be converted to the version 2 of the
 * GNU General Public License.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
 */
#include <stdio.h>
#include <stdlib.h>

unsigned int get_line(char **LINEPTR, size_t *N, FILE *STREAM)
{
	int c;
	unsigned int i=0;

	if (*LINEPTR==NULL || *N==0)
	{
		/* 128 sounds reasonable */
		*LINEPTR=malloc(128);
		if (!(*LINEPTR))
			return -1; /* Malloc failed. Erk. */
		*N=128;
	}

	while (1)
	{
		c=getc(STREAM);
		if (c < 0)
			return -1; /* EOF */

		(*LINEPTR)[i]=(char) c;
		i++;
		/* Time to expand the buffer */
		if (i >= *N)
		{
			char *new;
			new=realloc(*LINEPTR, (*N) * 2);
			if (!new) return -1; /* Yow */
			*LINEPTR=new;
			(*N)*=2;
		}
		if (c=='\n')
			break;
	};

	(*LINEPTR)[i]=0;
	return i;
}

unsigned int get_line_at(char **LINEPTR, size_t *POS, size_t *N, FILE *STREAM)
        /* Grab a line beginning at a particular position in the original
	 * buffer. */
{
	int c;
	unsigned int i=*POS;

	if (*LINEPTR==NULL || *N==0)
	{
		/* 128 sounds reasonable */
		*LINEPTR=malloc(128);
		if (!(*LINEPTR))
			return -1; /* Malloc failed. Erk. */
		*N=128;
	}

	while (1)
	{
		c=getc(STREAM);
		if (c < 0)
			return -1; /* EOF */

		(*LINEPTR)[i]=(char) c;
		i++;
		/* Time to expand the buffer */
		if (i >= *N)
		{
			char *new;
			new=realloc(*LINEPTR, (*N) * 2);
			if (!new) return -1; /* Yow */
			*LINEPTR=new;
			(*N)*=2;
		}
		if (c=='\n')
			break;
	};

	(*LINEPTR)[i]=0;
	*POS = i;
	return i;
}

