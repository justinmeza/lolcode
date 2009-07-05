/*
 * args.h
 *
 * Simple functions for manipulating command line arguments.
 *
 * MAINTAINER
 *
 *      Justin J. Meza < justin dot meza at gmail dot com >
 *
 * LICENSE
 *
 *      Copyright (c) 2007 Justin J. Meza
 *
 *      Permission is hereby granted, free of charge, to any person obtaining a
 *      copy of this software and associated documentation files (the
 *      "Software"), to deal in the Software without restriction, including
 *      without limitation the rights to use, copy, modify, merge, publish,
 *      distribute, sublicense, and/or sell copies of the Software, and to
 *      permit persons to whom the Software is furnished to do so, subject to
 *      the following conditions:
 *
 *      The above copyright notice and this permission notice shall be included
 *      in all copies or substantial portions of the Software.
 *
 *      THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 *      OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 *      MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 *      IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 *      CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 *      TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 *      SOFTWARE R THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#ifndef __ARGS__
#define __ARGS__

    int
get_arg(int ARGC, char **ARGV, char MNEMONIC, int *INDEX)
    /* Determines if a particular argument is present and optionally returns
     * its associated value. */
{
    /* Loop through arguments */
    while (--ARGC) {
        /* If we have a matching argument format at argc and a value at argc +
         * 1, */
        if (ARGV[ARGC][0] == '-' && ARGV[ARGC][1] == MNEMONIC) {
            /* Assign the argument value index */
            if (!ARGV[ARGC + 1]) *INDEX = -1;
            else *INDEX = ARGC + 1;
            break;
        }
    }
    /* Return 1 on success, 0 on error */
    return ARGC ? 1 : 0;
}

#endif /* __ARGS__ */
