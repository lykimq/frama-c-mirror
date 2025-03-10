/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2025                                               */
/*    CEA (Commissariat à l'énergie atomique et aux énergies              */
/*         alternatives)                                                  */
/*                                                                        */
/*  you can redistribute it and/or modify it under the terms of the GNU   */
/*  Lesser General Public License as published by the Free Software       */
/*  Foundation, version 2.1.                                              */
/*                                                                        */
/*  It is distributed in the hope that it will be useful,                 */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         */
/*  GNU Lesser General Public License for more details.                   */
/*                                                                        */
/*  See the GNU Lesser General Public License version 2.1                 */
/*  for more details (enclosed in the file licenses/LGPLv2.1).            */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_NL_TYPES
#define __FC_NL_TYPES
#include "features.h"
__PUSH_FC_STDLIB
__BEGIN_DECLS

typedef unsigned long nl_catd;
typedef unsigned long nl_item;
#define NL_SETD 1
#define NL_CAT_LOCALE 1
extern int       catclose(nl_catd);
extern char     *catgets(nl_catd, int, int, const char *);
extern nl_catd   catopen(const char *, int);

__END_DECLS

__POP_FC_STDLIB
#endif
