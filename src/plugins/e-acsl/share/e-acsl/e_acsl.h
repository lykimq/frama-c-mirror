/**************************************************************************/
/*                                                                        */
/*  This file is part of the Frama-C's E-ACSL plug-in.                    */
/*                                                                        */
/*  Copyright (C) 2012-2025                                               */
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

/*! ***********************************************************************
 * \file
 * \brief  Public C API of E-ACSL Runtime Library
 *
 * Functions and variables with non-static linkage used for instrumentation.
 **************************************************************************/

/* Memory model settings
 *    Memory model:
 *      E_ACSL_BITTREE_MMODEL - use Patricia-trie (tree-based) memory model, or
 *      E_ACSL_SEGMENT_MMODEL - use segment-based (shadow) memory model
 *    Verbosity level:
 *      E_ACSL_VERBOSE - put an executable in verbose mode that
 *        prints extra messages (unset by default)
 *    Debug Features:
 *      E_ACSL_DEBUG - enable debug features in RTL (unset by default)
 *      E_ACSL_DEBUG_VERBOSE - verbose debug output (via DVLOG macro)
 *      E_ACSL_DEBUG_LOG - name of the log file where debug messages are
 *        output. The file name should be unquoted string with '-'
 *        (set by default) indicating a standard stream
 *    Validity:
 *      E_ACSL_WEAK_VALIDITY - use notion of weak validity
 *        Given an expression `(p+i)`, where `p` is a pointer and `i` is an
 *        integer offset weak validity indicates that `(p+i)` is valid if it
 *        belongs to memory allocation. In strong validity `(p+i)` is valid
 *        iff both `p` and `(p+i)` belong to memory allocation and to the same
 *        memory block.
 *    Temporal analysis:
 *      E_ACSL_TEMPORAL - enable temporal analysis in RTL
 *    Assertions:
 *      E_ACSL_NO_ASSERT_FAIL - do not issue abort signal of E-ACSL
 *        assertion failure
 *      E_ACSL_FAIL_EXITCODE - do not issue abort signal but exit with a
 *        given code
 *      E_ACSL_DEBUG_ASSERT - print the valid predicates.
 *    Shadow spaces (only for segment model):
 *      E_ACSL_STACK_SIZE - size (in MB) of the tracked program stack
 *      E_ACSL_HEAP_SIZE - size (in MB) of the tracked program heap
 *    String functions:
 *      E_ACSL_NO_COMPILER_BUILTINS - if undefined (default) then use
 *      compiler builtin string functions (e.g., memset -> __builtin_memset)
 *    Behaviour of assert:
 *      E_ACSL_EXTERNAL_ASSERT - if this macro is defined then function
 *      `__e_acsl_assert` is excluded from compilation. This is to allow
 *      providing alternative definitions of assertions by users.
 *      E_ACSL_EXTERNAL_PRINT_VALUE - if this macro is defined then function
 *      `__e_acsl_print_value` is excluded from compilation. This is to allow
 *      providing alternative definitions of data printing functions by users.
 *    Memory deallocation:
 *      E_ACSL_FREE_VALID_ADDRESS -- Clause 7.20.3.2 of C99 standard states
 *      that NULL is a valid input to free:
 *        "The free function causes the space pointed to by ptr [its argument]
 *         to be deallocated, that is, made available for further allocation.
 *         If ptr is a null pointer, no action occurs."
 *      Yet, some tools insist that it is a bug. For instance, there is a
 *      bunch of test cases in Toyota ITC Benchmarks. To make such tools
 *      happy the following option is introduced. By default it should be
 *      undefined (disabled) though.
*/

/************************************************************************/
/*** User API {{{ ***/
/************************************************************************/

#include "instrumentation_model/e_acsl_assert.h"
#include "instrumentation_model/e_acsl_assert_data.h"
#include "observation_model/e_acsl_heap.h"

/* }}} */

/************************************************************************/
/*** Generated code API {{{ ***/
/************************************************************************/

#include "instrumentation_model/e_acsl_assert_data_api.h"
#include "instrumentation_model/e_acsl_contract.h"
#include "instrumentation_model/e_acsl_temporal.h"
#include "numerical_model/e_acsl_floating_point.h"
#include "numerical_model/e_acsl_gmp_api.h"
#include "observation_model/e_acsl_observation_model.h"

/* }}} */

/************************************************************************/
/*** Builtins {{{ ***/
/************************************************************************/

#include "libc_replacements/e_acsl_stdio.h"
#include "libc_replacements/e_acsl_string.h"

/* }}} */
