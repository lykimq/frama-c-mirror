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

#include "signal.h"
#include "errno.h"
#include "stdlib.h"
__PUSH_FC_STDLIB

static volatile sig_atomic_t __fc_signal_nondet;

struct sigaction __fc_sigaction[SIGRTMAX+1];

__fc_sighandler_t __fc_signal_handlers[SIGRTMAX+1];

void __fc_sig_dfl(int sig) {
  if (__fc_signal_nondet) exit(__fc_signal_nondet);
}
void __fc_sig_ign(int sig) {}
void __fc_sig_err(int sig) {}

static void set_signal_handler(int sig, __fc_sighandler_t func) {
  __fc_signal_handlers[sig] = func;
}

static __fc_sighandler_t get_signal_handler(int sig) {
  __fc_sighandler_t handler = __fc_signal_handlers[sig];

  // If __fc_signal_handlers[sig] was not already set, choose between
  // SIG_DFL and SIG_IGN now
  if (handler == 0) {
    if (__fc_signal_nondet) {
      handler = SIG_DFL;
    } else {
      handler = SIG_IGN;
    }
    set_signal_handler(sig, handler);
  }

  return handler;
}

__fc_sighandler_t signal(int sig, __fc_sighandler_t func) {
  if (__fc_signal_nondet && sig >= 0 && sig <= SIGRTMAX && func != 0) {
    __fc_sighandler_t old = get_signal_handler(sig);
    set_signal_handler(sig, func);
    return old;
  } else {
    errno = EINVAL;
    return SIG_ERR;
  }
}

int raise(int sig) {
  if (__fc_signal_nondet && sig >= 0 && sig <= SIGRTMAX) {
    __fc_sighandler_t handler = get_signal_handler(sig);
    (*handler)(sig);
    return 0;
  } else {
    return -1;
  }
}

__POP_FC_STDLIB
