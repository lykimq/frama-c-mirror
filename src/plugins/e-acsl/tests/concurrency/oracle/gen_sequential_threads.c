/* Generated by Frama-C */
#include "pthread.h"
#include "sched.h"
#include "signal.h"
#include "stddef.h"
#include "stdint.h"
#include "stdio.h"
#include "stdlib.h"
#include "time.h"
extern  __attribute__((__FC_BUILTIN__)) int __e_acsl_sound_verdict;

/*@ requires valid_thread: \valid(thread);
    requires valid_null_attr: attr == \null || \valid_read(attr);
    requires valid_routine: \valid_function(start_routine);
    requires valid_null_arg: arg == \null || \valid((char *)arg);
    ensures
      initialization: success_or_error:
        (\result == 0 && \initialized(\old(thread))) || \result == 11 ||
        \result == 22 || \result == 1;
    assigns *thread, \result;
    assigns *thread \from *attr;
    assigns \result \from (indirect: *attr);
 */
int __gen_e_acsl_pthread_create(pthread_t * restrict thread,
                                pthread_attr_t const * restrict attr,
                                void *(*start_routine)(void *),
                                void * restrict arg);

/*@ requires valid_or_null_retval: retval == \null || \valid(retval);
    ensures
      success_or_error:
        \result == 0 || \result == 35 || \result == 22 || \result == 3;
    assigns *retval, \result;
    assigns *retval \from thread;
    assigns \result \from (indirect: thread);
    
    behavior ignore_retval:
      assumes null_retval: retval == \null;
      assigns \result;
      assigns \result \from (indirect: thread);
    
    behavior use_retval:
      assumes valid_retval: \valid(retval);
      assigns *retval, \result;
      assigns *retval \from thread;
      assigns \result \from (indirect: thread);
 */
int __gen_e_acsl_pthread_join(pthread_t thread, void **retval);

int *values[10];
void *write_value(void *arg)
{
  void *__retres;
  __e_acsl_store_block((void *)(& __retres),8UL);
  __e_acsl_store_block((void *)(& arg),8UL);
  int idx = *((int *)arg);
  __e_acsl_store_block((void *)(& idx),4UL);
  __e_acsl_full_init((void *)(& idx));
  __e_acsl_initialize((void *)(& values[idx]),sizeof(int *));
  values[idx] = (int *)malloc(sizeof(int));
  __e_acsl_initialize((void *)values[idx],sizeof(int));
  *(values[idx]) = idx;
  __e_acsl_full_init((void *)(& __retres));
  __retres = (void *)0;
  __e_acsl_delete_block((void *)(& arg));
  __e_acsl_delete_block((void *)(& idx));
  __e_acsl_delete_block((void *)(& __retres));
  return __retres;
}

void *read_value(void *arg)
{
  void *__retres;
  __e_acsl_store_block((void *)(& __retres),8UL);
  __e_acsl_store_block((void *)(& arg),8UL);
  int idx = *((int *)arg);
  __e_acsl_store_block((void *)(& idx),4UL);
  __e_acsl_full_init((void *)(& idx));
  {
    int __gen_e_acsl_initialized;
    int __gen_e_acsl_and;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data = {.values = (void *)0};
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_2 =
      {.values = (void *)0};
    __gen_e_acsl_initialized = __e_acsl_initialized((void *)(& values[idx]),
                                                    sizeof(int *));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_2,"&values[idx]",
                                 (void *)(& values[idx]));
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_2,
                                   "sizeof(int *)",0,sizeof(int *));
    if (__gen_e_acsl_initialized) {
      int __gen_e_acsl_valid_read;
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_3 =
        {.values = (void *)0};
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_3,"idx",0,idx);
      __gen_e_acsl_assert_data_3.blocking = 1;
      __gen_e_acsl_assert_data_3.kind = "RTE";
      __gen_e_acsl_assert_data_3.pred_txt = "idx < 10";
      __gen_e_acsl_assert_data_3.file = "sequential_threads.c";
      __gen_e_acsl_assert_data_3.fct = "read_value";
      __gen_e_acsl_assert_data_3.line = 17;
      __gen_e_acsl_assert_data_3.name = "index_bound";
      __e_acsl_assert(idx < 10,& __gen_e_acsl_assert_data_3);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_3);
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_4 =
        {.values = (void *)0};
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_4,"idx",0,idx);
      __gen_e_acsl_assert_data_4.blocking = 1;
      __gen_e_acsl_assert_data_4.kind = "RTE";
      __gen_e_acsl_assert_data_4.pred_txt = "0 <= idx";
      __gen_e_acsl_assert_data_4.file = "sequential_threads.c";
      __gen_e_acsl_assert_data_4.fct = "read_value";
      __gen_e_acsl_assert_data_4.line = 17;
      __gen_e_acsl_assert_data_4.name = "index_bound";
      __e_acsl_assert(0 <= idx,& __gen_e_acsl_assert_data_4);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_4);
      __gen_e_acsl_valid_read = __e_acsl_valid_read((void *)values[idx],
                                                    sizeof(int),
                                                    (void *)values[idx],
                                                    (void *)(& values[idx]));
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_2,
                                   "values[idx]",(void *)values[idx]);
      __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_2,
                                     "sizeof(int)",0,sizeof(int));
      __gen_e_acsl_and = __gen_e_acsl_valid_read;
    }
    else __gen_e_acsl_and = 0;
    __gen_e_acsl_assert_data_2.blocking = 1;
    __gen_e_acsl_assert_data_2.kind = "RTE";
    __gen_e_acsl_assert_data_2.pred_txt = "\\valid_read(values[idx])";
    __gen_e_acsl_assert_data_2.file = "sequential_threads.c";
    __gen_e_acsl_assert_data_2.fct = "read_value";
    __gen_e_acsl_assert_data_2.line = 17;
    __gen_e_acsl_assert_data_2.name = "mem_access";
    __e_acsl_assert(__gen_e_acsl_and,& __gen_e_acsl_assert_data_2);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_2);
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_5 =
      {.values = (void *)0};
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_5,"idx",0,idx);
    __gen_e_acsl_assert_data_5.blocking = 1;
    __gen_e_acsl_assert_data_5.kind = "RTE";
    __gen_e_acsl_assert_data_5.pred_txt = "idx < 10";
    __gen_e_acsl_assert_data_5.file = "sequential_threads.c";
    __gen_e_acsl_assert_data_5.fct = "read_value";
    __gen_e_acsl_assert_data_5.line = 17;
    __gen_e_acsl_assert_data_5.name = "index_bound";
    __e_acsl_assert(idx < 10,& __gen_e_acsl_assert_data_5);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_5);
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_6 =
      {.values = (void *)0};
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_6,"idx",0,idx);
    __gen_e_acsl_assert_data_6.blocking = 1;
    __gen_e_acsl_assert_data_6.kind = "RTE";
    __gen_e_acsl_assert_data_6.pred_txt = "0 <= idx";
    __gen_e_acsl_assert_data_6.file = "sequential_threads.c";
    __gen_e_acsl_assert_data_6.fct = "read_value";
    __gen_e_acsl_assert_data_6.line = 17;
    __gen_e_acsl_assert_data_6.name = "index_bound";
    __e_acsl_assert(0 <= idx,& __gen_e_acsl_assert_data_6);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_6);
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"*(values[idx])",
                                 0,*(values[idx]));
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,"idx",0,idx);
    __gen_e_acsl_assert_data.blocking = 1;
    __gen_e_acsl_assert_data.kind = "Assertion";
    __gen_e_acsl_assert_data.pred_txt = "*(values[idx]) == idx";
    __gen_e_acsl_assert_data.file = "sequential_threads.c";
    __gen_e_acsl_assert_data.fct = "read_value";
    __gen_e_acsl_assert_data.line = 17;
    __e_acsl_assert(*(values[idx]) == idx,& __gen_e_acsl_assert_data);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data);
  }
  /*@ assert *(values[idx]) == idx; */ ;
  free((void *)values[idx]);
  __e_acsl_full_init((void *)(& __retres));
  __retres = (void *)0;
  __e_acsl_delete_block((void *)(& arg));
  __e_acsl_delete_block((void *)(& idx));
  __e_acsl_delete_block((void *)(& __retres));
  return __retres;
}

/*@ requires valid_or_null_retval: retval == \null || \valid(retval);
    ensures
      success_or_error:
        \result == 0 || \result == 35 || \result == 22 || \result == 3;
    assigns *retval, \result;
    assigns *retval \from thread;
    assigns \result \from (indirect: thread);
    
    behavior ignore_retval:
      assumes null_retval: retval == \null;
      assigns \result;
      assigns \result \from (indirect: thread);
    
    behavior use_retval:
      assumes valid_retval: \valid(retval);
      assigns *retval, \result;
      assigns *retval \from thread;
      assigns \result \from (indirect: thread);
 */
int __gen_e_acsl_pthread_join(pthread_t thread, void **retval)
{
  __e_acsl_contract_t *__gen_e_acsl_contract;
  int __retres;
  __e_acsl_store_block((void *)(& __retres),4UL);
  {
    int __gen_e_acsl_or;
    int __gen_e_acsl_valid_2;
    __e_acsl_store_block((void *)(& retval),8UL);
    __e_acsl_store_block((void *)(& thread),8UL);
    __gen_e_acsl_contract = __e_acsl_contract_init(2UL);
    __e_acsl_assert_data_t __gen_e_acsl_assert_data = {.values = (void *)0};
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data,"retval",
                                 (void *)retval);
    if (retval == (void **)0) __gen_e_acsl_or = 1;
    else {
      int __gen_e_acsl_valid;
      __gen_e_acsl_valid = __e_acsl_valid((void *)retval,sizeof(void *),
                                          (void *)retval,(void *)(& retval));
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data,"retval",
                                   (void *)retval);
      __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data,
                                     "sizeof(void *)",0,sizeof(void *));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,
                                   "\\valid(retval)",0,__gen_e_acsl_valid);
      __gen_e_acsl_or = __gen_e_acsl_valid;
    }
    __gen_e_acsl_assert_data.blocking = 1;
    __gen_e_acsl_assert_data.kind = "Precondition";
    __gen_e_acsl_assert_data.pred_txt = "retval == \\null || \\valid(retval)";
    __gen_e_acsl_assert_data.file = "FRAMAC_SHARE/libc/pthread.h";
    __gen_e_acsl_assert_data.fct = "pthread_join";
    __gen_e_acsl_assert_data.line = 405;
    __gen_e_acsl_assert_data.name = "valid_or_null_retval";
    __e_acsl_assert(__gen_e_acsl_or,& __gen_e_acsl_assert_data);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data);
    __e_acsl_contract_set_behavior_assumes(__gen_e_acsl_contract,0UL,
                                           retval == (void **)0);
    __gen_e_acsl_valid_2 = __e_acsl_valid((void *)retval,sizeof(void *),
                                          (void *)retval,(void *)(& retval));
    __e_acsl_contract_set_behavior_assumes(__gen_e_acsl_contract,1UL,
                                           __gen_e_acsl_valid_2);
  }
  __retres = pthread_join(thread,retval);
  {
    int __gen_e_acsl_or_2;
    int __gen_e_acsl_or_3;
    int __gen_e_acsl_or_4;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_2 =
      {.values = (void *)0};
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,"\\result",0,
                                 __retres);
    if (__retres == 0) __gen_e_acsl_or_2 = 1;
    else {
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,"\\result",0,
                                   __retres);
      __gen_e_acsl_or_2 = __retres == 35;
    }
    if (__gen_e_acsl_or_2) __gen_e_acsl_or_3 = 1;
    else {
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,"\\result",0,
                                   __retres);
      __gen_e_acsl_or_3 = __retres == 22;
    }
    if (__gen_e_acsl_or_3) __gen_e_acsl_or_4 = 1;
    else {
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,"\\result",0,
                                   __retres);
      __gen_e_acsl_or_4 = __retres == 3;
    }
    __gen_e_acsl_assert_data_2.blocking = 1;
    __gen_e_acsl_assert_data_2.kind = "Postcondition";
    __gen_e_acsl_assert_data_2.pred_txt = "\\result == 0 || \\result == 35 || \\result == 22 || \\result == 3";
    __gen_e_acsl_assert_data_2.file = "FRAMAC_SHARE/libc/pthread.h";
    __gen_e_acsl_assert_data_2.fct = "pthread_join";
    __gen_e_acsl_assert_data_2.line = 409;
    __gen_e_acsl_assert_data_2.name = "success_or_error";
    __e_acsl_assert(__gen_e_acsl_or_4,& __gen_e_acsl_assert_data_2);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_2);
    __e_acsl_contract_clean(__gen_e_acsl_contract);
    __e_acsl_delete_block((void *)(& retval));
    __e_acsl_delete_block((void *)(& thread));
    __e_acsl_delete_block((void *)(& __retres));
    return __retres;
  }
}

/*@ requires valid_thread: \valid(thread);
    requires valid_null_attr: attr == \null || \valid_read(attr);
    requires valid_routine: \valid_function(start_routine);
    requires valid_null_arg: arg == \null || \valid((char *)arg);
    ensures
      initialization: success_or_error:
        (\result == 0 && \initialized(\old(thread))) || \result == 11 ||
        \result == 22 || \result == 1;
    assigns *thread, \result;
    assigns *thread \from *attr;
    assigns \result \from (indirect: *attr);
 */
int __gen_e_acsl_pthread_create(pthread_t * restrict thread,
                                pthread_attr_t const * restrict attr,
                                void *(*start_routine)(void *),
                                void * restrict arg)
{
  pthread_t *__gen_e_acsl_at;
  int __retres;
  __e_acsl_store_block((void *)(& __retres),4UL);
  {
    int __gen_e_acsl_valid;
    int __gen_e_acsl_or;
    int __gen_e_acsl_or_2;
    __e_acsl_store_block((void *)(& arg),8UL);
    __e_acsl_store_block((void *)(& start_routine),8UL);
    __e_acsl_store_block((void *)(& attr),8UL);
    __e_acsl_store_block((void *)(& thread),8UL);
    __gen_e_acsl_at = thread;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data = {.values = (void *)0};
    __gen_e_acsl_valid = __e_acsl_valid((void *)thread,sizeof(pthread_t),
                                        (void *)thread,(void *)(& thread));
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data,"thread",
                                 (void *)thread);
    __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data,
                                   "sizeof(pthread_t)",0,sizeof(pthread_t));
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,
                                 "\\valid(thread)",0,__gen_e_acsl_valid);
    __gen_e_acsl_assert_data.blocking = 1;
    __gen_e_acsl_assert_data.kind = "Precondition";
    __gen_e_acsl_assert_data.pred_txt = "\\valid(thread)";
    __gen_e_acsl_assert_data.file = "FRAMAC_SHARE/libc/pthread.h";
    __gen_e_acsl_assert_data.fct = "pthread_create";
    __gen_e_acsl_assert_data.line = 355;
    __gen_e_acsl_assert_data.name = "valid_thread";
    __e_acsl_assert(__gen_e_acsl_valid,& __gen_e_acsl_assert_data);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data);
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_2 =
      {.values = (void *)0};
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_2,"attr",
                                 (void *)attr);
    if (attr == (pthread_attr_t const *)0) __gen_e_acsl_or = 1;
    else {
      int __gen_e_acsl_valid_read;
      __gen_e_acsl_valid_read = __e_acsl_valid_read((void *)attr,
                                                    sizeof(pthread_attr_t const),
                                                    (void *)attr,
                                                    (void *)(& attr));
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_2,"attr",
                                   (void *)attr);
      __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_2,
                                     "sizeof(pthread_attr_t const)",0,
                                     sizeof(pthread_attr_t const));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,
                                   "\\valid_read(attr)",0,
                                   __gen_e_acsl_valid_read);
      __gen_e_acsl_or = __gen_e_acsl_valid_read;
    }
    __gen_e_acsl_assert_data_2.blocking = 1;
    __gen_e_acsl_assert_data_2.kind = "Precondition";
    __gen_e_acsl_assert_data_2.pred_txt = "attr == \\null || \\valid_read(attr)";
    __gen_e_acsl_assert_data_2.file = "FRAMAC_SHARE/libc/pthread.h";
    __gen_e_acsl_assert_data_2.fct = "pthread_create";
    __gen_e_acsl_assert_data_2.line = 356;
    __gen_e_acsl_assert_data_2.name = "valid_null_attr";
    __e_acsl_assert(__gen_e_acsl_or,& __gen_e_acsl_assert_data_2);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_2);
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_4 =
      {.values = (void *)0};
    __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_4,"arg",arg);
    if (arg == (void *)0) __gen_e_acsl_or_2 = 1;
    else {
      int __gen_e_acsl_valid_2;
      __gen_e_acsl_valid_2 = __e_acsl_valid(arg,sizeof(char),arg,
                                            (void *)(& arg));
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_4,"arg",arg);
      __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_4,
                                     "sizeof(char)",0,sizeof(char));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_4,
                                   "\\valid((char *)arg)",0,
                                   __gen_e_acsl_valid_2);
      __gen_e_acsl_or_2 = __gen_e_acsl_valid_2;
    }
    __gen_e_acsl_assert_data_4.blocking = 1;
    __gen_e_acsl_assert_data_4.kind = "Precondition";
    __gen_e_acsl_assert_data_4.pred_txt = "arg == \\null || \\valid((char *)arg)";
    __gen_e_acsl_assert_data_4.file = "FRAMAC_SHARE/libc/pthread.h";
    __gen_e_acsl_assert_data_4.fct = "pthread_create";
    __gen_e_acsl_assert_data_4.line = 358;
    __gen_e_acsl_assert_data_4.name = "valid_null_arg";
    __e_acsl_assert(__gen_e_acsl_or_2,& __gen_e_acsl_assert_data_4);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_4);
  }
  __retres = __e_acsl_pthread_create(thread,attr,start_routine,arg);
  {
    int __gen_e_acsl_and;
    int __gen_e_acsl_or_3;
    int __gen_e_acsl_or_4;
    int __gen_e_acsl_or_5;
    __e_acsl_assert_data_t __gen_e_acsl_assert_data_5 =
      {.values = (void *)0};
    __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_5,"\\result",0,
                                 __retres);
    if (__retres == 0) {
      int __gen_e_acsl_initialized;
      __gen_e_acsl_initialized = __e_acsl_initialized((void *)__gen_e_acsl_at,
                                                      sizeof(pthread_t));
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_5,
                                   "\\old(thread)",(void *)__gen_e_acsl_at);
      __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_5,
                                     "sizeof(pthread_t)",0,sizeof(pthread_t));
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_5,
                                   "\\initialized(\\old(thread))",0,
                                   __gen_e_acsl_initialized);
      __gen_e_acsl_and = __gen_e_acsl_initialized;
    }
    else __gen_e_acsl_and = 0;
    if (__gen_e_acsl_and) __gen_e_acsl_or_3 = 1;
    else {
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_5,"\\result",0,
                                   __retres);
      __gen_e_acsl_or_3 = __retres == 11;
    }
    if (__gen_e_acsl_or_3) __gen_e_acsl_or_4 = 1;
    else {
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_5,"\\result",0,
                                   __retres);
      __gen_e_acsl_or_4 = __retres == 22;
    }
    if (__gen_e_acsl_or_4) __gen_e_acsl_or_5 = 1;
    else {
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_5,"\\result",0,
                                   __retres);
      __gen_e_acsl_or_5 = __retres == 1;
    }
    __gen_e_acsl_assert_data_5.blocking = 1;
    __gen_e_acsl_assert_data_5.kind = "Postcondition";
    __gen_e_acsl_assert_data_5.pred_txt = "(\\result == 0 && \\initialized(\\old(thread))) || \\result == 11 ||\n\\result == 22 || \\result == 1";
    __gen_e_acsl_assert_data_5.file = "FRAMAC_SHARE/libc/pthread.h";
    __gen_e_acsl_assert_data_5.fct = "pthread_create";
    __gen_e_acsl_assert_data_5.line = 362;
    __gen_e_acsl_assert_data_5.name = "initialization/success_or_error";
    __e_acsl_assert(__gen_e_acsl_or_5,& __gen_e_acsl_assert_data_5);
    __e_acsl_assert_clean(& __gen_e_acsl_assert_data_5);
    __e_acsl_delete_block((void *)(& arg));
    __e_acsl_delete_block((void *)(& start_routine));
    __e_acsl_delete_block((void *)(& attr));
    __e_acsl_delete_block((void *)(& thread));
    __e_acsl_delete_block((void *)(& __retres));
    return __retres;
  }
}

void __e_acsl_globals_init(void)
{
  static char __e_acsl_already_run = 0;
  if (! __e_acsl_already_run) {
    __e_acsl_already_run = 1;
    __e_acsl_store_block((void *)(values),80UL);
    __e_acsl_full_init((void *)(& values));
    __e_acsl_store_block((void *)(& __fc_p_random48_counter),8UL);
    __e_acsl_full_init((void *)(& __fc_p_random48_counter));
    __e_acsl_store_block((void *)(random48_counter),6UL);
    __e_acsl_full_init((void *)(& random48_counter));
    __e_acsl_store_block((void *)(& __fc_random48_init),4UL);
    __e_acsl_full_init((void *)(& __fc_random48_init));
    __e_acsl_store_block((void *)(& __fc_rand_max),8UL);
    __e_acsl_full_init((void *)(& __fc_rand_max));
    __e_acsl_store_block((void *)(& __fc_interrupted),4UL);
    __e_acsl_full_init((void *)(& __fc_interrupted));
    __e_acsl_store_block((void *)(& __fc_p_time_tm),8UL);
    __e_acsl_full_init((void *)(& __fc_p_time_tm));
    __e_acsl_store_block((void *)(& __fc_time_tm),56UL);
    __e_acsl_full_init((void *)(& __fc_time_tm));
    __e_acsl_store_block((void *)(& __fc_p_ctime),8UL);
    __e_acsl_full_init((void *)(& __fc_p_ctime));
    __e_acsl_store_block((void *)(__fc_ctime),26UL);
    __e_acsl_full_init((void *)(& __fc_ctime));
    __e_acsl_store_block((void *)(& __fc_time),4UL);
    __e_acsl_full_init((void *)(& __fc_time));
    __e_acsl_store_block((void *)(& __fc_p_sigaction),8UL);
    __e_acsl_full_init((void *)(& __fc_p_sigaction));
    __e_acsl_store_block((void *)(sigaction),2080UL);
    __e_acsl_full_init((void *)(& sigaction));
  }
  return;
}

void __e_acsl_globals_clean(void)
{
  __e_acsl_delete_block((void *)(values));
  __e_acsl_delete_block((void *)(& __fc_p_random48_counter));
  __e_acsl_delete_block((void *)(random48_counter));
  __e_acsl_delete_block((void *)(& __fc_random48_init));
  __e_acsl_delete_block((void *)(& __fc_rand_max));
  __e_acsl_delete_block((void *)(& __fc_interrupted));
  __e_acsl_delete_block((void *)(& __fc_p_time_tm));
  __e_acsl_delete_block((void *)(& __fc_time_tm));
  __e_acsl_delete_block((void *)(& __fc_p_ctime));
  __e_acsl_delete_block((void *)(__fc_ctime));
  __e_acsl_delete_block((void *)(& __fc_time));
  __e_acsl_delete_block((void *)(& __fc_p_sigaction));
  __e_acsl_delete_block((void *)(sigaction));
  return;
}

int main(void)
{
  int __retres;
  pthread_t t;
  int args[10];
  __e_acsl_memory_init((int *)0,(char ***)0,8UL);
  __e_acsl_globals_init();
  __e_acsl_store_block((void *)(args),40UL);
  __e_acsl_store_block((void *)(& t),8UL);
  __e_acsl_store_block((void *)(& __retres),4UL);
  {
    int i = 0;
    __e_acsl_store_block((void *)(& i),4UL);
    __e_acsl_full_init((void *)(& i));
    while (i < 10) {
      __e_acsl_initialize((void *)(& args[i]),sizeof(int));
      args[i] = i;
      __gen_e_acsl_pthread_create(& t,(pthread_attr_t const *)0,
                                  & write_value,(void *)(& args[i]));
      /*@ assert Eva: initialization: \initialized(&t); */
      __gen_e_acsl_pthread_join(t,(void **)0);
      __e_acsl_full_init((void *)(& i));
      i ++;
    }
    __e_acsl_delete_block((void *)(& i));
  }
  {
    int i_0 = 0;
    __e_acsl_store_block((void *)(& i_0),4UL);
    __e_acsl_full_init((void *)(& i_0));
    while (i_0 < 10) {
      __gen_e_acsl_pthread_create(& t,(pthread_attr_t const *)0,& read_value,
                                  (void *)(& args[i_0]));
      __gen_e_acsl_pthread_join(t,(void **)0);
      __e_acsl_full_init((void *)(& i_0));
      i_0 ++;
    }
    __e_acsl_delete_block((void *)(& i_0));
  }
  __e_acsl_full_init((void *)(& __retres));
  __retres = 0;
  __e_acsl_delete_block((void *)(args));
  __e_acsl_delete_block((void *)(& t));
  __e_acsl_delete_block((void *)(& __retres));
  __e_acsl_globals_clean();
  __e_acsl_memory_clean();
  return __retres;
}


