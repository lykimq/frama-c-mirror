/* Generated by Frama-C */
#include "pthread.h"
#include "sched.h"
#include "signal.h"
#include "stddef.h"
#include "stdint.h"
#include "stdio.h"
#include "stdlib.h"
#include "time.h"
char *__gen_e_acsl_literal_string;
char *__gen_e_acsl_literal_string_2;
extern  __attribute__((__FC_BUILTIN__)) int __e_acsl_sound_verdict;

/*@ behavior exists:
      assumes
        \exists integer i; 0 <= i < (int)n && (int)*((char *)buf + i) == c;
      ensures
        \forall int j;
          0 <= j < (int)\offset((char *)\result) ==>
          (int)*((char *)\old(buf) + j) != \old(c);
    
    behavior not_exists:
      assumes
        \forall integer k; 0 <= k < (int)n ==> (int)*((char *)buf + k) != c;
      ensures \result == (void *)0;
 */
void *__gen_e_acsl_memchr(void const *buf, int c, size_t n);

void *memchr(void const *buf, int c, size_t n)
{
  void *__retres;
  int i;
  __e_acsl_store_block((void *)(& __retres),8UL);
  __e_acsl_store_block((void *)(& buf),8UL);
  char *s = (char *)buf;
  __e_acsl_store_block((void *)(& s),8UL);
  __e_acsl_full_init((void *)(& s));
  i = 0;
  while ((size_t)i < n) {
    if ((int)*s == c) {
      __e_acsl_full_init((void *)(& __retres));
      __retres = (void *)s;
      goto return_label;
    }
    __e_acsl_full_init((void *)(& s));
    s ++;
    i ++;
  }
  __e_acsl_full_init((void *)(& __retres));
  __retres = (void *)0;
  return_label:
  __e_acsl_delete_block((void *)(& buf));
  __e_acsl_delete_block((void *)(& s));
  __e_acsl_delete_block((void *)(& __retres));
  return __retres;
}

/*@ behavior exists:
      assumes
        \exists integer i; 0 <= i < (int)n && (int)*((char *)buf + i) == c;
      ensures
        \forall int j;
          0 <= j < (int)\offset((char *)\result) ==>
          (int)*((char *)\old(buf) + j) != \old(c);
    
    behavior not_exists:
      assumes
        \forall integer k; 0 <= k < (int)n ==> (int)*((char *)buf + k) != c;
      ensures \result == (void *)0;
 */
void *__gen_e_acsl_memchr(void const *buf, int c, size_t n)
{
  __e_acsl_contract_t *__gen_e_acsl_contract;
  void const *__gen_e_acsl_at_2;
  int __gen_e_acsl_at;
  void *__retres;
  __e_acsl_store_block((void *)(& __retres),8UL);
  {
    int __gen_e_acsl_exists;
    int __gen_e_acsl_i;
    int __gen_e_acsl_forall;
    int __gen_e_acsl_k;
    __e_acsl_store_block((void *)(& buf),8UL);
    __gen_e_acsl_at = c;
    __gen_e_acsl_at_2 = buf;
    __gen_e_acsl_contract = __e_acsl_contract_init(2UL);
    __gen_e_acsl_exists = 0;
    __gen_e_acsl_i = 0;
    while (1) {
      if (__gen_e_acsl_i < (int)((unsigned int)n)) ; else break;
      {
        int __gen_e_acsl_valid_read;
        __e_acsl_assert_data_t __gen_e_acsl_assert_data =
          {.values = (void *)0};
        __gen_e_acsl_valid_read = __e_acsl_valid_read((void *)((char *)buf + __gen_e_acsl_i),
                                                      sizeof(char),
                                                      (void *)buf,
                                                      (void *)(& buf));
        __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data,"buf",
                                     (void *)buf);
        __e_acsl_assert_register_int(& __gen_e_acsl_assert_data,
                                     "__gen_e_acsl_i",0,__gen_e_acsl_i);
        __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data,
                                       "sizeof(char)",0,sizeof(char));
        __gen_e_acsl_assert_data.blocking = 1;
        __gen_e_acsl_assert_data.kind = "RTE";
        __gen_e_acsl_assert_data.pred_txt = "\\valid_read((char *)buf + __gen_e_acsl_i)";
        __gen_e_acsl_assert_data.file = "bts1390.c";
        __gen_e_acsl_assert_data.fct = "memchr";
        __gen_e_acsl_assert_data.line = 8;
        __gen_e_acsl_assert_data.name = "mem_access";
        __e_acsl_assert(__gen_e_acsl_valid_read,& __gen_e_acsl_assert_data);
        __e_acsl_assert_clean(& __gen_e_acsl_assert_data);
        if (! ((int)*((char *)buf + __gen_e_acsl_i) == c)) ;
        else {
          __gen_e_acsl_exists = 1;
          goto e_acsl_end_loop1;
        }
      }
      __gen_e_acsl_i ++;
    }
    e_acsl_end_loop1: ;
    __e_acsl_contract_set_behavior_assumes(__gen_e_acsl_contract,0UL,
                                           __gen_e_acsl_exists);
    __gen_e_acsl_forall = 1;
    __gen_e_acsl_k = 0;
    while (1) {
      if (__gen_e_acsl_k < (int)((unsigned int)n)) ; else break;
      {
        int __gen_e_acsl_valid_read_2;
        __e_acsl_assert_data_t __gen_e_acsl_assert_data_2 =
          {.values = (void *)0};
        __gen_e_acsl_valid_read_2 = __e_acsl_valid_read((void *)((char *)buf + __gen_e_acsl_k),
                                                        sizeof(char),
                                                        (void *)buf,
                                                        (void *)(& buf));
        __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_2,"buf",
                                     (void *)buf);
        __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_2,
                                     "__gen_e_acsl_k",0,__gen_e_acsl_k);
        __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_2,
                                       "sizeof(char)",0,sizeof(char));
        __gen_e_acsl_assert_data_2.blocking = 1;
        __gen_e_acsl_assert_data_2.kind = "RTE";
        __gen_e_acsl_assert_data_2.pred_txt = "\\valid_read((char *)buf + __gen_e_acsl_k)";
        __gen_e_acsl_assert_data_2.file = "bts1390.c";
        __gen_e_acsl_assert_data_2.fct = "memchr";
        __gen_e_acsl_assert_data_2.line = 11;
        __gen_e_acsl_assert_data_2.name = "mem_access";
        __e_acsl_assert(__gen_e_acsl_valid_read_2,
                        & __gen_e_acsl_assert_data_2);
        __e_acsl_assert_clean(& __gen_e_acsl_assert_data_2);
        if ((int)*((char *)buf + __gen_e_acsl_k) != c) ;
        else {
          __gen_e_acsl_forall = 0;
          goto e_acsl_end_loop2;
        }
      }
      __gen_e_acsl_k ++;
    }
    e_acsl_end_loop2: ;
    __e_acsl_contract_set_behavior_assumes(__gen_e_acsl_contract,1UL,
                                           __gen_e_acsl_forall);
  }
  __retres = memchr(buf,c,n);
  {
    int __gen_e_acsl_assumes_value;
    __gen_e_acsl_assumes_value = __e_acsl_contract_get_behavior_assumes
    ((__e_acsl_contract_t const *)__gen_e_acsl_contract,0UL);
    if (__gen_e_acsl_assumes_value) {
      int __gen_e_acsl_forall_2;
      unsigned int __gen_e_acsl_j;
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_3 =
        {.values = (void *)0};
      __gen_e_acsl_forall_2 = 1;
      __gen_e_acsl_j = 0U;
      while (1) {
        {
          unsigned long __gen_e_acsl_offset;
          __gen_e_acsl_offset = __e_acsl_offset(__retres);
          if (__gen_e_acsl_j < (unsigned int)((int)((unsigned int)__gen_e_acsl_offset))) 
            ;
          else break;
        }
        {
          int __gen_e_acsl_valid_read_3;
          __e_acsl_assert_data_t __gen_e_acsl_assert_data_4 =
            {.values = (void *)0};
          __gen_e_acsl_valid_read_3 = __e_acsl_valid_read((void *)((char *)__gen_e_acsl_at_2 + __gen_e_acsl_j),
                                                          sizeof(char),
                                                          (void *)__gen_e_acsl_at_2,
                                                          (void *)(& __gen_e_acsl_at_2));
          __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_4,
                                       "__gen_e_acsl_at_2",
                                       (void *)__gen_e_acsl_at_2);
          __e_acsl_assert_register_uint(& __gen_e_acsl_assert_data_4,
                                        "__gen_e_acsl_j",0,__gen_e_acsl_j);
          __e_acsl_assert_register_ulong(& __gen_e_acsl_assert_data_4,
                                         "sizeof(char)",0,sizeof(char));
          __gen_e_acsl_assert_data_4.blocking = 1;
          __gen_e_acsl_assert_data_4.kind = "RTE";
          __gen_e_acsl_assert_data_4.pred_txt = "\\valid_read((char *)__gen_e_acsl_at_2 + __gen_e_acsl_j)";
          __gen_e_acsl_assert_data_4.file = "bts1390.c";
          __gen_e_acsl_assert_data_4.fct = "memchr";
          __gen_e_acsl_assert_data_4.line = 9;
          __gen_e_acsl_assert_data_4.name = "mem_access";
          __e_acsl_assert(__gen_e_acsl_valid_read_3,
                          & __gen_e_acsl_assert_data_4);
          __e_acsl_assert_clean(& __gen_e_acsl_assert_data_4);
          if ((int)*((char *)__gen_e_acsl_at_2 + __gen_e_acsl_j) != __gen_e_acsl_at) 
            ;
          else {
            __gen_e_acsl_forall_2 = 0;
            goto e_acsl_end_loop3;
          }
        }
        __gen_e_acsl_j ++;
      }
      e_acsl_end_loop3: ;
      __e_acsl_assert_register_int(& __gen_e_acsl_assert_data_3,
                                   "exists:\n  \\forall int j;\n    0 <= j < (int)\\offset((char *)\\result) ==>\n    (int)*((char *)\\old(buf) + j) != \\old(c)",
                                   0,__gen_e_acsl_forall_2);
      __gen_e_acsl_assert_data_3.blocking = 1;
      __gen_e_acsl_assert_data_3.kind = "Postcondition";
      __gen_e_acsl_assert_data_3.pred_txt = "\\forall int j;\n  0 <= j < (int)\\offset((char *)\\result) ==>\n  (int)*((char *)\\old(buf) + j) != \\old(c)";
      __gen_e_acsl_assert_data_3.file = "bts1390.c";
      __gen_e_acsl_assert_data_3.fct = "memchr";
      __gen_e_acsl_assert_data_3.line = 9;
      __gen_e_acsl_assert_data_3.name = "exists";
      __e_acsl_assert(__gen_e_acsl_forall_2,& __gen_e_acsl_assert_data_3);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_3);
    }
    __gen_e_acsl_assumes_value = __e_acsl_contract_get_behavior_assumes
    ((__e_acsl_contract_t const *)__gen_e_acsl_contract,1UL);
    if (__gen_e_acsl_assumes_value) {
      __e_acsl_assert_data_t __gen_e_acsl_assert_data_5 =
        {.values = (void *)0};
      __e_acsl_assert_register_ptr(& __gen_e_acsl_assert_data_5,"\\result",
                                   __retres);
      __gen_e_acsl_assert_data_5.blocking = 1;
      __gen_e_acsl_assert_data_5.kind = "Postcondition";
      __gen_e_acsl_assert_data_5.pred_txt = "\\result == (void *)0";
      __gen_e_acsl_assert_data_5.file = "bts1390.c";
      __gen_e_acsl_assert_data_5.fct = "memchr";
      __gen_e_acsl_assert_data_5.line = 12;
      __gen_e_acsl_assert_data_5.name = "not_exists";
      __e_acsl_assert(__retres == (void *)0,& __gen_e_acsl_assert_data_5);
      __e_acsl_assert_clean(& __gen_e_acsl_assert_data_5);
    }
    __e_acsl_contract_clean(__gen_e_acsl_contract);
    __e_acsl_delete_block((void *)(& buf));
    __e_acsl_delete_block((void *)(& __retres));
    return __retres;
  }
}

void __e_acsl_globals_init(void)
{
  static char __e_acsl_already_run = 0;
  if (! __e_acsl_already_run) {
    __e_acsl_already_run = 1;
    __gen_e_acsl_literal_string = "toto";
    __e_acsl_store_block((void *)__gen_e_acsl_literal_string,sizeof("toto"));
    __e_acsl_full_init((void *)__gen_e_acsl_literal_string);
    __e_acsl_mark_readonly((void *)__gen_e_acsl_literal_string);
    __gen_e_acsl_literal_string_2 = "tata";
    __e_acsl_store_block((void *)__gen_e_acsl_literal_string_2,
                         sizeof("tata"));
    __e_acsl_full_init((void *)__gen_e_acsl_literal_string_2);
    __e_acsl_mark_readonly((void *)__gen_e_acsl_literal_string_2);
  }
  return;
}

int main(void)
{
  int __retres;
  __e_acsl_memory_init((int *)0,(char ***)0,8UL);
  __e_acsl_globals_init();
  __gen_e_acsl_memchr((void const *)__gen_e_acsl_literal_string,'o',
                      (size_t)4);
  __gen_e_acsl_memchr((void const *)__gen_e_acsl_literal_string_2,'o',
                      (size_t)4);
  __retres = 0;
  __e_acsl_memory_clean();
  return __retres;
}


