/* run.config
 DEPS: enum.h
   STDOPT: +"%{dep:./enum2.c}"
*/


#include "enum.h"

int e1() {
  return E1;
}

int f1() {
  return F11;
}

int k1() {
  return K11;
}

int i1() {
  return I1;
}

// Bug 2090
enum Foo { EN1, EN2, EN3 };

int f() { return !EN1; }

int g() { return EN1; }

enum Bar { CONST_ENUM = 1 == 2 };

int h() {
  return CONST_ENUM ? 0 : 1;
}
