/* run.config
 STDOPT: +"-slice-return main -then-on 'Slicing export' -set-project-as-default -print -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i"
 STDOPT: +"-slice-annot main -then-on 'Slicing export' -set-project-as-default -print -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i"
 STDOPT: +"-sparecode-analysis"
*/

struct {int a; int ab; int b; int c ; int d;} S;
int X, Y;
void g (void) {
  S.a = 1;
  S.ab = 0;
  S.b = 2; /* here, better can be done ! */
  S.d = 4;
}
int main (void) {
  g();
  //@  slice_preserve_expr S.b;
  S.ab = 1; /* so that S.ab is sparecode in g() */
  return S.a ;
}
