/* run.config
   LOG: @PTEST_NAME@.dot
   STDOPT: #"-eva-domains traces -eva-msg-key d-traces -eva-traces-dot @PTEST_NAME@.dot -eva-slevel 10 -eva-traces-project" +"-then-last -eva -print -eva-msg-key=-d-traces"
*/

extern volatile int entropy_source;

/*@ requires min <= max;
    assigns \result \from min, max, entropy_source;
    assigns entropy_source \from entropy_source;
    ensures min <= \result <= max ;
 */
extern int interval(int min, int max);


int g = 42;

int main(int c){
  /* c = interval(0,1); */
  int tmp;
  tmp = 0;
  if (c) tmp = g;
  else tmp = 2;
  for(int i = 0; i < 3; i++){
    tmp ++;
  }
  g = tmp;
  return tmp;
}
