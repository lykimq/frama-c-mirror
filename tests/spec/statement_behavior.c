/* run.config
PLUGIN: eva,inout,scope
   OPT: -eva @EVA_CONFIG@ -inout
*/
/*@ ensures \result == (int)(5 * x);
*/
int pfsqopfc(int x) {
int five_times;
/*@
  assigns five_times \from x;
  ensures five_times == (int)(5 * x);
*/
asm ("leal (%1,%1,4), %0"
             : "=r" (five_times)
             : "r" (x)
             );
/*@ assert five_times == (int) (5 * x);*/ // valid
return five_times;
}

int main () {
  int x = 1;
  int y = pfsqopfc(x);
  return 0;
}
