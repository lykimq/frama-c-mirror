/* run.config
   OPT: -print -keep-comments
*/
/* ABC */
void f() {}
//ABD/*FOO*/
/*ABC*/
/*ABC
     */
/*@ requires \true ; // FOO */
void g() {
  /*@ ghost int x = 0; // ghost_comment */
}

int bts_2176() {
  int r=0;
  int i=0;
  /* comment
     1 */
  r = /* comment 2 */ 1;

  //@ loop unfold 10;
  for(i=0; i<10; i++) {
    r += 1;
  }
  /* comment 3 */
  return r;
}
