/* run.config
   STDOPT: +"-main main -slice-annot main -ulevel 10 -then-on 'Slicing export' -set-project-as-default -print -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i"
*/
int choix ;
int state = 1;
int cumul =0 ;
int step =0;

//initialisation


/*@
ensures \result==0 || \result==1 || \result==2 ;
 */
int choisir(void) ;

void lecture() {
choix = choisir() ;
}

void fsm_transition() {
  switch (state) {
  case 1:
    if (choix == 2) {
      cumul = cumul +2 ;
      state = 2 ;
    }
    else
      cumul++;
    break ;
  case 2:
    if ((step==50) && (choix==1))
      state = 3 ;
    else
      cumul++ ;
    break ;
  case 3: if ((choix==0) && (cumul==10)) state = 1;
 default: break ;
  }
}

int main() {

  while (step>=0){
  lecture() ;
    fsm_transition() ;
    if (state == 3) {
      /*@ slice_preserve_ctrl ;*/
      break ;
    }
    step ++ ;
   }
  return 0 ;
}
