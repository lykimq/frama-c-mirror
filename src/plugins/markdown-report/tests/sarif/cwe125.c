/* run.config
NOFRAMAC: use execnow for proper sequencing of executions
EXECNOW: BIN @PTEST_NAME@_parse.sav LOG @PTEST_NAME@.parse.log LOG @PTEST_NAME@.parse.err @frama-c@ @PTEST_FILE@ -save ./@PTEST_NAME@_parse.sav > ./@PTEST_NAME@.parse.log 2> ./@PTEST_NAME@.parse.err
EXECNOW: BIN @PTEST_NAME@_eva.sav LOG @PTEST_NAME@.eva.log LOG @PTEST_NAME@.eva.err @frama-c@ -load %{dep:./@PTEST_NAME@_parse.sav} -eva -save ./@PTEST_NAME@_eva.sav > ./@PTEST_NAME@.eva.log 2> ./@PTEST_NAME@.eva.err
EXECNOW: BIN @PTEST_NAME@.sarif.unfiltered LOG @PTEST_NAME@.sarif.log LOG @PTEST_NAME@.sarif.err @frama-c@ -load %{dep:./@PTEST_NAME@_eva.sav} -then -mdr-out ./@PTEST_NAME@.sarif.unfiltered -mdr-gen sarif -mdr-no-print-libc -mdr-sarif-deterministic > ./@PTEST_NAME@.sarif.log 2> ./@PTEST_NAME@.sarif.err
EXECNOW: LOG @PTEST_NAME@.sarif sed -e "s:@PTEST_SESSION@:PTEST_SESSION:" %{dep:@PTEST_NAME@.sarif.unfiltered} > @PTEST_NAME@.sarif 2> @DEV_NULL@
*/
#include "__fc_builtin.h"
#define LENGTH 10

int getValueFromArray(int *array, int len, int index) {

int value;

// check that the array index is less than the maximum

// length of the array
if (index < len) {

// get the value at the specified index of the array
value = array[index];
}
// if array index is invalid then output error message

// and return value indicating error
else {
printf("Value is: %d\n", array[index]);
value = -1;
}

return value;
}

int main() {
  int arr[LENGTH] = { 0 };
  int test = Frama_C_interval(-LENGTH,LENGTH);
  return getValueFromArray(arr,LENGTH,test);
}
