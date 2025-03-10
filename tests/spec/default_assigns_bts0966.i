/* run.config
PLUGIN: eva,inout,scope
   OPT: -eva -print
 */
int auto_states[4] ; //   = { 1 , 0 , 0, 0 };

enum states {
  Init = 0,
  Copy = 1,
  Set=2,
  Final = 3
};

// contract with missing "complete behaviors"
/*@
    ensures \true;
    behavior from_init:
      assumes auto_states[Init] == 1;
      ensures (auto_states[Copy] == 1) && (auto_states[Init] == 0);
      assigns auto_states[Init], auto_states[Copy] \from \nothing;

    behavior from_other:
      assumes (auto_states[Init] == 0);
      assigns \nothing;

*/
void copy(int x);

int main() {
  auto_states[Init] = 1;
  auto_states[Copy] = 0;
  auto_states[Set] = 0;
  auto_states[Final] = 0;
  copy(0);
}
