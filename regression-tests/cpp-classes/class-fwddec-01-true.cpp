#include <cassert>

class C {
  public:
  int fun();
};

int C::fun() { return 1; };
int main() {
  C c_obj;

  assert(c_obj.fun() == 1);



  return 0;
}
