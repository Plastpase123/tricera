#include <cassert>

class C {
  public:
  int data;
  int fun();
};

int C::fun() {
  this->data = 1;
  return this->data;
}
int main() {
  C c_obj;

  assert(c_obj.fun() == 1);



  return 0;
}
