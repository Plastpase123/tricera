#include <cassert>


class Parent {
  public:
    int parent_val;

    Parent(int val) {
      parent_val = val;
    }
};

class Child : public Parent {
  public:
    int child_val;

    Child(int val) : Parent(val) {
      child_val = val;
    }

    void set_child_val(int val) {
      child_val = val;
    }
};

int main() {
  Child c(5);

  c.set_child_val(2);

  assert(c.child_val < c.parent_val);

  return 0;
}
