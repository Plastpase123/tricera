#include <cassert>

class Parent { public: void p_fun(); };
class Class_a : public Parent { };
class Class_b : public Parent { };
class Child : public Class_a, public Class_b { };

int main() {
//  Child c;
  return 0;
}

