#include <cassert>

class Counter {
  public:
    int value;


    Counter(int val) {
      value = val;
    }


    int num() {
      return 0;
    }


    void set(int val) {
      value = val;
    }

    void inc(){
      value = value + 1;
    }
    int get() {
      return value;
    }

    int f();
};


int main() {

  Counter c(1);
  c.set(2);

  while (c.get() < 3) {
    c.inc();
  }

  assert(c.get() == 3);
  return 0;
}


