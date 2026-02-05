#include <cassert>

class Counter {
  public:
    int value;
    int value2;

    Counter(int val1, int val2) {
      value = val1;
      value2 = val2;
    }

    void inc(){
      value = value + 1;
    }
    int get() {
      return value;
    }
};

int main() {
  /*
  Counter c(0,2);

  while (c.get() < 3) {
    c.inc();
  }

  assert(c.get() == 3); */
  return 0;
}




