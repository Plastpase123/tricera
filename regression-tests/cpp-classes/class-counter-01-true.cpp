#include <cassert>

class Counter {
  public:
    int value;

    Counter(int val) {
      this->value = val;
    }


    int num() {
      return 0;
    }


    void set(int val) {
      this->value = val;
    }

    void inc(){
      this->value = this->value + 1;
    }
    int get() {
      return this->value;
    }
};


int main() {

  /*& class &*/ Counter c(2);

  while (c.get() < 3) {
    c.inc();
  }

  assert(c.get() == 3);
  return 0;
}


