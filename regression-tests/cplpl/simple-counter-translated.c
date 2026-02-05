#include <assert.h>


struct Counter {
  int value;
};

void Counter_init(struct Counter *c, int val) {
  c->value = val;
}

void Counter_inc(struct Counter *c) {
  c->value = c->value + 1;
}

int Counter_get(struct Counter *c) {
  return c->value;
}

int main() {
  struct Counter c;
  Counter_init(&c, 0);

  while (Counter_get(&c) < 3) {
    Counter_inc(&c);
  }

  assert(Counter_get(&c) == 3);

  return 0;
}
