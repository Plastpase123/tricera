class Global {
  public:
    int x;
    void set(int val) {
      x = val;
    }
} g;

int main() {
  class Local {
    public:
      int x;
      void set(int val) {
        x = val;
      }
  } l;

  l.set(1);
  g.set(2);

  assert(l.x == 1 && g.x == 2);
  return 0;
}
