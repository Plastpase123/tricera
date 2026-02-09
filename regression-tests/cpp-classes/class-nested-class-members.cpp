class A {
    public:
        int data;
        void set() {
            this->data = 0;
        }

        int num() {
            return 5;
        }
};

class B {
    public:
        int data;
        /*&class&*/ A a;
        /*
        int num() {
          return 3;
        }*/
};

class C {
    public:
        int data;
        /*&class&*/ B b;
};



int main() {
    /*&class&*/ C c;
    c.b.a.data = 5;
    //c.b.a.set(4);
    c.data = c.b.a.num();
    //assert(c.b.num() == 3 && c.b.a.num() == 5);
    assert(c.b.a.data == 5 && c.data == 5);
  return 0;
}
