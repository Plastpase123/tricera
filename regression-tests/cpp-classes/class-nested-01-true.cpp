class Outer {
  private:
    int data;

  public:
    class Inner {
      public:
        int val;

        //int get_outer(/*& class &*/ Outer &o) {
         //return o.data;
        //}
    };
};

int main() {
//  Outer o;
 // Outer::Inner i;

 // i.get_outer(o);

  return 0;
}
