class Main inherits IO {
   main(): Int {
      (new Test).main()
   };
   test(): Int {
    1
   };
  hex(): SELF_TYPE{
    let i: Main <- self in {(new Test).anoth(i);i;}
  };
};
class Test{
  main(): Int {
    (new Main).hex().test()
  };
  anoth(o: Test): Int{0};
};
