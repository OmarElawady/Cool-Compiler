class Main inherits Object{
   main(): Object {
     (new IO).out_string(self.type_name())
   };
   test(): SELF_TYPE{
     new SELF_TYPE
   };
};

class Test inherits Main{
   test(): SELF_TYPE{
     1
   };
   func(): Test{
     test()
   };
};


