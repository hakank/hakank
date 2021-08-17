/*
  Se HELP macros
*/


define macro swapx x y;
  lvars x y;
;;;  itemread() -> x;
;;;  itemread() -> y;
  pr('i macro:\n');
  pr('x:');
  pr(x);
  pr(' y:');
  pr(y);
  pr(newline);
  x, ",", y, "->", x, "->", y
enddefine;

trace swapx;

vars a,b;
2->a;
3->b;
a==>;
b==>;

;;; Note: There should not be a "," between the arguments!
swapx a b;
a==>;
b==>;

;;; swap is the same as:
a, b -> a -> b;
a==>;
b==>;

;;; or as
;;;(a,b) -> (b,a); ;;; this works as well
a,b -> (b,a);
a==>;
b==>;

;;; Note:  => prints all elements from the stack, ==> takes only the top element
'\n'==>;
1,2,3,4,5==>;
'\n'==>;
;;; takes everything from the stack
1,2,3,4,5=>;


define 1 assign(x, y);
    lvars x,y;
    y -> x;
enddefine;


vars a,b;
100 -> a;
200 -> b;

'before:'=>;

'a:',a=>;
'b:',b=>;

lvars x,y
x assign y;

'after:'=>;
'a:',a=>;
'b:',b=>;
