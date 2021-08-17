/*

   Test of calling Fortran libraries in Pop-11.

   From poplog-dev list:
   From: hebisch at math.uni.wroc.pl (Waldek Hebisch)
   Date: Thu Oct 19 14:38:45 2006
   Subject: poplog-dev Fortran libraries under Linux


   

   This Pop-11 program was created by Hakan Kjellerstrand (hakank@bonetmail.com
   See also my Pop-11/Poplog page: http://www.hakank.org/poplog/


*/
compile('/home/hakank/Poplib/init.p');

exload 'example'
  [tstlib]
  (language C)
  tstlib1(n, m, arr) : void <- tstlib1_;
  tstlib2(n, m, arr) : void <- tstlib2_;
endexload;

define tst_arr(pop_n, pop_m, pop_arrvec, fun);
  lvars pop_arr = newanyarray([1 ^(pop_n(1)) 1 ^(pop_m(1))],
                                 0, pop_arrvec);

  exacc (x, y, z):void fun(pop_n, pop_m, pop_arrvec);

  lvars i, j;
  for i from 1 to pop_n(1) do
    for j from 1 to pop_m(1) do
      printf('arr( ' >< i >< ', ' >< j >< ' ) = ' >< pop_arr(i, j), '%p\n');
    endfor;
  endfor;
enddefine;

lvars pop_n = consintvec(3, 1);
lvars pop_m = consintvec(5, 1);

/* Pass single precision array */
defclass sfltvec :sfloat;
lconstant pop_arrvec1 = initsfltvec(pop_n(1)*pop_m(1));
tst_arr(pop_n, pop_m, pop_arrvec1, tstlib1);

/* Pass double precision array */
defclass dfltvec :dfloat;
lconstant pop_arrvec2 = initdfltvec(pop_n(1)*pop_m(1));
tst_arr(pop_n, pop_m, pop_arrvec2, tstlib2);
