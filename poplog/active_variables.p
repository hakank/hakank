/*
   Thu May  1 10:01:00 2008/hakank@bonetmail.com

  Se 
    help active_variables

*/
compile('/home/hakank/Poplib/init.p');

vars acc_av3 = 0, upd_av3 = 0;
lconstant vec_av3 = {1 2 3};

define active:3 av3;
   acc_av3 + 1 -> acc_av3;
   'accessing av3'=>      
   explode(vec_av3)
enddefine;

define updaterof active:3 av3(x1,x2,x3);
  lvars x1,x2,x3;
  'updating av3'=>
  fill(x1,x2,x3,vec_av3) ->;
  upd_av3 + 1 -> upd_av3;
enddefine;

av3 =>
4,5,6 -> av3;

av3 =>                                          

av3 + 4 -> av3;

av3 =>

;;; How many times has AV3 been accessed?
'accessed av3 number of times: '=>
acc_av3 =>

;;; and updated?
'updated av3 number of times:'=>
upd_av3 =>

;;; print vec_av3
vec_av3=>
