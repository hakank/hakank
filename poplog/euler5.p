/*

  Problem 5
  """
  2520 is the smallest number that can be divided by each of the numbers from 
  1 to 10 without any remainder.

  What is the smallest number that is evenly divisible by all of the numbers from 1 to 20?
  """

  This Pop-11 program was created by Hakan Kjellerstrand (hakank@bonetmail.com).
  See also my Pop-11 / Poplog page: http://www.hakank.org/poplog/


*/
compile('/home/hakank/Poplib/init.p');


define problem5();
   lvars i;
   lcm_n([%for i from 1 to 20 do i endfor%].explode,20)=>
enddefine;

'problem5()'=>
problem5();