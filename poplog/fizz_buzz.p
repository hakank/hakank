/*

  FizzBuzz program (Rosetta Code) in Poplog.

  http://www.rosettacode.org/wiki/FizzBuzz
  """
  Task

  Write a program that prints the integers from   1   to   100   (inclusive).

  But:

  * for multiples of three, print   Fizz (instead of the number)
  * for multiples of five, print Buzz (instead of the number)
  * for multiples of both three and five, print FizzBuzz (instead of the number) 


  The   FizzBuzz   problem was presented as the lowest level of comprehension required to illustrate adequacy.



  """  
    

  This Pop-11 program was created by Hakan Kjellerstrand (hakank@gmail.com)
  See also my Pop-11/Poplog page: http://www.hakank.org/poplog/


  */

compile('/home/hakank/Poplib/init.p');


define fizz_buzz1();
  lvars i;
  for i from 1 to 100 do
    if i mod 3 = 0 and i mod 5 = 0 then
      'FizzBuzz'=>;
    elseif i mod 3 = 0 then
      'Fizz'=>;
    elseif i mod 5 = 0 then
     'Buzz'=>;
    else
      i=>;
    endif;
  endfor;
enddefine;


define fb2(n);
  if n mod 3 = 0 and n mod 5 = 0 then
      'FizzBuzz';
  elseif n mod 3 = 0 then
      'Fizz';
  elseif n mod 5 = 0 then
      'Buzz';
  else
      n;
  endif;
enddefine;

define fizz_buzz2();
   lvars i;
   [%for i from 1 to 100 do fb2(i) endfor%]=>;
enddefine;


define fizz_buzz3();
  lvars n, t;
  for n from 1 to 100 do
    '' -> t;   
    if n mod 3 = 0 then 'Fizz'->t endif;
    if n mod 5 = 0 then t><'Buzz'->t endif;
    if t = '' then n->t; endif;
    t;
  endfor;
enddefine;

fizz_buzz1();
'\n\n'=>;
fizz_buzz2();
'\n\n'=>;
fizz_buzz3()=>;
