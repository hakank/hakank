/*

  Euler problem 19
  """
  You are given the following information, but you may prefer 
  to do some research for yourself.
 
  * 1 Jan 1900 was a Monday.
  * Thirty days has September,
    April, June and November.
    All the rest have thirty-one,
    Saving February alone,
    Which has twenty-eight, rain or shine.
    And on leap years, twenty-nine.
  * A leap year occurs on any year evenly divisible by 4, but not 
    on a century unless it is divisible by 400.
 
  How many Sundays fell on the first of the month during the 
  twentieth century (1 Jan 1901 to 31 Dec 2000)?
  """ 


  This Pop-11 program was created by Hakan Kjellerstrand (hakank@bonetmail.com).
  See also my Pop-11 / Poplog page: http://www.hakank.org/poplog/

*/
compile('/home/hakank/Poplib/init.p');

;;;
;;; Converting Julian date <-> Gregorian date from:
;;; http://www.hermetic.ch/cal_stud/jdn.htm
;;;

;;;
;;; Gregorian date to Julian date
;;;
define date2julian(y,m,d)->jd;
    lvars jd = ( 1461 * ( y + 4800 + ( m - 14 ) div 12 ) )  div  4 +
         ( 367 * ( m - 2 - 12 * ( ( m - 14 )  div  12 ) ) )  div  12 -
         ( 3 * ( ( y + 4900 + ( m - 14 )  div  12 )  div  100 ) )  div  4 +
         d - 32075;
enddefine;


;;;
;;; Julian date to Gregorian date
;;;
define julian2date(jd);
    lvars l = jd + 68569;
    lvars n = ( 4 * l )  div  146097;
    l - ( 146097 * n + 3 )  div  4->l;
    lvars i = ( 4000 * ( l + 1 ) )  div  1461001;
    l - ( 1461 * i )  div  4 + 31->l;
    lvars j = ( 80 * l )  div  2447;
    lvars d = l - ( 2447 * j )  div  80;
    j  div  11->l;
    lvars m = j + 2 - ( 12 * l );
    lvars y = 100 * ( n - 49 ) + i + l;
    [^y ^m ^d];
enddefine;


;;;
;;; Day of week, Sakamoto's method
;;; http://en.wikipedia.org/wiki/Weekday_determination#Sakamoto.27s_Method
;;;
define dow(y, m, d);
       lvars t = [0 3 2 5 0 3 5 1 4 6 2 4];
       if m < 3 then
           y-1->y;
       endif;
       (y + y div 4 - y div 100 + y div 400 + t(m) + d) mod 7;
enddefine;

define problem19;   
    lvars sum = 0;
    lvars d;
    for d from date2julian(1901,1,1) to date2julian(2000,12,31) do
        lvars dd = julian2date(d);
        if dd(3) = 1 then 
            if dow(dd(1),dd(2),dd(3)) = 0 then
                ;;; dd=>
                sum+1->sum;
            endif;
        endif;
    endfor;
    sum=>;
enddefine;


'problem19()'=>
problem19();
