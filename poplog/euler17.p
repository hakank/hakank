/*

  Problem 17
  """
  If the numbers 1 to 5 are written out in words: one, two, three, four, five, 
  then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

  If all the numbers from 1 to 1000 (one thousand) inclusive were written out in 
  words, how many letters would be used?

  NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) 
  contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of 
  "and" when writing out numbers is in compliance with British usage.
  """

  This Pop-11 program was created by Hakan Kjellerstrand (hakank@gmail.com).
  See also my Pop-11 / Poplog page: http://www.hakank.org/poplog/

*/
compile('/home/hakank/Poplib/init.p');

lconstant
     Divs      =  {1e9     1e6     1e3      1e2},
     Divnames  =  {billion million thousand hundred},
     Prefixes  =  {0 twen thir for fif six seven eigh nine},
     Ordinals  =  {first second third fourth fifth sixth seventh
		   eighth ninth tenth eleventh twelfth thirteenth fourteenth
		   fifteenth sixteenth seventeenth eighteenth nineteenth},
     Cardinals =  {one two three four five six seven
		   eight nine ten eleven twelve thirteen fourteen
		   fifteen sixteen seventeen eighteen nineteen},
     ;


;;; Inspired by English_pr() in pop/lisp/flib/f_R.p
define English(n);
    lvars d, i, printed, str;
    '' -> str;
    false -> printed;
    if n < 0 then
        'minus' >< str -> str;
        negate(n) -> n
    endif;
    for i from 1 to #_<datalength(Divs)>_# do
        n // fast_subscrv(i, Divs) -> d -> n;
        unless d == 0 do
	    str >< English(d) -> str;
            str >< fast_subscrv(i, Divnames) -> str;
	    true -> printed
        endunless
    endfor;
    if n > 0 and printed then
        str >< "and" -> str
    endif;
    if n == 0 then
        ;;; str >< "zero" -> str;
    elseif n fi_> 19 then
        n fi_// 10 -> d -> n;
        str >< fast_subscrv(d, Prefixes) -> str;
        str >< "ty" -> str;
        str >< English(n) -> str
    else
        str >< fast_subscrv(n, Cardinals) -> str;
    endif;
    return(str);

enddefine;



define problem17();
    lvars i, str, len, total;
    0->total;
    for i from 1 to 1000 do
        English(i)->str;
        length(str)->len;
        total + len -> total;
    endfor;

    total=>
enddefine;



'problem17()'=>
problem17();
