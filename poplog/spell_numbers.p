/*

   Spelling numbers in Pop-11.

   From my Euler problem #17 implementation
   http://www.hakank.org/poplog/euler17.p

   Inspired by English_pr() in pop/lisp/flib/f_R.p

   This Pop-11 program was created by Hakan Kjellerstrand (hakank@bonetmail.com
   See also my Pop-11/Poplog page: http://www.hakank.org/poplog/


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
        str >< ' and ' -> str
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


for i from 1 to 20 do
    lvars rand = random(10000);
    lvars spell = English(rand);
    [^rand ^spell]=>;
endfor;

