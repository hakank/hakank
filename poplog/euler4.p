/*

  Problem 4
  """
  A palindromic number reads the same both ways. The largest palindrome made 
  from the product of two 2-digit numbers is 9009 = 91  99.

  Find the largest palindrome made from the product of two 3-digit numbers.

  """

  This Pop-11 program was created by Hakan Kjellerstrand (hakank@gmail.com).
  See also my Pop-11 / Poplog page: http://www.hakank.org/poplog/


*/
compile('/home/hakank/Poplib/init.p');


define palindrome(list);
    list = rev(list);
enddefine;


define problem4();
    lvars i,j;
    last(
    syssort(
    [% for i from 100 to 999 do 
           for j from 100 to i do 
               if palindrome(unpackitem(i*j)) then 
                   [^(i*j) ^i ^j ] 
               endif; 
           endfor; 
       endfor %],  
    procedure(l1,l2); hd(l1) < hd(l2); endprocedure)
    )=>

enddefine;

;;; alternative version, using just a variable maxval
define problem4b();
    lvars i,j,x,imax, jmax;
    lvars maxval = 0;
    
    for i from 100 to 999 do 
        for j from 100 to i do 
            i*j -> x;
            if x > maxval then
                if palindrome(unpackitem(i*j)) then 
                    x -> maxval; i -> imax;  j -> jmax;
                endif; 
            endif;
        endfor; 
    endfor;
    [^maxval ^imax ^jmax]=>

enddefine;

'problem4()'=>
problem4()=>

;;; 'problem4b()'=>
;;; problem4b()=>
