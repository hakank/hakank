/*

  Problem 14
  """
  The following iterative sequence is defined for the set of positive integers:

  n n/2 (n is even)
  n 3n + 1 (n is odd)

  Using the rule above and starting with 13, we generate the following sequence:
  13 40 20 10 5 16 8 4 2 1

  It can be seen that this sequence (starting at 13 and finishing at 1) contains 
  10 terms. Although it has not been proved yet (Collatz Problem), it is thought that 
  all starting numbers finish at 1.

  Which starting number, under one million, produces the longest chain?

  NOTE: Once the chain starts the terms are allowed to go above one million.)
  """

  This Pop-11 program was created by Hakan Kjellerstrand (hakank@gmail.com).
  See also my Pop-11 / Poplog page: http://www.hakank.org/poplog/

*/
compile('/home/hakank/Poplib/init.p');

;;;
;;; This is very slow...
;;;

;;; memoizes the collatz length
vars c_len=newproperty([], 100000, 0, "perm");
;;; vars c_len = newmapping([], 1000000, 0, "perm");

define collatz1(n);
  if n mod 2 = 0 then
      n/2;
  else
      3*n + 1;
  endif;
enddefine;

newmemo(collatz1, 100000)->collatz1;

define collatz_seq(n);
    lvars m = n;
    ;;; lvars seq = [^n];
    ;;; while m > 1 do
    ;;;     collatz1(m) -> m;
    ;;;     [^^seq ^m] -> seq 
    ;;; endwhile;
    ;;; This is much faster than the concat version
    lvars seq = [^n] <> [% while m > 1 do 
                                    collatz1(m)->m; 
                                    m; 
                           endwhile; 
                         %];
                               
    seq.length -> c_len(n); 
    seq;
enddefine;

;;; newmemo(collatz_seq, 10000)->collatz_seq;

define collatz_len(n);
    lvars len = c_len(n);
    if len = 0 then
       collatz_seq(n).length -> len;
       len -> c_len(n)
   endif;

   return(len);
enddefine;

;;; newmemo(collatz_len, 10000)->collatz_len;


;;;
;;; Here we check if there is a number
;;; we already know, and insert all the 
;;; numbers we get from a sequence into
;;; the hash table (c_len)
;;;
define collatz_len2(n);
    lvars seq, tmp, this_len, i;
    lvars len = c_len(n);
    if len = 0 then
        collatz_seq(n)->seq;
        collatz_seq(n).length -> this_len;
        fast_for i from 1 to this_len do 
            ;;; check if we have this value in the
            ;;; hash table.
            c_len(seq(i)) -> tmp;
            if tmp > 0 then            
                i+tmp-1->c_len(n);
                return(c_len(n));
            endif;
        endfast_for;
        len -> c_len(n)
        
    endif;
    
    return(len);
enddefine;

;;; newmemo(collatz_len2, 10000)->collatz_len2;


;;;
;;; 5.35s
;;;
define problem14();
    lvars i, max_seq, seq;
    lvars len = 0;
    lvars max_len = 0;
    lvars max_i = 0;
    fast_for i from 3 by 2 to 1000000-1 do
        collatz_seq(i) -> seq;
        seq.length -> len;
        len -> c_len(i);
        if len > max_len then
            ;;; seq -> max_seq ;
            len -> max_len;
            i -> max_i;
        endif;
    endfast_for;
    ;;; 'Res'=>, [max_i ^max_i max_len ^max_len]=>
    [^max_i]=>
enddefine;

;;;
;;; 7.93s;
;;;
define problem14b();
    lvars i, max_seq, seq;
    lvars len = 0;
    lvars max_len = 0;
    lvars max_i = 0;
    for i from 3 by 2 to 1000000-1 do
        collatz_len2(i) -> len;
        if len > max_len then
            seq -> max_seq ;
            len -> max_len;
            i -> max_i;
        endif;
    endfor;
    'Res'=>, [max_i ^max_i max_len ^max_len]=>
enddefine;

;;;
;;; 1.36s (3..2..limit)
;;; 1.62s (2..limit)
;;;
define problem14c;
    lvars limit = 999999;
    lvars hash = newmapping([], 100000, 0, true);
    lvars n,m,c_len=0,max_len=0, max_n=0;
    for n from 3 by 2 to limit do
        n->m;
        1->c_len;
        while m > 1 do
            if hash(m) > 0 then
                c_len+hash(m)-1->c_len;
                1->m;
            else
                collatz1(m)->m;
                c_len+1->c_len;
            endif;
        endwhile;
        if hash(n) = 0 then
            c_len->hash(n);
        endif;
        if c_len > max_len then
            c_len->max_len;
            n->max_n;
        endif;
    endfor;
    max_n=>;
enddefine;

;;; 'problem14()'=>
;;; problem14();
;;; timediff()=>;

;;; 'problem14b()'=>
;;; problem14b();
;;; timediff()=>;


'problem14c()'=>
problem14c();
timediff()=>;


