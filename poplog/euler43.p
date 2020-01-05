/*

  Euler problem 43
  """
  The number, 1406357289, is a 0 to 9 pandigital number because it is made up of 
  each of the digits 0 to 9 in some order, but it also has a rather interesting 
  sub-string divisibility property.
 
  Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we 
  note the following:
 
      * d2d3d4=406 is divisible by 2
      * d3d4d5=063 is divisible by 3
      * d4d5d6=635 is divisible by 5
      * d5d6d7=357 is divisible by 7
      * d6d7d8=572 is divisible by 11
      * d7d8d9=728 is divisible by 13
      * d8d9d10=289 is divisible by 17
 
  Find the sum of all 0 to 9 pandigital numbers with this property.
  """ 

  This Pop-11 program was created by Hakan Kjellerstrand (hakank@gmail.com).
  See also my Pop-11 / Poplog page: http://www.hakank.org/poplog/

*/

compile('/home/hakank/Poplib/init.p');

;;; Uncomment for trying problem43b()
;;; uses ploginpop;

;;; prolog_compile(stringin('                                          \
;;; delete(A, [A|L], L).                                               \
;;; delete(A, [X|L1], [X|L2]) :- delete(A, L1, L2).                    \
;;; permutation([], []).                                               \
;;; permutation(L1, [A|L2]) :- delete(A, L1, L3), permutation(L3, L2). \
;;; '));

define not_div(n, p);
    n mod p /= 0
enddefine;

define sum(a);
    applist(0,a, nonop +); 
enddefine;


define next_permutation(p);

    lvars n = p.length;
    lvars k = n - 1;

    while p(k) > p(k+1) then
        k-1->k;
        if k = 0 then
            return([]);
        endif;
    endwhile;
    if k = 0 then
        return([]);
    else
        lvars j = n;
        while p(k) > p(j) then
            j-1->j;
        endwhile;
        (p(k),p(j))->(p(j),p(k));
        lvars r = n;
        lvars s = k+1;
        while r > s then
            (p(r),p(s))->(p(s),p(r));
            r-1->r;
            s+1->s;
        endwhile;
    endif;

    return(p);

enddefine;

;;;
;;; 4.92s
;;;
define problem43;
    
    lvars P = [2 3 5 7 11 13 17];
    lvars i;
    lvars s = [% for i from 0 to 9 do i endfor%];

    lvars s2;
    lvars tsum = 0;

    while s /= [] do
         lvars s3 = s.packitem;
         lvars s4 = s3><'';
         if s4.length = 9 then '0'<>s4 -> s4 endif;
         lvars c = true;
         ;;; 1->i;
         ;;; while c = true and i <= 7 do
         ;;;     strnumber(substring(1+i,3, s4)) mod P(i) = 0->c;
         ;;;     i+1->i;
         ;;; endwhile;
         for i from 1 to 7 do
             lvars s5 = strnumber(substring(1+i,3, s4));
             ;;; [i ^i Pi ^(P(i)) s5 ^s5]=>
             if s5 mod P(i) /= 0 then
                 false->c;
                 ;;;[i ^i s5 ^s5 quitloop]=>
                 quitloop;
             endif;
         endfor;
         if c then
             ;;; [^s3]=>
             tsum+s3->tsum;
         endif;
         next_permutation(s)->s;
    endwhile;

    tsum=>;

enddefine;

;;; Uncomment to try Prolog approach (which is slower)
;;;
;;; Using Prolog approach: 5.12s
;;;
;;; define problem43b;
;;;     lvars P = [2 3 5 7 11 13 17];
;;;     lvars i;
;;;     lvars s = [% for i from 0 to 9 do i endfor%];
;;;     lvars s2;
;;;     lvars tsum = 0;
;;;     plogwhile |< permutation(^s, ?s2) >| do
;;;          lvars s3 = s2.packitem;
;;;          lvars s4 = s3><'';
;;;          if s4.length = 9 then '0'<>s4 -> s4 endif;
;;;          lvars i, c = true;
;;;          for i from 1 to 7 do
;;;              lvars s5 = strnumber(substring(1+i,3, s4));
;;;              if not_div(s5,P(i)) then
;;;                  false->c;
;;;                  quitloop(1);
;;;              endif;
;;;          endfor;
;;;          if c then
;;;              tsum+s3->tsum;
;;;          endif;
;;;     endplogwhile;
;;;     tsum=>;     
;;; enddefine;

;;;
;;; 2.63s
;;;
define problem43c;
    lvars primes = [2 3 5 7 11 13 17];
    lvars tsum = 0;
    lvars j, i = 1;    
    lvars P = [% for j from 0 to 9 do j endfor%]; ;;; the permutation
    while P /= [] do
        ;;; P=>;
        1 -> i;    
        while i <= 7 do
            lvars found = 1;
            if (100*P(i+1) + 10*P(i+2) + P(i+3)) mod primes(i) > 0 then
                0->found;
                quitloop;
            endif;
            i+1->i;
        endwhile;
        if found = 1 then
            tsum + P.packitem -> tsum;
        endif;
        next_permutation(P)->P;
    endwhile;
    tsum=>;
enddefine;


;;; 'problem43()'=>
;;; problem43();
;;; timediff()=>;

;;; 'problem43b()'=>
;;; problem43b();
;;; timediff()=>;

'problem43c()'=>
problem43c();
timediff()=>;


