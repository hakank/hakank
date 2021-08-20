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
;;; Almost exactly as fast as next_permutation(p).
;;;
define next_permutation2(p);

    lvars n = p.length;
    lvars i = n;
    while (i > 1 and p(i-1) >= p(i)) then
        i-1->i;
    endwhile;
    if (i <= 1) then
        return([]);
    endif;

    lvars j = n;
    ;;; [j ^j (i-1) ^(i-1)]=>;
    while j > 1 and p(j) <= p(i-1) then
        j-1 -> j;
    endwhile;

    ;;; swap
    (p(j),p(i-1)) -> (p(i-1),p(j));

    n -> j;
    while (i < j and i <= n and j <= n and i >= 1 and j >= 1) then
        ;;; [i ^i j ^j]=>;
        ;;; swap
        (p(j),p(i)) -> (p(i),p(j)),
        i+1->i;
        j-1->j;
    endwhile;
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
    lvars primes = {2 3 5 7 11 13 17};
    lvars tsum = 0;
    lvars i = 1;    
    ;;; lvars P = [% for j from 0 to 9 do j endfor%]; ;;; the permutation
    lvars P = [1 0 2 3 4 5 6 7 8 9];
    while P /= [] do
        ;;; P => ;
        1 -> i;
        lvars found = 1;   
        while i <= 7 do
            if (100*P(i+1) + 10*P(i+2) + P(i+3)) mod primes(i) > 0 then
                0->found;
                quitloop();
            endif;
            i+1->i;
        endwhile;
        if found == 1 then
            tsum + P.packitem -> tsum;
        endif;
        next_permutation(P)->P;
        ;;; next_permutation2(P)->P;        
    endwhile;
    tsum=>;
enddefine;

;;; This use the permutations lib from
;;; http://users.sussex.ac.uk/~davidy/poplog/permutations.p
;;; which should be download and put somewhere relevant
;;; This takes about 3.36s (and is thus slower than problem43c).
;;;
define problem43d;
    lib permutations;
    lvars primes = {2 3 5 7 11 13 17};
    lvars tsum = 0;
    lvars i = 1;    
    lvars P = [1 0 2 3 4 5 6 7 8 9];
    lvars perm;
    permutations(P)->perm;
    while P /= termin do
        ;;; P => ;
        1 -> i;
        ;;; Skip if first element is 0 -> slightly faster (3.36s instead of 3.61s)
        if P(1) = 0 then
            perm() -> P;
            nextloop();
        endif;
        lvars found = 1;   
        while i <= 7 do
            if (100*P(i+1) + 10*P(i+2) + P(i+3)) mod primes(i) > 0 then
                0->found;
                quitloop();
            endif;
            i+1->i;
        endwhile;
        if found == 1 then
            tsum + P.packitem -> tsum;
        endif;
        perm()->P;
        endwhile;
    tsum=>;
enddefine;


;;; Test plain permutation. This takes 1.77s
;;; define test;
;;;     lvars P = [1 0 2 3 4 5 6 7 8 9];
;;;     while P /= [] do
;;;         next_permutation(P)->P;
;;;     endwhile;
;;; enddefine;

;;;
;;; test();
;;; timediff()=>;


;;; 'problem43()'=>
;;; problem43();
;;; timediff()=>;

;;; 'problem43b()'=>
;;; problem43b();
;;; timediff()=>;

'problem43c()'=>
problem43c();
timediff()=>;

;;; 'problem43d()'=>
;;; problem43d();
;;; timediff()=>;


