/*

  Euler problem 24
  """
  A permutation is an ordered arrangement of objects. For example, 3124 is one 
  possible permutation of the digits 1, 2, 3 and 4. If all of the permutations are 
  listed numerically or alphabetically, we call it lexicographic order. The 
  lexicographic permutations of 0, 1 and 2 are:
  
     012   021   102   120   201   210
 
  What is the millionth lexicographic permutation of the digits 
  0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
  """ 

  This Pop-11 program was created by Hakan Kjellerstrand (hakank@bonetmail.com).
  See also my Pop-11 / Poplog page: http://www.hakank.org/poplog/

*/

compile('/home/hakank/Poplib/init.p');

uses ploginpop;

;;;
;;; Define permutation via Prolog
;;; Note: This was inspired by David Young's 
;;; permutation implementation:
;;; http://www.sussex.ac.uk/Users/davidy/poplog/permutations.p
;;; 
prolog_compile(stringin('                                          \
delete(A, [A|L], L).                                               \
delete(A, [X|L1], [X|L2]) :- delete(A, L1, L2).                    \
permutation([], []).                                               \
permutation(L1, [A|L2]) :- delete(A, L1, L3), permutation(L3, L2). \
'));


;;; Slightly faster than problem24b: 1.30s
define problem24;

  vars list = [0 1 2 3 4 5 6 7 8 9];
  vars l2;
  vars c = 0; ;;; counter
  plogwhile |< permutation(^list, ?l2) >| do
    c + 1 -> c;
    if c >= 1000000 then
        quitloop();
    endif;
  endplogwhile;
  l2=>;

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
;;; Slightly slower than problem24: 1.39s
;;;
define problem24b;
    vars p = [0 1 2 3 4 5 6 7 8 9];
    vars c = 1; 
    while p /= [] do 
        if c == 1000000 then
            quitloop();
        endif;
        c + 1 -> c;
        next_permutation(p)->p;
    endwhile;

    p=>;

enddefine;


;;; timediff()=>
'problem24()'=>
problem24();
timediff()=>
'problem24b()'=>
problem24b();
timediff()=>

