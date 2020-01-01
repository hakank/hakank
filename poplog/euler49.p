/*

  Euler problem 49
  """
  The arithmetic sequence, 1487, 4817, 8147, in which each of the terms 
  increases by 3330, is unusual in two ways: (i) each of the three terms are 
  prime, and, (ii) each of the 4-digit numbers are permutations of one another.

  There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, 
  exhibiting this property, but there is one other 4-digit increasing sequence.

  What 12-digit number do you form by concatenating the three terms 
  in this sequence?
  """ 

  This Pop-11 program was created by Hakan Kjellerstrand (hakank@gmail.com).
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


vars seen = newmapping([], 1000, 0, true);

define is_prime(n);
    lvars i;
    if n = 2 or n = 3 then
        return(true);
    endif;
    if n mod 2 = 0 then
        return(false);
    endif;
    for i from 3 by 2 to round(sqrt(n))+1 do
        if n mod i = 0 then
            return(false);
        endif;
    endfor;
    return(true);
enddefine;


;;; this contains only numbers > 1000 and primes
define all_permutations(p);
    lvars L = [], p2;
    plogwhile |< permutation(^p, ?p2) >| do
       lvars p3 = p2.packitem;
       if hd(p2)>0 and is_prime(p3) then
           L<>[^p3]->L;
       endif;
    endplogwhile;
    L;

enddefine;


define get_element(n, L, diff);
    lvars p;
    lvars res = 0;
    for p in L do
        if p > n and p-n = diff then
            p->res;
        endif;
    endfor;

    res;

enddefine;


define check_perms(n, diff);
    lvars all_perms = all_permutations(n.unpackitem);

    if all_perms = [] then
        return([]);
    endif;

    lvars L = [], p1 = 0, p2 = 0;
    get_element(n, all_perms, diff)->p1;
    if p1 > 0 then
        get_element(p1, all_perms, diff)->p2;
    endif;
    if p2 > 0 then
        [^n ^p1 ^p2]->L
    endif;

    L;
enddefine;

define problem49;
    lvars diff = 3330, n, res;
    for n from 1001 by 2 to 9999 do
        if is_prime(n) then
            lvars c = check_perms(n, diff);
            if c /= [] then
                ;;; c=>;
                if n /= 1487 then
                    c->res;
                endif;
            endif;
        endif;
    endfor;

    lvars result = '';
    for r in res do result><r->result endfor;
    result=>

enddefine;


'problem49()'=>
problem49();
