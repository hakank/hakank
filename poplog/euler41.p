/*

  Euler problem 41
  """
  We shall say that an n-digit number is pandigital if it makes use of all 
  the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital 
  and is also prime.
  
  What is the largest n-digit pandigital prime that exists?
  """ 

  This Pop-11 program was created by Hakan Kjellerstrand (hakank@bonetmail.com).
  See also my Pop-11 / Poplog page: http://www.hakank.org/poplog/

*/

compile('/home/hakank/Poplib/init.p');

uses ploginpop;

prolog_compile(stringin('                                          \
delete(A, [A|L], L).                                               \
delete(A, [X|L1], [X|L2]) :- delete(A, L1, L2).                    \
permutation([], []).                                               \
permutation(L1, [A|L2]) :- delete(A, L1, L3), permutation(L3, L2). \
'));


define is_prime(n);
    lvars i;
    
    if n = 1 then return(false); endif;

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


define problem41;
    ;;; Simplification (from one of the answers)
    ;;; n=9 is not possible since 1+2+3+4+5+6+7+8+9=45 is divisible by 3
    ;;; n=8 is not possible since 1+2+3+4+5+6+7+8=36 is divisible by 3

    ;;; It's quite fast even if we start from 9: 0.88s
    ;;; lvars n = 9;
    lvars n = 7; ;;; 0.18s
    lvars m = 0;
    while m = 0 and n >= 4 do
        lvars p = [], i;
        for i from n by -1 to 1 do
            p<>[^i]->p;
        endfor;
        lvars p2;
        plogwhile |< permutation(^p, ?p2) >| do
            if is_prime(p2.packitem) then
                p2.packitem->m;
                quitloop;
            endif;
        endplogwhile;
        n-1->n;
    endwhile;

    m=>

enddefine;

'problem41()'=>
problem41();


