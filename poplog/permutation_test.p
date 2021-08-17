/*

   Just a test of permutations.

   This Pop-11 program was created by Hakan Kjellerstrand (hakank@bonetmail.com
   See also my Pop-11/Poplog page: http://www.hakank.org/poplog/


*/
compile('/home/hakank/Poplib/init.p');
uses ploginpop;

;;;
;;; Define permutation via Prolog
;;; Note: This was inspired by David Young's 
;;; permutation implementation:
;;; http://www.sussex.ac.uk/Users/davidy/poplog/permutations.p
;;; 
prolog_compile(stringin('                                         \
delete(A, [A|L], L).                                              \
delete(A, [X|L1], [X|L2]) :- delete(A, L1, L2).                   \
permutation([], []).                                              \
permutation(L1, [A|L2]) :- delete(A, L1, L3),permutation(L3, L2). \
'));



;;; This takes about 4.12s
define test1;
    lvars c = 0;
    vars list = [0 1 2 3 4 5 6 7 8 9];
    vars l2;
    plogwhile |< permutation(^list, ?l2) >| do
       c+1->c;
    endplogwhile;

    [It is ^c permutations]=>
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


;;; This takes about 4.8s (slighly slower)
define test2;

    vars p = [0 1 2 3 4 5 6 7 8 9];
    lvars c = 0; 
    while p /= [] do 
        next_permutation(p)->p;
        c+1->c;
    endwhile;

    [It was ^c permutations]=>;

enddefine;


'test1()'=>
test1();
timediff()=>
'test2()'=>
test2();
timediff()=>
