/*
   Sat Nov 29 19:56:48 2008/hakank@bonetmail.com

   

*/
compile('/home/hakank/Poplib/init.p');

;;; This has moved to ~/Poplib/newmemo.p

;;; From popbook.pdf page 289 Memo functions
;;; 
;;; define auxmemo(O1, Prop, P,n, ref_i)->O2;

;;;     lvars O1,O2, Prop, P, n,i, ref_i;
;;;     ref_i.cont-1 ->> i -> ref_i.cont;
;;;     if i = 0 then n -> ref_i.cont;
;;;         clearproperty(Prop);
;;;     endif;

;;;     P(O1) -> O2;
;;;     O2 -> Prop(O1);

;;; enddefine;

;;; ;;;
;;; ;;; P: Procedure
;;; ;;; n: number of values to keep, then the cache is cleared and
;;; ;;;    memoizing restarts.
;;; ;;;
;;; define newmemo(P,n);
;;;   newanyproperty([], n, false, false, syshash, nonop=, false, undef,
;;;                  auxmemo(%P,n,consref(n)%));
;;; enddefine;



define fact(n);
    if n= 0 then 1 else n*fact(n-1)
    endif;
enddefine;

define fib(n);
    if n <= 2 then 
        1
    else
        fib(n-1)+fib(n-2);
    endif;
enddefine;


newmemo(fact,20) -> fact;
'trace fact'=>;
trace fact; ;;; trace don't work in a procedure
'fact(2)'=>;
fact(2)=>;

;;;'fact(4)'=>;
;;;fact(4)=>;
'fact(14)'=>;
fact(14)=>;
;;; 'fact(21)'=>;
;;; fact(21)=>;

'[%for i from 1 to 100 do fact(i) endfor%]'=>;
[%for i from 1 to 100 do fact(i) endfor%]=>    

'trace fib'=>;
trace fib;
'fib(14)'=>;
fib(14)=>

'newmemo(fib, 20) -> fib'=>;
newmemo(fib, 20) -> fib;
'fib(14)'=>;
fib(14)=>

'untrace'=>;
untrace;
'fib(14)'=>;
fib(14)=>

'[%for i from 1 to 100 do fib(i) endfor%]'=>;
[%for i from 1 to 100 do fib(i) endfor%]=>
