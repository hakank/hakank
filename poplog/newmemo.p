/*
   Sat Nov 29 19:56:48 2008/hakank@gmail.com   

*/

;;;
;;; This is from
;;; From popbook.pdf page 289 Memo functions
;;; 
;;; Also, see 
;;; "'Memo' Functions and Machine Learning" From Nature, Vol 218, April 6 1968.
;;; A Summary by Robin Popplestone, February 1998 
;;; http://www.cs.bham.ac.uk/research/projects/poplog/paradigms_lectures/michie.html
;;;

define auxmemo(O1, Prop, P,n, ref_i)->O2;

    lvars O1,O2, Prop, P, n,i, ref_i;
    ref_i.cont-1 ->> i -> ref_i.cont;
    if i = 0 then n -> ref_i.cont;
        clearproperty(Prop);
    endif;

    P(O1) -> O2;
    O2 -> Prop(O1);

enddefine;

;;;
;;; P: Procedure
;;; n: number of values to keep, then the cache is cleared and
;;;    memoizing restarts.
;;;
define newmemo(P,n);
  newanyproperty([], n, false, false, syshash, nonop=, false, undef,
                 auxmemo(%P,n,consref(n)%));
enddefine;


