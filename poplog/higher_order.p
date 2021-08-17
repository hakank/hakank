/*
   Sun Nov 23 10:48:10 2008/hakank@bonetmail.com

*/
compile('/home/hakank/Poplib/init.p');


;;; From http://www.poplog.org/talk/1994/msg00076.html
;;; Given a procedure, return a dynamic list.  The procedure is expected
;;; to suspend in order to return its results.  This is just more convenient
;;; for writing power_list.
;;;
define rptr_to_list( r ); lvars r;
    pdtolist( runproc(% 0, consproc( 0, r ) %) )
enddefine;


;;; From http://www.poplog.org/talk/1994/msg00076.html
;;; Given a list and a predicate, create a new list, elements of which
;;; are selected from the old list using the predicate.  It is important
;;; that this implementation works on dynamic lists.
;;;
define select( L, pred ); lvars L, pred;
    procedure();
        until null( L ) do
            lvars it = L.dest -> L;
            if pred( it ) then
                return( it )
            endif;
        enduntil;
        return( termin )
    endprocedure.pdtolist
enddefine;


;;; From http://www.poplog.org/talk/1994/msg00076.html
;;; Given a list of numbers, compute the sum.
;;;
define sumlist( L ); lvars L;
    applist( 0, L, nonop + )
enddefine;

;;; From http://www.poplog.org/talk/1994/msg00076.html
;;; Given a list, return a list of all sublists of that list.  Note that
;;; this implementation preserves order of elements.  (At the expense of
;;; efficiency.)  It also works on dynamic lists(!).
;;;
define power_list( L ); lvars L;
    lvars sofar = [];
    
    define lconstant solution( x ); lvars x;
        conspair( x, sofar ) -> sofar;
        chain( x.rev, 1, suspend )
    enddefine;
    
    procedure();
        solution( [] );
        lvars i, s;
        for i in L do
            for s in sofar do
                solution( i :: s )
            endfor
        endfor;
        termin
    endprocedure.rptr_to_list
enddefine;


define test1() -> answers;
    select( power_list( [ 1 3 4 7 8 2 9] ), sumlist <> nonop ==#(% 13 %) ) -> answers ;
enddefine;


;;; quite straight from the scheme definition
;;; http://www.cs.bham.ac.uk/research/projects/poplog/paradigms_lectures/lecture5.html#append
;;; Note: this would be even simpler:
;;;    l1 <> l2
;;;
define append(l1,l2); 
    if null(l1) then 
        l2
    endif; 
    hd(l1) :: append(tl(l1), l2); 
enddefine;



;;; from help popfaq
;;; e.g.
;;; : fold(0, [1 2 3 4 5], nonop +)=>
;;; ** 15
define fold( sofar, list, op );
     if null( list ) then
         sofar
     else
         fold( op( sofar, hd( list ) ), tl( list ), op )
     endif
 enddefine;


;;;
;;; range(from, to) -> list
;;;
define range(x_low, x_up);
    lvars i;
    [%for i from x_low to x_up do i endfor%];
enddefine;

;;; chaining procedures with <>.
;;; From http://www.poplog.org/talk/1994/msg00075.html
define test2();

  select([^(range(1,3)) ^(range(1, 4)) ^(range(1,10))], sumlist <> nonop >(% 6 %))=>
  ;;; ** [[1 2 3 4] [1 2 3 4 5 6 7 8 9 10]]

  range(1,10).(sumlist <> nonop >(% 6 %))=>
  ;;; ** <true>

  range(1,3).(sumlist <> nonop >(% 6 %))=>
  ;;;** <false>

enddefine;


;;; another example 
([1 2 3 4 5]).maplist(%sqrt <> round %)=>
;;; ** [1 1 2 2 2]

define sqrt_round(l);
   l.maplist(%sqrt <> round%)

enddefine;


;;; combined with a proc.
range(1,10).maplist(%sqrt <> round <> procedure(x); x >1  endprocedure %)=>
;;; ** [<false> <false> <true> <true> <true> <true> <true> <true> <true> <true>]

