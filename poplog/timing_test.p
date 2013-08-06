/*

   Two timing functions which only do one call.

   This Pop-11 program was created by Hakan Kjellerstrand (hakank@bonetmail.com
   See also my Pop-11/Poplog page: http://www.hakank.org/poplog/


*/

max(popmemlim, 50000000) -> popmemlim;

;;;
;;; "Borrowed" from showlib time.
;;; This runs the procedure just one time.
;;;
;;; Usage: 
;;;   timing2 load 'read_test.p';
;;;   timing2 lvars i,xxx; [%for i from 1 to 100 do i endfor%]->xxx;
;;;
define global vars syntax timing2;

    lvars gc, cpu, time, newcpu, input, proc;

    [%until (readitem() ->> input) == termin or input == newline do
          input
      enduntil%] -> input;
    
    pop11_compile([procedure; ^^ input endprocedure]) -> proc;

    popgctime -> gc;
    systime() -> cpu;

    proc(); ;;; the actual call

    max(systime(),cpu) -> newcpu;
    (newcpu fi_- cpu)/100.0 -> cpu;
    printf( (popgctime fi_- gc)/100.0, cpu, 'CPU TIME: %p   GC TIME: %p \n');
    
enddefine;


;;;
;;; This variant is a proper procedure.
;;;
;;; Note: proc must be encloced in procedure() ... procedure;
;;; Usage: 
;;;   timing(procedure();load 'read_test.p'; endprocedure);
;;;   timing(procedure(); lvars i,xxx; [%for i from 1 to 100 do i endfor%]->xxx; endprocedure);
;;;
define timing(proc);
    lvars gc, cpu, time, newcpu, proc;

    popgctime -> gc;
    systime() -> cpu;

    proc(); ;;; the actual call

    max(systime(),cpu) -> newcpu;
    (newcpu fi_- cpu)/100.0 -> cpu;
    printf( (popgctime fi_- gc)/100.0, cpu, 'CPU TIME: %p   GC TIME: %p \n');
    
enddefine;


define simple(n);
    n + 1;
enddefine;

;;; timing2 load 'read_test.p';

vars n = 1000000;

'timing1:'=>;
timing(procedure(); lvars i,xxx; [%for i from 1 to n do simple(i) endfor%]->xxx; endprocedure);

'timing2:'=>
timing2 lvars i, xxx;[%for i from 1 to n do simple(i) endfor%]->xxx;
