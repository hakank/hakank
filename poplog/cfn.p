/* 
  

  From
  ~/poplog/news/**/all.txt
  Newsgroups: comp.lang.pop,comp.ai,comp.lang.lisp,comp.lang.prolog
  From: pop@dcs.glasgow.ac.uk (Robin Popplestone)
  Subject: Re:languages for self-modifying code
  Message-ID: <CHoHFy.21s@dcs.glasgow.ac.uk>
       <23528@cosmos.cme.nist.gov>
  Date: Tue, 7 Dec 1993 18:45:33 GMT
  Lines: 49
*/

/*
cfn is a POP-11 procedure which takes as arguments -f-, the name of an
function in the C language, and -string-, its definition. It compiles
the C code, links it into POPLOG and defines a POP-11 procedure
with the same name which calls the external code.
*/


define cfn(f,string);
  lvars string,i,
    rep = discout('temp.c'),              ;;; Make output file
    ;
    for i from 1 to datalength(string) do ;;; Copy string to file
      rep(string(i));
    endfor;
    rep('\n');                            ;;; hakank: add newline
    rep(termin);                          ;;; Close file
    sysobey('gcc -c temp.c');             ;;; Call GNU-C compiler
    sysobey('ld -shared -o temp.so temp.o');
    lvars Exptr = "Exptr_"<>f;            ;;; Name of pointer to external code
    popval([exload  ^f ['temp.so']         ;;; Link in the compiled code
             (language C)
             ^Exptr(1):int <- ^f;         ;;; With -f- code in Exptr
            endexload
           ]);
    popval([define ^f(x); exacc ^Exptr(x); ;;; Define POP-11 procedure
            enddefine]);

enddefine;

;;; Define a C-function "twice", compile it and incrementally link it to
;;; POPLOG

;;; cfn("twice", 'int twice(int x) {return x*2;}');

;;; twice is now a POP-11 procedure which uses the compiled C - call it and
;;; print out the answer

;;; twice(23)=>
;;; twice(231111111)=>

;;; ** 46


;;; alternative solution with a more complicated function in C. 
;;; This is from 
;;;     help intvec
;;; Compile fib.c with
;;;   gcc -c fib.c
;;;   ld -shared -o fib.so fib.o
;;; and then load in pop-11 by:
exload 'testload'
   [fib]      ;;; object file containing fib
   fib(2)     ;;; declares fib as having 2 arguments, no result
endexload;

vars iv = initintvec(150);

exacc fib(150, iv);
iv =>

;;; This don'w work if we have defined twice above!
cfn("trice", 'int trice(int x) {return x*3;}');
trice(123)=>

