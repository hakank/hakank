/*
   Wed Apr 11 21:45:47 2001/hakank@netch.se

   With printing of first element

time perl -ne 'my @x=split, $_;print $x[0],"\n"' /usr/local/misc/news/skeptic1 > xxx
perl -ne my @x=split, $_;print $x[0],"\n" /usr/local/misc/news/skeptic1 > xxx  
9.14s user 0.11s system 100% cpu 9.204 total

pop11 bench.p > xxx  
11.33s user 0.28s system 100% cpu 11.514 total

NO PRINTING

perl -ne 'my @x=split, $_;' /usr/local/misc/news/skeptic1 > xxx  
7.31s user 0.10s system 99% cpu 7.440 total

pop11 bench.p > xxx  9.12s user 0.26s system 99% cpu 9.445 total

time perl -ne 'my @x=split, $_;' /usr/dict/words > xxx

*/

;;; Note this use the GOSPL library (for the split function).
compile('/home/hakank/Poplib/init.p');

;;; lvars filename = '/home/hakank/Mail/comp.lang.lisp',
lvars filename = '/home/hakank/Mail_2015/comp.lang.lisp',     
;;; lvars filename= '/home/hakank/mail_old/comp.constraints',
;;; lvars filename= '/usr/dict/words',
      nextline = vedfile_line_repeater(filename, true),
      line;

vars count = 0;
for line from_repeater nextline do
   ;;; vars splitted = [%split(line,' ')%]; ;;; 
   vars splitted = [%split(line)%];
   ;;; line=>
   ;;; pr(length(splitted)><'\n');

   ;;; print first word (or not)
   ;;; if length(splitted)>0 do
   ;;;   splitted(1)=>
   ;;;endif;

   count + 1 -> count;
endfor;

[It was ^count words]=>
