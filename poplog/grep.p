/*
  
   Simple grep program in Pop-11.


   This Pop-11 program was created by Hakan Kjellerstrand (hakank@bonetmail.com
   See also my Pop-11/Poplog page: http://www.hakank.org/poplog/


*/

/*
   Mon Apr  9 22:10:53 2001/hakank@netch.se


*/
compile('/home/hakank/Poplib/init.p');

;;; lvars filename= '/home/hakank/Mail/comp.constraints',
lvars filename= '/home/hakank/Mail_2015/comp.lang.pop',
      nextline = vedfile_line_repeater(filename, true),
      line;

;;; vars search_p;
;;; regexp_compile('@^Subject: ') -> (, search_p);
vars subject_len = length('Subject: ');


for line from_repeater nextline do
    ;;; regexp
    ;;;   if search_p(1, line, false, false) do
    ;;;      pr(line><'\n');
    ;;;   endif;

    ;;; variant 2: split()
    ;;; Note: it's a string to match
    ;;;lvars splitted = [%split(line)%];   ;;; obs! lista! [% %]
    ;;;if splitted matches ['Subject:' ??subject] do
    ;;; vars subject = '';  ;;; 
    ;;;   if [%split(line)%] matches ['Subject:' ==] do
    ;;;      pr(line><'\n');
    ;;;   endif;
    
       ;;; isstartstring
    ;;;    if isstartstring('Subject: ', line) do
    ;;;       pr(line><'\n');
    ;;;    endif;
    
     ;;; substring
    if length(line) > subject_len and substring(1,subject_len,line) = 'Subject: ' do
        pr(line><'\n');
    endif; 
    
endfor;

