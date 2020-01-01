/*

  Euler problem 22
  """
  Using names.txt (right click and 'Save Link/Target As...'), a 46K 
  text file containing over five-thousand first names, begin by sorting 
  it into alphabetical order. Then working out the alphabetical value 
  for each name, multiply this value by its alphabetical position in the 
  list to obtain a name score.
 
  For example, when the list is sorted into alphabetical order, COLIN, 
  which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in 
  the list. So, COLIN would obtain a score of 938*53 = 49714.
 
  What is the total of all the name scores in the file?")
  """ 

  This Pop-11 program was created by Hakan Kjellerstrand (hakank@gmail.com).
  See also my Pop-11 / Poplog page: http://www.hakank.org/poplog/

*/

compile('/home/hakank/Poplib/init.p');

;;; load 'strmatches.p';

define get_score(name, ix)->total;
    lvars total=0;
    lvars i;
    for i from 1 to length(name) do
        total+(name(i)-64)->total;
    endfor;
    total*ix->total;
enddefine;

define problem22();
    lvars nextline = vedfile_line_repeater('euler_names.txt', true);
    lvars line;
    lvars all=[%for line from_repeater nextline do line endfor%];

    ;;; First Remove all '"' (as ASCII 34)
    ;;; Don't forget to force it to a string (with ><'')
    lvars stripped=[%for i in all(1).unpackitem do 
                              if i(1) /= 34 then i; endif; 
                          endfor%].packitem><'';


    lvars names=[%split_with(stripped,',')%];

    ;;; alternative solutions:
    ;;; lvars newnames=[];
    ;;; vars n;
    ;;; lvars name;
    ;;; remove the '"'
    ;;; for name in names do 
    ;;;     ;;; using strmatches from
    ;;;     ;;; http://www.cs.bham.ac.uk/research/projects/poplog/auto/strmatches.p
    ;;;     ;;; name strmatches ['"' ??n '"'];
    ;;;     ;;; newnames <> [^n]->newnames;
    ;;;     substring(2,length(name)-2,name)->name;
    ;;;     newnames <> [^name]->newnames;
    ;;; endfor;
    ;;; sort(newnames)->newnames;

    lvars newnames = sort(names);
    lvars score = 0;
    lvars i;
    for i from 1 to length(newnames) do
        get_score(newnames(i), i)+score->score;
    endfor;

    score=>;
enddefine;

'problem22()'=>
problem22();


