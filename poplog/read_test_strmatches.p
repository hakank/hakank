/*

  String pattern matching in Pop-11.

  This program reads a word list and tests patterns of
  consecutive characters, e.g. [== a == b == c ==].

  It uses the strmatches library from:
    http://www.cs.bham.ac.uk/research/projects/poplog/auto/strmatches.p
  with the elp file here:
    http://www.cs.bham.ac.uk/research/projects/poplog/help/strmatches
  Note that it is not included in the standard distribution.

  Compare with
     http://www.hakank.org/poplog/read_test.p 
  which uses regular expressions and includes some results.

  Note that this version is slower than the regular expression
  variant. For the word list /usr/dicts/words (on Linux) and n=5
   * read_test.p take about 6 seconds
   * read_test_strmatches.p takes about 26 seconds.


  This Pop-11 program was created by Hakan Kjellerstrand (hakank@gmail.com).
  See also my Pop-11 / Poplog page: http://www.hakank.org/poplog/

*/

compile('/home/hakank/Poplib/init.p');

;;; Note: this library is not in the standard distribution.
;;; Download it from 
;;; http://www.cs.bham.ac.uk/research/projects/poplog/auto/strmatches.p
load 'strmatches.p';


;;; Define the word list
lvars filename= 'sv_spelling_org_utf8.txt',
;;; lvars filename= 'words_lower.txt',
;;; lvars filename= '/usr/share/dict/words',
     nextline = vedfile_line_repeater(filename, true),
     line;

vars search_p;

lvars n = 5;
;;; lvars n = 6; ;;; swedish word list
[File ^filename n ^n]=>

lvars list = [a b c d e f g h i j k l m n o p q r s t u v w x y z å ä ö];
list=>

/*
   join2(list) joins each elements in _list_ list
   with the separator ==.

   Usage: 
       join2([a b c d])=>;
       ** [== a == b == c == d ==]
*/
define join2(list)->res;
    lvars res = [==];
    lvars s;
    for s in list do
        [^^res ^s ==] -> res;
    endfor;
enddefine;


;;; read all words
vars all_words;
[%for line from_repeater nextline do line endfor%]->all_words;

[Checking ^(all_words.length) words]=>;

lvars i;
lvars list_length2 = length(list) - n + 1;
for i from 1 to list_length2 do 
    lvars j, x;
    [%for j from i to i+n-1 do list(j); endfor%]->x;

    ;;; create the pattern
    lvars rx;
    join2(x) -> rx;
    ;;; [Testing ^^rx]==>

    lvars counter = 0;
    lvars start_index, num_chars;
    lvars words;

     [%
     for line in all_words do        
         if line strmatches rx do
             line;
             counter + 1 -> counter;
         endif;        
     endfor;
     %]->words;

    if counter > 0 then
        npr('');
        [Testing ^x ^rx]=>
        words=>;
        pr('Counter: '); npr(counter);
        npr('');
    endif;

endfor;


