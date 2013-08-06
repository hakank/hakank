/*

   Concordance program in Pop-11.

   This program reads a file and prints a concordance of the words
   in order of occurrence.

   Note: this program requires the GOSPL library for the split_with function,
   GOSPL is available from 
   http://www.cs.bham.ac.uk/research/projects/poplog/freepoplog.html#gosplsite


   This Pop-11 program was created by Hakan Kjellerstrand (hakank@bonetmail.com
   See also my Pop-11/Poplog page: http://www.hakank.org/poplog/


*/

lvars  string,
       filename = 'concord.p',
       nextline = vedfile_line_repeater(filename, true),
       hash = newmapping([], 100, 0, true);

define last_char(x)->c;
  substring(length(x),1,x)->c
enddefine;

;;; for string from_repeater vedfile_line_repeater('test.txt', true) do
lvars num_words = 0;
for string from_repeater nextline do

   ;;; split the words on the line into a list
   lvars x = [%split_with(string,'\'\s\t\r\n!.;,-><"[]?)(:=*/\\_')%];

   ;;; increment the hash table for the words
   lvars w;
   for w in x do 
     uppertolower(w)->w;
     hash(w)+1->hash(w);
     1 + num_words->num_words;
   endfor;

endfor;

;;; sort the hash, numerical value
define hash_sort1(list1, list2);
  list1(2) > list2(2);
enddefine;


;;; Create a list of the hash table
lvars hash_list = [%explode(hash)%];
[It was ^num_words words]=>

;;; Print words in order of occurrences
for x in syssort(hash_list, hash_sort1) do
  pr(x(1) >< ': ' >< x(2) >< '\n');
endfor;
