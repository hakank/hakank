/*
  Find five five-letter words that share no letter in MiniZinc.

  https://twitter.com/iamreddave/status/1571571771937005568
  """
  You might find this interesting. Find 5 words that share no letters. Initial 
  program took a month to run 
  https://docs.google.com/spreadsheets/d/11sUBkPSEhbGx2K8ah6WbGV62P8ii5l5vVeMpkzk17PI/edit#gid=0
  now one millisecond
  """

  https://twitter.com/iamreddave/status/1571577145373392902
  """
  It came from a popular youtube channel https://youtube.com/watch?v=_-AfhLQfb6w
  but its really impressive how/what people did to speed it up. When I heard his algorithm 
  it seemed reasonable at the time
  """

  The five-letter words are from 
  https://github.com/dwyl/english-words/blob/master/words_alpha.txt

  This is a port of my Picat model
     http://hakank.org/picat/five_words_that_share_no_letter.pi

  The data was converted to DZN format by the Picat model.

  It should be 831 different combinations of all (10175) five five-letter words.

  However, it is much faster to instead use the 5977 anagrams, which yields 538
  different solutions (which expands to 831 solutions).

  Some timings of the anagram approach (538 solutions):
  * Gecode:
    - solve satisfy with -f and -p12: 3min22s
    - int_search(x, first_fail,indomain_split) satisfy with -p12: 1min42.96s <--- best for Gecode
    - int_search(x, max_regret,indomain_split) satisfy with -p12: 2min14.61s
    . int_search(x, most_constrained,indomain_split) satisfy with -p12: 1min45.77s.       
    - int_search(x, occurrence,indomain_split) satisfy with -p12: 3min35.32s
    - int_search(x, anti_first_fail,indomain_min) satisfy with -p12: 8min24.59s
    - int_search(x, first_fail,indomain_min) satisfy with -p12: 8min30.37s

  * JaCoP:
    - solve satisfy: 8min19.7s
    - int_search(x, first_fail,indomain_split) satisfy: > 10min

  * Chuffed:
    - solve satisfy with -f and -p12: 32min52.65s
    - int_search(x, first_fail,indomain_split) satisfy with -p12: 42min0.92s

  * OR-Tools
    Note: The -f (free search flag) does not work with -a (all solutions) 
   - solve satisfy with -p12: >4hours
   - int_search(x, first_fail,indomain_split) satisfy with -p12: >30minutes

   * SICStus Prolog
     - solve :: int_search(x, first_fail,indomain_split) satisfy; 7min46.79s
     - solve satisfy: 1min28.82s <--- Overall best
     - with -f: 1min30.92s


  The single threaded Picat model took 3min30s for this task.


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my MiniZinc page: http://www.hakank.org/minizinc/

*/

include "globals.mzn";
% include "five_letter_words.dzn"; % http://hakank.org/minizinc/five_letter_words.dzn
include "five_letter_words_anagrams.dzn"; % sorted words http://hakank.org/minizinc/five_letter_words_anagrams.dzn
        
int: n = 5;
int: num_words;
array[1..num_words,1..n] of int: words;
array[1..num_words] of string: words_s;

array[1..n] of var 1..num_words: x; % The words (index)
array[1..n,1..n] of var 1..26: y; % The individual characters 

constraint 
  % Distinct and ordered words
  all_different(x) /\
  increasing(x) /\

  % Ensure that all letters are distinct
  all_different(array1d(y)) /\
  % Connect the word and the characters
  forall(i,j in 1..n) (
    y[i,j] = words[x[i],j]     
  )
;

solve satisfy;
% solve :: int_search(x, first_fail,indomain_split) satisfy;
% solve :: int_search(x, max_regret,indomain_split) satisfy;
% solve :: int_search(x, first_fail,indomain_min) satisfy;
% solve :: int_search(x, occurrence,indomain_split) satisfy;
% solve :: int_search(x, most_constrained,indomain_split) satisfy;


output [
 % "x:\(x)\n",
 "\([words_s[fix(x[i])] | i in 1..n])\n",
    
];    
