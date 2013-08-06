/*

  Hakan Kjellerstrand (hakank@bonetmail.com)

  This program reads a word list and tests regular expressions of
  consecutive characters, e.g. a.*b.*c (or in Pop-11 a@.@*b@.@*c@.@*).


  Results
  -------
  In swedish I have found some words which matches for n = 6:

  ** [Testing klmnop k@.@*l@.@*m@.@*n@.@*o@.@*p]
  ** [alkoholmonopol kaliumtetracyanokuprat kaliumtetracyanoplatinat komplemento
   peration kulminationspunkt vinkelmätningsmikroskop]

  In english (/usr/dict/words), there are no result for n = 6,
  however a lot for n = 5, e.g.

  ** [Testing abcde a@.@*b@.@*c@.@*d@.@*e]
  ** [abecedaire abecedaries abjectedness aborticide absconded abscondedly
        abscondence absconder absconders abstractedness ambuscade ambuscaded
        ambuscader ambuscades ambuscadoed amebicide amoebicide bambocciade
        bambochade carbacidometer Cerambycidae nonabstractedness Oxylabracidae
        scabicide unabstractedness]
  Counter: 25


  ** [Testing cdefg c@.@*d@.@*e@.@*f@.@*g]
  ** [card-perforating care-defying twice-defaulting]
  Counter: 3

  Note: the name read_test.p is for historial reasons. Sorry about that. 


  For regular expressions in Pop-11, see * HELP REGEXP * TEACH REGEXP.

    Pattern     Meaning
     --------------------------------------------------------------------
     @.          matches any one character
     @*          matches zero or more occurrences of the last character
     @[ and @]   matches first occurrence of a character in the brackets
     @^ and @$   constrains a match to the start or end of the line
     @< and @>   constrains a match to the start or end of a word
     @{ and @}   constrains a match to a certain number of occurrences
     @( and @)   denotes a sub-expression
     @n          where n is a number 1-9 refers back to a previously
                 denoted sub-expression.


  This Pop-11 program was created by Hakan Kjellerstrand 
  (hakank@bonetmail.com).
  See also my Pop-11 / Poplog page: http://www.hakank.org/poplog/

*/

lvars filename= '/usr/dict/words',
     nextline = vedfile_line_repeater(filename, true),
     line;

vars search_p;

lvars n = 5;
;;;lvars n = 6; ;;; swedish word list
lvars join_str = '@.@*'; ;;; Which string to join
[File ^filename join_str ^join_str n ^n]=>

lvars str = 'abcdefghijklmnopqrstuvwxyzåäö';
lvars str_len = str.length;

;;; read all words
vars all_words;
[%for line from_repeater nextline do line endfor%]->all_words;

[Checking ^(all_words.length) words]=>;

lvars i, x, rx;
for i from 1 to str_len - n + 1 do 

    substring(i, n, str)->x;
    join(x,join_str) -> rx;

    ;;; the regexp is actually ".*a.*b.*c.*"
    regexp_compile(rx) -> (, search_p);

    lvars counter = 0;
    lvars start_index, num_chars;
    lvars words;

    [%
    for line in all_words do
        search_p(1, line, false, false) -> (start_index, num_chars);
        if start_index do
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


