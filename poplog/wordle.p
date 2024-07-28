/*

  Wordle solver in Pop-11.

  This Pop-11 program was created by Hakan Kjellerstrand (hakank@gmail.com)
  See also my Pop-11/Poplog page: http://www.hakank.org/poplog/


*/
compile('/home/hakank/Poplib/init.p');


;;;
;;; Ensure that all chars - except '' - are in correct positions in word.
;;;
define check_correct_pos(chars,words);
    lvars i c cc ok word;
    lvars matched = [];    
    for word in words do
        true -> ok;
        for i from 1 to 5 do
            chars(i) -> c;
            ;;; 46 = ''
            if c /= 46 and c /= word(i)do
                false -> ok;
                quitloop(1);
            endif;
        endfor;
        if ok = true do
            [^^matched ^word] -> matched;
        endif;
    endfor;
    matched
enddefine;


;;;
;;; Ensure that all chars in chars - except '' - that are not in
;;; correct position are in word.
;;;
define check_correct_chars(chars,words);
    lvars i c cc ok word wordint ;
    lvars matched = [];
    for word in words do
        [% word.explode %]->wordint;
        true -> ok;
        for i from 1 to 5 do
            if cc /= 46 do
                [% chars(i).explode %] -> cc;
                for c in cc do
                    if c = wordint(i) or not(locchar(c,1,word)) do
                        false -> ok;
                        quitloop(2);
                    endif
                endfor;
            endif;
        endfor;
        if ok = true do
            [^^matched ^word] -> matched;
        endif;
    endfor;
    matched;    
enddefine;

;;;
;;; Ensire that all characters in chars are not in word
;;;
define check_not_in_word(chars,words);
    lvars c ok word;
    lvars matched = [];
    for word in words do
        true -> ok;        
        for c in [% chars.explode %] do
            if locchar(c,1,word) do
                false -> ok;
                quitloop(1);
            endif;
        endfor;
        if ok = true do
            [^^matched ^word] -> matched;
        endif;
    endfor;
    matched;
enddefine;

;;;
;;; Return the unique characters in a list
;;;
define uniq_map(str);
    lvars hash=newmapping([], 100, false, true);
    lvars el;
    ;;; for el in str.unpackitem do
    for el in [% str.explode %] do        
        true -> hash(el);
    endfor;
    hash;
enddefine;

;;;
;;; Score the words
;;;
define score_words(words,freq);
    lvars i j s c word score;
    lvars hash = newmapping([], 100, false, true);
    lvars k pairs sorted;
    for word in words do
        0 -> score;
        for i from 1 to 5 do
            for s from 1 to 26 do
                freq(i)(s) -> c;
                if word(i) = c do
                    score + s + i / 2.0 -> score;
                endif;
            endfor;
        endfor;
        if uniq_map(word).length = 5 do
            score + 100 -> score;
        endif;
        score -> hash(word);
    endfor;
    ;;; Sort on score (decreasing)
    [% for k in [% hash.explode %] do k endfor %] -> pairs;
    lvars sorted = syssort(pairs, procedure(l1,l2); l1(2) > l2(2); endprocedure);
    maplist(sorted,hd); ;;; pick the words
enddefine;

define sort_candidates(candidates);
    ;;; This frequency table (reversed) is created by
    ;;; create_freq for a certain wordlist.
    lvars freq = ['xzyjkquinovhewlrmdgfaptbcs'
                  'jzqfkgxvsbdymcwptnhulieroa'
                  'qjhzkxfwyvcbpmgdstlnrueoia'
                  'qjyxzbwhfvpkmdguotrcilasne'
                  'vqjuzxibwfcsgmpoakdnhlrtye'];
    score_words(candidates,freq)
enddefine;


define wordle(correct_pos, correct_chars, not_in_word,words);
    lvars candidates;
    [wordle(^correct_pos, ^correct_chars, ^not_in_word,words)]=>;
    check_correct_pos(correct_pos,words) -> candidates;
    check_correct_chars(correct_chars,candidates) -> candidates;
    check_not_in_word(not_in_word,candidates) -> candidates;
    ;;;; This requires that the first parameter is the words. TODO?
    ;;;; check_correct_pos(correct_pos,words).check_correct_chars(correct_chars,candidates).check_not_in_word(not_in_word,candidates) -> candidates;
    
    sort_candidates(candidates);
enddefine;


;;;
;;; Tests
;;;
lvars filename = 'wordle_small.txt';
;;; lvars filename = 'wordle_large.txt';
lvars nextline = vedfile_line_repeater(filename, true),line;
lvars words;
[%for line from_repeater nextline do line endfor%]->words;
;;; words=>;
words.length=>;

wordle('i.e..', [''  ''  'nr'  'n'  ''], 'sla',words)=>;
nl(2);
;;; -> ['inert']


wordle('...n.',['' '' '' '' ''],'slat',words)=>;
nl(2);
;;; ;;; -> ['brine', 'crone', 'crony', 'briny', 'prone', 'corny', 'borne', 'prune', 'drone', 'phone', 'brink', 'bound', 'phony', 'pound', 'frond', 'grind', 'found', 'bring', 'drink', 'being', 'whine', 'fiend', 'chunk', 'mound', 'whiny', 'prong', 'horny', 'urine', 'drunk', 'round', 'irony', 'doing', 'wound', 'hound', 'opine', 'wring', 'rhino', 'downy', 'wrong', 'wrung', 'ebony', 'ovine', 'dying', 'eying', 'owing', 'young', 'vying', 'eking', 'penne', 'penny', 'bunny', 'funny', 'going', 'ninny', 'ozone', 'icing']

wordle('.r.n.',['' '' '' '' ''],'slatcoe',words)=>;
nl(2);
;;; ;;; -> ['briny', 'brink', 'grind', 'bring', 'drink', 'drunk', 'wring', 'wrung']

wordle('.r.n.',['' '' '' '' ''],'slatcoebiy',words)=>;
nl(2);
;;; ;;; -> ['drunk', 'wrung']

wordle('.run.',['' '' '' '' ''],'slatcoebiydk',words)=>;
nl(2);
;;; ;;; -> ['wrung']


wordle('...st',['s' '' '' '' ''],'flancre',words)=>;
nl(2);
;;; ;;; -> ['moist', 'ghost', 'hoist', 'midst', 'joist', 'joust', 'boost', 'twist']

wordle('.l...',['' '' 'a' '' 't'],'sn',words)=>;
nl(2);
;;; ;;; -> ['alter', 'ultra', 'altar']


wordle('.....',['' '' '' '' ''],'',words)=>;
nl(2);
;;; All words sorted to the scores
;;; -> ['saint', 'crane', 'coast', 'crone', 'brine', 'boast', 'crony', 'briny',
;;;    'cause', 'slant', 'paint', 'poise', 'shine', 'point'
;;;   ...
;;;   'pizza', 'offer', 'kebab', 'ninja', 'rabbi', 'jiffy', 'offal', 'igloo',
;;;   'jazzy', 'affix'

