/* 

  Anagrams/Deranged anagrams in JavaScript.

  http://rosettacode.org/wiki/Anagrams/Deranged_anagrams
  """
  Two or more words are said to be anagrams if they have the same characters, but in 
  a different order.

  By analogy with derangements we define a deranged anagram as two words with the same 
  characters, but in which the same character does not appear in the same position 
  in both words.

  Task
  Use the word list at unixdict [http://wiki.puzzlers.org/pub/wordlists/unixdict.txt] 
  to find and display the longest deranged anagram. 
  """

  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/

'use strict';
const {combinations,zip,zip2,timing2} = require('./js_utils.js');

const fs = require("fs");


// are w1 and w2 deranged?
function is_deranged(w1,w2) {
    for(let i = 0; i < w1.length; i++) {
        if (w1[i] === w2[i]) {
            return false;
        }
    }
    return true;
}

// are w1 and w2 deranged?
// slower than is_deranged/2
function is_deranged2(w1,w2) {
    return zip(w1.split(""),w2.split("")).filter(w=>w[0]===w[1]).length === 0;
}


// collect all the anagram words
function get_sorted_hash(words) {
    let sorted_hash = [];
    for(const word of words) {
        const sorted = [...word].sort();
        if (!sorted_hash[sorted]) {
            sorted_hash[sorted] = [];
        }
        sorted_hash[sorted].push(word);
        
    }
    return sorted_hash;
}

const wordlist = "unixdict.txt";
// const wordlist = "/home/hakank/public_html/combograms/sv_spelling_org_utf8.txt";    


// 124ms
function deranged_anagram() {
    const words = fs.readFileSync(wordlist,"utf8").split("\n");
    let sorted_hash = get_sorted_hash(words);

    let max_len = 0;
    let max_words = [];
    for(const [sorted,ws] of Object.entries(sorted_hash)) {
        const len = ws.length; // number of words
        const wlen = ws[0].length; // word lengths
        if (len > 1 && wlen >= max_len) {
            const found = combinations(2,ws).filter(([w1,w2])=>is_deranged(w1,w2));
            if (found.length > 0) {
                if (max_len === wlen) {
                    max_words.push(found[0]);
                } else {
                    max_words = found;
                    max_len = wlen;
                }
            }            
        }
    }
    
    return max_words;
}


// Faster: 104ms
function deranged_anagram2() {
    const words = fs.readFileSync(wordlist,"utf8").split("\n");

    const res = Object.values(get_sorted_hash(words))
          .filter(ws=>ws.length>1)
          .filter(ws=>combinations(2,ws))
          .filter(([w1,w2])=>is_deranged(w1,w2))
          .map(ws=>[ws[0].length, ws]) // add the length of the words
          .sort((a,b)=> b[0]-a[0]);
    ;
    // Get the max val from the first entry.
    // We have to consider that more than one pair can be largest
    const maxLen = res[0][0];
    return res.filter(ws=>ws[0]===maxLen).map(ws=>ws[1]);
}

timing2(deranged_anagram);
timing2(deranged_anagram2);

