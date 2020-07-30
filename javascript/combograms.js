/* 
  Combograms in JavaScript.

  List all the possible words that can created given a source word.
  
  This JavaScript program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my JavaScript page: http://www.hakank.org/javascript_progs/

*/
'use strict';
// const utils = require('./js_utils.js');
const fs = require("fs");

// Allowed types
const TYPES = {
    as_many_as_wanted: 1,
    as_many_as_in_source: 2,
    plain_subwords: 3
}

// Allowed languages
const LANGUAGES = {
    "eng": 1,
    "swe": 2
}

/**
 * 
 * Create a hash from a word with keys are the characters and the
 * values are the count of each character.
 * Here is the word map of "programming":
 *   { p: 1, r: 2, o: 1, g: 2, a: 1, m: 2, i: 1, n: 1 }
 *
 * @param{string} word 
 * 
 * @returns a word hash
 * 
 */
function word_map(word) {
    let h = {};
    for(let c of word) {
        h[c] = 0;
    }
    for(let c of word) {
        h[c]++;
    }
    return h;    
}



/**
 * 
 * @param {string} word 
 * @param {string} type (as_many_as_wanted,as_many_as_in_source, plain_subwords)
 * @param {string} language ("eng", "swe")
 * 
 * @returns an array of the matched words
 * 
 */
function combogram(word,
                   lang="eng",
                   type="as_many_as_in_source",
                   must_contain="",
                   min_length=0) {

    // All wordlists are in lower case
    word = word.toLowerCase();

    console.log("word:",word, "lang:",lang, "type:", type);

    if (TYPES[type] === undefined) {
        console.log("Invalid type: ", type);
        return [];
    } 
    if (LANGUAGES[lang] === undefined ) {
        console.log("Invalid language:", lang);
        return [];
    } 

    // let wordlist = "words_lower.txt";
    let wordlist = "eng_dict.txt";
    // let wordlist = "unixdict.txt"; // English (much smaller)

    if (lang === "swe") {
        wordlist = "/home/hakank/public_html/combograms/sv_spelling_org_utf8.txt";
    }

    const words = fs.readFileSync(wordlist,"utf8").split("\n");
    console.log("number of words: " + words.length);
    // words.forEach(w=>console.log(`"${w}",`));

    
    // Word map of source word
    const h = word_map(word);

    let res = [];
    LOOP: 
    for (let w of words) {

        if (w.length === 0) {
            continue;
        }

        if (min_length > 0 && w.length < min_length) {
            continue;
        }
  
        // Check this word
        let wh = word_map(w);
        for (let c of Object.keys(wh)) {
            // Character not in source word
            if (h[c] === undefined) {
                continue LOOP;
            }

            if (must_contain.length > 0) {
                for(let mc of must_contain) {
                    if (wh[mc] === undefined) {
                        continue LOOP;
                    }
                }
            }

            if (type === "plain_subwords") {
                // Not a subword
                if (word.indexOf(w) === -1) {
                    continue LOOP;
                }

            } else if (type === "as_many_as_in_source") {
                if (wh[c] > h[c]) {
                    // Too many occurrences of this character
                    continue LOOP;
                }
            }
        }
        console.log(w);
        res.push(w);
    }
    return res;
}

// Test it
function test() {

    const t1_start = +new Date();

    let word = "programming";
    // let word = "javascript";
    // let word = "kjellerstrand";

    // Language
    // let lang = "eng";
    let lang = "swe";

    // Type
    // let type ="plain_subwords";
    // let type ="as_many_as_in_source";
    let type = "as_many_as_wanted";

    let must_contain = "";
    let min_length = 0;

    const cs = combogram(word, lang, type, must_contain, min_length);
    
    console.log("count", cs.length);

    const t1_end = +new Date();
    console.log("time (ms): ", t1_end - t1_start);

}

test();