#= 

   Wordle solver in Julia.

   This Wordle solver uses a scoring method which calculates
   the score for each word according to the frequencies in
   each of the position in the word.

   This Julia program was created by Hakan Kjellerstrand, hakank@gmail.com
   See also my Julia page: http://www.hakank.org/julia/

=#

# For sorting byvalues
using OrderedCollections

#
# Characters that are in the correct position
#
function check_correct_pos(chars,words)
    filter(w -> occursin(Regex(chars),w),words)
end

#
# Characters that are in the words but not in
# the specific position
#
function check_correct_chars(chars,words)
    candidates = []
    if chars === ""
        return words
    end
    for w in words
        found = true
        for (ww,c) in zip(w,chars)
            if c === ""
                continue
            end
            for cc in c
                if !occursin(cc,w) || cc == ww
                    found = false
                    continue
                end
            end
        end
        if found
            push!(candidates,w)         
        end
    end
    return candidates            
end

#
# Characters that are not in the word
#
function check_not_in_word(chars,words)
    if chars === ""
        return words
    end
    s = split(chars,"")
    filter(w -> all(s .|> c -> !occursin(c,w)),words)
end

#
# The Wordle solver
#
function wordle(correct_pos, correct_chars, not_in_word,words)
    candidates = check_correct_pos(correct_pos,words)
    candidates = check_correct_chars(correct_chars,candidates)
    candidates = check_not_in_word(not_in_word,candidates)
    sort_candidates(candidates)
end

#
# Sort the words accordigin to the frequency scores
#
function sort_candidates(words)
    # This frequency table (reversed) is created by
    # create_freq for a certain wordlist.
    freq = ["xzyjkquinovhewlrmdgfaptbcs",
            "jzqfkgxvsbdymcwptnhulieroa",
            "qjhzkxfwyvcbpmgdstlnrueoia",
            "qjyxzbwhfvpkmdguotrcilasne",
            "vqjuzxibwfcsgmpoakdnhlrtye"]
    return score_words(words,freq)
end

#
# Return a list of words according to the scores
#
function score_words(words,freq)
    m = Dict()
    for word in words
        # Score according to frequencies
        score = 0
        for i in 1:5
            for (c,s) in zip(freq[i],1:26)
                if word[i] == c
                    score += s + i / 2
                end
            end
        end
        # We prefer distinct words
        if length(unique(word)) == 5
            score += 100
        end
        m[word] = score
    end
    word_sorted = sort(m,byvalue=true) |> keys |> collect |> reverse
end

# Create the frequency table
# - create a dict for each position
# - loop through all words and get the number
#   of occurrences for each character
# - for each position: add the missing characters
#   
function create_freq(words)
    n = 5
    m = [Dict() for i in 1:n]
    
    for word in words
        for i in 1:n
            m[i][word[i]] = get(m[i],word[i],0)+1
        end
    end
    alpha = "abcdefghijklmnopqrstuvwxyz"
    freq = []
    for i in 1:n
        t = sort(m[i],byvalue=true) |> keys |> collect
        setdiff(alpha,t) .|> c->pushfirst!(t,c)
        push!(freq,t)
    end
    freq .|> s -> join(s,"")
end

wordlist = "wordle_small.txt"
words = readlines(wordlist)

println(wordle("i.e..", ["", "", "nr", "n", ""], "sla",words))
println()
# -> Any["inert"]

println(wordle("...n.",["","","","",""],"slat",words))
println()
# -> Any["brine", "crone", "crony", "briny", "prone", "corny", "borne", "prune", "drone", "phone", "brink", "bound", "phony", "pound", "frond", "grind", "found", "bring", "drink", "being", "whine", "fiend", "chunk", "mound", "whiny", "prong", "horny", "urine", "drunk", "round", "irony", "doing", "wound", "hound", "opine", "wring", "rhino", "downy", "wrong", "wrung", "ebony", "ovine", "dying", "eying", "owing", "young", "vying", "eking", "penne", "penny", "bunny", "funny", "going", "ninny", "ozone", "icing"]

println(wordle(".r.n.",["","","","",""],"slatcoe",words))
println()
# -> Any["briny", "brink", "grind", "bring", "drink", "drunk", "wring", "wrung"]

println(wordle(".r.n.",["","","","",""],"slatcoebiy",words))
println()
# -> Any["drunk", "wrung"]

println(wordle(".run.",["","","","",""],"slatcoebiydk",words))
println()
# -> Any["wrung"]


println(wordle("...st",["s","","","",""],"flancre",words))
println()
# -> Any["moist", "ghost", "hoist", "midst", "joist", "joust", "boost", "twist"]

println(wordle(".l...",["","","a","","t"],"sn",words))
println()
# -> Any["alter", "ultra", "altar"]


# println(wordle(".....",["","","","",""],"",words))
# println()
# All words sorted to the scores
# -> Any["saint", "crane", "coast", "crone", "brine", "boast", "crony", "briny",
#    "cause", "slant", "paint", "poise", "shine", "point"
#   ...
#   "pizza", "offer", "kebab", "ninja", "rabbi", "jiffy", "offal", "igloo",
#   "jazzy", "affix"

println("\nfrequency table:")
create_freq(words) .|> s -> println(s)
# xzyjkquinovhewlrmdgfaptbcs
# jzqfkgxvsbdymcwptnhulieroa
# qjhzkxfwyvcbpmgdstlnrueoia
# qjyxzbwhfvpkmdguotrcilasne
# vqjuzxibwfcsgmpoakdnhlrtye


