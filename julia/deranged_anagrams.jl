#=

  Anagrams/Deranged anagrams in Julia.

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

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

include("jl_utils.jl")
using Underscores

# are w1 and w2 deranged?
function is_deranged(w1,w2)
    for i in 1:length(w1)
        if w1[i] === w2[i]
            return false
        end
    end
    return true
end

function sorted_hash(words)
    d = Dict()
    for word in words
        s = sort(split(word,""))
        get!(d,s,[])
        push!(d[s],word)
    end
    return d
end

function deranged_angrams(words)
    d = sorted_hash(words)

    max_len = 0
    max_words = []
    for (sorted,ws) in collect(d) 
        if length(ws) == 1 
            continue 
        end
        len = length(ws)
        wlen  = length(join(sorted,""))
        if len > 1 && wlen > max_len 
            found = filter(w->is_deranged(w[1],w[2]),combinations(2,ws))
            if length(found) > 0
                if max_len == wlen 
                    push!(max_words,found[1])
                else 
                    max_words = found 
                    max_len = wlen
                end
            end
        end
    end
    
    println("max_len:$max_len")
    println("max_words:$max_words")
end

# wordlist = "eng_dict.txt" 
wordlist = "unixdict.txt" 
# wordlist = "words_lower.txt"
words = readlines(wordlist)
deranged_angrams(words)
