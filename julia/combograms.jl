#=
  Combograms in Julia.

  List all the possible words that can created given a source word,
  with some options:
  - as many characters as in the word, or as many as wanted 
  - plain subwords 
  - anagrams 
  - minimum length 
  - must contain certain characters 

  This program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#


#
# Return a Dict of the elements and their occurences
# in the collection a.
#
function make_hash(a)
    d = Dict{Char,Int}()
    for e in collect(a)
        if !haskey(d,e)
            d[e] = 0
        end
        d[e] += 1
    end
    return d
end

#
# combogram(word,
#           lang,         # :eng, :swe
#           type,         # :as_many_as_in_source,:as_many_as_wanted, :plain_subwords, :anagram
#           must_contain, # character that must be in the word 
#           min_length,   # minimum length of the word 
#           )
#
function combogram(word,
                   lang=:eng,
                   type=:as_many_as_in_source,
                   must_contain="",
                   min_length=0)

    word = lowercase(word)
    println("word:$word lang:$lang type:$type must_contain:$must_contain min_length:$min_length")

    wordlist = "eng_dict.txt" # :eng
    if lang == :swe 
        # Swedish words
        wordlist = "sv_spelling_org_utf8.txt"
    end

    if must_contain != "" 
        must_contain = collect(must_contain)
    end

    words = readlines(wordlist)
    println("Number of words: $(length(words))")

    d = make_hash(word) # Characters in source word

    res = []
    cc = 0
    for w in words

        w_len = length(w)
        if w_len == 0
            continue
        end

        if min_length > 0 && w_len < min_length
            continue
        end 

        # Hash for this word
        wd = make_hash(w)

        # Anagrams
        if type == :anagram 
            if wd != d 
                continue
            end
        end

        # Plain subword
        if type == :plain_subwords
            if !occursin(w,word)
                continue
            end
        end

        good = true
        # Check all characters
        for c in keys(wd)

            # Char is not in source word
            if !haskey(d,c)
                good = false
                break
            end
            if !good continue end 

            # Has not a mandatory char
            if must_contain != "" 
                for mc in must_contain 
                    if !haskey(wd,mc)
                        good = false
                        break
                    end
                end
            end 
            if !good continue end 

            
            if type == :as_many_as_in_source 
                # Too many occurrences of this character
                if wd[c] > d[c] 
                    good = false
                    break 
                end
            end 
            if !good continue end 
            
        end

        if good 
            push!(res,w)
        end

    end
    return res
end


# 747 Swedish words
# res = combogram("programming",:swe,:as_many_as_wanted,"",0) 

# 615 English words
# res = combogram("programming",:eng,:as_many_as_wanted,"",0) 

# Plain subwords
# res = combogram("kjellerstrand",:swe,:plain_subwords,"",0) 

# Anagrams
# res = combogram("glare",:eng,:anagram,"",0) 

# min length
# Any["darkeners", "slanderer", "terrellas", "treadlers"]
# res = combogram("kjellerstrand",:eng,:as_many_as_in_source,"",9)


len = length(res)
println(res)
println("found $len words")

