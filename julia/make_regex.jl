#=
    A port of my Perl/Java/Python MakeRegex in Julia.

    make_regex(words)
    generates a regex for words. It use a simple approach which 
    combined common prefixes in generating the regex. 

    Some examples:
    * words = ["a", "al", "all", "alla", "an", "ann", "anna", "annas", "ananas"]
      regex: a(l(la?)?|n(anas|n(as?)?)?)?

    * words: ["and", "at", "do", "end", "for", "in", "is", "not", "of", "or", "use"]
      regex: (a(nd|t)|do|end|for|i[ns]|not|o[fr]|use)

    There is a simple way of handling character classes
    * words: ["price1", "price2", "price3", "price4"]
      regex: price[1234]


    If there is no common prefix then it just put '|' between the words 
    * words: ["this", "is", "a", "very", "boring", "example", "with", "no", "common", "prefix"]
      regex: (a|boring|common|example|is|no|prefix|this|very|with)


    Also, see the (very old) page for my Perl package MakeRegex: http://hakank.org/makeregex/index.html
    The REAME file in that package states:
    """
    The Perl package MakeRegex composes a regex-expression from a list of
    words. It had been inspired by the emacs elisp module make-regex.el,
    by Simon Marshall.
    """
    
    This Julia program was created by Hakan Kjellerstrand, hakank@gmail.com
    See also my Julia page: http://www.hakank.org/julia/

=#

# 
# common_prefix(p,  list)
# 
# Here is where the main work is done. It's somewhat magically ported from my
# Perl/Java/Python versions...
# 
function common_prefix(p, list) 
    list_len = length(list)
    if list_len == 0 
        return "" 
    end
    if list_len == 1 
        return p * join(list,"")
    end
    
    #
    # fix for some - as of now - unknown bug. To fix in later version!
    # 
    if p == "" && list[1] == "" && list[2] == ""
        return ""
    end

    #
    # * Collect all the strings with the same prefix-char
    #
    hash = Dict()
    for word in sort(list)
        prefix = suffixed_word = ""
        if length(word) > 0
            prefix, suffixed_word... = word 
        end
        # put the suffix in the list of other suffixes for
        # the this prefix
        hash[prefix] = push!(get(hash,prefix,[]),suffixed_word)
    end
    
    # 
    # And recurse this list
    # 
    all = []
    for key in keys(hash)
        comm = ""
        values = hash[key]
        if length(key) > 0
            sort!(values)
            comm = common_prefix(key, values)
        end
        
        # hack to be able to use the '?' char . Should be re-written!
        if comm == ""
            comm = " "
        end
        push!(all,comm)
    end 
    sort!(all)
    
    # paren: what to put in parenthesis ('()' or '[]') if anything
    paren = ""
    all_len = length(all)
    if  all_len == 1
        paren = join(all,"")
    else
        len = maximum(length.(all))
        joinChar = len != 1 ? '|' : ""
            
        # joins all entries except for " "
        join_str = mark = ""
        count = 0
        for w in all
            got_hack_mark = w == " " ? true : false # This is a hack for handling '?'
            if length(w) > 0 && w != " "
                join_str *= w
                if count < all_len-1
                    join_str *=  joinChar
                end
            end

            if got_hack_mark
                mark = '?'
            end
                    
            count = count + 1
        end

        paren = ""
        if length(join_str) === 1
            paren = join_str * mark
        else
            if len == 1
                paren = '[' * join_str * ']' * mark
            else
                paren = '(' * join_str * ')' * mark
            end 
        end
    end
    return p * paren

end

function make_regex(words) 
    replace.(words,r"([*?+])"=>s"\\\1") # replace meta characters
    # We sort the words to induce more common prefixes
    return common_prefix("", sort(words))

end

#
# check_regex(regex, words)
# 
# Checks the regex againts a list of words.
# 
function check_regex(regex, words)
    p = Regex(regex)
    for word in words
        println(word, " matches", !occursin(p,word) ? " NOT!" : "")
    end 
end


tests = [
    ["all","alla"],

    # A lot of Swedish words
    [ "all", "alla", "alle", "alls", "palle", "palla", "pelle", "perkele",
      "ann", "anna", "annas", "anders", "håkan", "ångest", "ärlig", 
      "solsken", "sture", "stina", "hörapparat", "hörsel", "hårig"],

    ["alla", "palla", "balla", "kalla", "all", "pall", "ball", "kall"],

    # "ananas" is the Swedish word for pineapple
    ["a", "al", "all", "alla", "an", "ann", "anna", "annas", "ananas"],

    ["a", "an", "ann", "anna", "annan", "annas", "annans", "ananas", "ananasens"],

    ["a", "ab", "abc", "abcd", "abcde", "abcdef", "b", "bc", "bcd", "bcde", "bcdef", 
     "bcdefg", "abb", "abbc", "abbcc", "abbccdd"],

    ["this", "is", "a", "very", "boring", "example", "with", "no", "common", "prefix"],

    ["price1","price2","price3","price4"],
    

    # This is from Marshall's make-regex.el
    ["and", "at", "do", "end", "for", "in", "is", "not", "of", "or", "use"],    

    # This is from Marshall's make-regex.el
    ["cond", "if", "while", "let*?", "prog1", "prog2", "progn",
     "catch", "throw", "save-restriction", "save-excursion", 
     "save-window-excursion", "save-match-data", "unwind-protect", 
     "condition-case", "track-mouse"],
  
    # This is from Marshall's make-regex.el
    ["abort", "abs", "accept", "access", "array",
      "begin", "body", "case", "constant", "declare",
      "delay", "delta", "digits", "else", "elsif", "entry",
      "exception", "exit", "function", "generic", "goto",
      "if", "others", "limited", "loop", "mod", "new",
      "null", "out", "subtype", "package", "pragma",
      "private", "procedure", "raise", "range", "record",
      "rem", "renames", "return", "reverse", "select",
      "separate", "task", "terminate", "then", "type",
      "when", "while", "with", "xor"]
]

for t in tests 
    println("testing $t")
    println(make_regex(t))
    println()
end

#=
words = last(tests)
rx = make_regex(words)
println(words)
println("regex:$rx")
check_regex(rx,words)
=#