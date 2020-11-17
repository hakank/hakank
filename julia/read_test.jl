#=
    Read test in Julia.

    This is one of my standard test when learning a new programming
    language: read a word file and filters the words that match
    the regular expression
      a.*b.*c..., b.*c.*d.., c.*d.*e..., etc

    Timing in millis for the different functions.
    Note: it excludes reading the wordlist etc.

    * Swedish:
        n=4: 0.534053001
        n=5: 0.518007155
        n=6: 0.490448808

    * English:
        n=4: 0.495075664
        n=5: 0.478335018
        n=6: 0.448823873 (no result)

  This Julia model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#


lang = "swe"
# lang = "eng"

# n = 4
n = 5
# n = 6

if length(ARGS) >= 1
   lang = ARGS[1]
   println("set LANG: $lang")
end

if length(ARGS) >= 2
   n = parse(Int,ARGS[2])
   println("set N: $n")
end

println("lang:$lang n:$n")

wordlist = "words_lower.txt"
alpha = split("abcdefghijklmnopqrstuvwxyz","") # English (etc) words

if lang == "swe"
    wordlist = "/home/hakank/public_html/combograms/sv_spelling_org_utf8.txt";
    alpha = split("abcdefghijklmnopqrstuvwxyzåäö","") # Swedish words
end

words = readlines(wordlist)
println("number of words $(length(words))")

#
# Check all variants
#
function read_test()
    println("n:$n")
    for i in 1:length(alpha)-n+1
        r = Regex(join(alpha[i:i+n-1],".*"))
        println("\t$r")
        m = filter(word->occursin(r,word),words)
        if length(m) > 0
            println("$m")
            println(" len: $(length(m))")
        end
        println()
    end
end

t1 = @timed read_test()
println("\nTime: $(t1.time)")
