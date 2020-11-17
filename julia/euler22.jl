#=

  Euler #22 in Julia.

  Problem 22:
  """
  Using names.txt (right click and 'Save Link/Target As...'), a 46K
  text file containing over five-thousand first names, begin by sorting
  it into alphabetical order. Then working out the alphabetical value
  for each name, multiply this value by its alphabetical position in the
  list to obtain a name score.

  For example, when the list is sorted into alphabetical order, COLIN,
  which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in
  the list. So, COLIN would obtain a score of 938 53 = 49714.

  What is the total of all the name scores in the file?")
  """


  This Julia program was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

include("Euler.jl")

function to_code(s)
    return [Int(s[i])-64 for i in 1:length(s)]
end

# 0.00231866s
function euler22a()
    words = sort(split(readline("euler22_names.txt"),","))
    a = 1;
    s = 0;
    for word in words
        word = replace(word,"\""=>"")
        s += a*sum(to_code(word));
        a += 1;
    end
    return s;

end

# 0.02438064s
function euler22b()
    a = 1
    return sum(map(word->(word=replace(word,"\""=>"");a=a+1; (a-1)*sum(to_code(word))
                   ),
                  sort(split(readline("euler22_names.txt"),","))))

end

# Nicer chaining.
# Note the () before |>sum
# 0.02813539s
function euler22c()
    a = 1
    return (sort(split(readline("euler22_names.txt"),",")).|>word->(
                    word=replace(word,"\""=>"");
                    a=a+1; (a-1)*sum(to_code(word))
                    ))|>sum
end


run_euler(euler22a)
# run_euler(euler22b)
# run_euler(euler22c)
