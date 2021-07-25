#=
   Banach's match box problem

   From 
   Gunnar Blom, Lars Holst, Dennis Sandell:
   "Problems and Snapshots from the World of Probability"
   Page 9f, Problem 1.7 Number of walks until no shoes

   """
   A person has, in each of his two pockets, a box with n matches.
   Now and then he talkes a match from a randomly chosen box until
   he finds the selected box empty. Find the expectation of the 
   number, R, of remaining matches in the other box.
   """

=#

using Turing
include("jl_utils.jl")

function theoretical(n)
    # The Stirling approach (from the book page 11):
    return 2* sqrt(n/π) - 1 + 3/(4*sqrt(n*π))
end

@model function banachs_match_box_problem(n)
    function selectMatchBox(a,left,right)
        pick = rand(Bernoulli()) == true ? "l" : "r"        
        if (pick == "l" && left == 0) ||
            (pick == "r" && right == 0)
            numLeftInOtherBox = pick == "l" ? right : left
            return [a,numLeftInOtherBox]
        else
            newa = push!(a,pick)
            if pick == "l"
                return selectMatchBox(newa,left-1,right)
            else
                return selectMatchBox(newa,left,right-1)
            end
        end
    end

    a, numLeftInOtherBoxTmp = selectMatchBox([],n,n)

    len ~ Dirac(length(a))
    numLeftInOtherBox ~ Dirac(numLeftInOtherBoxTmp)

end

n = 50
println("theoretical: ", theoretical(n)) 
model = banachs_match_box_problem(n)

num_chains = 4
# chs = sample(model, Prior(), 1000)
# chs = sample(model, MH(), 10_000)
# chs = sample(model, PG(15), 10_000)
# chs = sample(model, IS(), 10_000)
chs = sample(model, SMC(), 10_000)
# chs = sample(model, SMC(), MCMCThreads(), 10_000, num_chs)

# chs = sample(model, SGLD(), 10_000)
# chs = sample(model,NUTS(), 10_000)
# chs = sample(model,HMC(0.01,5), 10_000)

display(chs)
# display(plot(chs))

show_var_dist_pct(chs,:len)
show_var_dist_pct(chs,:numLeftInOtherBox)

println("\nTheoretical values for 10, 100, and 1000:")
for i in [10,100,1000]
    println([i theoretical(i)])
end
