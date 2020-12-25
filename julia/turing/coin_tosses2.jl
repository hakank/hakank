#=
  https://edu.swi-prolog.org/mod/assign/view.php?id=254&forceview=1
  """
  Coin tosses

  http://cplint.eu/p/coin_tosses.swinb

  Coin tosses

  Consider a process where you repeatedly toss coins and record the results.

  After each toss, you continue tossing with probability 0.8.

  Write a predicate tosses(Coin,L) that, given an initial coin id Coin, returns in L the list of
  results of coin tosses obtained using the process above.

  Moreover, write a predicate length_tosses(N) that returns the number of coin tosses.

  Compute the probability of the sequence [h,h,h] using MCINTYRE.

  Compute the probability of the sequence [h,h,h,h,h] given that the subsequence [h,h,h] was observed
  using rejection sampling and Metropolis Hastings.

  Compute the probability of the sequences of 10 coin tosses using MCINTYRE.

  Compute the expected length of the sequences of coin tosses using MCINTYRE.
  """

  Note: The cplint version (corrected by  Fabrizio Riguzzi) at http://cplint.eu/p/coin_tosses_hakank_rzf.swinb
  give another solutions:
   - first problem: probability of the sequence [h,h,h] -> 0.015
   - second problem: probability of the sequence [h,h,h,h,h] given that the subsequence [h,h,h] -> about 0.03
   - third problem: probability of the sequences of 10 coin tosses -> about 0.026
   - fourth problem: expected length of the sequences of coin tosses -> about 4.

  Cf ~/blog/coin_tosses.blog
     ~/webppl/coin_tosses.wppl

   This is a recursive approach.

=#

#=

# using
#   chains = sample(model, MH(), MCMCThreads(), 10_000, num_chains)
#
Julia> @time include("coin_tosses2.jl")
Problem 1
Probability of problem 1:

Distributions of variable (num:20)
0.00000 =>   37429  (0.935725)
1.00000 =>    2571  (0.064275)


Problem 2
Probability of problem 2:

Distributions of variable (num:20)
0.00000 =>   36836  (0.920900)
1.00000 =>    3164  (0.079100)


Problem 3
Probability of problem 3:

Distributions of variable (num:20)
0.00000 =>   38931  (0.973275)
1.00000 =>    1069  (0.026725)


Problem 4
Probability of problem 4:

Distributions of variable (num:20)
1.00000 =>    7959  (0.198975)
2.00000 =>    6391  (0.159775)
3.00000 =>    5168  (0.129200)
4.00000 =>    4184  (0.104600)
5.00000 =>    3259  (0.081475)
6.00000 =>    2610  (0.065250)
7.00000 =>    2083  (0.052075)
8.00000 =>    1661  (0.041525)
9.00000 =>    1329  (0.033225)
10.00000 =>    1081  (0.027025)
11.00000 =>     872  (0.021800)
12.00000 =>     711  (0.017775)
13.00000 =>     525  (0.013125)
14.00000 =>     422  (0.010550)
15.00000 =>     373  (0.009325)
16.00000 =>     301  (0.007525)
17.00000 =>     211  (0.005275)
18.00000 =>     175  (0.004375)
19.00000 =>     155  (0.003875)
20.00000 =>      99  (0.002475)
Mean:4.9843


  4.923102 seconds (71.82 M allocations: 3.655 GiB, 11.41% gc time)

=#
using Turing, StatsPlots, DataFrames, StatsBase
include("jl_utils.jl")

@model function coin_tosses(problem)
    head = 1
    tail = 2

    len ~ DiscreteUniform(1,100)
    # We create a fixed length list which seems to be much easier
    # than dynamically adding elements to tosses, especially
    # since for problem 2 we always access to tosses[1:5]
    function toss(a)
        if length(a) == 0
            # Alway do the first throw
            t ~ Categorical([0.5,0.5])
            return toss(vcat(a, t))
        else
            if problem == 2 && length(a) <= 3
                # For problem 2 we know that there are 3 heads
                return toss(vcat(a, head))
            else
                # For other problems
                if rand(flip(0.8))
                    t ~ Categorical([0.5,0.5])
                    return toss(vcat(a, t))
                else
                    return a
                end
            end
        end
    end

    tosses = toss(Int8[])
    len = length(tosses)

    # * first problem: probability of the sequence [h,h,h] -> 0.015
    # * second problem: probability of the sequence [h,h,h,h,h] given that the subsequence [h,h,h] -> about 0.03
    # * third problem: probability of the sequences of 10 coin tosses -> about 0.026
    # * fourth problem: expected length of the sequences of coin tosses -> about 4.
    if problem == 1
        return len == 3 && tosses[1] == head && tosses[2] == head && tosses[3] == head
    elseif problem == 2
        return len == 5 && tosses[1] == head && tosses[2] == head && tosses[3] == head &&
                           tosses[4] == head && tosses[5] == head
    elseif problem == 3
        return len == 10
    else
        return len
    end
end


function run_problem(problem)
    println("Problem $problem")
    model = coin_tosses(problem)
    num_chains = 4

    # chains = sample(model, MH(), MCMCThreads(), 100_000, num_chains)
    # chains = sample(model, MH(), MCMCThreads(), 10_000, num_chains)
    # chains = sample(model, MH(), 10_000)

    # chains = sample(model, PG(15), MCMCThreads(), 1_000, num_chains)

    # chains = sample(model, SMC(1000), MCMCThreads(), 10_000, num_chains)
    # chains = sample(model, SMC(1000), 10_000)
    chains = sample(model, IS(), 10_000)

    display(chains)
    println("avg length: $(mean(chains[:len]))")
    println("fit(length): ")
    cc = round.(Int64,vcat(chains[:len]...)) # Convert to integer
    d = fit_mle(Geometric{Int64}, cc)
    println("d: $d")
    # println("avg t: $(mean(chains[:t]))")
    println("Probability of problem $problem:")
    genq = generated_quantities(model,chains)

    show_var_dist_pct(genq,100)
    if problem == 4
        println("Mean:",mean(genq))
    end
end

for problem in 1:4
    run_problem(problem)
    println("\n")
end
