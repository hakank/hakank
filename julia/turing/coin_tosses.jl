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

  Note: compared to BLOG and WebPPL models, this is a for loop solutions.
        (To check: It seems that using recursion as in those models don't
                   work in Turing.
        )

=#

#=
ulia> @time include("coin_tosses.jl")
Problem 1
Probability of problem 1:

Distributions of variable (num:20)
0.00000 =>   35055  (0.876375)
1.00000 =>    4945  (0.123625)


Problem 2
Probability of problem 2:

Distributions of variable (num:20)
0.00000 =>   29875  (0.746875)
1.00000 =>   10125  (0.253125)


Problem 3
Probability of problem 3:

Distributions of variable (num:20)
0.00000 =>   38991  (0.974775)
1.00000 =>    1009  (0.025225)


Problem 4
Chains MCMC chain (10000×14×4 Array{Float64,3}):

Iterations        = 1:10000
Thinning interval = 1
Chains            = 1, 2, 3, 4
Samples per chain = 10000
parameters        = i, tosses[1], tosses[2], tosses[3], tosses[4], tosses[5], tosses[6], tosses[7], tosses[8], tosses[9], tosses[10]
internals         = le, lp, weight

Summary Statistics
  parameters      mean       std   naive_se      mcse          ess      ⋯
      Symbol   Float64   Float64    Float64   Float64      Float64   Fl ⋯

           i    5.4932    2.8637     0.0143    0.0150   39858.1687    1 ⋯
   tosses[1]    1.5024    0.5000     0.0025    0.0024   38948.5470    0 ⋯
   tosses[2]    1.5983    0.6639     0.0033    0.0032   38547.3980    1 ⋯
   tosses[3]    1.6799    0.7610     0.0038    0.0039   39447.3220    0 ⋯
   tosses[4]    1.7387    0.8228     0.0041    0.0041   39791.3739    1 ⋯
   tosses[5]    1.8036    0.8677     0.0043    0.0040   39909.2109    1 ⋯
   tosses[6]    1.8379    0.8995     0.0045    0.0044   40658.2973    0 ⋯
   tosses[7]    1.8668    0.9222     0.0046    0.0045   40137.0842    1 ⋯
   tosses[8]    1.8955    0.9403     0.0047    0.0047   39153.5236    1 ⋯
   tosses[9]    1.9117    0.9531     0.0048    0.0049   38279.3118    1 ⋯
  tosses[10]    1.9292    0.9628     0.0048    0.0051   39562.1308    0 ⋯
                                                         1 column omitted

Quantiles
  parameters      2.5%     25.0%     50.0%     75.0%     97.5%
      Symbol   Float64   Float64   Float64   Float64   Float64

           i    1.0000    3.0000    5.0000    8.0000   10.0000
   tosses[1]    1.0000    1.0000    2.0000    2.0000    2.0000
   tosses[2]    1.0000    1.0000    1.0000    2.0000    3.0000
   tosses[3]    1.0000    1.0000    1.0000    2.0000    3.0000
   tosses[4]    1.0000    1.0000    1.0000    2.0000    3.0000
   tosses[5]    1.0000    1.0000    2.0000    3.0000    3.0000
   tosses[6]    1.0000    1.0000    2.0000    3.0000    3.0000
   tosses[7]    1.0000    1.0000    1.0000    3.0000    3.0000
   tosses[8]    1.0000    1.0000    2.0000    3.0000    3.0000
   tosses[9]    1.0000    1.0000    1.0000    3.0000    3.0000
  tosses[10]    1.0000    1.0000    1.0000    3.0000    3.0000

Probability of problem 4:

Distributions of variable (num:20)
1.00000 =>    8045  (0.201125)
2.00000 =>    6381  (0.159525)
3.00000 =>    5062  (0.126550)
11.00000 =>    4328  (0.108200)
4.00000 =>    4067  (0.101675)
5.00000 =>    3231  (0.080775)
6.00000 =>    2676  (0.066900)
7.00000 =>    2057  (0.051425)
8.00000 =>    1718  (0.042950)
9.00000 =>    1334  (0.033350)
10.00000 =>    1101  (0.027525)
Mean:4.580975


 24.121595 seconds (193.14 M allocations: 12.141 GiB, 35.09% gc time)

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function coin_tosses(problem)
    head = 1
    tail = 2

    if problem == 3 || problem == 4
        n = 10
    else
        n = 5
    end
    i ~ DiscreteUniform(1,n)
    # We create a fixed length list which seems to be much easier
    # than dynamically adding elements to tosses, especially
    # since for problem 2 we always access to tosses[1:5]
    tosses = tzeros(n)

    # We always do the first toss
    i = 1
    tosses[i] ~ Categorical([0.5,0.5])
    while rand(flip(0.8))
        i += 1
        if i > n
            break
        end
        tosses[i] ~ Categorical([0.5,0.5])
    end

    # We must add dummy value (here 3) for the unhandled cases in tosses[1:n]
    for t in 1:n
        if tosses[t] == 0
            tosses[t] ~ Categorical([0,0,1]) # dummy 3
        end
    end

    # * first problem: probability of the sequence [h,h,h] -> 0.015
    # * second problem: probability of the sequence [h,h,h,h,h] given that the subsequence [h,h,h] -> about 0.03
    # * third problem: probability of the sequences of 10 coin tosses -> about 0.026
    # * fourth problem: expected length of the sequences of coin tosses -> about 4.
    if problem == 1
        t ~ Dirac(tosses[1] == head && tosses[2] == head && tosses[3] == head)
    elseif problem == 2
        true ~ Dirac(tosses[1] == head)
        true ~ Dirac(tosses[2] == head)
        true ~ Dirac(tosses[3] == head)
        t ~ Dirac(tosses[1] == head && tosses[2] == head && tosses[3] == head &&
                  tosses[4] == head && tosses[5] == head)
    elseif problem == 3
        t ~ Dirac(i == 10)
    else
        return i
    end
end


function run_problem(problem)
    println("Problem $problem")
    model = coin_tosses(problem)
    num_chains = 4

    # HH has problem with this!
    # chains = sample(model, MH(), MCMCThreads(), 100_000, num_chains)
    # chains = sample(model, MH(), 100_000)

    # chains = sample(model, PG(15), MCMCThreads(), 1_000, num_chains)

    # chains = sample(model, SMC(1000), MCMCThreads(), 10_000, num_chains)
    # chains = sample(model, SMC(1000), 10_000)

    chains = sample(model, IS(), MCMCThreads(), 10_000, num_chains)

    display(chains)
    println("Probability of problem $problem:")

    show_var_dist_pct(chains,:i)
    if problem != 4
        show_var_dist_pct(chains,:t)
    end
end

for problem in 1:4
    run_problem(problem)
    println("\n")
end
