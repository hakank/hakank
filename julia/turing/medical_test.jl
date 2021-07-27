#=
  Medical test.

  https://www.math.hmc.edu/funfacts/ffiles/30002.6.shtml
  """
  Suppose that you are worried that you might have a rare disease. You decide to get tested,
  and suppose that the testing methods for this disease are correct 99 percent of the time
  (in other words, if you have the disease, it shows that you do with 99 percent probability,
  and if you don't have the disease, it shows that you do not with 99 percent probability).
  Suppose this disease is actually quite rare, occurring randomly in the general population
  in only one of every 10,000 people.

  If your test results come back positive, what are your chances that you actually have the disease?

  Do you think it is approximately: (a) .99, (b) .90, (c) .10, or (d) .01?

  Surprisingly, the answer is (d), less than 1 percent chance that you have the disease!

  """

  Cf ~/cplint/medical_test.pl
     ~/blog/medical_test.blog
     ~/psi/medical_test.psi
     ~/webppl/medical_test.wppl

=#

# using Memoization
using Turing, StatsPlots, DataFrames
using ReverseDiff, Zygote, Tracker
using Printf
# Turing.setadbackend(:reversediff)
# Turing.setadbackend(:zygote)
# Turing.setadbackend(:tracker)

include("jl_utils.jl")

@model function medical_test(test, disease,prob_of_disease=1/10000.0,reliability=0.99)
    # Suppose this disease is actually quite rare, occurring randomly in the general population
    # in only one of every 10,000 people.
    # prob_of_disease = 1/10000.0
    # prob_of_disease = 1/1000.0

    # Probability that a person has a disease (and there's a reason to do a test)
    disease ~ flip(prob_of_disease)

    # The test is quite accurate: It shows correct result (test is positive if disease) in 99%.
    # However, in 1% of the case it shows incorrect result (positive even if there is no disease).
    # reliability = 0.99    # => disease:0.009803921568627446
    # reliability = 0.999  # A more reliable test => disease:0.09083469721767588
    # reliability = 0.9999 # An even more reliable test => disease:0.500000000000028

    test ~ disease ? flip(reliability) : flip(1-reliability)


end

function run_medical_test()
    for t in [missing,true,false],
        d in [missing,true,false],
        pd in [1/10000.0, 1/1000.0],
        r in [0.99, 0.999,0.9999]
        println("\n\ntest:$t disease:$d prob disease:$pd reliability:$r ")
        # model = medical_test(t, d,pd,r)
        model = medical_test(t, d)

        # chns = sample(model, Prior(), 40_000)
        chns = sample(model, MH(), 100_000)
        # display(chns)
        show_var_dist(chns,:test)
        show_var_dist(chns,:disease)

    end
end

@time run_medical_test()
