#=
  https://dtai.cs.kuleuven.be/problog/tutorial/basic/03_dice.html
  """
  The following example illustrates the use of a logic program with recursion and lists.
  We start by rolling the first die. Every roll determines the next die to roll, but we stop
  if we have used that die before. We query for the possible sequences of rolled dice.
  We use three-sided dice instead of the regular six-sided ones simply to restrict the number
  of possible outcomes (and thus inference time).
  """

  Cf ~/blog/rolling_dice5.blog
     ~/webppl/rolling_dice5.wppl
=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function rolling_dice5(n=3)

    function roll(a)
        # t ~ DiscreteUniform(1,n)
        t = rand(DiscreteUniform(1,n))
        if t in a
            return a
        else
            return roll(vcat(a,t))
        end
    end


    a = roll([])
    len ~ DiscreteUniform(1,n)
    len ~ Dirac(length(a))

    # In the example, the first roll is 1.
    true ~ Dirac(a[1] == 1)

end


function run_model(len=0)
    model = rolling_dice5(len)

    num_chns = 4

    # chns = sample(model, Prior(), 10_000)

    # chns = sample(model, MH(), 10_000)
    chns = sample(model, MH(), 100_000)

    # chns = sample(model, PG(15), MCMCThreads(), 1_000, num_chns)

    # chns = sample(model, SMC(1000), MCMCThreads(), 10_000, num_chns)
    # chns = sample(model, SMC(1000), 10_000)
    # chns = sample(model, IS(), 10_000)
    #
    display(chns)
    show_var_dist_pct(chns,:len,1000)

end

genq = undef
for val in 1:6
    global genq
    println("\nval:$val")
    @time run_model(val)
end

println("\nval=10")
# println("mean:", mean(run_model(10)))
run_model(10)
println("\nval=100")
# println("mean:", mean(run_model(100)))
run_model(100)
