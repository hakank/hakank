#=
  Random walk model

  from Foundations of Probabilistic Programming,
  page 3

  The distribution of u and v are about the same as this
ulia> show_var_dist_pct(rand(DiscreteUniform(-100,100),10000),10)

Distributions of variable (num:10)
-76.00000 =>      67  0.006700
-38.00000 =>      65  0.006500
17.00000 =>      64  0.006400
59.00000 =>      64  0.006400
81.00000 =>      63  0.006300
24.00000 =>      62  0.006200
74.00000 =>      61  0.006100
-6.00000 =>      61  0.006100
-100.00000 =>      60  0.006000
-95.00000 =>      60  0.006000



=#

using Turing, StatsPlots, DataFrames
include("../jl_utils.jl")

data = []
@model function random_walk()
    u ~ DiscreteUniform(-10,10)
    v ~ DiscreteUniform(-10,10)
    function step(tu=0,tv=0)
        x ~ flip()
        y ~ flip()
        tu = tu + (x-y)
        tv = tv + (x+y-1)
        return tu,tv
    end
    # println("\nSTART")
    datad = [[0,0]]
    u,v = step(0,0)
    push!(datad,[u,v])
    c = 0
    while u != 0 || v != 0
        u,v = step(u,v)
        push!(datad,[u,v])
        c += 1
        # Note: We must restrict the walk, otherwise
        #       it'll blow up...
        if u == 0 && v == 0
            println("BACK TO 0,0!")
        end
        if c < 10
            push!(data,datad)
        else
            break
        end

    end
end

model = random_walk()

num_chains = 4
chains = sample(model, MH(), MCMCThreads(), 1000, num_chains)
# chains = sample(model, MH(), 10_000)

display(chains)
show_var_dist_pct(chains, :x)
show_var_dist_pct(chains, :y)
show_var_dist_pct(chains, :u,20)
show_var_dist_pct(chains, :v,20)

# for d in data
#    println(d)
# end
