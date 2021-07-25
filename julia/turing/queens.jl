#=

   N-queens problem in WebPPL

   This model use the standard formulation of the n-queens problem.

   For n = 4
   Distributions of variable (num:0)
   [3, 1, 4, 2]	=>	5129 (0.5129)
   [2, 4, 1, 3]	=>	4871 (0.4871)

   But for larger n's it don't give all the correct solutions, and sometimes
   incorrect solutions.

  Cf ~/webppl/queens.wppl

=#
using Turing, StatsPlots
include("jl_utils.jl")

@model function queens(n=4)

    queen ~ filldist(DiscreteUniform(1,n),n)

    # All different
    for i in 1:n
        for j in 1:n
            if i != j
                # All different rows
                true ~ Dirac(queen[i] != queen[j])
                # Different diagonal 1
                true ~ Dirac(queen[i]+i != queen[j]+j)
                # Different diagonal 2                
                true ~ Dirac(queen[i]-i != queen[j]-j)
            end
        end
    end

    return queen
end

n = 4
model = queens(n)
# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
# chns = sample(model, PG(5), 10_000)
chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

# display(chns)
# display(plot(chns))
                    
chains_params = Turing.MCMCChains.get_sections(chns, :parameters)
genq = generated_quantities(model, chains_params)
show_var_dist_pct(genq)
