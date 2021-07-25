#=

  Number lock problem in Turing.jl
  
  From Presh Talwalkar (MindYourDecisions) 
  """
  Puzzles like this have been shared with the dubious claim that "only a
  genius can solve" them. But they are still fun problems so let's work one
  out.

  A number lock requires a 3 digit code. Based on these hints, can you crack
  the code?

   682 - one number is correct and in the correct position
   645 - one number is correct but in the wrong position
   206 - two numbers are correct but in the wrong positions
   738 - nothing is correct
   780 - one number is correct but in the wrong position

  Video:  https://youtu.be/-etLb-8sHBc
  """

  This model use the same approach as
  - MiniZinc model: http://hakank.org/minizinc/number_lock.mzn
  - Picat model: http://hakank.org/picat/number_lock.pi

  Summary Statistics
  parameters      mean       std   naive_se      mcse       ess      rhat   ess_per_sec 
      Symbol   Float64   Float64    Float64   Float64   Float64   Float64       Float64 

        x[1]    0.0000    0.0000     0.0000    0.0000       NaN       NaN           NaN
        x[2]    5.0000    0.0000     0.0000    0.0000       NaN       NaN           NaN
        x[3]    2.0000    0.0000     0.0000    0.0000       NaN       NaN           NaN

   Distributions of variable (num:0)
   [0, 5, 2]	=>	10000 (1.0)


  Cf ~/webppl/number_lock2.wppl

=#

using Turing, StatsPlots
include("jl_utils.jl")

@model function number_lock2(y, hints)
    # m = 5 # number of rows (i.e. number of hints)    
    # n = 3 # number of columns (i.e. length of the numbers)
    m,n = size(y)
    
    # The unknown number
    x ~ filldist(DiscreteUniform(0,9),n)

    #= 
    # Number of digits that has correct position and value.
    num_correct_positions = tzeros(m)
    for r in 1:m
        num_correct_positions[r] ~ Dirac(sum([y[r,i] == x[i] ? 1 : 0 for i in 1:n]))
    end

    # Number of digits that has correct value (wether in correct position or not)
    num_correct_values = tzeros(m)
    for r in 1:m
        num_correct_values[r] ~ Dirac(sum([y[r,j] == x[i] ? 1 : 0  for i in 1:n for j in 1:n]))
    end
    
    for r in 1:m
        hints[r,1] ~ Dirac(num_correct_positions[r])
        hints[r,2] ~ Dirac(num_correct_values[r])
    end
    =#

    # It's faster if we skip the two intermediate arrays num_correct_position and num_correct_values
    for r in 1:m
        hints[r,1] ~ Dirac(sum([y[r,i] == x[i] ? 1 : 0 for i in 1:n])) 
        hints[r,2] ~ Dirac(sum([y[r,j] == x[i] ? 1 : 0  for i in 1:n for j in 1:n]))
    end

    return x
end


function run_number_lock2(y, hints)

    model = number_lock2(y,hints)

    # chns = sample(model, Prior(), 10_000)
    # chns = sample(model, MH(), 10_000)
    # chns = sample(model, PG(5), 10_000)
    chns = sample(model, SMC(), 10_000)
    # chns = sample(model, IS(), 10_000)

    display(chns)
    # display(plot(chns))

    chains_params = Turing.MCMCChains.get_sections(chns, :parameters)
    genq = generated_quantities(model, chains_params)
    show_var_dist_pct(genq)
    println()
    
end

# This instance: [0,5,2]
# The number combinations
y = [6 8 2
     6 4 5
     2 0 6
     7 3 8
     7 8 0]

# Hints
hints = [1 1
         0 1
         0 2
         0 0
         0 1]

run_number_lock2(y, hints)


# Another instance: https://twitter.com/natebrix/status/1418256269052321793
# The only changed is that the second row is
#   6 1 4
# instead of
#   6 4 5

# Solution:
# [0, 4, 2]	=>	10000 (1.0)#
#
y = [6 8 2
     6 1 4 # <- changed from 6 4 5
     2 0 6
     7 3 8
     7 8 0]

# Hints
hints = [1 1
         0 1
         0 2
         0 0
         0 1]
run_number_lock2(y, hints)
