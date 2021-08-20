#=

   Dice game.

  From
  http://www.informs.org/ORMS-Today/Public-Articles/December-Volume-38-Number-6/THE-PUZZLOR
  """
  A         B           C           D
     7          4          6            5 
   1 1 1 1    4 4 4 4    2 2 2 2      3 5 3 3
     7          4          6            5
  Figure 1 shows four dice with varying numbers on each face. You and 
  three friends will play a simple game where you will each roll one 
  of the dice and the highest number wins. You get first pick from 
  the dice.

  Which die should you choose in order to maximize your chance of winning?
  """

  Analysis
    A vs B:  A wins 2 times, B wins 4 times
    A vs C:  A wins 2 times, C wins 4 times
    A vs D:  A wins 2 times, D wins 4 times
    B vs C:  B wins 4 times, C wins 2 times
    B vs D:  B wins 3 times, D wins 3 times
    C vs D:  C wins 2 times, D wins 4 times

    Wins (of row vs col)
      A  B  C  D    Sum (winning)
   A  -  2  2  2      6    0.166667
   B  4  -  4  3     11    0.305556   <---
   C  4  2  -  2      8    0.222222
   D  4  4  3  -     11    0.305556   <---
                    --- 
                     36

  This model give simular result:

  Summary Statistics
  parameters      mean       std   naive_se      mcse        ess      rhat   ess_per_sec 
      Symbol   Float64   Float64    Float64   Float64    Float64   Float64       Float64 

           A    2.0490    1.9556     0.0618    0.0520   638.1785    0.9994      115.5283
           B    3.5920    1.8480     0.0584    0.0572   600.4959    0.9990      108.7067
           C    2.6830    0.6519     0.0206    0.0173   638.1785    0.9994      115.5283
           D    3.6760    0.9726     0.0308    0.0263   677.3357    1.0016      122.6169
          Ap    0.1708    0.1630     0.0052    0.0043   638.1785    0.9994      115.5283
          Bp    0.2993    0.1540     0.0049    0.0048   600.4959    0.9990      108.7067
          Cp    0.2236    0.0543     0.0017    0.0014   638.1785    0.9994      115.5283
          Dp    0.3063    0.0811     0.0026    0.0022   677.3357    1.0016      122.6169

   Matrix of throws[i,j]:
   0.0000 1.6550 2.3100 2.9650 
   1.6620 0.0000 2.3380 3.0700 
   2.3240 2.3450 0.0000 3.6550 
   2.9860 3.0140 3.6620 0.0000 

   So one should select either die D (or perhaps die B).
    
=#

using Turing, StatsPlots, DataFrames
using Printf
include("jl_utils.jl")

@model function dice_game()
    S = [1 1 1 1 7 7
         4 4 4 4 4 4
         2 2 2 2 6 6
         3 5 3 3 5 5]

    throws = tzeros(4,4)
    for I in 1:4
        for J in 1:4
            if I != J
                # Roll the two dice
                Iix ~ DiscreteUniform(1,6)
                Jix ~ DiscreteUniform(1,6)
                # Save the winner
                throws[I,J] ~ Dirac(S[I,Iix] > S[J,Jix] ? I : J)
            else
                throws[I,J] ~ Dirac(0)
            end
        end
    end

    # Number of wins
    A ~ Dirac(sum([throws[i,j] == 1 for i in 1:4 for j in 1:4]))
    B ~ Dirac(sum([throws[i,j] == 2 for i in 1:4 for j in 1:4]))
    C ~ Dirac(sum([throws[i,j] == 3 for i in 1:4 for j in 1:4]))
    D ~ Dirac(sum([throws[i,j] == 4 for i in 1:4 for j in 1:4]))
    
    # Percentages
    total = A + B + C + D
    Ap ~ Dirac(A / total)
    Bp ~ Dirac(B / total)
    Cp ~ Dirac(C / total)
    Dp ~ Dirac(D / total)

end

model = dice_game()

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 1_000)
chns = sample(model, PG(5), 1_000)
# chns = sample(model, SMC(10), 1_000)
# chns = sample(model, IS(), 1_000)

display(chns[[:A,:B,:C,:D,:Ap,:Bp,:Cp,:Dp]])
# display(chns)
# display(plot(chns))

# show_var_dist_pct(chns,:A)
# show_var_dist_pct(chns,:B)
# show_var_dist_pct(chns,:C)
# show_var_dist_pct(chns,:D)

println("Matrix of throws[i,j]:")
for i in 1:4
    for j in 1:4
        @printf("%2.4f ", mean(chns[Symbol("throws[$i,$j]")]))
    end
    println()
end
