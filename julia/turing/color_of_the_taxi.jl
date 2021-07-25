#=
  What is the color of the taxi?

  From https://www.bayesia.com/2018-03-02-probabilistic-reasoning-under-uncertainty

  Originally from Kahnemann, Slovic, Tversky "Judgement under uncertainty"
  

  There has been an accicent involving a taxi.
  There are two kinds of taxis:
    - yellow taxi: 85% 
    - white taxi: 15%

  A witness say: It was a white taxi involved in the accident.

  Researcher:
    - 80% of all witness statements are true
    - 20% of all witness statements are false.

  What is the probability that it was a white taxi involved in
  the accident?

  Answer: The probability that it was a white taxi involved in the accident
          is about 41-42%. And it's the same as in the talk.
  
  This model:
  Distributions of variable involved
  yellow     =>    5897  (0.589700)
  white      =>    4103  (0.410300)

  Distributions of variable witness_white (num:0)
  1.00000 =>    9999  (0.999900)
  0.00000 =>       1  (0.000100)

  Distributions of variable witness_yellow (num:0)
  1.00000 =>    5614  (0.561400)
  0.00000 =>    4386  (0.438600)

  Cf ~/blog/color_of_the_taxi.blog
     ~/psi/color_of_the_taxi.psi
     ~/webppl/color_of_the_taxi.wppl


=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function color_of_the_taxi()
    white = 1
    yellow = 2
    #  Prior distributions of the different taxis.
    involved ~ Categorical([0.15,0.85]) # [white,yellow]
    
    #  Witness says color but is is only x percent reliable.
    #  Witness experts states that a witness can only be 80% reliable
    #  (given the same circumstances as the accidents).
    witness = tzeros(2)
    for c in 1:2
        if c == involved
            witness[c] ~ flip(0.80)
        else
            witness[c] ~ flip(0.20)
        end
    end
    
    true ~ Dirac(witness[white] == true)
    # true ~ Dirac(witness[yellow] == true)

    witness_white ~ Dirac(witness[white])
    witness_yellow ~ Dirac(witness[yellow])

end

model = color_of_the_taxi()
num_chains = 4

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(5), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

display(chns)
# display(plot(chns))

println("white: 1 yellow: 2")
show_var_dist_pct(chns,:involved,["white","yellow"])
show_var_dist_pct(chns,:witness_white)
show_var_dist_pct(chns,:witness_yellow)
