#=

   From https://reference.wolfram.com/language/ref/HypergeometricDistribution.html
   """
   Suppose an urn has 100 elements, of which 40 are special.
   ...
   Compute the probability that there are more than 25 special elements in a 
   draw of 50 elements.
     Answer: 0.0120902

   Compute the expected number of special elements in a draw of 50 elements.
     Answer: 20
   """

   mean(sumSpecialOf50): 19.9992

   Distributions of variable sumSpecialOf50 (num:0)
   19.00000 =>    1210  (0.121000)
   20.00000 =>    1139  (0.113900)
   21.00000 =>    1118  (0.111800)
   18.00000 =>     972  (0.097200)
   22.00000 =>     902  (0.090200)
   17.00000 =>     794  (0.079400)
   23.00000 =>     732  (0.073200)
   24.00000 =>     619  (0.061900)
   16.00000 =>     596  (0.059600)
   15.00000 =>     429  (0.042900)
   25.00000 =>     384  (0.038400)
   26.00000 =>     256  (0.025600)
   14.00000 =>     255  (0.025500)
   13.00000 =>     151  (0.015100)
   27.00000 =>     151  (0.015100)
   28.00000 =>     102  (0.010200)
   12.00000 =>      72  (0.007200)
   29.00000 =>      43  (0.004300)
   11.00000 =>      28  (0.002800)
   30.00000 =>      21  (0.002100)
   10.00000 =>      13  (0.001300)
   31.00000 =>       7  (0.000700)
   35.00000 =>       1  (0.000100)
   8.00000 =>       1  (0.000100)
   32.00000 =>       1  (0.000100)
   9.00000 =>       1  (0.000100)
   7.00000 =>       1  (0.000100)
   34.00000 =>       1  (0.000100)

   moreThan25SpecialOf50): 0.0583



   Cf ~/blog/urn_model1.blog
      ~/webppl/urn_model1.wppl

=#
using Turing, StatsPlots, Distributions
include("jl_utils.jl")

@model function urn_model1(n=100,NumDraws=50,NumSpecial=40)

    nonspecial = 1
    special = 2

    element ~ filldist(Categorical(simplex([n-NumSpecial,NumSpecial])),n) # [nonspecial,special]
    
    # We have (/observe) exactly 40 special elements (no random there!)
    numSpecial ~ Dirac(sum([element[i] == special for i in 1:n]))
   
    # Compute the expected number of special elements in a draw of 50 elements.
    sumSpecialOf50 ~ Dirac(sum([element[i] == special ? 1 : 0 for i in 1:NumDraws]))

    # What's the probability that there are more than 25 special elements in a draw of 50 elements
    moreThan25SpecialOf50 ~ Dirac(sumSpecialOf50 > 25)
    

end

model = urn_model1()

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(5), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

# display(chns)
# display(plot(chns))

println("mean(sumSpecialOf50): ", mean_val(chns,:sumSpecialOf50))
show_var_dist_pct(chns, :sumSpecialOf50)
println("moreThan25SpecialOf50): ", mean_val(chns,:moreThan25SpecialOf50))
