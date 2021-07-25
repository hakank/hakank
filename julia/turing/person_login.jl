#=

  From Yi Wu, Lei Li, Stuart Russell
  "BFiT: From Possible-World Semantics to Random-Evaluation Semantics in Open Universe"
  Page 3 (BLOG model)

  Distributions of variable numPeople (num:0)
  4.00000 =>    1763  (0.176300)
  5.00000 =>    1757  (0.175700)
  6.00000 =>    1519  (0.151900)
  3.00000 =>    1406  (0.140600)
  7.00000 =>    1067  (0.106700)
  2.00000 =>     845  (0.084500)
  8.00000 =>     632  (0.063200)
  9.00000 =>     372  (0.037200)
  1.00000 =>     326  (0.032600)
  10.00000 =>     180  (0.018000)
  11.00000 =>      83  (0.008300)
  12.00000 =>      28  (0.002800)
  13.00000 =>      12  (0.001200)
  14.00000 =>       5  (0.000500)
  15.00000 =>       4  (0.000400)
  16.00000 =>       1  (0.000100)

  Distributions of variable sumHonest (num:0)
  4.00000 =>    1893  (0.189300)
  5.00000 =>    1743  (0.174300)
  3.00000 =>    1727  (0.172700)
  6.00000 =>    1314  (0.131400)
  2.00000 =>    1124  (0.112400)
  7.00000 =>     840  (0.084000)
  1.00000 =>     472  (0.047200)
  8.00000 =>     461  (0.046100)
  9.00000 =>     221  (0.022100)
  10.00000 =>     106  (0.010600)
  11.00000 =>      40  (0.004000)
  0.00000 =>      36  (0.003600)
  12.00000 =>      15  (0.001500)
  14.00000 =>       5  (0.000500)
  13.00000 =>       3  (0.000300)

  Distributions of variable honest1 (num:0)
  1.00000 =>    9061  (0.906100)
  0.00000 =>     939  (0.093900)

  Distributions of variable login1 (num:0)
  1.00000 =>    9208  (0.920800)
  0.00000 =>     750  (0.075000)
  2.00000 =>      30  (0.003000)
  3.00000 =>       9  (0.000900)
  4.00000 =>       3  (0.000300)

  Note: Currently this model got problems when using SMC() etc.
  For a discussion about this, see https://discourse.julialang.org/t/turing-jl-sometimes-this-error-is-thrown-cannot-convert-an-object-of-type-geometric-float64-to-an-object-of-type-dirac-float64/65202/3

  Since this model don't have any observable data, the use of an inference sampler might
  give problems (and it does now). However, using Prior() seems to work without any problem.

   Cf ~/webppl/person_login.wppl

=#
using Turing, StatsPlots
include("jl_utils.jl")

@model function person_login()

    # Number of people (exclude 0 people)
    numPeople ~ truncated(Poisson(5),0,Inf)
    
    # Is this person honest?
    honest ~ filldist(Bernoulli(0.9),numPeople)   
    sumHonest ~ Dirac(sum(honest))
    
    # An honest person have just one login
    login = tzeros(numPeople)
    for p in 1:numPeople
        if honest[p] 
            login[p] ~ Dirac(1.0)
        else
            login[p] ~ Geometric(0.8)
        end
    end

    login1 ~ Dirac(login[1])
    honest1 ~ Dirac(honest[1])    

end


model = person_login()


chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
# chns = sample(model, PG(5), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

# display(chns)
# display(plot(chns))

show_var_dist_pct(chns,:numPeople)
show_var_dist_pct(chns,:sumHonest)
show_var_dist_pct(chns,:honest1)
show_var_dist_pct(chns,:login1)

