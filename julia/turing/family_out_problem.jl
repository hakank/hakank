#=
   From Eugene Charniak
   "Bayesian Networks without Tears"
   page 51
   
   """
   Suppose when I go home at night, I want to know if my family is home before I try the doors. 
   (Perhaps the most convenient door to enter is double locked when nobody is home.) 
   Now, often when my wife leaves the house, she turns on an outdoor light. However, 
   she sometimes turns on this light if she is expecting a guest. Also, we have a dog. 
   When nobody is home, the dog is put in the back yard. The same is true if the dog
   has bowel troubles. Finally, if the dog is in the backyard, I will probably hear her barking (or
   what I think is her barking), but sometimes I can be confused by other dogs barking. This
   example, partially inspired by Pearl’s (1988) earthquake example, is illustrated in figure 1.
   There we find a graph not unlike many we see in AI. We might want to use such diagrams to
   predict what will happen (if my family goes out, the dog goes out) or to infer causes from
   observed effects (if the light is on and the dog is out, then my family is probably out).

   The important thing to note about this example is that the causal connections are
   not absolute. Often, my family will have left without putting out the dog or turning on a
   light. Sometimes we can use these diagrams anyway, but in such cases, it is hard to know
   what to infer when not all the evidence points the same way. Should I assume the family is
   out if the light is on, but I do not hear the dog? What if I hear the dog, but the light is
   out? Naturally, if we knew the relevant probabilities, such as P(family-out | light-on, ¬ hear-
   bark), then we would be all set. However, typically, such numbers are not available for
   all possible combinations of circumstances. Bayesian networks allow us to calculate them
   from a small set of probabilities, relating only neighboring nodes.
   """

  Summary Statistics
     parameters      mean       std   naive_se      mcse          ess      rhat   ess_per_sec 
         Symbol   Float64   Float64    Float64   Float64      Float64   Float64       Float64 

     family_out    0.5434    0.4981     0.0050    0.0238     412.4049    1.0003       43.8169
  bowel_problem    0.0045    0.0669     0.0007    0.0027     617.6361    1.0026       65.6222
       light_on    0.9996    0.0200     0.0002    0.0004    2332.5805    1.0003      247.8305
        dog_out    0.4205    0.4937     0.0049    0.0206     564.5462    1.0002       59.9815
      hear_bark    0.0001    0.0100     0.0001    0.0001   10004.0032    1.0000     1062.8988


   Cf ~/webppl/family_out_problem.wppl
=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function family_out_problem()
    family_out    ~ flip(0.15)
    bowel_problem ~ flip(0.01)
    light_on      ~ family_out ? flip(0.6) : flip(0.05)
    dog_out       ~
        (family_out==true && bowel_problem==true)   ? flip(0.99) :
        (family_out==true && bowel_problem==false)  ? flip(0.90) :
        (family_out==false && bowel_problem==true)  ? flip(0.97) :
        (family_out==false && bowel_problem==false) ? flip(0.3)  : Dirac(false)
    
    hear_bark ~ dog_out ? flip(0.7) : flip(0.01);
    
    #=
      To take the earlier example, if I observe that the light is on (light-on = true) but 
      do not hear my dog (hear-bark = false), I can calculate the conditional probability 
      of family-out given these pieces of evidence. (For this case, it is .5.)
    =#
    true ~ Dirac(light_on == true)
    true ~ Dirac(hear_bark == false)


end

model = family_out_problem()

num_chains = 4

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(5), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

display(chns)
# display(plot(chns))

