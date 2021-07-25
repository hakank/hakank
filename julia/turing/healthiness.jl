#=

  From BLOG examples/healtiness.blog:
  """
  healthiness model
  This BLOG program describes the model for healthiness in 
  a person.
  See exercise 3.5 in:
  Probabilistic Graphical Models: Principles and Techniques
  Daphne Koller, Nir Friedman, MIT 2009
  Each person is described by whether they are health
  conscious, have free time, exercise, have a good diet, 
  have normal weight, have high cholesterol, and whether
  they tested positive for high cholesterol.
  @author: jnieh
  @date: 2012-09-13
  """

  For evidence TestedHighCholesterol == false:
  Summary Statistics
             parameters      mean       std   naive_se      mcse          ess      rhat   ess_per_sec 
                 Symbol   Float64   Float64    Float64   Float64      Float64   Float64       Float64 

        HealthConscious    0.5614    0.4962     0.0050    0.0080    4159.9755    0.9999      405.4952
         LittleFreeTime    0.4870    0.4999     0.0050    0.0081    4242.7390    0.9999      413.5626
               Exercise    0.5430    0.4982     0.0050    0.0075    4070.4820    1.0002      396.7718
               GoodDiet    0.6641    0.4723     0.0047    0.0074    4392.4673    1.0001      428.1575
           NormalWeight    0.5620    0.4962     0.0050    0.0069    4156.5796    1.0010      405.1642
        HighCholesterol    0.0987    0.2983     0.0030    0.0043    4344.1532    0.9999      423.4480
  TestedHighCholesterol    0.0001    0.0100     0.0001    0.0001   10000.0008    1.0000      974.7540

  For evidence exercise = true
  Summary Statistics
             parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
                 Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

        HealthConscious    0.6805    0.4663     0.0047    0.0071   4317.9635    1.0006      425.9607
         LittleFreeTime    0.2970    0.4570     0.0046    0.0070   4361.5047    1.0000      430.2560
               Exercise    1.0000    0.0000     0.0000    0.0000         NaN       NaN           NaN
               GoodDiet    0.5774    0.4940     0.0049    0.0072   4022.1381    1.0000      396.7780
           NormalWeight    0.6787    0.4670     0.0047    0.0074   3908.1858    0.9999      385.5367
        HighCholesterol    0.4715    0.4992     0.0050    0.0078   4117.2828    1.0004      406.1638
  TestedHighCholesterol    0.4780    0.4995     0.0050    0.0078   4117.0265    0.9999      406.1386



  Cf ~/webppl/healthiness.wppl

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function healthiness()
   
    HealthConscious ~ flip(0.5)    
    LittleFreeTime ~ flip(0.5)
    
    Exercise ~ 
            (HealthConscious==true && LittleFreeTime==true) ? flip(0.5) : 
            (HealthConscious==true && LittleFreeTime== false) ? flip(0.9) :
            (HealthConscious==false && LittleFreeTime== true) ? flip(0.1) :
            (HealthConscious==false && LittleFreeTime== false) ? flip(0.5) : Dirac(false)
    
    GoodDiet ~ HealthConscious ? flip(0.7) : flip(0.3)
  
    
    NormalWeight ~ (GoodDiet==true && Exercise==true) ? flip(0.8) :
                   (GoodDiet==true && Exercise== false) ? flip(0.5) : 
                   (GoodDiet==false && Exercise== true) ? flip(0.5) :
                   (GoodDiet==false && Exercise== false) ? flip(0.2) : Dirac(false)
    
    HighCholesterol ~ GoodDiet ? flip(0.3) : flip(0.7)

    TestedHighCholesterol ~ HighCholesterol ? flip(0.9) : flip(0.1)
    
    # Evidence
    true ~ Dirac(TestedHighCholesterol == false) #  Original evidence
    # true ~ Dirac(TestedHighCholesterol == true)
    # true ~ Dirac(Exercise == true)
    # true ~ Dirac(LittleFreeTime == true)

end

model = healthiness()

num_chains = 4

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(5), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)


display(chns)
# display(plot(chns))
