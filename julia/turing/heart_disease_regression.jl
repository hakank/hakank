#=
  From Hugin's BN heart_disease_regression.net
   
  Risk of dying from heart disease in the next 10 years


  Cf ~/webppl/hear_disease_regression.wppl
  In the WebPPL model, I did several alternative, but here I just settle with one.
  For two of four models I got very high risk factor and commented:
  """
  Darn, according to this model, there's a probability of 0.9999999983124701
  that I will die from a heart disease in the next 10 years...
  """

  But this Turing.jl model give a much better fate for me: a risk of only 31% of
  dying from heart disease in the next 10 years.


  Summary Statistics
         parameters       mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
             Symbol    Float64   Float64    Float64   Float64     Float64   Float64       Float64 

                sex     1.4957    0.5000     0.0050    0.0048   9718.4386    0.9999     2893.2535
        cholesterol     5.8248    9.8761     0.0988    0.0952   9897.9782    0.9999     2946.7038
                age    39.9645   24.8574     0.2486    0.2697   9714.7104    1.0002     2892.1436
  score_cholesterol     0.9898   11.8513     0.1185    0.1143   9897.9782    0.9999     2946.7038
          score_sex    -1.4957    0.5000     0.0050    0.0048   9718.4386    0.9999     2893.2535
          score_age   -20.0711   49.7148     0.4971    0.5395   9714.7104    1.0002     2892.1436
              score   -25.5770   50.9925     0.5099    0.5488   9756.6159    1.0001     2904.6192
               risk     0.3088    0.4545     0.0045    0.0048   9709.6351    0.9999     2890.6327
       oneMinusRisk     0.6912    0.4545     0.0045    0.0048   9709.6351    0.9999     2890.6327



=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function heart_disease_regression(sexVal=0,ageVal=64)
    male = 0
    female = 1
    
    #  Priors
    sex ~ Categorical([0.5,0.5]) # [male,female]
    cholesterol ~ Normal(6,10.0) #  In the Hugin model the variance is 100.0.
    age ~ Normal(40,25) #  In the Hugin model the variance is 625.

    intercept = -5.0
    
    score_cholesterol ~ Dirac(1.2 * (cholesterol - 5.0))
    score_sex ~ Dirac(-1 * sex)
    score_age ~ Dirac(2.0 * (age - 50.0))
    
    score ~ Dirac(intercept + score_age + score_sex + score_cholesterol)
    
    #  Risk of dying from heart disease in the next 10 years
    risk ~ Dirac(1.0/(1.0 + exp(-score)))

    true ~ Dirac(sex==male)
    # true ~ Dirac(age>=64 && age <= 65)
    true ~ Dirac(age == ageVal)    
    
    # true ~ Dirac(age>=60)
    # true ~ Dirac(cholesterol>=1)
    true ~ Dirac(cholesterol>=4.5 && cholesterol<=4.9) #  ??? I'm note sure about this
    #  true ~ Dirac(score<=0.5)

    oneMinusRisk ~ Dirac(1.0 - risk)
end

model = heart_disease_regression()

num_chains = 4

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
# chns = sample(model, PG(15), 10_000)
chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

# chns = sample(model, NUTS(), 10_000)
# chns = sample(model, Gibbs(PG(5,:sex),NUTS(1000,0.65,:cholesterol,:age,:score,:score_cholesterol,:score_sex,:score_age,:risk)), 10_000)

display(chns)
# display(plot(chns))
