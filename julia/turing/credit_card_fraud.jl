#=
   Credit-card fraud.

   From David Heckerman
   "A Tutorial on Learning With Bayesian Networks"
   page 12ff
   """
   To illustrate the process of building a Bayesian network, consider the problem of de-
   tecting credit-card fraud. We begin by determining the variables to model. One possible
   choice of variables for our problem is Fraud(F), Gas(G), Jewelry(J) Age(A) and Sex(S) 
   representing whether or not the current purchase is fraudulent, whether or not there
   was a gas purchase in the last 24 hours, whether or not there was a jewelry purchase in
   the last 24 hours, and the age and sex of the card holder, respectively.
   """

   Distributions of variable fraud (num:0)
   1.00000 =>   20244  (0.506100)
   0.00000 =>   19756  (0.493900)

   Distributions of variable sex
   male       =>   21678  (0.541950)
   female     =>   18322  (0.458050)

   Distributions of variable age
   age_30_to_50 =>   17315  (0.432875)
   age_above_50 =>   15237  (0.380925)
   age_below_30 =>    7448  (0.186200)

   Distributions of variable gas (num:0)
   0.00000 =>   39821  (0.995525)
   1.00000 =>     179  (0.004475)

   Distributions of variable jewlery (num:0)
   0.00000 =>   39980  (0.999500)
   1.00000 =>      20  (0.000500)


   Cf ~/webppl/credit_card_fraud.wppl

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function credit_card_fraud()
    male = 1
    female = 2
    
    #  type Age;
    age_below_30 = 1
    age_30_to_50 = 2
    age_above_50 = 3
    
    fraud ~   flip(0.00001)    
    age ~ Categorical(simplex([0.25, 0.40, 0.35]))  # [age_below_30, age_30_to_50, age_above_50]
    sex ~ Categorical(simplex([0.5,0.5]))           # [male, female]
    
    gas ~ fraud ? flip(0.2) : flip(0.01);
    
    jewlery ~  fraud ? flip(0.05) :
        (age == age_below_30 && sex==male)   ? flip(0.0001) :
        (age == age_30_to_50 && sex==male)   ? flip(0.0004) :
        (age == age_above_50 && sex==male)   ? flip(0.0002) :
        (age == age_below_30 && sex==female) ? flip(0.0005) :
        (age == age_30_to_50 && sex==female) ? flip(0.002)  : 
        (age == age_above_50 && sex==female) ? flip(0.001)  : Dirac(0)
    
    
    # true ~ Dirac(age == age_below_30)
    # true ~ Dirac(age == age_30_to_50)
    # true ~ Dirac(age == age_above_50)
    # true ~ Dirac(gas == true)
    # true ~ Dirac(sex == female)
    # true ~ Dirac(sex == male)    
    # true ~ Dirac(jewlery == true)
    true ~ Dirac(fraud == true)
    # true ~ Dirac(fraud == false)
  
end

model = credit_card_fraud()

num_chains = 4

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 100_000)
chns = sample(model, PG(5), 40_000)
# chns = sample(model, SMC(), 40_000)
# chns = sample(model, IS(), 100_000)

display(chns)
# display(plot(chns))

show_var_dist_pct(chns, :fraud)
show_var_dist_pct(chns, :sex,["male","female"])
show_var_dist_pct(chns, :age,["age_below_30","age_30_to_50","age_above_50"])
show_var_dist_pct(chns, :gas)
show_var_dist_pct(chns, :jewlery)
