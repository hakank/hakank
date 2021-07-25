#=

  From
  https://mathematica.stackexchange.com/questions/128945/problems-encoding-a-bayesian-network-with-just-five-nodes-using-probabilitydistr

  Example is from "page 53 in Probabilistic Graphical Models (2009), by Daphne Koller and Neir Friedman:"

  """                                                                                                                                            
  The network has five nodes (random variables):
                                                                                                                                              
  Difficulty of a class taken by a student (0 = easy, 1 = hard)
  Intelligence of the student (0 = low, 1 = high)
  Grade achieved by the student (1 = A, 2 = B, 3 = C)
  SAT score of the student (0 = low, 1 = high)
  Letter of recommendation by the teacher (0 = False, 1 = True)
  We would like to use this network to do probabilistic inference (causal or evidential) like:                                                   
  "What is the probability of the student achieving an A, given that he is intelligent?"
  """
  
  For observation/evidence: intelligence = intelligence_high

  Summary Statistics
    parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
        Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

    difficulty    1.4083    0.4915     0.0049    0.0101   2853.3277    1.0002      526.7358
  intelligence    2.0000    0.0000     0.0000    0.0000         NaN       NaN           NaN
         grade    1.3519    0.6449     0.0064    0.0105   3016.3692    1.0005      556.8339
        letter    0.7597    0.4273     0.0043    0.0072   2982.1758    1.0003      550.5216
           sat    1.1925    0.3943     0.0039    0.0072   3001.3436    0.9999      554.0601


  Distributions of variable grade
  a          =>    7420  (0.742000)
  b          =>    1641  (0.164100)
  c          =>     939  (0.093900)

  Distributions of variable intelligence
  high       =>   10000  (1.000000)

  Distributions of variable difficulty
  easy       =>    5917  (0.591700)
  hard       =>    4083  (0.408300)

  Distributions of variable letter (num:0)
  1.00000 =>    7597  (0.759700)
  0.00000 =>    2403  (0.240300)

  Distributions of variable sat
  high       =>    8075  (0.807500)
  low        =>    1925  (0.192500)


  Cf ~/blog/intelligence_test.blog
     ~/webppl/intelligence_test.wppl

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function intelligence_test()
    difficulty_easy = 1
    difficulty_hard = 2
    
    intelligence_low = 1
    intelligence_high = 2
    difficulty ~ Categorical([0.6,0.4]) # [difficulty_easy,difficulty_hard]
        
    intelligence ~ Categorical([0.7,0.3]) # [intelligence_low, intelligence_high]

    grade_a = 1
    grade_b = 2
    grade_c = 3
    grades = [grade_a,grade_b,grade_c]
    grade ~
        (intelligence == intelligence_low && difficulty==difficulty_easy)  ? Categorical([0.3 , 0.4 , 0.3 ]) : 
        (intelligence == intelligence_low && difficulty==difficulty_hard)  ? Categorical([0.05, 0.25, 0.7 ]) :
        (intelligence == intelligence_high && difficulty==difficulty_easy) ? Categorical([0.9 , 0.08, 0.02]) :
        (intelligence == intelligence_high && difficulty==difficulty_hard) ? Categorical([0.5 , 0.3 , 0.2 ]) : Dirac(grade_c);
    
    
    letter ~
        (grade == grade_a) ? flip(0.9) : 
        (grade == grade_b) ? flip(0.6) :
        (grade == grade_c) ? flip(0.01) : Dirac(false)
    
    
    sat ~ 
        (intelligence == intelligence_high) ? Categorical([0.8,0.2]) : # [sat_high,sat_low]}
        Categorical([0.05,0.95])
    
    
    # What is the probability of the student achieving an A, given that he is intelligent?
    # true ~ Dirac(sat == sat_high);
    # true ~ Dirac(letter == false);
    true ~ Dirac(intelligence == intelligence_high);


end

model = intelligence_test()

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 100_000)
# chns = sample(model, PG(5), 10_000)
chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

display(chns)
# display(plot(chns))

show_var_dist_pct(chns, :grade,["a","b","c"])
show_var_dist_pct(chns, :intelligence,["low","high"])
show_var_dist_pct(chns, :difficulty,["easy","hard"])
show_var_dist_pct(chns, :letter)
show_var_dist_pct(chns, :sat,["high","low"])
