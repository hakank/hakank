#=
  From Joost-Pieter Katoen
  "Probabilistic Programming Quantitative Modeling for the Masses?"
  (MMB 2018 Conference, Erlangen)
  
  Slide 4:
  """
  How likely does a student end up with a bad mood after getting
  a bad grade for an easy exam, given that she is well prepared?
  """

Distributions of variable difficulty
easy       =>    6022  (0.602200)
hard       =>    3978  (0.397800)

Distributions of variable preparation
good       =>   10000  (1.000000)

Distributions of variable grade
good       =>    6821  (0.682100)
bad        =>    3179  (0.317900)

Distributions of variable mood
bad        =>    5004  (0.500400)
good       =>    4996  (0.499600)

  Cf ~/webppl/student_mood_after_exam.wppl

=#
using Turing, StatsPlots, Distributions
include("jl_utils.jl")

@model function student_mood_after_exam()
    bad = 1
    good = 2
    badGood = [bad,good]

    easy = 1
    hard = 2
    
    difficulty ~ Categorical([0.6,0.4]) # [easy,hard]
    
    preparation ~ Categorical([0.7,0.3]) # badGood
    
    grade ~
        (difficulty==easy && preparation==bad)  ? Categorical([0.95,0.05]) : # badGood
        (difficulty==easy && preparation==good) ? Categorical([0.5,0.5])   :
        (difficulty==hard && preparation==bad)  ? Categorical([0.6,0.4])   :
        (difficulty==hard && preparation==good) ? Categorical([0.05,0.95]) : Dirac(easy)
    
    mood ~ grade == bad ? # badGood
        Categorical([0.9,0.1]) :
        Categorical([0.3,0.7])
    
    
    # true ~ Dirac(difficulty == easy)
    true ~ Dirac(preparation == good)
    # true ~ Dirac(grade == bad)
    # true ~ Dirac(mood == bad)

end

model = student_mood_after_exam()

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(5), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

display(chns)
# display(plot(chns))

bad_good = ["bad","good"]
show_var_dist_pct(chns, :difficulty, ["easy","hard"])

println()
show_var_dist_pct(chns, :preparation, bad_good)

println()
show_var_dist_pct(chns, :grade, bad_good)

println()
show_var_dist_pct(chns, :mood, bad_good)
