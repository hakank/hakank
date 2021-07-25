#=
  Two children problem in Turing.jl

  https://robeastaway.com/blog/boy-girl
  """
  I have two children, at least one of whom is a boy.  
  What is the chance that the second child is also a boy?

  ...

  The classic, surprising answer is that the chance of a second 
  boy is 1/3, not 1/2 as most would expect.  Why?  Because if I 
  have two children, there is an equal chance that they will be 
  Boy-Boy, Boy-Girl, Girl-Boy or Girl-Girl.  There are three 
  equally likely combinations in which at least one child is 
  a boy (BB, BG and GB) and only in the first scenario is the 
  other child also a boy.
  """

  We assume the inital probability of a boy/girl as 0.5.

  Here we model two similiar but different problems to show
  the difference between these two views of the problem.

  Cf ~/webppl/two_children_problem.wppl

=#

using Turing
include("jl_utils.jl")

#=
  First model: 
  """
  I have two children, _at least one of whom_ is a boy.  
  What is the chance that the second child is also a boy?
  """

  Probability of 2 boys: 1/3.
  Probability of 1 boy and 1 girl: 2/3.

  Distributions of variable twoBoys (num:0)
  0.00000 =>    6601  (0.660100)
  1.00000 =>    3399  (0.339900)

  Distributions of variable oneBoyOneGirl (num:0)
  1.00000 =>    6601  (0.660100)
  0.00000 =>    3399  (0.339900)

=#
@model function two_children_problem_1()
    boy = 1
    girl = 2

    child1 ~ Categorical([0.5,0.5])
    child2 ~ Categorical([0.5,0.5])

    # Note: it's important that we state the condition
    #   "at least one of whom is a boy"
    # as something like this.
    true ~ Dirac((child1 == boy) + (child2 == boy) >= 1)
    
    twoBoys ~ Dirac((child1 == boy) + (child2 == boy) == 2)
    oneBoyOneGirl ~ Dirac((child1 == boy) + (child2 == boy) == 1)

end

println("\nModel 1")
model = two_children_problem_1()
# chains = sample(model, Prior(), 10_000)
# chains = sample(model, MH(), 10_000)
chains = sample(model, PG(5), 10_000)
# chains = sample(model, IS(), 10_000)
# chains = sample(model, SMC(), 10_000)
# chains = sample(model, NUTS(), 10_000)
# chains = sample(model, HMC(0.1,10), 10_000)

# display(chains)
show_var_dist_pct(chains,:twoBoys)
show_var_dist_pct(chains,:oneBoyOneGirl)


#=
 Second model:
  """
  I have two children, the eldest is a boy.  
  What is the chance that the second child is also a boy?
  """

  Probability that the second child is also a boy: 1/2
  Probability that the second child is a girl: 1/2

  Distributions of variable twoBoys (num:0)
  0.00000 =>    5067  (0.506700)
  1.00000 =>    4933  (0.493300)
  Distributions of variable oneBoyOneGirl (num:0)
  1.00000 =>    5067  (0.506700)
  0.00000 =>    4933  (0.493300)

=#
@model function two_children_problem_2()
    boy = 1
    girl = 2

    child1 ~ Categorical([0.5,0.5])
    child2 ~ Categorical([0.5,0.5])
    
    # "The eldest is a boy"
    true ~ Dirac(child1 == boy)
    
    twoBoys ~ Dirac((child1 == boy) + (child2 == boy) == 2)
    oneBoyOneGirl ~ Dirac((child1 == boy) + (child2 == boy) == 1)

end

println("\nModel 2")
model = two_children_problem_2()
# chains = sample(model, Prior(), 10_000)
# chains = sample(model, MH(), 10_000)
chains = sample(model, PG(5), 10_000)
# chains = sample(model, IS(), 10_000)
# chains = sample(model, SMC(), 10_000)
# chains = sample(model, NUTS(), 10_000)
# chains = sample(model, HMC(0.1,10), 10_000)

# display(chains)
show_var_dist_pct(chains,:twoBoys)
show_var_dist_pct(chains,:oneBoyOneGirl)
