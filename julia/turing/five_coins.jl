#=
  From https://math.stackexchange.com/questions/3633307/bayes-rule-broken
  """
  Bayes' Rule broken?!?!

  This question has been driving me CRAZY for 4 days now. The question comes from 
  the textbook 'One Thousand Exercises in Probability', specifically Exercise 3 in 
  section 1.4. The solution does not make sense! The question goes as follows:

     'A man possesses five coins, two double-headed, two normal and one double-tailed. 
     The man shuts his eyes, picks a coin at random, and tosses the coin. He opens 
     his eyes, sees a head: what is the probability the lower face is also a head?.

  The book gives an answer that is 2/3
  ....
  """

  Distributions of variable coin (num:0)
  1.00000 =>    6585  (0.658500)
  2.00000 =>    3415  (0.341500)

  Distributions of variable flip_is_head (num:0)
  1.00000 =>    6585  (0.658500)
  0.00000 =>    3415  (0.341500)

   Cf ~/blog/five_coins.blog
      ~/webppl/family_out_problem.wppl
=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function five_coins()
    doubleHead = 1
    normalCoin = 2
    doubleTail = 3

    head = 1
    tail = 2
    
    coin ~ Categorical([2/5,2/5,1/5]) # [doubleHead,normalCoin,doubleTail]
    
    #  Toss the coin
    toss ~ 
        (coin == doubleHead) ? Dirac(head) :
        (coin == doubleTail) ? Dirac(tail) : 
        Categorical([1/2,1/2]) # [head,tail]

    #  Flip the coin (i.e. turn it around)
    flip ~ Dirac((coin == doubleHead) ? head :
                 (coin == doubleTail) ? tail : 
                 (toss == head) ? tail : head
                 )

    #  We observe that the toss give a head.
    true ~ Dirac(toss == head);

    #  What is the probability that the other side (flip side)
    #  is head?
    flip_is_head ~ Dirac(flip == head)
end

model = five_coins()

num_chains = 4

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(5), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

display(chns)
# display(plot(chns))

show_var_dist_pct(chns,:coin)
show_var_dist_pct(chns,:flip_is_head)

