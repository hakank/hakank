#=
   False coin in Turing.jl

   Inspired by Peter Winkler in
   From Celebration of Mind (Gathering For Gardner):
   "CoM Apr 2021 - Drawing from Urns - Peter Winkler"
   https://www.youtube.com/watch?v=A0GxvYPTKDk&list=PL5_D5QQxvWKVtX4Vklvhsf5KTYZBLDSwO&index=3

   Note: This is not what Peter talked about, but what I - wrongly - 
   remembered.

   *  We have one fake and one proper coin:
   1. head, head
   2. head, tail

  * We take one coin randomly and flip it: it's a head
  * To identify the fake coin, is it better to flip the same
    coin or flip the other coin?

  According to this model, it's better to throw the same coin again, i.e.
    coinB_switchIsFake: 1/3
  vs
    coinB_sameIsFake: 2/3

  """
  Distributions of variable coinB_switchIsFake (num:0)
  0.00000 =>    6586  (0.658600)
  1.00000 =>    3414  (0.341400)

  Distributions of variable coinB_sameIsFake (num:0)
  1.00000 =>    6586  (0.658600)
  0.00000 =>    3414  (0.341400)
  """


  Note: The problem that Peter Winkler's talked about was a little different:
  @3:43
  """
  You have two quarters.
  Both are sitting on a table heads up.
  One of them is an ordinary quarter, but the
  other has a head on the other side as well.

  You get two flips to help you guess which is which.

  Should you flip each coin once or one coin twice.
  """

  See false_coin2.jl for this version.

=#

using Turing
include("jl_utils.jl")

@model function false_coin()
    #  The two coins
    coin1 = 1 #  fake coin: head,head
    coin2 = 2 #  proper coin: head,tail
    coins = [coin1,coin2];

    #  Sides of the coins
    head = 1
    tail = 2

    function flip_coin(cond) 
        return cond ? Categorical([1.0,0.0]) :  Categorical([0.5,0.5])
    end
    
    #  1. Pick a coin randomly and flip it. It's head.
    coinA ~ Categorical([0.5,0.5])
    sideA ~ flip_coin(coinA == coin1)

    #  We observe that the first flip was a head.
    true ~ Dirac(sideA == head)
    
    # 2a: Take the other coin and flip it.
    coinB_switch ~ Dirac(coinA == coin1 ? coin2 : coin1)
    sideB_switch ~ flip_coin(coinB_switch == coin1)
    
    # 2b: Take the same coin and flip it.
    coinB_same ~ Dirac(coinA)
    sideB_same ~ flip_coin(coinB_same == coin1);
        
    # The probability of fake coin
    coinB_switchIsFake ~ Dirac(coinB_switch==coin1)
    coinB_sameIsFake ~ Dirac(coinB_same==coin1)

end

model = false_coin()
# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(5), 10_000)
# chns = sample(model, IS(), 10_000)
# chns = sample(model, SMC(), 10_000)

display(chns)
show_var_dist_pct(chns,:coinB_switchIsFake)
show_var_dist_pct(chns,:coinB_sameIsFake)
