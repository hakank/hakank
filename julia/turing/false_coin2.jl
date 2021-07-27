#=
  False coin in Turing.jl

  From Peter Winkler
  Celebration of Mind (Gathering For Gardner):
  "CoM Apr 2021 - Drawing from Urns - Peter Winkler"
  https://www.youtube.com/watch?v=A0GxvYPTKDk&list=PL5_D5QQxvWKVtX4Vklvhsf5KTYZBLDSwO&index=3

  @3:43
  """
  You have two quarters.
  Both are sitting on a table heads up.
  One of them is an ordinary quarter, but the
  other has a head on the other side as well.

  You get two flips to help you guess which is which.

  Should you flip each coin once or one coin twice.
  """

  According to this model it's better to flip the same coin twice
  than once for each coin. But that's under the condition that
  all the flips shows head. If either flip shows tail then it's
  settled that it's the ordinary coin.

  """
  Distributions of variable coinA (num:0)
  1.00000 =>    8066  (0.806600)
  2.00000 =>    1934  (0.193400)

  Distributions of variable sideA (num:0)
  1.00000 =>   10000  (1.000000)

  Distributions of variable coinB (num:0)
  2.00000 =>    8066  (0.806600)
  1.00000 =>    1934  (0.193400)

  Distributions of variable sideB (num:0)
  1.00000 =>   10000  (1.000000)

  Distributions of variable coinSameIsFake (num:0)
  1.00000 =>    8066  (0.806600)
  0.00000 =>    1934  (0.193400)

  Distributions of variable coinBothIsFake (num:0)
  0.00000 =>    8066  (0.806600)
  1.00000 =>    1934  (0.193400)

  """


  (See false_coin.jl for an easier variant of the problem.) 


=#

using Turing
include("jl_utils.jl")

@model function false_coin2()
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
    
    #  The setup: both coins are head (no real flipping)
    coinA ~ Categorical([0.5,0.5])
    sideA ~ flip_coin(coinA == coin1)
    true ~ Dirac(sideA == head)

    coinB ~ Dirac(coinA == coin1 ? coin2 : coin1)
    sideB ~ flip_coin(coinB == coin1);
    true ~ Dirac(sideB == head)

    #  condition(coinA == coin1 || coinB == coin1)

    #  Flip same coin
    coinSame_1 ~ Dirac(coinA)
    coinSame_1_flip ~ flip_coin(coinSame_1 == coin1);
    coinSame_2 ~ Dirac(coinSame_1)
    coinSame_2_flip ~ flip_coin(coinSame_2 == coin1);
    true ~ Dirac(coinSame_1_flip == head)
    true ~ Dirac(coinSame_2_flip == head)
    
    #  Flip both coins
    coinBoth_1 ~ Dirac(coinA)
    coinBoth_1_flip ~ flip_coin(coinBoth_1 == coin1);
    coinBoth_2 ~ Dirac(coinB)
    coinBoth_2_flip ~ flip_coin(coinBoth_2 == coin1);
    true ~ Dirac(coinBoth_1_flip == head)
    true ~ Dirac(coinBoth_2_flip == head)
    
    coinSameIsFake ~ Dirac(coinA ==coin1)
    coinBothIsFake ~ Dirac(coinB ==coin1)
end

model = false_coin2()
# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(5), 10_000)
# chns = sample(model, IS(), 10_000)
# chns = sample(model, SMC(), 10_000)

display(chns)
show_var_dist_pct(chns,:coinA)
show_var_dist_pct(chns,:sideA)
show_var_dist_pct(chns,:coinB)
show_var_dist_pct(chns,:sideB)

show_var_dist_pct(chns,:coinSameIsFake)
show_var_dist_pct(chns,:coinBothIsFake)
