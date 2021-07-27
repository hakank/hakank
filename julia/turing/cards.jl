#=
  Second assignment of the CPLINT course:
  https://edu.swi-prolog.org/mod/assign/view.php?id=243
  """
  http://cplint.eu/p/cards.swinb
  Cards

  Suppose you have two decks of poker cards (52 cards, 4 suits, 13 ranks: A, K, Q, J, 10, 9, 8, 7, 6, 5, 4, 3, 2).

  Suppose you draw a card from the first deck and one from the second.

  Write a program to compute the probability that in this way you obtain one pair (two cards of the same rank).

  Can you do it with a single probabilistic clause?

  Add code to compute the probability that you draw at least one ace.
  """

  Note that the two cards are from two different deck of cards.

  Compare woth ~/blog/cards.blog
      ~/psi/cards.blog
      ~/webppl/cards.wppl

    Result:
    Summary Statistics
                   parameters      mean       std   naive_se      mcse         ess      rhat
                       Symbol   Float64   Float64    Float64   Float64     Float64   Float64

  a_king_or_a_queen_in_spades    0.0684    0.2524     0.0009    0.0060   1274.0013    1.0011
              at_least_an_ace    0.1650    0.3712     0.0013    0.0088   1327.6489    1.0020
              exactly_one_ace    0.1603    0.3669     0.0013    0.0087   1311.2268    1.0024
                         pair    0.0736    0.2612     0.0009    0.0061   1442.6291    1.0015
                      rank[1]    6.9292    3.7412     0.0132    0.0890   1256.8658    1.0015
                      rank[2]    7.0677    3.7526     0.0133    0.0885   1412.4389    1.0023
                      suit[1]    2.5018    1.1335     0.0040    0.0270   1184.4944    1.0059
                      suit[2]    2.4984    1.1315     0.0040    0.0275   1208.0383    1.0030
                     two_aces    0.0055    0.0738     0.0003    0.0017   1298.4260    1.0009

    ....

    2.485510 seconds (36.82 M allocations: 1.911 GiB, 20.05% gc time)

    According to the WebPPL model (cards.wppl) the _exact_ results are:
    'pair', 0.07692307692307696
    'at_least_an_ace', 0.14792899408284022
    'exactly_one_ace', 0.1420118343195266
    'two_aces', 0.005917159763313609
    'a_kind_or_a_queen_in_spades', 0.07544378698224857

=#

import Turing #, StatsPlots, DataFrames

@model function cards()
    # TODO: Check if it would work with @enums
    # @enum Suits hearts spades clubs diamonds
    # @enume Ranks ace king queen jack v10 v9 v8 v7 v6 v5 v4 v3 v2

    num_suits = 4
    hearts, spades, clubs, diamonds = 1:num_suits

    num_ranks = 13
    ace, king, queen, jack, v10, v9, v8, v7, v6, v5, v4, v3, v2 = 1:num_ranks

    num_cards = 2

    rank = tzeros(num_cards) # TArray{Int}(undef,num_cards)
    suit = tzeros(num_suits) # TArray{Int}(undef,num_cards)
    for c in 1:num_cards
        suit[c] ~ DiscreteUniform(1,num_suits)
        rank[c] ~ DiscreteUniform(1,num_ranks)
    end

    # Probability of a pair
    # It would be less complex if Turing has an @observe macro
    pair ~ Bernoulli(0.5)
    if pair
       true ~ Dirac(rank[1] == rank[2])
    else
       true ~ Dirac(rank[1] != rank[2])
    end

    # At least an ace
    at_least_an_ace ~ Bernoulli(0.5)
    if at_least_an_ace
        true ~ Dirac(rank[1] == ace || rank[2] == ace)
    else
        true ~ Dirac(rank[1] != ace && rank[2] != ace)
    end

    # Exactly one ace
    exactly_one_ace ~ Bernoulli(0.5)
    if exactly_one_ace
        true ~ Dirac((rank[1] == ace) + (rank[2] == ace) == 1)
    else
        true ~ Dirac((rank[1] == ace) + (rank[2] == ace) != 1)
    end

    # Two aces
    two_aces ~ Bernoulli(0.5)
    if two_aces
        true ~ Dirac((rank[1] == ace) + (rank[2] == ace) == 2)
    else
        true ~ Dirac((rank[1] == ace) + (rank[2] == ace) != 2)
    end

    # A King or a Queen in spades
    a_king_or_a_queen_in_spades ~ Bernoulli(0.5)
    if a_king_or_a_queen_in_spades
        true ~ Dirac(  (
             ((rank[1] == king) || (rank[1] == queen)) && suit[1] == spades
            ) ||
            (
             ((rank[2] == king) || (rank[2] == queen)) && suit[2] == spades
            )
        )
    else
        true ~ Dirac(!((((rank[1] == king) || (rank[1] == queen)) && suit[1] == spades) ||
         (((rank[2] == king) || (rank[2] == queen)) && suit[2] == spades) ))
    end

end

model = cards()

num_chns = 4
# chns = sample(model, Prior(), MCMCThreads(), 1000, num_chns)
chns = sample(model, MH(), MCMCThreads(), 10_000, num_chns) # 1.47s

display(chns)
