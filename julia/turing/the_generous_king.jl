#=
   The Generous King

   From 
   Gunnar Blom, Lars Holst, Dennis Sandell:
   "Problems and Snapshots from the World of Probability"
   Page 11f, Problem 1.8 The generous king

   A) This is the first part (run1)
      """
      The king of a country with m inhabitants is generous enough to give
      each citizen a gold coin as a Christmas gift. 
      ...
      All m citizen line up in a random order in front of the Royal Palace.
      The king comes out on to a balcony and tosses k gold coins in the air.
      [The probabilities of head/tails is 0.5.]
      The first person in the queue receives as a gift all coins showing
      tails and returns the other to the king. The kind tosses the remaining
      coins; the second person obtains those which come up tails and returns
      the rest, and so on.
      The procedure stops at the m'th toss or earlier if there are no coins 
      left.
      """

   B) The second part is to select k so that the last m citizen will 
      receive a gold coin with at least probability 1/2.
      However, the model (run2) does not give the same expectation as the
      theoretical in the book and is perhaps not the best approach to
      this problem...


  * First part: 5 citizen and 6 coins

    parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
        Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

           len    3.6846    1.1191     0.0112    0.0150   6288.7212    0.9999     1530.1025
   lastCitizen    0.1984    0.4350     0.0044    0.0054   6080.2801    0.9999     1479.3869


   Distributions of variable len (num:0)
   5.00000 =>    3192  (0.319200)
   3.00000 =>    2825  (0.282500)
   4.00000 =>    2299  (0.229900)
   2.00000 =>    1531  (0.153100)
   1.00000 =>     153  (0.015300)

   Distributions of variable lastCitizen (num:0)
   0.00000 =>    8162  (0.816200)
   1.00000 =>    1697  (0.169700)
   2.00000 =>     136  (0.013600)
   3.00000 =>       5  (0.000500)


   * Second part (for 8 citizen)
     Note: the value of k1 and the theoretical value are too distinct, so this second 
           model is probably not correct. One can note that k is about the expected
           value of the prior of k (Poisson(8*8)  ~ 64) which indicates that something
           is strange...

     Theoretical: 88.722839111673

     Summary Statistics
      parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
          Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

              k1   64.7986    7.8484     0.0785    0.1921   1892.3737    0.9999      434.3295
     lastCitizen    1.1325    0.3725     0.0037    0.0086   1993.1598    1.0018      457.4615


     Distributions of variable lastCitizen (num:0)
     1.00000 =>    8779  (0.877900)
     2.00000 =>    1132  (0.113200)
     3.00000 =>      74  (0.007400)
     4.00000 =>      15  (0.001500)


  Cf ~/webppl/the_generous_king.wppl

=#
using Turing, StatsPlots, Distributions
include("jl_utils.jl")

# First part of the problem
@model function the_generous_king1(m,k)
    
    function toss(a,m,k) 
        if m == 0 || k == 0
            return a
        else 
            numTails = rand(Binomial(k,0.5))
            return toss(push!(a,numTails),m-1,k-numTails)
        end
    end
    
    a = toss([],m,k)

    len ~ Dirac(length(a))
                
    # How many coins did the last citizen in the queue got?
    lastCitizen ~ Dirac(length(a) >= m ? a[end] : 0)

end

println("Part 1: 5 citizen and 6 coins")
model = the_generous_king1(5,6)
# model = the_generous_king1(15,36)

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(5), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

display(chns)
# display(plot(chns))

show_var_dist_pct(chns,:len)
show_var_dist_pct(chns,:lastCitizen)


# Second part:
#

# Theoretical probability
theoreticalProbPart2 = m -> log(2)*2^(m-1)


# Calculate k such that the last m citizen has a 100% chance
# of winning a gold coin.
#
@model function the_generous_king2(m)
    
    # Prior for k.
    # Note: The mean of k is quite sensitive to the prior
    # 
    k1 ~ Poisson(m*m)
    # k1 ~ truncated(Poisson(m*m),0,Inf)
    # k1 ~ DiscreteUniform(1, m*m)    
    # k1 ~ Geometric(1/(m*m))

    # This is the same as in part1.
    function toss(a,m,k) 
        if m == 0 || k == 0
            return a
        else 
            numTails = rand(Binomial(k,0.5))
            return toss(push!(a,numTails),m-1,k-numTails)
        end
    end
    
    a = toss([],m,k1);
        
    # How many coins did the last citizen in the queue got?
    lastCitizen ~ Dirac(length(a) >= m ? a[end] : 0)
   
    # Ensure the probability of 1/2 of last citizen getting a coin
    # if flip() == true
    #     true ~ Dirac(lastCitizen == 1)
    # end
    true ~ Dirac(lastCitizen >= 0.5)
    

end

m = 8
println("\nPart 2: $m citizen")

println("Theoretical: ", theoreticalProbPart2(m))

model = the_generous_king2(m)
# model = the_generous_king1(15,36)

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(5), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

display(chns)
# display(plot(chns))

show_var_dist_pct(chns,:len)
show_var_dist_pct(chns,:lastCitizen)


