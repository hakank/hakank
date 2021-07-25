#=
   Same rank problem

   From 
   Gunnar Blom, Lars Holst, Dennis Sandell:
   "Problems and Snapshots from the World of Probability"
   Page 22.
  
   100 students are seating (randomly) on seats numbered 0 to 99. 
   The lectures says: assume that I rank you according to your age. 
   How many will have the same rank number as the seat number?


   Here are three different approaches to this problem which all
   give the same expectation, i.e. E(same_rank) = 1.

   1) Implementing an "all_different" constraint, i.e. 
      ensure that it's an permutation.

      n: 6 all_diff: true
      mean(numSameRank: 0.9653

      Distributions of variable numSameRank (num:0)
      1.00000 =>    3942  (0.394200)
      0.00000 =>    3638  (0.363800)
      2.00000 =>    1796  (0.179600)
      3.00000 =>     377  (0.037700)
      4.00000 =>     247  (0.024700)

      
   2) Generating randomInteger(n) _without_ the all_different
      constraint.

      n: 6 all_diff: false
      mean(numSameRank: 1.0145

      Distributions of variable numSameRank (num:0)
      1.00000 =>    4024  (0.402400)
      0.00000 =>    3312  (0.331200)
      2.00000 =>    1982  (0.198200)
      3.00000 =>     586  (0.058600)
      4.00000 =>      82  (0.008200)
      5.00000 =>      13  (0.001300)
      6.00000 =>       1  (0.000100)

      n: 100 all_diff: false
      mean(numSameRank: 1.0009


   3) Generate n 0/1 numbers with probability of 1 is 1/n.
      Summing this array gives E(same_rank).
      This is the method mentioned in the book, and is implemented
      as same_rank2.

      same_rank2 n: 6
      mean(numSameRank: 1.0064
      Distributions of variable numSameRank (num:0)
      1.00000 =>    4017  (0.401700)
      0.00000 =>    3321  (0.332100)
      2.00000 =>    2035  (0.203500)
      3.00000 =>     539  (0.053900)
      4.00000 =>      80  (0.008000)
      5.00000 =>       8  (0.000800)

      same_rank2 n: 100
      mean(numSameRank: 1.0219

      same_rank2 n: 1000
      mean(numSameRank: 0.9891

 
     I.e. all these has - also - the same expectation: E(same_rank) = (about) 1.


   Cf ~/webppl/same_rank.wppl

=#
using Turing, StatsPlots
include("jl_utils.jl")

@model function same_rank1(n=6,alldiff=true)    
    x ~ filldist(DiscreteUniform(1,n),n)
    if (alldiff) 
        # Ensure permutation (see all_different.jl)
        for i in 1:n
            for j in 1:n
                if i != j
                    true ~ Dirac(x[i] != x[j])
                end
            end
        end
    end
    numSameRank ~ Dirac(sum([x[i] == i ? 1 : 0 for i in 1:n])) 
end


function run_same_rank1(n, all_diff)
    println("\nsame rank1 n: $n all_diff: $all_diff")
    model = same_rank1(n,all_diff)


    # chns = sample(model, Prior(), 10_000)
    # chns = sample(model, MH(), 10_000)
    # chns = sample(model, PG(5), 10_000)
    chns = sample(model, SMC(), 10_000)
    # chns = sample(model, IS(), 10_000)

    # display(chns)
    # display(plot(chns))
    println("mean(numSameRank: ", mean_val(chns,:numSameRank))

    if n < 10
        show_var_dist_pct(chns,:numSameRank)
    end
end


run_same_rank1(6,true)
run_same_rank1(6,false)
run_same_rank1(100,false)

# Generate n 0/1 values with probability 1/n of getting a 1
@model function same_rank2(n=6)    
    x ~ filldist(flip(1/n),n)
    numSameRank ~ Dirac(sum(x))
end


function run_same_rank2(n)
    println("\nsame_rank2 n: $n")
    model = same_rank2(n)

    # chns = sample(model, Prior(), 10_000)
    # chns = sample(model, MH(), 10_000)
    # chns = sample(model, PG(5), 10_000)
    chns = sample(model, SMC(), 10_000)
    # chns = sample(model, IS(), 10_000)

    # display(chns)
    # display(plot(chns))
    println("mean(numSameRank: ", mean_val(chns,:numSameRank))
    if n < 10
        show_var_dist_pct(chns,:numSameRank)
    end
end


run_same_rank2(6)
run_same_rank2(100)
run_same_rank2(1000)
