#=
 Jaywalking model in WebPPL.

  This model simulates how many people (and why) that will jaywalk
  of a crossing line with red light, given that they have some
  prospensity of jaywalk if some <limit> number of people already
  has jaywalked .

  The mode:
     1) Assign some number n (might be fixed)
     2) Foreach agent: generate a random tendency to jaywalk.
     3) For each time slot t=0..n-1
          foreach a=agent (0..n-1): 
            if there are more than or equal number of <limit> number 
            of people that has already jaywalked, then a will also
            jaywalk.

  The number of people that - eventually - jaywalks depends very
  much on the distribution of the limits and espespecially 
  the probability of the 0 limits jaywalkers, i.e. those that always
  jaywalk.

  Here is an some example where n_max = 15 (
 - endTime: number of timeslots
 - n=number of people
 - t0: number of people that initially jaywalk
 - tn: number of people that eventually will jaywalk 

  Mean n:  8.5141
  Distributions of variable n (num:0)
  3.00000 =>     762  (0.076200)
  13.00000 =>     740  (0.074000)
  15.00000 =>     728  (0.072800)
  8.00000 =>     721  (0.072100)
  5.00000 =>     720  (0.072000)
  4.00000 =>     717  (0.071700)
  9.00000 =>     715  (0.071500)
  11.00000 =>     714  (0.071400)
  14.00000 =>     714  (0.071400)
  12.00000 =>     712  (0.071200)
  6.00000 =>     702  (0.070200)
  10.00000 =>     696  (0.069600)
  2.00000 =>     687  (0.068700)
  7.00000 =>     672  (0.067200)

  Mean t0:  0.5049
  Distributions of variable t0 (num:0)
  0.00000 =>    5896  (0.589600)
  1.00000 =>    3272  (0.327200)
  2.00000 =>     729  (0.072900)
  3.00000 =>      95  (0.009500)
  4.00000 =>       6  (0.000600)
  5.00000 =>       2  (0.000200)

  Mean tn:  6.3268
  Distributions of variable tn (num:0)
  0.00000 =>    5896  (0.589600)
  15.00000 =>     661  (0.066100)
  16.00000 =>     577  (0.057700)
  17.00000 =>     512  (0.051200)
  14.00000 =>     491  (0.049100)
  13.00000 =>     403  (0.040300)
  18.00000 =>     393  (0.039300)
  19.00000 =>     257  (0.025700)
  12.00000 =>     254  (0.025400)
  11.00000 =>     176  (0.017600)
  20.00000 =>     133  (0.013300)
  10.00000 =>      74  (0.007400)
  21.00000 =>      64  (0.006400)
  22.00000 =>      34  (0.003400)
  9.00000 =>      28  (0.002800)
  8.00000 =>      21  (0.002100)
  23.00000 =>      15  (0.001500)
  7.00000 =>       5  (0.000500)
  6.00000 =>       3  (0.000300)
  24.00000 =>       2  (0.000200)
  5.00000 =>       1  (0.000100)


  Cf ~/webppl/jaywalking.wppl

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function jaywalking(n_max=15)
    endTime = n_max*2
    
    # n = 15; # number of people
    n ~ DiscreteUniform(2,n_max) # number of people
  
    # The limits for jaywalking, i.e.
    # how many persons that have to jaywalk
    # before this agent jaywlk.
    limit ~ filldist(DiscreteUniform(0,n*2-1),endTime)
    
    t0 ~ Dirac(sum([limit[i] == 0 ? 1 : 0 for i in 1:n]))
    
    # The number of people that will jaywalk at time i
    t = tzeros(endTime)
    for i in 1:endTime
        t[i] ~ Dirac(sum([ t0 > 0 && limit[j] <= i ? 1 : 0 for j in 1:endTime]))
    end
    tn ~ Dirac(t0 > 0  ? t[n-1] : 0)    
end

model = jaywalking()

chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 100_000)
# chns = sample(model, PG(5), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

# display(chns)
# display(plot(chns))

println("Mean n:  ", mean_val(chns, :n))
show_var_dist_pct(chns, :n)
println("Mean t0:  ", mean_val(chns, :t0))
show_var_dist_pct(chns, :t0)
println("Mean tn:  ", mean_val(chns, :tn))
show_var_dist_pct(chns, :tn)
