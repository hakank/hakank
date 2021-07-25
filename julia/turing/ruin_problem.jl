#=
   Ruin problem.

   This version use a fixed max time (length) of the sequences, here
   maxT.

   Summary Statistics
   parameters      mean       std   naive_se      mcse          ess      rhat   ess_per_sec 
      Symbol   Float64   Float64    Float64   Float64      Float64   Float64       Float64 

      ...
         len    4.6035    4.5269     0.0453    0.0431    9406.7545    1.0000     3560.4673
    amount_t    0.9586    2.0299     0.0203    0.0203   10001.5751    1.0003     3785.6075
      ruin_t    0.7785    0.4153     0.0042    0.0040    9748.3505    1.0000     3689.7618

   Distributions of variable len (num:0)
   1.00000 =>    5004  (0.500400)
   12.00000 =>    2215  (0.221500)
   3.00000 =>    1255  (0.125500)
   5.00000 =>     660  (0.066000)
   7.00000 =>     386  (0.038600)
   9.00000 =>     298  (0.029800)
   11.00000 =>     182  (0.018200)

   Distributions of variable amount_t (num:0)
   0.00000 =>    7785  (0.778500)
   4.00000 =>     801  (0.080100)
   2.00000 =>     658  (0.065800)
   6.00000 =>     527  (0.052700)
   8.00000 =>     195  (0.019500)
   10.00000 =>      32  (0.003200)
   12.00000 =>       2  (0.000200)

   Distributions of variable ruin_t (num:0)
   1.00000 =>    7785  (0.778500)
   0.00000 =>    2215  (0.221500)


.  Cf ~/blog/ruin_problem.blog
      ~/webppl/ruin_problem.wppl

=#
using Turing, StatsPlots
include("jl_utils.jl")

@model function ruin_problem(maxT=12,start=1,win=1,loose=1)
    head = 1
    tail = 2
    coins = [head, tail]

    # Draw a coin at timestep t
    draw = tzeros(maxT)
    for t in 1:maxT
        if t == 1
            draw[t] = head
        else
            draw[t] ~ Categorical([0.5,0.5])
        end
    end
    
    # What is the score at time t?
    # After ruin there's no way back at the game again.
    amount = tzeros(maxT)
    for t in 1:maxT
        if t == 1
            amount[t] = start
        elseif amount[t-1] <= 0
            amount[t] = 0
        elseif draw[t] == head
            amount[t] = amount[t-1] + win
        else
            if amount[t-1] - loose < 0
                amount[t] =  0
            else 
                amount[t] = amount[t-1] - loose
            end
        end
    end

    
    # Is the player busted at time t?
    # This is the first time the player bused
    ruin = tzeros(maxT)
    for t in 1:maxT
        if amount[t] <= 0
            ruin[t] = 1
        else
            ruin[t] = 0
        end
    end
    
    len ~ Dirac(length(filter(i -> i > 0, amount)))
    amount_t ~ Dirac(amount[maxT])
    ruin_t ~ Dirac(ruin[maxT])
    
    # true ~ Dirac(ruin[5]==true)
    # true ~ Dirac(amount[maxT]==8)

        
end

maxT=12
start=1
win=1
loose=1
model = ruin_problem(maxT,start,win,loose)

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
# chns = sample(model, PG(5), 10_000)
chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

display(chns)
# display(plot(chns))

show_var_dist_pct(chns, :len)
show_var_dist_pct(chns, :amount_t)
show_var_dist_pct(chns, :ruin_t)

# chains_params = Turing.MCMCChains.get_sections(chns, :parameters)
# genq = generated_quantities(model, chains_params)
# show_var_dist_pct(genq)

