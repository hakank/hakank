#=
   Ruin problem.

   Cf ruin_problem.jl

   Compared to ruin_problem.jl this version uses a recursive approach 
   with concatenated arrays instead.

   The arrays always ends with a 0 (ruin).
   Note: We have to have an maxLen since otherwise the sequence goes to 
   infinity...
  
   Summary Statistics
   parameters      mean       std   naive_se      mcse          ess      rhat   ess_per_sec 
       Symbol   Float64   Float64    Float64   Float64      Float64   Float64       Float64 

          len   11.0188   15.7735     0.1577    0.1505    9765.3342    1.0001    30233.2330
         ruin    0.8925    0.3098     0.0031    0.0028   10015.3143    0.9999    31007.1649


   Distributions of variable len (num:0)
   2.00000 =>    4999  (0.499900)
   4.00000 =>    1320  (0.132000)
   50.00000 =>    1097  (0.109700)
   6.00000 =>     603  (0.060300)
   8.00000 =>     393  (0.039300)
   10.00000 =>     265  (0.026500)
   12.00000 =>     204  (0.020400)
   14.00000 =>     148  (0.014800)
   16.00000 =>     126  (0.012600)
   18.00000 =>     126  (0.012600)
   20.00000 =>      99  (0.009900)
   22.00000 =>      87  (0.008700)
   24.00000 =>      65  (0.006500)
   30.00000 =>      58  (0.005800)
   26.00000 =>      56  (0.005600)
   28.00000 =>      51  (0.005100)
   36.00000 =>      48  (0.004800)
   32.00000 =>      42  (0.004200)
   38.00000 =>      41  (0.004100)
   34.00000 =>      39  (0.003900)
   40.00000 =>      30  (0.003000)
   46.00000 =>      29  (0.002900)
   42.00000 =>      28  (0.002800)
   44.00000 =>      27  (0.002700)
   48.00000 =>      19  (0.001900)

   Distributions of variable ruin (num:0)
   1.00000 =>    8925  (0.892500)
   0.00000 =>    1075  (0.107500)

.  Cf ~/webppl/ruin_problem2.wppl

=#
using Turing, StatsPlots
include("jl_utils.jl")

@model function ruin_problem2(maxLen=50,start=1,win=1,loose=1)
    head = 1
    tail = 2
    coins = [head, tail]

    function draw(arr)
        if length(arr) == 0
            return draw([start])
        elseif arr[end] == 0 || length(arr) >= maxLen
            return arr
        else 
            lastVal = arr[end]
            c = rand(coins)
            if c == head
                return draw(push!(arr,lastVal+win))
            else 
                if lastVal - loose <= 0
                    return push!(arr,0)
                else 
                    return draw(push!(arr,lastVal-loose))
                end
            end
        end
    end

    a = draw([])
    len ~ Dirac(length(a))
    ruin ~ Dirac(a[end] == 0)

    # Use PG(5) when using these. Otherwise Prior() works and is faster.
    # true ~ Dirac(len == 6)
    # true ~ Dirac(a[end] ==8)
        
end

maxLen=50
start=1
win=1
loose=1
model = ruin_problem2(maxLen,start,win,loose)

chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
# chns = sample(model, PG(5), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

display(chns)
# display(plot(chns))

show_var_dist_pct(chns, :len)
show_var_dist_pct(chns, :ruin)

# chains_params = Turing.MCMCChains.get_sections(chns, :parameters)
# genq = generated_quantities(model, chains_params)
# show_var_dist_pct(genq)

