#=
   https://dtai.cs.kuleuven.be/problog/tutorial/basic/02_bayes.html
   """
   Since the random variables in the Bayesian network are all Boolean, we only need a single literal
   in the head of the rules. We can extend the Bayesian network to have a multi-valued variable by
   indicating the severity of the earthquake. The literal earthquake now has three possible values
   none, mild, heavy instead of previously two (no or yes).
   """

   Summary Statistics
   parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
      Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

    burglary    0.9798    0.1407     0.0014    0.0022   4381.1474    1.0005      499.2192
  earthquake    2.7649    0.4544     0.0045    0.0060   4707.8022    1.0000      536.4405
       alarm    0.9898    0.1005     0.0010    0.0015   4266.5933    0.9999      486.1661
    calls[1]    1.0000    0.0000     0.0000    0.0000         NaN       NaN           NaN
    calls[2]    0.8061    0.3954     0.0040    0.0052   4662.4821    1.0001      531.2764


   Distributions of variable earthquake
   none       =>    7782  (0.778200)
   mild       =>    2085  (0.208500)
   heavy      =>     133  (0.013300)

   Distributions of variable alarm (num:0)
   1.00000 =>    9898  (0.989800)
   0.00000 =>     102  (0.010200)

   Distributions of variable burglary (num:0)
   1.00000 =>    9798  (0.979800)
   0.00000 =>     202  (0.020200)


   Cf ~/blog/alarm_multi.blog
      ~/psi/alarm_multi.psi
      ~/webppl/alarm_multi.wppl
=#


using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function alarm_multi()

    heavy = 1
    mild  = 2
    none  = 3
    earthquake_status = [heavy,mild,none]

    john = 1
    mary = 2
    people = [john,mary]
    num_people = length(people)

    burglary ~ flip(0.7)
    earthquake ~ Categorical([0.01,0.19,0.8])

    alarm ~ if     burglary && earthquake  == heavy flip(0.9)
            elseif burglary && earthquake  == mild  flip(0.85)
            elseif burglary && earthquake  == none  flip(0.80)
            elseif !burglary && earthquake == heavy flip(0.30)
            elseif !burglary && earthquake == mild  flip(0.10)
            else flip(0.0)
            end

    calls = tzeros(num_people)
    for p in people
        if alarm
            calls[p] ~ flip(0.8)
        else
            calls[p] ~ flip(0.01)
        end
    end

    true ~ Dirac(calls[john] == true)
    true ~ Dirac(calls[mary] == true)

    # true ~ Dirac(calls[john] == true)
    # true ~ Dirac(calls[mary] == false)

    return earthquake
end

model = alarm_multi()

num_chains = 4

# chains = sample(model, Prior(), 10_000)
# chains = sample(model, MH(), 100_000)

# chains = sample(model, PG(15), 10_000)
chains = sample(model, SMC(), 10_000)
# chains = sample(model, IS(), 10_000)
#

display(chains)
# display(plot(chains))

show_var_dist_pct(chains,:earthquake,["heavy","mild","none"])
show_var_dist_pct(chains,:alarm)
show_var_dist_pct(chains,:burglary)
