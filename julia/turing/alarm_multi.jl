#=
   https://dtai.cs.kuleuven.be/problog/tutorial/basic/02_bayes.html
   """
   Since the random variables in the Bayesian network are all Boolean, we only need a single literal
   in the head of the rules. We can extend the Bayesian network to have a multi-valued variable by
   indicating the severity of the earthquake. The literal earthquake now has three possible values
   none, mild, heavy instead of previously two (no or yes).
   """

   Cf ~/blog/alarm_multi.blog
      ~/psi/alarm_multi.psi
      ~/webppl/alarm_multi.wppl


    cplint program:

    person(john).
    person(mary).

    0.7::burglary.
    0.01::earthquake(heavy); 0.19::earthquake(mild); 0.8::earthquake(none).

    0.90::alarm :-   burglary, earthquake(heavy).
    0.85::alarm :-   burglary, earthquake(mild).
    0.80::alarm :-   burglary, earthquake(none).
    0.10::alarm :- \+burglary, earthquake(mild).
    0.30::alarm :- \+burglary, earthquake(heavy).

    0.8::calls(X) :- alarm, person(X).
    0.1::calls(X) :- \+alarm, person(X).

    evidence(calls(john),true).
    evidence(calls(mary),true).

    query(burglary).
    query(earthquake(_)).

=#

#=
julia> @time include("alarm_multi.jl")
...

Summary Statistics
  parameters      mean       std   naive_se      mcse          ess      rhat
      Symbol   Float64   Float64    Float64   Float64      Float64   Float64

       alarm    1.0000    0.0000     0.0000    0.0000          NaN       NaN
    burglary    0.9885    0.1067     0.0005    0.0009   13653.3301    1.0001
    calls[1]    1.0000    0.0000     0.0000    0.0000          NaN       NaN
    calls[2]    1.0000    0.0000     0.0000    0.0000          NaN       NaN
  earthquake    2.7675    0.4532     0.0023    0.0037   14275.1136    1.0003

Quantiles
  parameters      2.5%     25.0%     50.0%     75.0%     97.5%
      Symbol   Float64   Float64   Float64   Float64   Float64

       alarm    1.0000    1.0000    1.0000    1.0000    1.0000
    burglary    1.0000    1.0000    1.0000    1.0000    1.0000
    calls[1]    1.0000    1.0000    1.0000    1.0000    1.0000
    calls[2]    1.0000    1.0000    1.0000    1.0000    1.0000
  earthquake    2.0000    3.0000    3.0000    3.0000    3.0000

earthquake status: heavy=1 mild=2 none=3]
Distributions of variable (num:20)
3.00000 =>   31240  (0.781000)
2.00000 =>    8221  (0.205525)
1.00000 =>     539  (0.013475)
  5.424711 seconds (36.39 M allocations: 2.780 GiB, 23.75% gc time)

julia>
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
# chains = sample(model, MH(), MCMCThreads(), 100_000, num_chains)
# chains = sample(model, MH(), MCMCThreads(), 10_000, num_chains)
# chains = sample(model, MH(), 100_000)

# chains = sample(model, PG(15), MCMCThreads(), 1_000, num_chains)

# chains = sample(model, SMC(1000), MCMCThreads(), 10_000, num_chains)
# chains = sample(model, SMC(1000), 10_000)
chains = sample(model, IS(), 10_000)
#
#chains = sample(model, Gibbs(HMC(0.1,5,:a,:b),PG(15,:p)), 10_000)
# chains = sample(model, Gibbs(NUTS(1000,0.65,:a,:b),PG(15,:p)), 10_000)
# chains = sample(model, Gibbs(HMC(0.1,5,:a,:b),SMC(1000,:p)), 10_000) # Nope

display(chains)
# display(plot(chains))

println("earthquake status: heavy=1 mild=2 none=3] ")
show_var_dist_pct(chains,:earthquake,1000)
show_var_dist_pct(chains,:alarm,1000)
show_var_dist_pct(chains,:burglary,1000)
