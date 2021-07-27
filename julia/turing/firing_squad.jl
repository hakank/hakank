#=

  Firing squad example (from Judea Pearl)


  court_order: a court order to shoot a prisoner is given
  captain_signals: captain signal to a and b to shoot the prisoner
  a_shoots: person a shoots at the prisoner
  b_shoots: person b shoots at the prisoner
  death: the prisoner is dead (due to the shooting of a and/or b)

  Cf the ProbLog model firing_squad.pl

  See ~/cplint/firing_squad.pl
      ~/blog/firing_squad.blog
      ~/psi/firing_squad.psi
      ~/webppl/firing_squad.wppl

=#

import Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model firing_squad(court_order=missing,
                    death=missing,
                    captain_signals=missing,
                    a_shoots=missing,
                    b_shoots=missing) = begin

    court_order ~ Bernoulli(0.8)
    captain_signals ~ court_order ? Bernoulli(0.9) : Bernoulli(0.1)
    a_shoots ~ captain_signals ? Bernoulli(0.95) : Bernoulli(0.1)
    b_shoots ~ captain_signals ? Bernoulli(0.95) : Bernoulli(0.1)

    if a_shoots || b_shoots
        death ~ Bernoulli(0.9)
    else
        death ~ Bernoulli(0) # the prisoner don't die if not neither a or b shoots
    end


    # true ~ Dirac(court_order == false)
    # true ~ Dirac(captain_signals == false)
    # true ~ Dirac(a_shoots == true)
    # true ~ Dirac(b_shoots == true)
    true ~ Dirac(death == true)


end

court_order=missing
captain_signals=missing
a_shoots=missing # 'missing' is perhaps a bit confusing here :-)
b_shoots=missing
death=missing # missing
println("""
court_order:$court_order
captain_signals:$captain_signals
a_shoots:$a_shoots
b_shoots:$b_shoots
death:$death
""")
model = firing_squad(court_order,death,captain_signals,a_shoots,b_shoots)

num_chns = 4
# chns = sample(model, Prior(), MCMCThreads(), 1000, num_chns)

chns = sample(model, MH(), MCMCThreads(), 10_000, num_chns) # 1.47s
# chns = sample(model, IS(), MCMCThreads(), 10_000, num_chns) # 1.48s # don't handle Turing.@addlogprob! correct!


# chns = sample(model, PG(20), MCMCThreads(), 10_000, num_chns) # Too slow!
# chns = sample(model, SMC(), MCMCThreads(), 10_000, num_chns) # 4.1s, don't handle observes!

# chns = sample(model, HMC(0.05,10), MCMCThreads(), 10_000, num_chns) # Nope!
# chns = sample(model, NUTS(), MCMCThreads(), 10_000, num_chns) # Nope!


display(chns)
