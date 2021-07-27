#=
  Port of WebPPL code from https://github.com/probmods/ppaml2016/blob/gh-pages/chapters/5-election.md
  """
  Basic model
  Learning a state-wide preference from poll data
  """

  Clinton: 1 Trump: 2
  Distributions of variable (num:0)
  2.00000 =>    8750  (0.875000)
  1.00000 =>    1250  (0.125000)

  According to the WebPPL model, the probability that Clinton wins (won) is 0.12207
=#

using Turing
include("jl_utils.jl")

@model function sampleElection()
    function trueStatePref()
        pref ~ Beta(1,1)
        counts = Dict(:trump=>304, :clinton=>276)
        total = sum(values(counts))
        c ~ Binomial(total, pref)
        true ~ Dirac(c == counts[:clinton])
        return pref
    end
    clinton = 0
    trump = 1
    pref = trueStatePref()
    turnout = 2400000
    clintonVotes ~ Binomial(turnout,pref)
    trumpVotes ~ Dirac(turnout - clintonVotes)
    winner ~ Dirac(clintonVotes > trumpVotes ? clinton : trump)

    clintonWins ~ Dirac(winner == clinton)
    trumpWins ~ Dirac(winner == trump)    
    return winner
end

model = sampleElection()

# chns = sample(model, MH(), 10_000)
# chns = sample(model, PG(15), 10_000)
# chns = sample(model, IS(), 10_000)
chns = sample(model, SMC(), 10_000)
# chns = sample(model, SMC(), MCMCThreads(), 10_000, 4)

display(chns)

chns_params = Turing.MCMCChains.get_sections(chns, :parameters)
genq = generated_quantities(model, chns_params)
println("Clinton: 0 Trump: 1")
show_var_dist_pct(genq)
