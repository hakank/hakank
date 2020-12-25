#=
   Simpson's paradox

   http://cplint.eu/example/inference/simpson.swinb
   """
   From "Pearl, Judea. Causality. Cambridge university press, 2009"

   Simpson's paradox ... refers to the phenomenon whereby an event C

   increases the probability of E in a given population p and, at the same time, decreases the probability
   of E in every subpopulation of p. In other words, if F and ¬F are two complementary properties describing two
   subpopulations, we might well encounter the inequalities
     P(E|C)>P(E|¬C)
     P(E|C,F)<P(E|¬C,F)
     P(E|C,¬F)<P(E|¬C,¬F)
   ...
  For example, if we associate C (connoting cause) with taking a certain drug, E (connoting effect) with
  recovery, and F with being a female, then ... the drug seems to be harmful to both males and females yet
  beneficial to the population as a whole.
  """

  cf ~/cplint/simplson.pl
     ~/webppl/simpson.wppl

=#

#=
  From the cplint model
/*
female:0.5.

recovery:0.6:- drug,\+ female.
recovery:0.7:- \+ drug,\+ female.

recovery:0.2:- drug,female.
recovery:0.3:- \+ drug,female.

drug:30/40:- \+ female.
drug:10/40:-female.


If we query for the conditional probabilities of recovery given treatment
on the whole population and on the two subpopulations, we get the results
in the tables above:
?- prob(recovery,drug,P).
P = 0.49999999999999994

?- prob(recovery,\+ drug,P).
P = 0.40000000000000013

?- prob(recovery,(drug,female),P).
P = 0.2

?- prob(recovery,(\+drug,female),P).
P = 0.3000000000000001

?- prob(recovery,(drug,\+female),P).
P = 0.6

?- prob(recovery,(\+ drug,\+female),P).
P = 0.7000000000000002


  This model give the same values:
  prob(recovery|drug): 0.5
  prob(recovery|\+ drug): 0.4000000000000001
  prob(recovery|(drug,female)): 0.20000000000000007
  prob(recovery|(\+drug,female)): 0.30000000000000004
  prob(recovery|(drug,\+female)): 0.6000000000000001
  prob(recovery|(\+ drug,\+female)): 0.7


*/

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function simpson(problem)
    female ~ flip(0.5)
    drug ~ female ? flip(10/40.0) :  flip(30/40.0)

    #=
    recovery ~ (drug == true && female==true) ? flip(0.2) :
               (drug == true && female==false) ? flip(0.6) :
               (drug == false && female==true) ? flip(0.3) :
               (drug == false && female==false) ? flip(0.7) : flip(0.0)
    =#
    # Alternative way of doing this...
    recovery ~  if     drug == true  && female==true   flip(0.2)
                elseif drug == true  && female==false  flip(0.6)
                elseif drug == false && female==true   flip(0.3)
                elseif drug == false && female==false  flip(0.7)
                else                                   flip(0.0)
                end

    # 1. prob(recovery|drug): 0.5
    # 2. prob(recovery|\+ drug): 0.4000000000000001
    # 3. prob(recovery|(drug,female)): 0.20000000000000007
    # 4. prob(recovery|(\+drug,female)): 0.30000000000000004
    # 5. prob(recovery|(drug,\+female)): 0.6000000000000001
    # 6. prob(recovery|(\+ drug,\+female)): 0.7

    if problem == 1
        true ~ Dirac(drug == true)
    elseif problem == 2
        true ~ Dirac(drug == false)
    elseif problem == 3
        true ~ Dirac(drug == true && female == true)
    elseif problem == 4
        true ~ Dirac(drug == false && female == true)
    elseif problem == 5
        true ~ Dirac(drug == true && female == false)
    else
        true ~ Dirac(drug == false && female == false)
    end

    return female, recovery, drug
end


function run_simpson(problem)

    model = simpson(problem)
    num_chains = 4

    # chains = sample(model, MH(), MCMCThreads(), 100_000, num_chains)
    chains = sample(model, MH(), MCMCThreads(), 10_000, num_chains)
    # chains = sample(model, MH(), 10_000)

    # chains = sample(model, PG(15), MCMCThreads(), 10_000, num_chains)

    # chains = sample(model, SMC(1000), MCMCThreads(), 10_000, num_chains)
    # chains = sample(model, SMC(1000), 10_000)
    # chains = sample(model, IS(), 10_000)
    #
    display(chains)
    # display(plot(chains))

    # This the distributions of the different combinations of
    #   female, recovery, group
    genq = generated_quantities(model, chains)
    show_var_dist_pct(genq)
end

for p in 1:6
    println("\nproblem $p")
    run_simpson(p)

end
