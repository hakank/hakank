#=
   https://dtai.cs.kuleuven.be/problog/tutorial/basic/10_inhibitioneffects.html
   """
   Example 2: Social Network

   An infectious disease spreads through a population as follows: when- ever two people are in regular
   contact with each other and one is infected, there is a probability of 0.6 of the infection spreading
   also to the other person. Given a set of initially infected people and a graph of connections between
   individuals in the population, the goal is to predict the spread of the disease.
   """

   ProbLog model
   """
   person(a).
   person(b).

   0.1::initialInf(X) :- person(X).
   0.1::contact(X,Y) :- person(X), person(Y).

   inf(X)      :- initialInf(X).
   0.6::inf(X) :- contact(X, Y), inf(Y).

   query(inf(_)). % inf(a): 0.1054 inf(b): 0.1054
   """

   Cf ~/webppl/inhibition_infection.wppl

=#


using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function inhibition_infection()
    n = 3
    people = 1:n

    initialInf = tzeros(n)
    for p in 1:n
        initialInf[p] ~ flip(0.1)
    end

    contact = tzeros(n,n)
    for x in 1:n, y in 1:n
        if x == y
            contact[x,y] ~ flip(0.0)
        else
            contact[x,y] ~ flip(0.1)
        end
    end

    a_inf = tzeros(n)
    for p in 1:n
        if initialInf[p]==1
            a_inf[p] ~ flip(1.0)
        else
            check = any(q -> q != p && contact[p,q]==1 && a_inf[q]==1,people)
            if check==1
                a_inf[p] ~ flip(0.6)
            else
                a_inf[p] ~ flip(0.0)
            end
        end
    end

    return a_inf[1] # a_inf

end

model = inhibition_infection()

num_chains = 4

# chains = sample(model, Prior(), 10_000)

# chains = sample(model, MH(), MCMCThreads(), 10_000, num_chains)
# chains = sample(model, MH(), 10_000)

# chains = sample(model, PG(15), MCMCThreads(), 1_000, num_chains)
# chains = sample(model, PG(15), 1_000)

# chains = sample(model, SMC(1000), MCMCThreads(), 10_000, num_chains)
chains = sample(model, SMC(1000), 10_000)

# chains = sample(model, IS(), 1_000)

#
display(chains)
show_var_dist_pct(chains,:len,1000)


chains_params = Turing.MCMCChains.get_sections(chns, :parameters)
genq = generated_quantities(model, chains_params)
show_var_dist_pct(genq)
