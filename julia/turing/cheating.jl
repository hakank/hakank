#=
    Cheating model.

    The model is a port of the Cheating model (the first version)
    https://github.com/CamDavidsonPilon/Probabilistic-Programming-and-Bayesian-Methods-for-Hackers/blob/master/Chapter2_MorePyMC/Ch2_MorePyMC_PyMC3.ipynb
    """
    Social data has an additional layer of interest as people are not always honest with responses, which 
    adds a further complication into inference. For example, simply asking individuals "Have you ever cheated 
    on a test?" will surely contain some rate of dishonesty. What you can say for certain is that the true 
    rate is less than your observed rate (assuming individuals lie only about not cheating; I cannot 
    imagine one who would admit "Yes" to cheating when in fact they hadn't cheated).

    To present an elegant solution to circumventing this dishonesty problem, and to demonstrate Bayesian 
    modeling, we first need to introduce the binomial distribution.

    ...

    We will use the binomial distribution to determine the frequency of students cheating during an exam. 
    If we let $N$ be the total number of students who took the exam, and assuming each student is interviewed 
    post-exam (answering without consequence), we will receive integer $X$ "Yes I did cheat" answers. We 
    then find the posterior distribution of $p$, given $N$, some specified prior on $p$, and observed data $X$.

    This is a completely absurd model. No student, even with a free-pass against punishment, would admit to 
    cheating. What we need is a better algorithm to ask students if they had cheated. Ideally the algorithm 
    should encourage individuals to be honest while preserving privacy. The following proposed algorithm 
    is a solution I greatly admire for its ingenuity and effectiveness:

        In the interview process for each student, the student flips a coin, hidden from the interviewer. 
        The student agrees to answer honestly if the coin comes up heads. Otherwise, if the coin comes 
        up tails, the student (secretly) flips the coin again, and answers "Yes, I did cheat" if the 
        coin flip lands heads, and "No, I did not cheat", if the coin flip lands tails. This way, the 
        interviewer does not know if a "Yes" was the result of a guilty plea, or a Heads on a second 
        coin toss. Thus privacy is preserved and the researchers receive honest answers.
    """

    This model:

    Summary Statistics
    parameters      mean       std   naive_se      mcse        ess      rhat   ess_per_sec 
        Symbol   Float64   Float64    Float64   Float64    Float64   Float64       Float64 

            p    0.2273    0.1249     0.0040    0.0087   290.4227    1.0058      121.5666
        obs_prop    0.3559    0.0460     0.0015    0.0023   403.9539    0.9991      169.0891

    Quantiles
    parameters      2.5%     25.0%     50.0%     75.0%     97.5% 
        Symbol   Float64   Float64   Float64   Float64   Float64 

            p    0.0255    0.1364    0.2166    0.3005    0.4916
        obs_prop    0.2700    0.3200    0.3500    0.3800    0.4500
   


   Cf cheating2.jl for a port of a variant of the PyMC3 model (also from the "Hacker" book).
=#

using Turing, StatsPlots, Distributions, StatsBase
using CSV
include("jl_utils.jl")

@model function cheating_model(N,X)
    p ~ Uniform(0,1) # freq cheating

    truths       ~ filldist(Bernoulli(p),N)
    first_flips  ~ filldist(Bernoulli(0.5), N)
    second_flips ~ filldist(Bernoulli(0.5), N)

    val = tzeros(N) # We must defined val to be used as LHS in @.
    @. val = first_flips*truths + (1 - first_flips)*second_flips
    obs_prop ~ Dirac(sum(val) / N) 
    X ~ Binomial(N, obs_prop )

    p_prior ~ Uniform(0,1)

end

N = 100
yes_response = 35
model = cheating_model(N, yes_response)

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(5),  1_000)
# chns = sample(model, PG(5),  MCMCThreads(), 10_000, 4)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

# chns = sample(model, HMC(0.1,6), 1_000)
# chns = sample(model, NUTS(), 1_000)

display(chns[[:p,:obs_prop,:p_prior]])
# display(plot(chns[[:p,:obs_prop,:p_prior]]))

# Another way: 
println("mean(p): $(mean(group(chns, :truths).value.data,dims=1) |> mean)")

