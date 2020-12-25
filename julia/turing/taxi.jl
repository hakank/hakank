#=

 From http:  reasoning.eas.asu.edu/lpmln/examples/lpmln2al/taxi.lpmln
  """
  Conside a program where you know that you are going to be Late
  if you don't get a Taxi.

  Since we have put a hard constraint that we cannot be late the probability of being
  Late is going to be 0. The probability of there being a NoTaxi comes out to be 71 %.
  {NoTaxi = T, Late = F} satisfies the second soft rule and the hard constraint. Hence
  the probability for this model comes out to be e\textsuperscript{2}/(
  e\textsuperscript{2} + e\textsuperscript{1}) which comes out to be 73\%
  \begin{lstlisting}
  Late 4.9995e-05
  NoTaxi 0.711979
  \end{lstlisting}
  """

  Note: Since lpmln use weights instead of probability, I don't know how to get the no_taxi as 0.71
  Hmm in the lpmln there's an example where the weights are corresponding to percentages:
     0.5 -> -1.6094
     0.8 -> -0.2231
  which is ln(0.5) and ln(0.8)

  But this don't help us here, since the weights are positive, 1 and 2....

  Cf ~/cplint/taxi.pl
     ~/blog/taxi.blog
     ~/psi/taxi.psi
     ~/webpppl/taxi.wppl

=#

using Turing, StatsPlots, DataFrames

@model function taxi()
    # There is a chance for a taxi to be not available
    no_taxi ~ Bernoulli(0.3)

    # You are going to be late if you don't get a taxi
    late ~ no_taxi ? Bernoulli(0.65) : Bernoulli(0);

    # But you certainly cannot be late no matter what situation.
    # Hence this constraint is encoded as a hard rule
    late == false || begin Turing.@addlogprob! -Inf; return end
end


model = taxi()

num_chains = 4
# chains = sample(model, Prior(), MCMCThreads(), 1000, num_chains)

chains = sample(model, MH(), MCMCThreads(), 10_000, num_chains) # 1.47s
# chains = sample(model, IS(), MCMCThreads(), 10_000, num_chains) # 1.48s


# chains = sample(model, PG(20), MCMCThreads(), 10_000, num_chains) # Too slow!
# chains = sample(model, SMC(), MCMCThreads(), 10_000, num_chains) # 4.1s, don't handle observes!

# chains = sample(model, HMC(0.05,10), MCMCThreads(), 10_000, num_chains) # Nope!
# chains = sample(model, NUTS(), MCMCThreads(), 10_000, num_chains) # Nope!


display(chains)
