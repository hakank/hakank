#=
  From https://probmods.org/chapters/conditioning.html
  (WebPPL):
  """
  This classic Bayesian inference task is a special case of conditioning. Kahneman and Tversky, 
  and Gigerenzer and colleagues, have studied how people make simple judgments like the following:

  The probability of breast cancer is 1% for a woman at 40 who participates in a routine screening. 
  If a woman has breast cancer, the probability is 80% that she will have a positive mammography. 
  If a woman does not have breast cancer, the probability is 9.6% that she will also have a 
  positive mammography. A woman in this age group had a positive mammography in a routine 
  screening. What is the probability that she actually has breast cancer?

  What is your intuition? Many people without training in statistical inference judge the 
  probability to be rather high, typically between 0.7 and 0.9. The correct answer is much lower, 
  less than 0.1, as we can see by running this WebPPL inference:
  """

  p(breastCancer): 0.0737

  According to the WebPPL model:
    breastCancer:
    false : 0.922360248447205
    true : 0.07763975155279507

=#

using Turing
include("jl_utils.jl")

@model function medical_diagnosis2()
    breastCancer ~ flip(0.01)
    positiveMammogram ~ breastCancer ? flip(0.8) : flip(0.096)

    true ~ Dirac(positiveMammogram)

end

model = medical_diagnosis2()

chains = sample(model, MH(), 10_000)
# chains = sample(model, PG(15), 10_000)
# chains = sample(model, IS(), 10_000)
# chains = sample(model, SMC(), 10_000)

display(chains)
