#=
  Cantor distribution
  from Foundations of Probabilistic Programming,
  page 5
  """
  Finally, the program in Fig. 1.4 generates a real number between [0, 1] whose
  expansion in base 3 does not contain any 1’s. This program is not like the others
  in that it does not halt (nor is it meant to). The program generates a sample from
  a curious and in many respects counterintuitive distribution called the Cantor
  distribution. It cannot be described using discrete probability distributions (i.e.
  finite or countable weighted sums of point masses), although the program only
  uses a discrete fair coin as a source. The Cantor distribution is also an example of
  continuous probability distribution, which assigns probability zero to every element
  of the state space. It is also an example of a so-called singular distribution, since
  it can be shown that the set of all its possible outcomes—that is to say the set of
  all real numbers whose base-3 expansion contains no 1’s—has measure 0 in the
  Lebesgue measure on [0, 1]
  """
=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

# Plain simulation
function cantor_plain()
     x = 0
     d = 1
     i = 0
     while i < 10  # true
         d = d / 3
         x = x + 2*rand(DiscreteUniform(0,1))*d
         println("d:$d x:$x")
         i+=1
     end
end

# cantor_plain()

# Using Turing
@model cantor() = begin
    x ~ Uniform(0,1)
    d ~ Uniform(0,1)
    d = 1
    i = 0
    while i < 100  # true
        d /= 3
        t ~ DiscreteUniform(0,1)
        x += 2*t*d
        # println("d:$d x:$x")
        i += 1
    end
    dfinal ~ Uniform(0,1)
    dfinal = d
end

model = cantor()

num_chns = 4
# chns = sample(model, MH(), MCMCThreads(), 1000, num_chns)
chns = sample(model, MH(), 100)

display(chns)
show_var_dist_pct(chns, :d,20)
show_var_dist_pct(chns, :dfinal,20)
# show_var_dist_pct(chns, :y)
# show_var_dist_pct(chns, :u,20)
# show_var_dist_pct(chns, :v,20)
