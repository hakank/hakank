#=
  Regression fallacy.

  From Gelman et.al "Regression and other stories", page 88f
  """
  Regression to the mean can be confusing and it has led people to mistakenly attribute causality.
  To see how this can happen, we move from heights of parents and children to the mathematically
  equivalent scenario of students who take two tests.
  [...]
  Rather than using real data, we have simulated exam scores using the following simple process
  representing signal and noise:6
  1. Each student is assumed to have a true ability drawn from a distribution with 
     mean 50 and standard deviation 10.
  2. Each student’s score on the midterm exam is the sum of two components: the student’s true ability,
     and a random component with mean 0 and standard deviation 10, reflecting that performance on
     any given test will be unpredictable: a midterm exam is far from a perfect measuring instrument
  3. Likewise, each student’s score on the final exam is his or her true ability, plus another, independent,
     random component.
  """


  1) We simulate with rand and Distributions.jl and then fit with lm(),
     were we see that it's a "regression to the mean", i.e. slope < 1.

    score_final ~ 1 + score_midterm

    Coefficients:
    ───────────────────────────────────────────────────────────────────────────
                    Coef.  Std. Error      t  Pr(>|t|)  Lower 95%  Upper 95%
    ───────────────────────────────────────────────────────────────────────────
    (Intercept)    22.1621     1.44193    15.37    <1e-47  19.3326    24.9917
    score_midterm   0.565276   0.0278395  20.30    <1e-76   0.510646   0.619907
    ───────────────────────────────────────────────────────────────────────────


  2) Then we try to the recover the parameters with Turing.
      mu_ability = 50
      sigma_ability = 10

      mu_score = 0
      sigma_score = 10

    Summary Statistics
        parameters      mean       std   naive_se      mcse        ess      rhat   ess_per_sec 
            Symbol   Float64   Float64    Float64   Float64    Float64   Float64       Float64 

        mu_ability   49.7729    0.3167     0.0100    0.0090   948.7999    0.9997      129.3877
     sigma_ability    9.7822    0.2086     0.0066    0.0063   670.0839    1.0013       91.3792
          mu_score    0.1490    0.1154     0.0036    0.0039   625.4673    0.9999       85.2949
       sigma_score   10.1247    0.1610     0.0051    0.0045   683.0471    0.9991       93.1470

    Quantiles
        parameters      2.5%     25.0%     50.0%     75.0%     97.5% 
            Symbol   Float64   Float64   Float64   Float64   Float64 

        mu_ability   49.1503   49.5634   49.7748   49.9773   50.3799
     sigma_ability    9.3879    9.6409    9.7798    9.9150   10.2067
          mu_score    0.0040    0.0605    0.1272    0.2114    0.4748
       sigma_score    9.8106   10.0148   10.1218   10.2331   10.4427


=#
using Turing, StatsPlots, DataFrames, Distributions, Random
using GLM
include("jl_utils.jl")


#
# It's much faster to use random and Distributions to generate
# the data (instead of using Turing for this).
#
# It's almost the same model, except for the ~'s. 
#
function regression_fallacy_sim(n=1000)
    
    mu_ability = 50
    sigma_ability = 10

    mu_score = 0
    sigma_score = 10

    ability = rand(Normal(mu_ability,sigma_ability), n)
    score_midterm = Vector{Real}(undef, n) 
    score_final = Vector{Real}(undef, n) 
    # for i in 1:n 
    #     score_midterm[i] = rand(Normal(ability[i]+mu_score,sigma_score))
    #     score_final[i] = rand(Normal(ability[i]+mu_score,sigma_score))
    # end
    # Neater:
    @. score_midterm = rand(Normal(ability+mu_score,sigma_score))
    @. score_final = rand(Normal(ability+mu_score,sigma_score)) 
    
    return ability, score_midterm, score_final
end


n = 1000

ability, score_midterm, score_final = regression_fallacy_sim(n)

#
# Fit the data
#

df = DataFrame(ability=ability,score_midterm=score_midterm, score_final=score_final)
fit = lm(@formula(score_final ~ score_midterm), df)
display(fit)
println()

#=
pred = predict(fit)
# display(plot(xlabel="midterm", ylabel="final", legend=:bottomright))
display(plot(df.score_midterm, df.score_final, label="data", seriestype=:scatter,ms=1,xlabel="midterm", ylabel="final", legend=:bottomright))
display(plot!(score_midterm, pred, label="model", linewidth=3))
=#

#=
# What are the mean for those students that got 
# final < / > midterm scores
mean_midterm = mean(score_midterm)
mean_final = mean(score_final)

ix_gt = score_final .> score_midterm
ix_lt = score_final .< score_midterm
@show mean(score_final[ix_gt])
@show mean(score_final[ix_lt])
@show mean(score_final[ix_gt] .- score_midterm[ix_gt])
@show mean(score_final[ix_lt] .- score_midterm[ix_lt])
=#


#
# Now we try to recover the values.
#
# Note: Generating the data using this Turing model takes too long time.
# Hence the dedicated simulation model above.
# 
@model function regression_fallacy(ability, score_midterm, score_final)
    n = length(ability)
    # Use uninformed priors so we are not accused of cheating. :-)
    mu_ability ~ Uniform(1,1000)
    sigma_ability ~ Uniform(1,50)

    mu_score ~ Uniform(0,100)
    sigma_score ~ Uniform(1,50)

    ability .~ Normal(mu_ability,sigma_ability)
    @. score_midterm ~ Normal(ability+mu_score,sigma_score)
    @. score_final   ~ Normal(ability+mu_score,sigma_score)
end


model = regression_fallacy(ability,score_midterm,score_final)

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 1_000)
# chns = sample(model, PG(20), 1_000)
# chns = sample(model, IS(), 1_000)
# chns = sample(model, SMC(1000), 1_000)

# chns = sample(model, NUTS(1000,0.65), 1_000)
chns = sample(model, NUTS(), 1_000)
# chns = sample(model, HMC(0.1,5), 1_000)

display(chns)
