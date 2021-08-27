#=
    Port of SPPL model fairness-income-model-2.ipynb

    The SPPL model gives the following exact probabilities:
    female_prior: 0.33066502427854466
    female_given_no_hire: 0.33412533774074804
    p_female_given_no hire / p_female: 1.046470962495416

    However, the range of age in the SPPL model is really strange:
    age has a mean of 178 years and the values of capital_gain is way too large: mean = 42765784.8959
    (the decision model checks for capital_gain in the range of 4000...9000).

    Here's the summary of these variables (from this Turing.jl model where adjust_std=false):
    Summary Statistics
            parameters            mean             std      naive_se          mcse         ess      rhat   ess_per_sec 
                Symbol         Float64         Float64       Float64       Float64     Float64   Float64       Float64 

          capital_gain   43225351.7302   39465143.9277   394651.4393   558730.7594   3705.9663    1.0007      318.3546
                   age        176.7377        115.8681        1.1587        1.9092   3924.8800    1.0005      337.1600

    Quantiles
            parameters           2.5%           25.0%           50.0%           75.0%            97.5% 
                Symbol        Float64         Float64         Float64         Float64          Float64 

          capital_gain   1283446.4592   13635591.8941   30705497.7817   62573297.6974   147598252.8748
                   age        25.3189         84.0627        155.9575        243.3078         450.6392


    I guess that second parameter to Normal in the SPPL model is the variance, but it should be std. 
    After sqrt'ing all the second parameters of capital_gain and age, the quantiles are for:
    * age: between 19..65
    * capital gain: between about 500..21000

    When adjusted for this, the SPPL model shows these probabilities instead:
    female_prior: 0.33076164956248716
    female_given_no_hire: 0.21962121326454123
    p_female_given_no hire / p_female:  -33.60136716120391

    However, I'm not sure that this makes much more sense (the conclusion is opposite of the original).


    This Turing.jl model (for adjust_std = true):
        Summary Statistics
                    parameters        mean         std   naive_se      mcse         ess      rhat   ess_per_sec 
                        Symbol     Float64     Float64    Float64   Float64     Float64   Float64       Float64 

                        sex      1.7770      0.4163     0.0042    0.0079   2077.3376    0.9999      153.4336
               capital_gain   9779.7926   4291.9553    42.9196   90.0528   2065.8181    1.0002      152.5828
                        age     40.0510     11.9233     0.1192    0.2871   1684.2237    1.0000      124.3979
               relationship      2.0305      0.8788     0.0088    0.0203   1820.6243    0.9999      134.4726
                          t      0.0000      0.0000     0.0000    0.0000         NaN       NaN           NaN
                  sex_prior      1.6771      0.4676     0.0047    0.0062   6100.1244    1.0000      450.5594
           sex_female_prior      0.3229      0.4676     0.0047    0.0062   6100.1244    1.0000      450.5594
       sex_female_posterior      0.2230      0.4163     0.0042    0.0079   2077.3376    0.9999      153.4336

        Quantiles
                    parameters        2.5%       25.0%       50.0%        75.0%        97.5% 
                        Symbol     Float64     Float64     Float64      Float64      Float64 

                       sex      1.0000      2.0000      2.0000       2.0000       2.0000
              capital_gain   4998.2887   6494.5706   8685.1889   11816.3482   20384.3524
                       age     19.9492     30.9827     39.3438      48.0846      64.6989
              relationship      0.0000      2.0000      2.0000       3.0000       3.0000
                         t      0.0000      0.0000      0.0000       0.0000       0.0000
                 sex_prior      1.0000      1.0000      2.0000       2.0000       2.0000
          sex_female_prior      0.0000      0.0000      0.0000       1.0000       1.0000
      sex_female_posterior      0.0000      0.0000      0.0000       0.0000       1.0000

        Distributions of variable sex
        male       =>    7770  (0.777000)
        female     =>    2230  (0.223000)

        100*(sex_female_posterior) / sex_female_prior - 1): -30.93837101269743


=#

using Turing
include("jl_utils.jl")

# adjust_std == true: use sqrt'ed value of Normal 
@model function fairness_income_model(adjust_std=true)
    female = 1
    male   = 2
    sex ~ Categorical([0.3307, 0.6693]) # [female, male]
    if sex == female
        if adjust_std
            capital_gain ~ Normal(568.4105, sqrt(24248365.5428))
        else 
            capital_gain ~ Normal(568.4105, 24248365.5428)
        end
        if capital_gain < 7298.0000
            if adjust_std
                age ~ truncated(Normal(38.4208, sqrt(184.9151)),18.001,Inf)
            else 
                age ~ truncated(Normal(38.4208, 184.9151),18.001,Inf)
            end
            relationship ~ DiscreteNonParametric([0,1,2,3,4,5],[0.0491, 0.1556, 0.4012,0.2589, 0.0294, 0.1058] )
        else
            if adjust_std
                age ~ truncated(Normal(38.8125, sqrt(193.4918)),18.001,Inf)
            else 
                age ~ truncated(Normal(38.8125, 193.4918),18.001,Inf)
            end
            relationship ~ DiscreteNonParametric([0,1,2,3,4,5],[0.0416, 0.1667, 0.4583,0.2292, 0.0166, 0.0876])
        end
    else
        if adjust_std
            capital_gain ~ Normal(1329.3700, sqrt(69327473.1006))
        else 
            capital_gain ~ Normal(1329.3700, 69327473.1006)
        end
        if capital_gain < 5178.0000
            if adjust_std
                age ~ truncated(Normal(38.6361, sqrt(187.2435)),18.001,Inf)
            else 
                age ~ truncated(Normal(38.6361, 187.2435),18.001,Inf)
            end
            relationship ~ DiscreteNonParametric([0,1,2,3,4,5],[0.0497, 0.1545, 0.4021,0.2590, 0.0294, 0.1053])
        else
            if adjust_std
                age ~ truncated(Normal(38.2668, sqrt(187.2747)),18.001,Inf)
            else 
                age ~ truncated(Normal(38.2668, 187.2747),18.001,Inf)
            end
            relationship ~ DiscreteNonParametric([0,1,2,3,4,5],[0.0417, 0.1624, 0.3976,0.2606, 0.0356, 0.1021])
        end 
    end
    
    # true ~ Dirac(age > 18) # Used truncated(.) instead
    
    # Decision model.
    if relationship == 1
        if capital_gain < 5095.5
            t ~ Dirac(1)
        else
            t ~ Dirac(0)
        end
    elseif relationship == 2
        if capital_gain < 4718.5
            t ~ Dirac(1)
        else
            t ~ Dirac(0)
        end
    elseif relationship == 3
        if capital_gain < 5095.5
            t ~ Dirac(1)
        else
            t ~ Dirac(0)
        end
    elseif relationship == 4
        if capital_gain < 8296
            t ~ Dirac(1)
        else
            t ~ Dirac(0)
        end
    elseif relationship == 5
        t ~ Dirac(1)
    else
        if capital_gain < 4668.5
            t ~ Dirac(1)
        else
            t ~ Dirac(0)
        end 
    end

    # Observation:
    true ~ Dirac(t == 0)

    # Prior and posterior
    sex_prior ~ Categorical([0.3307, 0.6693])
    sex_female_prior ~ Dirac(sex_prior == female)
    sex_female_posterior ~ Dirac(sex == female)
    
end

# adjust_std = false # Original version of the SPPL model 
adjust_std = true # Adjust for Normal by sqrt'ing the scale parameter
model = fairness_income_model(adjust_std)


# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 1_000)
chns = sample(model, PG(5), 10_000)
# chns = sample(model, SMC(10), 1_000)
# chns = sample(model, IS(), 1_000)
              
display(chns)

show_var_dist_pct(chns,:sex,["female","male"])

sex_female_posterior = mean(chns[:sex_female_posterior])
sex_female_prior = mean(chns[:sex_female_prior])
println("sex_female_prior:$sex_female_prior sex_female_posterior:$sex_female_posterior")
println("100*(sex_female_posterior) / sex_female_prior - 1): $(100*(sex_female_posterior / sex_female_prior - 1))")

# gender = group(chns,:is_male).value.data |> make_hash
# println("gender: $gender")




# println("gender: 1 female, 2: male:")
# chains_params = Turing.MCMCChains.get_sections(chns, :parameters)
# genq = generated_quantities(model, chains_params)
# show_var_dist_pct(genq)


# hhh = genq[:,1] |> make_hash
# hire_male = get(hhh,[1,1],0) / gender[1]
# hire_female = get(hhh,[0,1],0) / gender[0]
# println("hire | male: $hire_male")
# println("hire | female: $hire_female")
# println("female / male: $(hire_male > 0 ? hire_female / hire_male : 0)")
