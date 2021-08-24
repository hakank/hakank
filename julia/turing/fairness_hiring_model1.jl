#=
    Port of SPPL model
    https://github.com/probcomp/sppl/blob/master/examples/fairness-hiring-model-1.ipynb

    The SPPL model gives the following exact probabilities:
        p_hire_given_minority:0.007131626828051439
        p_hire_given_majority:0.01945024229170891
        min/maj: 0.3666600508668959


    This Turing.jl model:
    Summary Statistics
            parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
                Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

           ethnicity    1.8531    0.3540     0.0035    0.0046   6492.4337    1.0000     1198.5294
    years_experience    7.5026    1.9193     0.0192    0.0230   6416.1713    0.9999     1184.4510
        college_rank   20.6405    7.0939     0.0709    0.0827   6775.2100    1.0000     1250.7310
                hire    0.0199    0.1397     0.0014    0.0018   7133.8377    1.0002     1316.9351

    Quantiles
            parameters      2.5%     25.0%     50.0%     75.0%     97.5% 
                Symbol   Float64   Float64   Float64   Float64   Float64 

           ethnicity    1.0000    2.0000    2.0000    2.0000    2.0000
    years_experience    4.0000    6.0000    8.0000    9.0000   11.0000
        college_rank    5.9163   16.9625   20.4955   24.4271   35.7348
                hire    0.0000    0.0000    0.0000    0.0000    0.0000

    Distributions of variable ethnicity
    majority   =>    8531  (0.853100)
    minority   =>    1469  (0.146900)
    ethnicity: 1 minority, 2: majority:
    Distributions of variable (num:0)
    [2, 0]  =>      8345 (0.8345)
    [1, 0]  =>      1456 (0.1456)
    [2, 1]  =>      186 (0.0186)
    [1, 1]  =>      13 (0.0013)
    hire | minority: 0.008849557522123894
    hire | majority: 0.021802836713163756
    min / maj: 0.4058901893614997
 

=#

using Turing
include("jl_utils.jl")

#=
SPPL model:
# Prior on ethnicity, experience, and college rank.
ethnicity ~= choice({'minority': .15, 'majority': .85})
years_experience ~= binom(n=15, p=.5)
if (ethnicity == 'minority'):
    college_rank ~= dlaplace(loc=25, a=1/5)
else:
    college_rank ~= dlaplace(loc=20, a=1/5)

# Top 50 colleges and at most 20 years of experience.
condition((0 <= college_rank) <= 50)
condition((0 <= years_experience) <= 20)

# Hiring decision.
if college_rank <= 5:
    hire ~= atomic(loc=1)
else:
    switch (years_experience) cases (years in range(0, 20)):
        if ((years - 5) > college_rank):
            hire ~= atomic(loc=1)
        else:
            hire ~= atomic(loc=0)
=#
@model function fairness_model1()
    minority = 1
    majority = 2

    ethnicity ~ Categorical([0.15,0.85])
    years_experience ~ Binomial(15,0.5)
    college_rank ~ ethnicity == minority ? Laplace(25,5) : Laplace(20,5)
    
    # Top 50 colleges and at most 20 years of experience.
    true ~ Dirac(college_rank <= 50)
    true ~ Dirac(years_experience <= 20)    

    # Hiring decision.
    if college_rank <= 5
        hire ~ Dirac(1)
    else
        # hakank: I'm not sure that I understand this: Why years_experience related to college rank?
        # Some seconds later: Ah, that's from the underlying decision tree!
        if ((years_experience - 5) > college_rank)
            hire ~ Dirac(1)
        else
            hire ~ Dirac(0)
        end 
    end 

    return [ethnicity, hire]

end

model = fairness_model1()


# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 1_000)
chns = sample(model, PG(5), 10_000)
# chns = sample(model, SMC(10), 1_000)
# chns = sample(model, IS(), 1_000)
              
display(chns)

show_var_dist_pct(chns,:ethnicity,["minority","majority"])
eth = group(chns,:ethnicity).value.data |> make_hash

println("ethnicity: 1 minority, 2: majority:")
chains_params = Turing.MCMCChains.get_sections(chns, :parameters)
genq = generated_quantities(model, chains_params)
show_var_dist_pct(genq)


hhh = genq[:,1] |> make_hash
hire_min = get(hhh,[1,1],0) / eth[1]
hire_maj = get(hhh,[2,1],0) / eth[2]
println("hire | minority: $hire_min")
println("hire | majority: $hire_maj")
println("min / maj: $(hire_maj > 0 ? hire_min / hire_maj : 0)")
