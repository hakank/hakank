#=
    Port of SPPL model
    https://github.com/probcomp/sppl/blob/master/examples/fairness-hiring-model-2.ipynb


    The SPPL model gives the following exact probabilities:
    p(hire): 0.911360805292618
    p(hire|male):0.9777674365554804
    p(hire|female)f:0.8449541740297555
    p(hire|female)/p(hire|male): 0.8641668176293485


    This model
        Summary Statistics
            parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
                Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

              is_male    0.4868    0.4999     0.0050    0.0060   6705.1411    0.9999     1358.1408
         college_rank   25.0421    9.8914     0.0989    0.1296   6305.3246    1.0000     1277.1571
            years_exp   12.4851    5.5975     0.0560    0.0745   6716.2764    0.9999     1360.3963
                 hire    0.9107    0.2852     0.0029    0.0036   6431.6033    0.9999     1302.7351

        Quantiles
            parameters      2.5%     25.0%     50.0%     75.0%     97.5% 
                Symbol   Float64   Float64   Float64   Float64   Float64 

              is_male    0.0000    0.0000    0.0000    1.0000    1.0000
         college_rank    6.3423   18.3223   25.0077   31.6350   44.4943
            years_exp    1.6199    8.6406   12.4411   16.2961   23.4701
                 hire    0.0000    1.0000    1.0000    1.0000    1.0000

        Distributions of variable is_male (num:0)
        0.00000 =>    5132  (0.513200)
        1.00000 =>    4868  (0.486800)
        gender: Dict{Any, Any}(0.0 => 5132, 1.0 => 4868)
        gender: 1 male, 2: majority:
        Distributions of variable (num:0)
        [1, 1]  =>      4741 (0.4741)
        [0, 1]  =>      4366 (0.4366)
        [0, 0]  =>      766 (0.0766)
        [1, 0]  =>      127 (0.0127)
        hire | male: 0.973911257189811
        hire | female: 0.8507404520654716
        female / male: 0.8735297449176789


=#

using Turing
include("jl_utils.jl")

@model function fairness_model2()
    # Population model
    is_male ~ Bernoulli(0.5)
    college_rank ~ Normal(25, 10)
    if is_male == 1
        years_exp ~ Normal(15, 5)
    else
        years_exp ~ Normal(10, 5)
    end

    # Hiring decision.
    if ((college_rank <= 5) | (years_exp > 5))
        hire ~ Dirac(1)
    else
        hire ~ Dirac(0)
    end

    return [is_male, hire]

end

model = fairness_model2()


# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 1_000)
chns = sample(model, PG(5), 10_000)
# chns = sample(model, SMC(10), 1_000)
# chns = sample(model, IS(), 1_000)
              
display(chns)

show_var_dist_pct(chns,:is_male)
gender = group(chns,:is_male).value.data |> make_hash
println("gender: $gender")

println("gender: 1 male, 2: majority:")
chains_params = Turing.MCMCChains.get_sections(chns, :parameters)
genq = generated_quantities(model, chains_params)
show_var_dist_pct(genq)


hhh = genq[:,1] |> make_hash
hire_male = get(hhh,[1,1],0) / gender[1]
hire_female = get(hhh,[0,1],0) / gender[0]
println("hire | male: $hire_male")
println("hire | female: $hire_female")
println("female / male: $(hire_male > 0 ? hire_female / hire_male : 0)")
