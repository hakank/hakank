#=

    This is a port of the SPPL model student-interviews.pynb

    Here is the output from the SPPL model of gpa_prior and gpa_posterior for gpa_student[1]:

        gpa                   gpa_prior (cumsum < gpa)  gpa_posterior (cumsum < gpa)
        -----------------------------------------------------------------------------
        0.0                   0.0                    0.0
        0.21052631578947367   2.9321229365490116e-08 3.938526167438616e-08
        0.42105263157894735   3.399923850680484e-06  4.566892092513705e-06
        0.631578947368421     5.235987400911553e-05  7.03315441989632e-05
        0.8421052631578947    0.0003515966799506029  0.0004722764885157954
        1.0526315789473684    0.0014935106089171875  0.0020061336928425886
        1.263157894736842     0.004734542792041406   0.0063595971522462345
        1.4736842105263157    0.012227795632612991   0.01642478644698973
        1.6842105263157894    0.027098290512422067   0.03639933542542442
        1.894736842105263     0.05325350529777253    0.07153189980839067
        2.1052631578947367    0.09485332168120815    0.12741017262720986
        2.3157894736842106    0.15542018995700252    0.20876562761532186
        2.526315789473684     0.23665218641802727    0.31787917797401755
        2.7368421052631575    0.3371086961787463     0.45281574132555596
        2.9473684210526314    0.45107070187375614    0.6058933411522036
        3.1578947368421053    0.5680350963535515     0.7630038505974432
        3.3684210526315788    0.6734850651989842     0.9046477961757059
        3.578947368421052     0.7517864026032522     0.983997710206192
        3.789473684210526     0.7922926319251767     0.9970552758034202
        4.0                   0.8                    0.9995398186972839

        
        The mean value of the prior is about: 3.050505050505051
        The mean value of the posterior is about: 2.8080808080808084
        (I am not sure how to get an exact value of the mean in SPPL. 
         For continous distributions, SPPL requires that one work with 
         intervals.)


    This Turing.jl model:

    * Prior()
    Summary Statistics
             parameters      mean       std   naive_se      mcse          ess      rhat   ess_per_sec 
                 Symbol   Float64   Float64    Float64   Float64      Float64   Float64       Float64 

     student_perfect[1]    0.1982    0.3987     0.0040    0.0036    9428.5188    0.9999    38018.2211
     student_perfect[2]    0.1940    0.3954     0.0040    0.0039   10299.3911    0.9999    41529.8029
         student_gpa[1]    3.0403    0.6872     0.0069    0.0069    8852.1375    0.9999    35694.1029
         student_gpa[2]    3.0270    0.6943     0.0069    0.0070   10029.1311    1.0001    40440.0447
         num_recruiters   24.9399    4.9591     0.0496    0.0502    9860.3856    1.0002    39759.6194
  student_interviews[1]   14.6656    5.4594     0.0546    0.0522    9365.7231    1.0001    37765.0125
  student_interviews[2]   14.5799    5.4370     0.0544    0.0518   10197.5782    1.0000    41119.2671
      student_offers[1]    5.8633    2.8757     0.0288    0.0262    9664.9849    1.0001    38971.7134
      student_offers[2]    5.8544    2.8591     0.0286    0.0278   10359.6432    0.9999    41772.7549


    * With the observation/condition: student_offers[1] == 1 && num_recruiters > 30 
      using PG(5)

      Note: I am not happy at all with some of the ess and rhat of this model.

    Summary Statistics
                parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
                    Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

        student_perfect[1]    0.0008    0.0283     0.0003    0.0008   2745.1436    1.0007      114.0626
        student_perfect[2]    0.5617    0.4962     0.0050    0.0493     21.6873    1.0500        0.9011
            student_gpa[1]    2.8155    0.3181     0.0032    0.0312     23.9498    1.0122        0.9951
            student_gpa[2]    3.4779    0.8543     0.0085    0.0847     21.8331    1.0939        0.9072
            num_recruiters   32.0304    1.1016     0.0110    0.1015     24.3081    1.3990        1.0100
     student_interviews[1]   12.7146    0.6928     0.0069    0.0576     34.6829    1.0038        1.4411
     student_interviews[2]   22.6132    3.1529     0.0315    0.3070     23.8147    1.0007        0.9895
         student_offers[1]    1.0187    0.3429     0.0034    0.0187    312.0347    1.0029       12.9653
         student_offers[2]    8.4442    1.9881     0.0199    0.1962     21.7784    1.2301        0.9049
    student_perfect1_prior    0.2032    0.4024     0.0040    0.0051   6213.4045    1.0000      258.1711
        student1_gpa_prior    3.0350    0.6952     0.0070    0.0086   6705.7080    1.0006      278.6267

    Quantiles
                parameters      2.5%     25.0%     50.0%     75.0%     97.5% 
                    Symbol   Float64   Float64   Float64   Float64   Float64 

        student_perfect[1]    0.0000    0.0000    0.0000    0.0000    0.0000
        student_perfect[2]    0.0000    0.0000    1.0000    1.0000    1.0000
            student_gpa[1]    1.9660    2.6852    2.6852    3.1288    3.1889
            student_gpa[2]    1.6329    3.6500    4.0000    4.0000    4.0000
            num_recruiters   31.0000   31.0000   32.0000   32.0000   34.0000
     student_interviews[1]   11.0000   13.0000   13.0000   13.0000   13.0000
     student_interviews[2]   16.0000   22.0000   25.0000   25.0000   25.0000
         student_offers[1]    1.0000    1.0000    1.0000    1.0000    1.0000
         student_offers[2]    4.0000    6.0000   10.0000   10.0000   10.0000
    student_perfect1_prior    0.0000    0.0000    0.0000    0.0000    1.0000
        student1_gpa_prior    1.6481    2.5444    3.0229    3.5678    4.0000



      Given that student 1 only get one offer (and from quite many interviews) makes it probable 
      that he/she get a worse GPA (2.8155) than the prior (about 3.04).

=#

using Turing, StatsPlots, Distributions, StatsBase
using CSV
include("jl_utils.jl")

@model function student_interviews(num_students=2) 
    # Students.
    student_perfect    = tzeros(num_students)
    student_gpa        = tzeros(num_students)
    student_interviews = tzeros(num_students)
    student_offers     = tzeros(num_students)

    # For each student, sample GPA.
    for s in 1:num_students
        student_perfect[s] ~ Bernoulli(0.2)
        if student_perfect[s] == false
            # Note: The SPPL model has (a=7,b=3,scale=4), but this form is not supported in Turing.jl 
            #       or Distributions.jl
            # This works and is translated to LocationScale automatically.
            student_gpa[s] ~ Beta(7, 3)*4  
        else
            student_gpa[s] ~ Dirac(4.0)
        end 
    end
        
    # Sample number of recruiters
    num_recruiters ~ truncated(Poisson(25),10,40)
    # num_recruiters ~ truncated(Poisson(25),30.001,40)  # Alternative version

    # For each student, sample interviews and offers.
    for j in 1:num_students
        student_interviews[j] ~ (student_gpa[j] == 4)      ? Binomial(num_recruiters, 0.9) : 
                                (3.5 < student_gpa[j] < 4) ? Binomial(num_recruiters, 0.6) : 
                                                             Binomial(num_recruiters, 0.5)

        student_offers[j] ~ Binomial(student_interviews[j], 0.4)
    end  

    # Observation/condition
    true ~ Dirac(student_offers[1] == 1 && num_recruiters > 30)
    # true ~ Dirac(student_offers[1] == 1)
    # true ~ Dirac(num_recruiters > 30)

    # Priors
    student_perfect1_prior ~ Bernoulli(0.2)
    if student_perfect1_prior == false
        student1_gpa_prior ~ Beta(7, 3)*4 
    else
        student1_gpa_prior ~ Dirac(4.0)
    end 

end 

model = student_interviews()

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 100_000)
chns = sample(model, PG(5),  10_000)
# chns = sample(model, PG(5),  MCMCThreads(), 10_000, 4)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

# chns = sample(model, HMC(0.1,6), 1_000)
# chns = sample(model, NUTS(), 1_000)

display(chns)
# display(plot(chns))

chains_params = Turing.MCMCChains.get_sections(chns, :parameters)


# show_var_dist_pct(chns,Symbol("student_offers[1]"))

