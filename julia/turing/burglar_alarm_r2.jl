#=

    This is a port of the R2 model BurglarAlarm.cs

    With the datafiles
    - survey_result_unbias_population.csv
    - survey_result_unbias_gender.csv
    - survey_result_unbias_person_gender.csv
    - survey_result_unbias_answer.csv

    Note: there is only 5 observations in the answer file, but reading the files makes it look 
          more general...

    Output from the R2 model (burglary)
    ```
    Mean: 0
    Variance: 0
    Number of accepted samples = 1000
    ```

    This model:
    parameters      mean       std   naive_se      mcse        ess      rhat   ess_per_sec 
        Symbol   Float64   Float64    Float64   Float64    Float64   Float64       Float64 

    earthquake    0.0000    0.0000     0.0000    0.0000        NaN       NaN           NaN
      burglary    0.0030    0.0547     0.0017    0.0030   334.2425    1.0020       51.4140
         alarm    0.0030    0.0547     0.0017    0.0030   334.2425    1.0020       51.4140
  phoneWorking    1.0000    0.0000     0.0000    0.0000        NaN       NaN           NaN
     maryWakes    1.0000    0.0000     0.0000    0.0000        NaN       NaN           NaN
        called    1.0000    0.0000     0.0000    0.0000        NaN       NaN           NaN


=#

using Turing, StatsPlots, Distributions, StatsBase
using CSV
include("jl_utils.jl")

@model function burglar_alarm_r2() 
    earthquake ~ flip(0.0001)
    burglary   ~ flip(0.001)
    alarm      ~ Dirac(earthquake || burglary)

    if earthquake
        phoneWorking ~ flip(0.7)
    else
        phoneWorking ~ flip(0.99)
    end

    if alarm
        if earthquake
            maryWakes ~ flip(0.8)
        else
            maryWakes ~ flip(0.6)
        end
    else
        maryWakes ~ flip(0.2)
    end

    called ~ Dirac(maryWakes && phoneWorking)

    true ~ Dirac(called)


end 

model = burglar_alarm_r2()

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 1_000)
chns = sample(model, PG(5), 1_000)
# chns = sample(model, SMC(), 1_000)
# chns = sample(model, IS(), 1_000)

# chns = sample(model, NUTS(), 1_000)
# chns = sample(model, HMC(0.1,6), 1_000)


display(chns)
