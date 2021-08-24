#=

    Indian GPA model 

    From cplint http://cplint.ml.unife.it/example/inference/indian_gpa.pl
    """
    The Indian GPA Problem. From 
    http://www.robots.ox.ac.uk/~fwood/anglican/examples/viewer/?worksheet=indian-gpa 
    "This example was inspired by Stuart Russell...the problem is: if you observe 
    that a student GPA is exactly 4.0 in a model of transcripts of students 
    from the USA (GPA's from 0.0 to 4.0 and India (GPA's from 0.0 to 
    10.0) what is the probability that the student is from India?... 
    As we know from statistics, given the mixture distribution and given the 
    fact that his/her GPA is exactly 4.0, the probability that the student 
    is American must be 1.0.
    (i.e. zero probability that the student is from India)."
    Probabilistic logic program from 
    https://github.com/davidenitti/DC/blob/master/examples/indian-gpa.pl

    """

    Also see the SPPL model indian-gpa.py which I used as inspiration for 
    this Turing.jl model.

    Some tests:
    * Plain model
        Summary Statistics
        parameters      mean       std   naive_se      mcse        ess      rhat   ess_per_sec 
            Symbol   Float64   Float64    Float64   Float64    Float64   Float64       Float64 

        nationality    1.5230    0.4997     0.0158    0.0194   604.5633    0.9997      458.6975
            perfect    0.1520    0.3592     0.0114    0.0143   690.4058    1.0015      523.8283
                gpa    4.0086    2.9342     0.0928    0.1346   502.5789    0.9991      381.3194

        Quantiles
        parameters      2.5%     25.0%     50.0%     75.0%     97.5% 
            Symbol   Float64   Float64   Float64   Float64   Float64 

        nationality    1.0000    1.0000    2.0000    2.0000    2.0000
            perfect    0.0000    0.0000    0.0000    0.0000    1.0000
                gpa    0.2925    1.6643    3.3376    5.8096   10.0000

        Distributions of variable nationality
        usa        =>     523  (0.523000)
        india      =>     477  (0.477000)

    * true ~ Dirac(gpa == 4)
        Summary Statistics
        parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
            Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

        nationality    1.9990    0.0316     0.0010    0.0010   1000.0080    1.0000      646.8357
            perfect    0.9980    0.0447     0.0014    0.0020    400.4815    1.0010      259.0437
                gpa    3.9959    0.1020     0.0032    0.0041    543.5300    1.0006      351.5718

        Distributions of variable nationality
        usa        =>     999  (0.999000)
        india      =>       1  (0.001000)
  
    * true ~ Dirac(perfect)
        Summary Statistics
        parameters      mean       std   naive_se      mcse        ess      rhat   ess_per_sec 
            Symbol   Float64   Float64    Float64   Float64    Float64   Float64       Float64 

        nationality    1.5200    0.4998     0.0158    0.0475    82.7796    0.9990       56.3893
            perfect    0.9980    0.0447     0.0014    0.0020   400.4815    1.0010      272.8076
                gpa    6.8704    3.0038     0.0950    0.2832    83.1920    0.9990       56.6703


        Distributions of variable nationality
        usa        =>     520  (0.520000)
        india      =>     480  (0.480000)

    * true ~ Dirac( (nationality == usa && gpa > 3) || (gpa >= 8 && gpa < 10))
        Summary Statistics
        parameters      mean       std   naive_se      mcse        ess      rhat   ess_per_sec 
            Symbol   Float64   Float64    Float64   Float64    Float64   Float64       Float64 

        nationality    1.7100    0.4540     0.0144    0.0326   226.4175    1.0000      152.7784
            perfect    0.2510    0.4338     0.0137    0.0303   234.7464    1.0005      158.3984
                gpa    5.2539    2.4201     0.0765    0.1821   223.4711    1.0000      150.7902

        Quantiles
        parameters      2.5%     25.0%     50.0%     75.0%     97.5% 
            Symbol   Float64   Float64   Float64   Float64   Float64 

        nationality    1.0000    1.0000    2.0000    2.0000    2.0000
            perfect    0.0000    0.0000    0.0000    1.0000    1.0000
                gpa    3.1515    3.6150    4.0000    8.1999    9.7962

        Distributions of variable nationality
        usa        =>     710  (0.710000)
        india      =>     290  (0.290000)
   

    * t1 ~ Dirac( (nationality == usa && gpa > 3) || (gpa >=8 && gpa < 10))
        Summary Statistics
        parameters      mean       std   naive_se      mcse        ess      rhat   ess_per_sec 
            Symbol   Float64   Float64    Float64   Float64    Float64   Float64       Float64 

        nationality    1.5020    0.5002     0.0158    0.0246   600.6463    0.9996      375.8738
            perfect    0.1120    0.3155     0.0100    0.0068   657.5644    0.9990      411.4921
                gpa    3.7279    2.8090     0.0888    0.1057   670.9449    0.9990      419.8654
                t1    0.2720    0.4452     0.0141    0.0154   645.9312    0.9990      404.2123

        Quantiles
        parameters      2.5%     25.0%     50.0%     75.0%     97.5% 
            Symbol   Float64   Float64   Float64   Float64   Float64 

        nationality    1.0000    1.0000    2.0000    2.0000    2.0000
            perfect    0.0000    0.0000    0.0000    0.0000    1.0000
                gpa    0.1251    1.4697    3.1822    4.9678   10.0000
                t1    0.0000    0.0000    0.0000    1.0000    1.0000

        Distributions of variable nationality
        usa        =>     502  (0.502000)
        india      =>     498  (0.498000)
   

=#

using Turing, StatsPlots, Distributions, StatsBase
using DataFrames,RDatasets
include("jl_utils.jl")

@model function indian_gpa() 
    india = 1
    usa   = 2
    nationality ~ Categorical([0.5,0.5])
    if nationality == india 
        perfect ~ flip(0.10)
        if perfect 
            gpa ~ Dirac(10.0)
        else 
            gpa ~ Uniform(0,10)
        end
    else
        perfect ~ flip(0.15)
        if perfect 
            gpa ~ Dirac(4.0)
        else 
            gpa ~ Uniform(0,4)
        end

    end

    # true ~ Dirac(gpa == 4)
    # true ~ Dirac(gpa > 4)
    # true ~ Dirac(perfect)
    # true ~ Dirac( (nationality == usa && gpa > 3) || (gpa >= 8 && gpa < 10))
    # t1 ~ Dirac( (nationality == usa && gpa > 3) || (gpa >=8 && gpa < 10))
    
end 



model = indian_gpa()

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 1_000)
chns = sample(model, PG(5), 1_000)
# chns = sample(model, SMC(), 1_000)
# chns = sample(model, IS(), 10_000)

# chns = sample(model, HMC(0.1,6), 1_000)
# chns = sample(model, NUTS(), 1_000)

display(chns)

show_var_dist_pct(chns,:nationality,["india","usa"])

