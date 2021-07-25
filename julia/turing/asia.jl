#= 

  Asia medical problem in Turing.jl

  Port of *Church model https://github.com/stuhlmueller/jschurch/blob/master/tests/asia.church

  WebPPL model (exact probabilities)
  [ 'smoker', 0.6700973677390694 ],
  [ 'tb', 0.04072414942060647 ],
  [ 'cancer', 0.07774610343933955 ],
  [ 'tb_or_cancer', 0.11473103550405392 ],
  [ 'bronchitis', 1 ],
  [ 'xray', 0.15669986301877015 ],
  [ 'bronchitis', 1 ] ]

  This Turing model (using SMC())
  """
  Summary Statistics
    parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
        Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

        smoker    0.6759    0.4681     0.0047    0.0073   4091.3819    1.0002     1207.2534
            tb    0.0400    0.1960     0.0020    0.0031   4124.4085    1.0004     1216.9987
        cancer    0.0719    0.2583     0.0026    0.0041   4258.4295    1.0001     1256.5446
  tb_or_cancer    0.1068    0.3089     0.0031    0.0052   4182.5913    1.0004     1234.1668
    bronchitis    1.0000    0.0000     0.0000    0.0000         NaN       NaN           NaN
          xray    0.1504    0.3575     0.0036    0.0057   4220.3910    1.0004     1245.3205
      dyspnoea    0.8080    0.3939     0.0039    0.0060   4220.4111    0.9999     1245.3264
  """

=#

using Turing
include("jl_utils.jl")

@model function asia()
    smoker ~ flip(0.5)  
    tb ~ flip(smoker ? 0.05 : 0.01)
    cancer ~ flip(smoker ? 0.1 : 0.01)
    tb_or_cancer ~ Dirac(tb || cancer)
    bronchitis ~ flip(smoker ? 0.6 : 0.3)
    xray ~ flip(tb_or_cancer ? 0.98 : 0.05)
    dyspnoea ~ flip(tb_or_cancer ? bronchitis ? 0.9 : 0.7
                    : bronchitis ? 0.8 : 0.1)    
    true ~ Dirac(bronchitis == true)
    true ~ Dirac(dyspnoea == true)

end

model = asia()

# chains = sample(model, Prior(), 10_000)
# chains = sample(model, MH(), 10_000)
# chains = sample(model, PG(15), 10_000)
# chains = sample(model, IS(), 10_000)
chains = sample(model, SMC(), 10_000)
# chains = sample(model, SMC(), MCMCThreads(), 10_000, 4)

display(chains)

