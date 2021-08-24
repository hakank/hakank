#=

    This is a port of the SPPL model piecewise_transformation.pynb


    This model:
    
    Summary Statistics
    parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
        Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

            X    0.0077    2.0044     0.0200    0.0237   7078.5661    1.0000     2453.5758
            Z    5.5088   20.9475     0.2095    0.2633   7000.0705    0.9999     2426.3676

    Quantiles
    parameters      2.5%     25.0%     50.0%     75.0%     97.5% 
        Symbol   Float64   Float64   Float64   Float64   Float64 

            X   -3.9877   -1.3344    0.0165    1.3398    3.8950
            Z   -4.0330   -1.9102    2.2845    4.7697   55.3869

    Distributions of variable (num:20)
    [0.3809939368419736, 2.3758163003224158]        =>      7 (0.0007)
    [2.389000294497004, 3.271804391552638]  =>      6 (0.0006)
    [0.05040145287768943, 0.30482098858237394]      =>      5 (0.0005)
    [2.510391787836159, 3.077892029522447]  =>      5 (0.0005)
    [3.085841287053201, 2.2167185985914113] =>      5 (0.0005)
    [-1.378297415313871, -3.7517239375768643]       =>      5 (0.0005)
    [0.878357224365129, 5.363992135974382]  =>      5 (0.0005)
    [-1.6564081852464383, -2.650093648226999]       =>      5 (0.0005)
    [2.4070872908778953, 3.2426046721887776]        =>      5 (0.0005)
    [-0.06442137343159365, -0.38211077123253523]    =>      5 (0.0005)
    [-0.9165981570628119, -3.889354819602364]       =>      4 (0.0004)
    [3.916099050927065, 1.1054319814770786] =>      4 (0.0004)
    [-1.68198255716986, -2.5043914946483774]        =>      4 (0.0004)
    [2.30763605135434, 3.404547328574913]   =>      4 (0.0004)
    [0.6805392443102667, 4.231188495843754] =>      4 (0.0004)
    [-1.5864706509433244, -3.008964048794879]       =>      4 (0.0004)
    [-0.20011477184419052, -1.1526289286285303]     =>      4 (0.0004)
    [-0.8592321987914273, -3.782759298124592]       =>      4 (0.0004)
    [1.2630493505290816, 5.380726580488626] =>      4 (0.0004)
    [-0.933908737521431, -3.9167252078853263]       =>      4 (0.0004)
 
=#

using Turing, StatsPlots, Distributions, StatsBase
using CSV
include("jl_utils.jl")

@model function piecewise_transformation() 
    X ~ Normal(0, 2)
    if X < 1
        Z ~ Dirac(-X^3+X^2+6*X)
    else
        Z ~ Dirac(-5*sqrt(X)+11)
    end
    
    return [X,Z]
end 

model = piecewise_transformation()

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(5), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

# chns = sample(model, HMC(0.1,6), 1_000)
# chns = sample(model, NUTS(), 1_000)

display(chns)
# display(plot(chns))

chains_params = Turing.MCMCChains.get_sections(chns, :parameters)
genq = generated_quantities(model, chains_params)
show_var_dist_pct(genq,20)
