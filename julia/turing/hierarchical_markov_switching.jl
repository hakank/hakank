#=

    This is a port of the SPPL model hierarchical-markov-switching.pynb

    Note: I'm not sure I understand the real point with the SPPL model, since it 
          includes has the p_transition and the two mu_x and mu_y in the model
          when predicting the values. The model has very nice images though. :-)
    The correspondance of the original zs and the generated Z is 92%.


    In this Turing model we get 81% using PG(5):

    As commented below, the SPPL model use the Bernoulli values (0 and 1) 
    as indices in the matrices which - of course - don't work in Julia. I first tried 
    to use Bernoulli(p) + 1 etc but finally settled with a more traditional if else 
    approach.

    That said, the Z values that's created are mostly very close to 1 or 2 (the states), e.g 
       [1.0 1.0 1.996 1.996 1.996 1.0 1.996 1.996 1.996 1.996 1.996 1.0 1.0 1.996 1.996 
        1.0 1.0 1.0 1.996 1.996 1.996 1.996 1.996 1.996 1.996 1.996 1.996 1.996 1.996 
        1.0 1.0 1.0 1.0 1.0 1.0 1.996 1.996 1.996 1.996 1.996 1.996 1.996 1.996 1.0 1.0 
        1.0 1.0 1.0 1.0 1.0 1.996 1.996 1.996 1.996 1.996 1.996 1.996 1.0 1.134 1.0 1.0 
        1.0 1.0 1.996 1.996 1.134 1.134 1.996 1.0 1.0 1.0 1.0 1.0 1.017 1.017 1.017 1.379 
        1.948 1.996 1.996 1.996 1.778 1.76 1.228 1.0 1.271 1.98 1.985 1.943 1.971 1.995 
        1.97 1.994 1.996 1.994 1.941 1.968 1.996 1.995 1.996])
 
    E.g. Z[2] = 1.0 means that all values are exactly 1.0. This - in turn - means that the 
    chain summary seems very strange with ess and rhat values as Nan, since all the values are
    exactly the same (i.e. there is no variance at all).

    This is using Pg(5):
    parameters      mean       std   naive_se      mcse        ess      rhat   ess_per_sec 
        Symbol   Float64   Float64    Float64   Float64    Float64   Float64       Float64 

       separated    1.0040    0.0632     0.0020    0.0040   167.2813    1.0030        5.5092
            Z[1]    1.0000    0.0000     0.0000    0.0000        NaN       NaN           NaN
            Z[2]    1.0000    0.0000     0.0000    0.0000        NaN       NaN           NaN
            Z[3]    1.9960    0.0632     0.0020    0.0040   167.2813    1.0030        5.5092
            Z[4]    1.9960    0.0632     0.0020    0.0040   167.2813    1.0030        5.5092
            Z[5]    1.9960    0.0632     0.0020    0.0040   167.2813    1.0030        5.5092
            Z[6]    1.0000    0.0000     0.0000    0.0000        NaN       NaN           NaN
            Z[7]    1.9960    0.0632     0.0020    0.0040   167.2813    1.0030        5.5092
            Z[8]    1.9960    0.0632     0.0020    0.0040   167.2813    1.0030        5.5092
        ⋮           ⋮         ⋮         ⋮          ⋮         ⋮          ⋮           ⋮
                                                                            92 rows omitted


    Interestingly, using SMC() we got exactly 92% correspondance between zs and Z (as in the 
    SPPL model) but to the price of some quite bad rhat values (and low ess values):

    parameters      mean       std   naive_se      mcse       ess      rhat   ess_per_sec 
        Symbol   Float64   Float64    Float64   Float64   Float64   Float64       Float64 

       separated    1.0000    0.0000     0.0000    0.0000       NaN       NaN           NaN
            Z[1]    1.0000    0.0000     0.0000    0.0000       NaN       NaN           NaN
            Z[2]    1.0000    0.0000     0.0000    0.0000       NaN       NaN           NaN
            Z[3]    1.6000    0.4901     0.0155    0.1165   12.3842    1.0975        1.5656
            Z[4]    2.0000    0.0000     0.0000    0.0000       NaN       NaN           NaN
            Z[5]    2.0000    0.0000     0.0000    0.0000       NaN       NaN           NaN
            Z[6]    1.6540    0.4759     0.0151    0.1210   15.2745    1.0035        1.9310
            Z[7]    1.9710    0.1679     0.0053    0.0290   34.6756    1.0293        4.3838
            Z[8]    2.0000    0.0000     0.0000    0.0000       NaN       NaN           NaN
        ⋮           ⋮         ⋮         ⋮          ⋮         ⋮         ⋮           ⋮
                                                                            92 rows omitted

    

    (Perhaps a more interesting application would be to recover the matrices from the data.  
     I've tried that but it don't work well...
    )
=#

using Turing, StatsPlots, Distributions, StatsBase
using CSV
include("jl_utils.jl")

"""
`flip2(p)`

As `flip(p)` (`Bernoulli(p)`) but returns 1 for false and 2 for two.
"""
function flip2(p)
    # flip(p) == true ? Dirac(2) : Dirac(1) # This only give 1
    # flip(p) == 1.0 ? Dirac(2) : Dirac(1)  # ibid.
    # Dirac((Bernoulli(p) == 1 ? 1 : 0)+1)  # ibid.
    Dirac(rand(Bernoulli(p)) + 1)
end

@model function hierarchical_markov_switching(X, Y, p_transition,mu_x,mu_y) 
    n = length(X)
    Z = tzeros(n)
    # Z = Vector{Real}(undef, n) 

    #=
    # Note: The model use separated as an index (in mu_x and mu_y) so we have 
    #       to use 1..2 instead of 0..1
    separated ~ Categorical([0.6,0.4]) # false = 1, true = 2

    # Sample initial point.
    Z[1] ~ flip2(0.5)
    # And we have to convert Z[.] to Int64 since it has magically converted to a Float64...
    X[1] ~ Normal(mu_x[separated,Int64(Z[1])])
    Y[1] ~ Poisson(mu_y[separated,Int64(Z[1])])

    for i in 2:n
        Z[i] ~ flip2(p_transition[Int64(Z[i-1])])
        X[i] ~ Normal(mu_x[separated,Int64(Z[i])])
        Y[i] ~ Poisson(mu_y[separated,Int64(Z[i])])
    end
    =#

    #
    # But using separated and Z[.] as indices makes it slow and not good.
    # So I rewrote it like this instead.
    #
    separated ~ flip2(0.4)

    # Sample initial point.
    Z[1] ~ flip2(0.5)  # flip2(p) returns 1 for false and 2 for true
    if Z[1] == 1 
        X[1] ~ Normal(mu_x[separated,1])
        Y[1] ~ Poisson(mu_y[separated,1])
    else 
        X[1] ~ Normal(mu_x[separated,2])
        Y[1] ~ Poisson(mu_y[separated,2])
    end

    for i in 2:n
        if Z[i-1] == 1
            Z[i] ~ flip2(p_transition[1])
        else 
            Z[i] ~ flip2(p_transition[2])
        end
        if Z[i] == 1
            X[i] ~ Normal(mu_x[separated,1])
            Y[i] ~ Poisson(mu_y[separated,1])
        else 
            X[i] ~ Normal(mu_x[separated,2])
            Y[i] ~ Poisson(mu_y[separated,2])
        end
    end

end 



p_transition = [0.2, 0.8]
mu_x = [5 7
        5 15]
mu_y = [5 8 
        3 8]

# From a simulation of the SPPL model
xs = [3.880793246124543, 3.5058061097812354, 7.237774689951984, 6.905669457081878, 7.995126208729154, 4.6898044637528224, 6.263190493579016, 7.278231850683178, 7.852084821649775, 5.855167322724675, 5.720088619624132, 5.391958684807007, 7.169767164696265, 6.9726298890755345, 5.1058192848261035, 7.2162584421734435, 5.978129341306567, 4.632434607148869, 7.350778010868155, 7.163199329872764, 3.312995640232463, 6.984375601895885, 7.724455510712415, 5.347415852836971, 6.9430666068360685, 6.182619378597637, 4.474288262345756, 6.39310952727029, 5.016568585393731, 3.395680248255592, 6.485147290740601, 5.3963458371627215, 4.139882819360646, 4.449201069426469, 3.6512381850843667, 5.116290730592944, 6.937674534921111, 7.0500723954996225, 9.812754469939957, 6.196020900659568, 7.0346040847010425, 6.504549077277077, 7.854425984948853, 7.205493992191783, 6.249403045244166, 4.9176270039175245, 2.4866629773701137, 4.41806051644474, 4.578169073441404, 5.554708298980772, 7.41669328653965, 6.835240203522926, 6.309709638182985, 7.420028428918369, 7.731733741224992, 7.842460992547666, 5.988555563719187, 5.691336019473092, 6.2453880266204695, 6.122901245600784, 4.809476409451023, 4.155260934607323, 5.614429568237265, 6.195549626249333, 5.640085080509843, 6.436848039947654, 7.710450159939799, 7.635502275425415, 3.854963010979329, 3.6886646377960455, 4.584214524115992, 5.024753832591269, 3.2863538125483407, 6.787303600428101, 4.770602129374909, 5.706485809946122, 5.555718053917937, 8.060446637998837, 7.737160780287548, 8.301669108932963, 6.111263452419774, 6.36317050704917, 6.831994416446748, 5.245384044497105, 4.513669998612345, 5.847666590699994, 8.009842936472989, 7.511212171510765, 6.392442074959624, 7.417459874615795, 8.44176627215282, 4.99804279054474, 7.479966948085545, 7.666437579480905, 6.973682646784251, 6.613120207032197, 6.014530091460834, 7.8888624942168715, 6.469044373467464, 8.529156359409793]
ys = [5.0, 6.0, 4.0, 11.0, 9.0, 7.0, 11.0, 6.0, 6.0, 9.0, 3.0, 2.0, 8.0, 10.0, 3.0, 10.0, 10.0, 7.0, 6.0, 10.0, 3.0, 8.0, 12.0, 9.0, 9.0, 8.0, 6.0, 9.0, 7.0, 8.0, 5.0, 4.0, 7.0, 7.0, 3.0, 9.0, 6.0, 13.0, 7.0, 14.0, 7.0, 8.0, 11.0, 7.0, 4.0, 6.0, 7.0, 6.0, 3.0, 5.0, 10.0, 9.0, 4.0, 9.0, 5.0, 8.0, 9.0, 2.0, 7.0, 2.0, 4.0, 2.0, 6.0, 6.0, 6.0, 4.0, 5.0, 6.0, 3.0, 3.0, 4.0, 3.0, 3.0, 5.0, 4.0, 8.0, 4.0, 9.0, 5.0, 7.0, 8.0, 5.0, 7.0, 8.0, 2.0, 5.0, 7.0, 7.0, 5.0, 4.0, 11.0, 11.0, 7.0, 8.0, 13.0, 4.0, 9.0, 7.0, 12.0, 9.0]
# zs are the states. SPPL (Python) use 0..1 but we use 1..2 instead
zs = 1 .+ [0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1]
# n = length(xs)    
n = length(xs)
println("p_transition: $p_transition")
println("mu_x: $mu_x")
println("mu_y: $mu_y")
model = hierarchical_markov_switching(xs,ys,p_transition,mu_x,mu_y)
# model = hierarchical_markov_switching_recover_matrices(xs,ys,zs,p_transition,mu_x,mu_y)

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(5), 1_000)
# chns = sample(model, SMC(), 1_000)
# chns = sample(model, IS(), 1_000)

# chns = sample(model, HMC(0.1,6), 1_000)
# chns = sample(model, NUTS(), 1_000)

display(chns)

# The given zs
println("zs: $zs")

println("Z before rounding: $(mean(group(chns,:Z).value.data,dims=1)))")
zz = vcat(round.(Int64,mean(group(chns,:Z).value.data,dims=1))...)
println(" Z: $zz")
println("p(zs == Z): $(sum(zz .== zs )/n)")
# for i in 1:n 
#     println("i:$i  zs:$(zs[i])  Z[i]:$(zz[i])  eq:$(zs[i] == zz[i])")
# end   

# There are quite few that are in the "undecided zone"
println("Elements that differs with 0.25 from 1.5 ('the undecided zone')")
println(filter(i-> abs(i-1.5) < 0.25,   mean(group(chns,:Z).value.data,dims=1)))