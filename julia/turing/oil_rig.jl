#=
   From BayesiaLab, example Oil Rig

   Summary Statistics
    parameters        mean          std   naive_se        mcse         ess      rhat   ess_per_sec 
        Symbol     Float64      Float64    Float64     Float64     Float64   Float64       Float64 

  seismic_test      1.0000       0.0000     0.0000      0.0000         NaN       NaN           NaN
     test_cost    -10.0000       0.0000     0.0000      0.0000         NaN       NaN           NaN
           oil      1.6826       0.7717     0.0077      0.0147   3270.0378    1.0000      361.1705
   test_result      2.1883       0.7880     0.0079      0.0144   3684.7816    1.0002      406.9783
         drill      0.5063       0.5000     0.0050      0.0076   4129.9400    0.9999      456.1453
          gain   9148.0000   74184.2600   741.8426   1348.6578   3466.2666    0.9999      382.8437


   Distributions of variable seismic_test (num:0)
   1.00000 =>   10000  (1.000000)

   Distributions of variable test_cost (num:0)
   -10.00000 =>   10000  (1.000000)

   Distributions of variable oil
   dry        =>    5068  (0.506800)
   wet        =>    3038  (0.303800)
   soaking    =>    1894  (0.189400)

   Distributions of variable test_result
   no_structure =>    4223  (0.422300)
   open_structure =>    3437  (0.343700)
   closed_structure =>    2340  (0.234000)

   Distributions of variable drill (num:0)
   1.00000 =>    5063  (0.506300)
   0.00000 =>    4937  (0.493700)

   Distributions of variable gain (num:0)
   0.00000 =>    4937  (0.493700)
   -70000.00000 =>    2581  (0.258100)
   50000.00000 =>    1495  (0.149500)
   200000.00000 =>     987  (0.098700)

   cf ~/blog/oil_rig.blog
      ~/psi/oil_rig.psi
      ~/webppl/oil_rig.wppl

=#
using Turing, StatsPlots
include("jl_utils.jl")


@model function oil_rig()
    closed_structure = 1
    open_structure = 2
    no_structure = 3
    
    structures = [closed_structure,open_structure,no_structure]
    
    seismic_test ~ flip(0.5)
    test_cost ~ seismic_test ? Dirac(-10) : Dirac(0)
    oil ~ Categorical(simplex([50,30,20])) # [dry, wet,soaking]

    dry = 1
    wet = 2
    soaking = 3
    test_result ~ seismic_test == true ?
         (
        (oil == dry)     ? Categorical(simplex([10,30,60])) : # structures
        (oil == wet)     ? Categorical(simplex([30,40,30])) :
        (oil == soaking) ? Categorical(simplex([50,40,10])) : Dirac(-1)
         ) : Dirac(4)                                                                
        
    drill ~ flip(0.5)
    
    gain ~ drill == true ?
        (
            (oil == dry)     ? Dirac(-70000) : 
            (oil == wet)     ? Dirac(50000)  :
            (oil == soaking) ? Dirac(200000) : Dirac(-1)
        ) : Dirac(0)
    
    # true ~ Dirac(drill == false)
    true ~ Dirac(seismic_test == true)
    # true ~ Dirac(oil == soaking)
    # true ~ Dirac(test_result == open_structure)
    # true ~ Dirac(drill == true)
    

end

model = oil_rig()

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(5), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

display(chns)
# display(plot(chns))

show_var_dist_pct(chns,:seismic_test)
show_var_dist_pct(chns,:test_cost)
show_var_dist_pct(chns,:oil, ["dry","wet","soaking"])
show_var_dist_pct(chns,:test_result, ["closed_structure","open_structure","no_structure"])
show_var_dist_pct(chns,:drill)
show_var_dist_pct(chns,:gain)
