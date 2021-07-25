#=
   Native Fish model.

   From
   A. Nicholson, O. Woodberry and C. Twardy
   "The 'Native Fish' Bayesian networks"
   Technical Report 2010/3, Bayesian Intelligence.
   Available from https://bayesian-intelligence.com/publications.php
   """
   A local river with tree-lined banks is known to contain native fish populations, which need
   to be conserved. Parts of the river pass through croplands, and parts are susceptible to drought
   conditions. Pesticides are known to be used on the crops. Rainfall helps native fish populations
   by maintaining water flow, which increases habitat suitability as well as connectivity between
   different habitat areas. However rain can also wash pesticides that are dangerous to fish from
   the croplands into the river. There is concern that the trees and native fish will be affected by
   drought conditions and crop pesticides.

   In short, we want to model the effect of pesticide use and rainfall on native fish abundance 
   and tree condition.
   ...
   In this model, native fish abundance has two main stressors: water-related and pesticide-related. 
   The model also has three variables describing the water-related stressor:
     - water flow and connectivity: More water keeps the river from fragmenting into ponds, and leads to faster
       flow, which washes out pollutants. Higher water levels are better for the fish.

     - rainfall: This is intended to be year-to-date rainfall, a relatively short-term indicator.

     - drought conditions: A long-term indicator intended to summarize historical conditions. A multi-year
       drought will leave the soil quite dry, so that rain which falls soaks into the ground before reach-
       ing the rivers. (For this reason, much of the rain in the Australian reservoir catchment areas has failed
       to reach the reservoirs.)

   Two variables describe the pesticide-related stressor:
    - Pesticide use: How much pesticide is being used in the river catchment.

    - Pesticide concentration in river: The amount of pesticide in the river itself – which for this example we
      imagine cannot easily be directly observed.
   """

    Distributions of variable drought_conditions
    no         =>    7232  (0.723200)
    yes        =>    2768  (0.276800)

    Distributions of variable annual_rainfall
    below_average =>   10000  (1.000000)

    Distributions of variable pesticide_use
    high       =>    8940  (0.894000)
    low        =>    1060  (0.106000)

    Distributions of variable river_flow
    poor       =>    7075  (0.707500)
    good       =>    2925  (0.292500)

    Distributions of variable pesticide_in_river
    low        =>    6532  (0.653200)
    high       =>    3468  (0.346800)

    Distributions of variable native_fish_abundance
    low        =>    4675  (0.467500)
    medium     =>    2745  (0.274500)
    high       =>    2580  (0.258000)

    Distributions of variable tree_condition
    good       =>    5367  (0.536700)
    damaged    =>    3584  (0.358400)
    dead       =>    1049  (0.104900)

  cf ~/webppl/native_fish.wppl

=#

using Turing, StatsPlots
include("jl_utils.jl")

@model function native_fish()
    yes = 1
    no = 2
    
    # Type Level;
    high = 1
    medium = 2
    low = 3
    levels = [high, medium, low]
    highLow = [high, low]

    good = 1
    damaged = 2
    dead = 3
    
    conditions1 = [good, damaged, dead]

    poor = 2
    conditions2 = [good, poor]

    below_average = 1
    average = 2
    above_average = 3
    rainfall = [below_average, average, above_average]
    
    drought_conditions ~ Categorical([0.25,0.75]) # [yes,no]
    annual_rainfall ~ Categorical([0.1,0.7,0.2])  # rainfall
    pesticide_use ~ Categorical([0.9,0.1]) # highLow

    river_flow ~
        (drought_conditions==yes && annual_rainfall==below_average) ? Categorical([0.05,0.95]) : # conditions2
        (drought_conditions==yes && annual_rainfall==average)       ? Categorical([0.15,0.85]) :
        (drought_conditions==yes && annual_rainfall==above_average) ? Categorical([0.80,0.20]) :
        
        (drought_conditions==no && annual_rainfall==below_average)  ? Categorical([0.40,0.60]) :
        (drought_conditions==no && annual_rainfall==average)        ? Categorical([0.60,0.40]) :
        (drought_conditions==no && annual_rainfall==above_average)  ? Categorical([0.99,0.01]) : Categorical([0.5,0.5])
    
    pesticide_in_river ~
        (pesticide_use==high && annual_rainfall==below_average) ? Categorical([0.3,0.7]) : # [high,low]
        (pesticide_use==high && annual_rainfall==average)       ? Categorical([0.6,0.4]) :
        (pesticide_use==high && annual_rainfall==above_average) ? Categorical([0.8,0.2]) : 
        
        (pesticide_use==low && annual_rainfall==below_average)  ? Categorical([0.1,0.9]) :
        (pesticide_use==low && annual_rainfall==average)        ? Categorical([0.2,0.8]) :
        (pesticide_use==low && annual_rainfall==above_average)  ? Categorical([0.3,0.7]) : Categorical([0.5,0.5])

    native_fish_abundance ~ 
        (pesticide_in_river==high && river_flow==good) ? Categorical([0.2,0.4,0.4]) : # levels
        (pesticide_in_river==high && river_flow==poor) ? Categorical([0.01,0.1,0.89]) :
        (pesticide_in_river==low && river_flow==good)  ? Categorical([0.8,0.15,0.05]) :
        (pesticide_in_river==low && river_flow==poor)  ? Categorical([0.05,0.15,0.8]) : Categorical(ones(3) ./ 3)
    
    tree_condition ~
        (drought_conditions==yes && annual_rainfall==below_average) ? Categorical([0.20,0.60,0.20]) : # conditions1
        (drought_conditions==yes && annual_rainfall==average)       ? Categorical([0.25,0.60,0.15]) :
        (drought_conditions==yes && annual_rainfall==above_average) ? Categorical([0.30,0.60,0.10]) :
        
        (drought_conditions==no && annual_rainfall==below_average)  ? Categorical([0.70,0.25,0.05]) :
        (drought_conditions==no && annual_rainfall==average)        ? Categorical([0.80,0.18,0.02]) : 
        (drought_conditions==no && annual_rainfall==above_average)  ? Categorical([0.90,0.09,0.01]) : Categorical(ones(3) ./ 3)

    
    # true ~ Dirac(tree_condition == dead)
    # true ~ Dirac(pesticide_use == high)
    
    # true ~ Dirac(annual_rainfall == above_average);
    true ~ Dirac(annual_rainfall == below_average);

end

model = native_fish()

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
# chns = sample(model, PG(15), 10_000)
chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

# chns = sample(model, NUTS(), 10_000)

display(chns)
# display(plot(chns))

show_var_dist_pct(chns, :drought_conditions, ["yes","no"])
show_var_dist_pct(chns, :annual_rainfall, ["below_average","average","above_average"])
show_var_dist_pct(chns, :pesticide_use, ["high","low"])
show_var_dist_pct(chns, :river_flow, ["good","poor"])
show_var_dist_pct(chns, :pesticide_in_river, ["high","low"])
show_var_dist_pct(chns, :native_fish_abundance, ["high","medium","low"])
show_var_dist_pct(chns, :tree_condition, ["good","damaged","dead"])
