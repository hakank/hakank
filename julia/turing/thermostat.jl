#=

  Thermostat problem.

  OMEGA: Fast, casual Inference from Simple Parts 
  From  https://www.youtube.com/watch?v=oCvbqKE2tWA
  @ ~ 21min
  
  The Omega (Julia) model is here:
  https://github.com/zenna/Omega.jl/blob/master/OmegaModels/src/causal/thermostat.jl

   Summary Statistics
      parameters      mean       std   naive_se      mcse         ess      rhat   ess_per_sec 
          Symbol   Float64   Float64    Float64   Float64     Float64   Float64       Float64 

            time    1.9912    0.8130     0.0081    0.0108   6852.7308    0.9999      574.6525
  is_window_open    0.4929    0.5000     0.0050    0.0065   6729.5331    1.0001      564.3214
        is_ac_on    0.2550    0.4359     0.0044    0.0056   6931.3351    1.0000      581.2440
    outside_temp   20.7561    9.2221     0.0922    0.1171   5860.1765    1.0002      491.4194
       room_temp   23.6955    2.9221     0.0292    0.0377   7120.8508    0.9999      597.1363
      thermostat   22.6500    4.0236     0.0402    0.0527   6486.0439    1.0000      543.9031


   Distributions of variable time
   afternoon  =>    3390  (0.339000)
   morning    =>    3349  (0.334900)
   evening    =>    3261  (0.326100)

   Distributions of variable is_window_open (num:0)
   0.00000 =>    5071  (0.507100)
   1.00000 =>    4929  (0.492900)

   Distributions of variable is_ac_on (num:0)
   0.00000 =>    7450  (0.745000)
   1.00000 =>    2550  (0.255000)



  Cf ~/cplint/thermostat.pl
     ~/blog/thermostat.blog
     ~/webppl/thermostat.wppl

=#
using Turing, StatsPlots, Distributions
include("jl_utils.jl")

@model function thermostat()
    morning = 1
    afternoon = 2
    evening = 3
    times = [morning, afternoon,evening];
    
    time ~ DiscreteUniform(morning,evening)
    
    is_window_open ~ flip(0.5)
    
    # a.c. is off when window is closed
    is_ac_on ~ is_window_open ? Dirac(false) : flip(0.5)
    
    # hottest at noon, cool at night
    outside_temp ~
        (time == morning)   ? Normal(20,2) :
        (time == afternoon) ? Normal(32,2) :
        (time == evening)   ? Normal(10,2) : Normal(25,2)
    
    
    # a.c. chills the room
    room_temp ~ is_ac_on ? Normal(20,2) : Normal(25,2)
   
    # great insulation
    thermostat ~ Dirac(is_window_open ? (outside_temp + room_temp) / 2.0 : room_temp)
    
    # true ~ Dirac(is_ac_on==true)
    # true ~ Dirac(is_window_open == false)
    # true ~ Dirac(room_temp < 20.0)
    # true ~ Dirac(room_temp > 20.0)
    # true ~ Dirac(time == evening)
    # true ~ Dirac(outside_temp > 10.0)
    

end

model = thermostat()

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(5), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

display(chns)
# display(plot(chns))

show_var_dist_pct(chns,:time,["morning","afternoon","evening"])
show_var_dist_pct(chns,:is_window_open)
show_var_dist_pct(chns,:is_ac_on)


