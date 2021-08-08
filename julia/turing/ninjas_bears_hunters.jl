#=
    Ninjas, Bears, hunters

    Port of the PSI model
    https://github.com/eth-sri/psi/blob/master/test/ninjasBearsHunters.psi

    Similar to Rock, Paper, Scissors but there are some differences:
    - At start there are certain amount of ninjas, bears, and hunters. It 
      might be different for each type.
    - In each turn, two of separare types are selected and 
      one of them is eliminated according to the elimination rules
      (see below).
    - We continue until there is just one left.
    - The elimination rules are 
        * bear kills ninja
        * hunter kills bear
        * ninja kills hunter
    
   What are the probabilities that the winner is a specific type?

   The PSI model starts with
      10 ninjas, 2 bears, and 5 hunters
   and the the exact probabilities of survival are:
    (probNinjaSurvive,probBearsSurvive,probHuntersSurvive)=
    (0.221480658306459,0.767229372971558,0.0112899687219824)

   It is - at first sight - a little surprising that the probability 
   of the ninja as the winner is so low (0.22). But since there are so many 
   ninjas, the hunters are killed quite soon which mean that there is no hunter 
   left that can kill the bears (which kills the ninjas). Thus the bears have the greatest 
   probability of survival (0.77).

   This Turing model:
   Summary Statistics
            parameters      mean       std   naive_se      mcse         ess       rhat   ess_per_sec 
               Symbol   Float64   Float64    Float64   Float64    Float64?   Float64?       Float64 

      probNinjaSurvive    0.2240    0.4171     0.0132    0.0167    951.6074     0.9992     5228.6121
      probBearsSurvive    0.7590    0.4279     0.0135    0.0151    963.7017     0.9990     5295.0643
    probHuntersSurvive    0.0170    0.1293     0.0041    0.0047   1037.0165     1.0005     5697.8929

   Quantiles
            parameters      2.5%     25.0%     50.0%     75.0%     97.5% 
               Symbol   Float64   Float64   Float64   Float64   Float64 

      probNinjaSurvive    0.0000    0.0000    0.0000    0.0000    1.0000
      probBearsSurvive    0.0000    1.0000    1.0000    1.0000    1.0000
    probHuntersSurvive    0.0000    0.0000    0.0000    0.0000    0.0000


    Some other examples:
    * start with 3 ninjas, 4 bears, and 3 hunters. 
      The probability of survival is
      probNinjaSurvive  : 0.2520
      probBearsSurvive  : 0.2880
      probHuntersSurvive: 0.4600
      (Exact probability from PSI: 
       3927004/15690675,2846092/9984975,3402579/7322315 = 0.250276294678209,0.285037468796867,0.464686236524924
       )

    * start with 1 ninjas, 2 bears, and 3 hunters. 
      The probability of survival is
      probNinjaSurvive  : 0.4420
      probBearsSurvive  : 0.0810
      probHuntersSurvive: 0.4770
    
      (Exact probability from PSI:
       339/770,111/1540,751/1540 = 0.44025974025974,0.0720779220779221,0.487662337662338
      )

    Remember; 
    * bear kills ninja
    * hunter kills bear
    * ninja kills hunter


    Also see 
    * https://en.wikipedia.org/wiki/Rock_paper_scissors
    * It seems to have been popularized by the FedEx commercial: Bear, hunter ninja.
      https://www.youtube.com/watch?v=AhFbbq0zpHY
    * (The PSI model refers to a link that's not accessible.)

=#
using Turing, StatsPlots, DataFrames, Distributions, Random
include("jl_utils.jl")


@model function ninjas_bears_hunters(ninjas=10, bears=2, hunters=5)
    n = ninjas + bears + hunters 
    NINJA = 1
    BEAR = 2
    HUNTER = 3
    p1 = tzeros(1)
    p2 = tzeros(1)
    i = 0
    while true
      i += 1

      s = ninjas + bears + hunters
      v = [ninjas,bears,hunters]
      # println("i:$i v:$v")
      if s <= 1 || count(t -> t > 0,v) == 1
         break
      end
      
      a = [ninjas/s,bears/s,hunters/s]

      push!(p1,0)
      push!(p2,0)
      p1[i] ~ Categorical(simplex(a))
      p2[i] ~ Categorical(simplex(a))
      # true ~ Dirac(p1[i] != p2[i])

      # there must be at least two remaining civilizations.
      if p1[i] != p2[i] && s > 1 && ninjas + bears > 0 && ninjas + hunters > 0 && bears + hunters > 0

         # true ~ Dirac(!(ninjas  == 0 && (p1[i] == NINJA  || p2[i] == NINJA)))
         # true ~ Dirac(!(bears	  == 0 && (p1[i] == BEAR   || p2[i] == BEAR)))
         # true ~ Dirac(!(hunters == 0 && (p1[i] == HUNTER || p2[i] == HUNTER)))
         
         if		((p1[i] == NINJA && p2[i] == BEAR)   || (p1[i] == BEAR   && p2[i] == NINJA) && ninjas  > 0) ninjas  -=1 # Bear kills ninja
         elseif ((p1[i] == NINJA && p2[i] == HUNTER) || (p1[i] == HUNTER && p2[i] == NINJA) && hunters > 0) hunters -=1 # Ninja kills hunter
         elseif ((p1[i] == BEAR  && p2[i] == HUNTER) || (p1[i] == HUNTER && p2[i] == BEAR)  && bears   > 0) bears   -=1 # Hunter kills bear
         end   
      end
   end

   # true ~ Dirac(!(ninjas == 0 && bears == 0 && hunters == 0))
   # true ~ Dirac(ninjas + bears == 0 || ninjas + hunters == 0 || bears + hunters == 0)
   probNinjaSurvive   ~ Dirac(ninjas > 0)
   probBearsSurvive   ~ Dirac(bears > 0)
   probHuntersSurvive ~ Dirac(hunters > 0)
   return 
end

num_ninjas=10
num_bears=2
num_hunters=5
# num_ninjas=3
# num_bears=4
# num_hunters=3

# num_ninjas=1
# num_bears=2
# num_hunters=3
model = ninjas_bears_hunters(num_ninjas,num_bears,num_hunters)

# Since we don't do any observations, we can use Prior()
chns = sample(model, Prior(), 1_000)
# chns = sample(model, MH(), 1_000)
# chns = sample(model, PG(5), 1_000)
# chns = sample(model, SMC(), 1_000)
# chns = sample(model, IS(), 1_000)

# chns = sample(model, HMC(0.1,6), 1_000)
# chns = sample(model, NUTS(), 1_000)

display(chns[[:probNinjaSurvive,:probBearsSurvive,:probHuntersSurvive]])

# show_var_dist_pct(chns,:probNinjaSurvive)
# show_var_dist_pct(chns,:probBearsSurvive)
# show_var_dist_pct(chns,:probHuntersSurvive)
