#=
  Hidden Markov Models

  This is from the BLOG distribution hmm.dblog
  """
  A hidden Markov model with four states and four output symbols.
  The parameters of this model are completely made-up.
  DBLOG model
  """

A:1 C:2 G:3 T:4

Distributions of variable S1 (num:0)
2.00000 =>    7524  (0.752400)
4.00000 =>     853  (0.085300)
3.00000 =>     813  (0.081300)
1.00000 =>     810  (0.081000)

Distributions of variable S2 (num:0)
1.00000 =>    9778  (0.977800)
2.00000 =>     176  (0.017600)
3.00000 =>      40  (0.004000)
4.00000 =>       6  (0.000600)

Distributions of variable S3 (num:0)
1.00000 =>    9226  (0.922600)
4.00000 =>     640  (0.064000)
2.00000 =>      84  (0.008400)
3.00000 =>      50  (0.005000)

Distributions of variable S4 (num:0)
1.00000 =>    8574  (0.857400)
4.00000 =>    1006  (0.100600)
2.00000 =>     300  (0.030000)
3.00000 =>     120  (0.012000)

Distributions of variable S5 (num:0)
3.00000 =>    7383  (0.738300)
4.00000 =>    1365  (0.136500)
1.00000 =>     724  (0.072400)
2.00000 =>     528  (0.052800)

Distributions of variable S6 (num:0)
4.00000 =>    3381  (0.338100)
2.00000 =>    2506  (0.250600)
1.00000 =>    2320  (0.232000)
3.00000 =>    1793  (0.179300)

Distributions of variable S7 (num:0)
1.00000 =>    2828  (0.282800)
4.00000 =>    2816  (0.281600)
2.00000 =>    2467  (0.246700)
3.00000 =>    1889  (0.188900)

Distributions of variable S8 (num:0)
2.00000 =>    3246  (0.324600)
3.00000 =>    2758  (0.275800)
4.00000 =>    2169  (0.216900)
1.00000 =>    1827  (0.182700)


  Cf ~/webppl/hmm1.wppl

=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function hmm1()
    n = 8
    
    A = 1
    C = 2
    G = 3
    T = 4 
    states = [A,C,G,T]
    
    AA ~ Uniform(0,1)
    AC ~ Uniform(0,1)
    AG ~ Uniform(0,1)
    AT ~ Uniform(0,1)
    
    CA ~ Uniform(0,1)
    CC ~ Uniform(0,1)
    CG ~ Uniform(0,1)
    CT ~ Uniform(0,1)
    
    GA ~ Uniform(0,1)
    GC ~ Uniform(0,1)
    GG ~ Uniform(0,1)
    GT ~ Uniform(0,1)
    
    TA ~ Uniform(0,1)
    TC ~ Uniform(0,1)
    TG ~ Uniform(0,1)
    TT ~ Uniform(0,1)
   
    S = tzeros(n)
    for t in 1:n
        if t == 1
            S[t] ~ Categorical([0.3,0.2,0.1,0.4])
        else 
            prev_t = S[t-1]
            S[t] ~
                (prev_t == A) ? Categorical(simplex([AA,AC,AG,AT])) :
                (prev_t == C) ? Categorical(simplex([CA,CC,CG,CT])) :
                (prev_t == G) ? Categorical(simplex([GA,GC,GG,GT])) :
                (prev_t == T) ? Categorical(simplex([TA,TC,TG,TT])) : Dirac(5)
        end
    end


    ResultA = 1
    ResultC = 2
    ResultG = 3
    ResultT = 4
    O = tzeros(n)
    for t in 1:n
        St = S[t]
        O[t] ~ (St == A) ? Categorical([0.85,0.05,0.05, 0.05]) : # [ResultA,ResultC,ResultG,ResultT]
               (St == C) ? Categorical([0.05,0.85,0.05, 0.05]) :
               (St == G) ? Categorical([0.05,0.05,0.85, 0.05]) :
               (St == T) ? Categorical([0.05,0.05,0.05, 0.85]) : Dirac(5)
    end
    
    # Evidence for the Hidden Markov Model.  
    true ~ Dirac(O[1] == ResultC)
    true ~ Dirac(O[2] == ResultA)
    true ~ Dirac(O[3] == ResultA)
    true ~ Dirac(O[4] == ResultA)
    true ~ Dirac(O[5] == ResultG)

    # """
    # Queries for the Hiddem Markov Model, given the evidence.  
    # Note that we can query S(5) even though our observations only 
    # went up to time 4.
    # """
    # Note: I changed from BLOG/WebPPL's 0-based to 1-based.
    S1 ~ Dirac(S[1])
    S2 ~ Dirac(S[2])
    S3 ~ Dirac(S[3])
    S4 ~ Dirac(S[4])
    S5 ~ Dirac(S[5])
    S6 ~ Dirac(S[6])
    S7 ~ Dirac(S[7])
    S8 ~ Dirac(S[8])

end

model = hmm1()

num_chains = 4

# chns = sample(model, Prior(), 10_000)
# chns = sample(model, MH(), 10_000)
chns = sample(model, PG(15), 10_000)
# chns = sample(model, SMC(), 10_000)
# chns = sample(model, IS(), 10_000)

display(chns)
# display(plot(chns))

println("A:1 C:2 G:3 T:4")
show_var_dist_pct(chns,:S1)
show_var_dist_pct(chns,:S2)
show_var_dist_pct(chns,:S3)
show_var_dist_pct(chns,:S4)
show_var_dist_pct(chns,:S5)
show_var_dist_pct(chns,:S6)
show_var_dist_pct(chns,:S7)
show_var_dist_pct(chns,:S8)
