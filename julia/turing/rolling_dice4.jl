#=

  https://dtai.cs.kuleuven.be/problog/tutorial/basic/03_dice.html
  """
  Let’s consider an infinite number of dice, which we roll one after the other until we see a
  six for the first time. What is the probability of stopping after n dice? The first die is always rolled,
  those with higher numbers D are only rolled if the previous roll did not stop the process.
  """

  Cf dice_6_throws.jl
     ~/blog/rolling_dice4.blog
     ~/psi/rolling_dice4.psi
     ~/webppl/rolling_dice4.wppl

  Theoretical values (1:50)
1:0.16666666666666666
2:0.1388888888888889
3:0.11574074074074076
4:0.09645061728395063
5:0.08037551440329219
6:0.06697959533607684
7:0.05581632944673069
8:0.04651360787227558
9:0.038761339893562986
10:0.032301116577969156
11:0.02691759714830763
12:0.022431330956923026
13:0.018692775797435855
14:0.015577313164529882
15:0.012981094303774901
16:0.010817578586479085
17:0.009014648822065905
18:0.0075122073517215875
19:0.006260172793101323
20:0.005216810660917769
21:0.0043473422174314744
22:0.003622785181192896
23:0.0030189876509940797
24:0.002515823042495067
25:0.002096519202079222
26:0.0017470993350660188
27:0.0014559161125550157
28:0.0012132634271291797
29:0.0010110528559409832
30:0.0008425440466174861
31:0.0007021200388479051
32:0.0005851000323732542
33:0.0004875833603110452
34:0.00040631946692587105
35:0.00033859955577155923
36:0.0002821662964762994
37:0.00023513858039691615
38:0.00019594881699743015
39:0.0001632906808311918
40:0.0001360755673593265
41:0.00011339630613277208
42:9.449692177731006e-5
43:7.874743481442506e-5
44:6.562286234535421e-5
45:5.468571862112852e-5
46:4.5571432184273776e-5
47:3.797619348689481e-5
48:3.164682790574568e-5
49:2.63723565881214e-5
50:2.19769638234345e-5


  The values found by this model (run_model(0))
  1.00000 =>    6631  (0.165775)
2.00000 =>    5661  (0.141525)
3.00000 =>    4593  (0.114825)
4.00000 =>    3867  (0.096675)
5.00000 =>    3215  (0.080375)
6.00000 =>    2700  (0.067500)
7.00000 =>    2210  (0.055250)
8.00000 =>    1845  (0.046125)
9.00000 =>    1515  (0.037875)
10.00000 =>    1282  (0.032050)
11.00000 =>    1137  (0.028425)
12.00000 =>     881  (0.022025)
13.00000 =>     728  (0.018200)
14.00000 =>     567  (0.014175)
15.00000 =>     505  (0.012625)
16.00000 =>     459  (0.011475)
17.00000 =>     364  (0.009100)
18.00000 =>     301  (0.007525)
19.00000 =>     245  (0.006125)
20.00000 =>     225  (0.005625)
22.00000 =>     163  (0.004075)
21.00000 =>     154  (0.003850)
23.00000 =>     126  (0.003150)
24.00000 =>     117  (0.002925)
25.00000 =>      83  (0.002075)
26.00000 =>      65  (0.001625)
27.00000 =>      54  (0.001350)
30.00000 =>      48  (0.001200)
28.00000 =>      45  (0.001125)
29.00000 =>      36  (0.000900)
31.00000 =>      36  (0.000900)
32.00000 =>      27  (0.000675)
34.00000 =>      22  (0.000550)
33.00000 =>      20  (0.000500)
35.00000 =>      12  (0.000300)
37.00000 =>       9  (0.000225)
38.00000 =>       7  (0.000175)
39.00000 =>       6  (0.000150)
42.00000 =>       5  (0.000125)
40.00000 =>       5  (0.000125)
36.00000 =>       5  (0.000125)
46.00000 =>       4  (0.000100)
41.00000 =>       4  (0.000100)
44.00000 =>       3  (0.000075)
47.00000 =>       3  (0.000075)
48.00000 =>       3  (0.000075)
57.00000 =>       2  (0.000050)
49.00000 =>       1  (0.000025)
45.00000 =>       1  (0.000025)
50.00000 =>       1  (0.000025)
43.00000 =>       1  (0.000025)
72.00000 =>       1  (0.000025)



=#

using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

# Closed form of the probability
function theoretical_prob(n)
    return (1/6.0) * ((5/6.0)^(n-1))
end

# For val = 0: show the full distribution of the length (len)
# otherwise show the probability of len == val
@model function dice_6_throws(val=0)

    max_len = 100
    function throws(a)
        if length(a) > max_len
           return a
        end

        # For some reason, this don't work: it just generates the same number repeatedly.
        # t ~  DiscreteUniform(1,6)
        t = rand(DiscreteUniform(1,6)) # This works.

        # If we see a 6 then we stop
        if t == 6
            return vcat(a,t)
        else
            return throws(vcat(a,t))
        end
    end

    len ~ DiscreteUniform(0,max_len)

    # a = throws(TArray{Int64}(undef,0))
    a = throws(Int64[])
    len = length(a)

    if val == 0
        return len
    else
        return len == val
    end
end


function run_model(len=0)
    model = dice_6_throws(len)

    num_chains = 4

    # chains = sample(model, Prior(), 10_000)

    # chains = sample(model, MH(), MCMCThreads(), 100_000, num_chains)
    chains = sample(model, MH(), MCMCThreads(), 10_000, num_chains)
    # chains = sample(model, MH(), 10_000)
    # chains = sample(model, MH(), 1_000)

    # chains = sample(model, PG(15), MCMCThreads(), 1_000, num_chains)

    # chains = sample(model, SMC(1000), MCMCThreads(), 10_000, num_chains)
    # chains = sample(model, SMC(1000), 10_000)
    # chains = sample(model, IS(), 10_000)
    #
    # display(chains)
    # show_var_dist_pct(chains,:len,1000)

    println("prob return value:")
    genq = generated_quantities(model, chains)
    show_var_dist_pct(genq,1000)
    if len > 0
        println("\ntheoretical_prob($len): ", theoretical_prob(len))
    end

end

for val in 0:10
    println("\nval:$val")
    run_model(val)
end

println("\nval:25")
run_model(25)

println("\nTheoretical values for 1:50")
for i in 1:50
    println("$i:", theoretical_prob(i))
end
