#=

   Sequence waiting times.

   From 
   Gunnar Blom, Lars Holst, Dennis Sandell:
   "Problems and Snapshots from the World of Probability"
   Page 4ff, Problem 1.4 Patterns I, (Part Problem 1)

   Problem: Waiting time for a pattern (coin flipping)
   
   What is the waiting time for the patterns[0,1] in a coin 
   flipping sequence? For the pattern [1,1]?
   Answer: 
     * waiting time for [0,1] = 4
     * waiting time for [1,1] = 6

   This model verifies this result:
   - [0,1]: 4.0031
   - [1,1]: 6.0300

   Mean length of some other sequences:
   - [0,1,0]: 9.9668
   - [1,1,1]: 14.1199


   Cf ~/webppl/sequence_waiting_times_1.wppl

=#
using Turing, StatsPlots
include("jl_utils.jl")

@model function sequence_waiting_times_1(pattern)
    patternLen = length(pattern)
        
    function flipSequence(a) 
        len = length(a)
        if len >= patternLen && a[len-patternLen+1:end] == pattern
            return a
        else
            return flipSequence(push!(a,rand(Bernoulli(0.5))*1))
        end
    end
        
    a = flipSequence([])
    len ~ Dirac(length(a))

end


function run_seqence_waiting_times_1(pattern)
    println("\npattern: $pattern")
    model = sequence_waiting_times_1(pattern)


    # chns = sample(model, Prior(), 10_000)
    # chns = sample(model, MH(), 10_000)
    chns = sample(model, PG(5), 10_000)
    # chns = sample(model, SMC(), 10_000)
    # chns = sample(model, IS(), 10_000)

    display(chns)
    # display(plot(chns))

    # show_var_dist_pct(chns,:len)
end

run_seqence_waiting_times_1([0,1])
run_seqence_waiting_times_1([1,1])
run_seqence_waiting_times_1([0,1,0])
run_seqence_waiting_times_1([1,1,1])
