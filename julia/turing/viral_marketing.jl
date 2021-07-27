#=
  http://cplint.eu/example/inference/viral.swinb
  """
  Viral Marketing

  A firm is interested in marketing a new product to its customers. These are connected in a social network
  that is known to the firm: the network represents the trust relationships between customers. The firm has
  decided to adopt a marketing strategy that involves giving the product for free to a number of its
  customers, in the hope that these influence the other customers and entice them to buy the product.
  The firm wants to choose the customers to which marketing is applied so that its return is maximized.
  This involves computing the probability that the non-marketed customers will acquire the product given the
  action to the marketed customers.

  This viral marketing scenario is inspired by [1].

  We can model this domain with an LPAD where the predicate trust/2 encodes the links between customers in the
  social network and predicate has/1 is true of customers that possess the product, either received as a gift
  or bought. Predicate trust/2 is defined by a number of certain facts, while predicate has/1 is defined by
  two rules, one expressing the prior probability of a customer to buy the product and one expressing the
  fact that if a trusted customer has the product, then there is a certain probability that the trusting
  customer buys the product.
  """

  Reference G. V. den Broeck, I. Thon, M. van Otterlo, L. D. Raedt,
  Dtproblog: A decision-theoretic probabilistic prolog, in: M. Fox, D. Poole (Eds.),
  24th AAAI Conference on Artificial Intelligence, AAAI’10, Atlanta, Georgia, USA,
  July 11-15, 2010, AAAI Press, 2010, pp. 1217–1222.

  """
  We want to compute the probability that customer 2 buys the product if we perform the action of
  giving the product to customer 3. We need to use causal reasoning so we use the action do(has(3))
  as evidence:
  ?- prob(has(2),do(has(3)),P).

  P = 0.136

  If instead we compute the effect of the action using regular probabilistic inference, we get:
  ?- prob(has(2),has(3),P).
  P = 0.4065135474609725
  So not distinguishing seeing from doing leads to an overly optimistic estimate.

  ""

  Cf ~/blog/viral_marketing.blog
     ~/psi/viral_marketing.psi
     ~/webppl/viral_marketing.wppl

  Distribution of values for has(p*)
  The BLOG model (-n 100000): given has(p3)
                  has(p1): 0.25285605483625506
                  has(p2): 0.32750708813609203
                  has(p3): 1.0
                  has(p4): 0.39824124623193263

  The PSI model: given has(p3)
                 has(p1): 0.254777070063694
                 has(p2): 0.331210191082803,
                 has(p3): 1,
                 has(p4): 0.4


  This Turing model,using
     chns = sample(model, MH(), MCMCThreads(), 100_000, num_chns)

  Summary Statistics
  parameters      mean       std   naive_se      mcse           ess      rhat
      Symbol   Float64   Float64    Float64   Float64       Float64   Float64

    has_a[1]    0.2512    0.4337     0.0007    0.0033    11421.0186    1.0006
    has_a[2]    0.3407    0.4739     0.0007    0.0035    10540.0175    1.0002
    has_a[3]    1.0000    0.0045     0.0000    0.0000   122493.4137    1.0000
    has_a[4]    0.3973    0.4893     0.0008    0.0034    12741.2279    1.0003

Quantiles
  parameters      2.5%     25.0%     50.0%     75.0%     97.5%
      Symbol   Float64   Float64   Float64   Float64   Float64

    has_a[1]    0.0000    0.0000    0.0000    1.0000    1.0000
    has_a[2]    0.0000    0.0000    0.0000    1.0000    1.0000
    has_a[3]    1.0000    1.0000    1.0000    1.0000    1.0000
    has_a[4]    0.0000    0.0000    0.0000    1.0000    1.0000

Distributions of variable (num:0)
(0.0, 0.0, 1.0, 0.0)    =>  122148 (0.30537)
(0.0, 0.0, 1.0, 1.0)    =>  81756 (0.20439)
(0.0, 1.0, 1.0, 0.0)    =>  56870 (0.142175)
(0.0, 1.0, 1.0, 1.0)    =>  38753 (0.0968825)
(1.0, 0.0, 1.0, 0.0)    =>  36105 (0.0902625)
(1.0, 1.0, 1.0, 0.0)    =>  25975 (0.0649375)
(1.0, 0.0, 1.0, 1.0)    =>  23722 (0.059305)
(1.0, 1.0, 1.0, 1.0)    =>  14663 (0.0366575)
(0.0, 0.0, 0.0, 1.0)    =>  4 (1.0e-5)
(1.0, 0.0, 0.0, 1.0)    =>  3 (7.5e-6)
(1.0, 0.0, 0.0, 0.0)    =>  1 (2.5e-6)
 21.266611 seconds (229.77 M allocations: 11.048 GiB, 23.33% gc time)

=#


using Turing, StatsPlots, DataFrames
include("jl_utils.jl")

@model function viral_marketing()
    p1,p2,p3,p4 = 1:4
    people = [p1,p2,p3,p4]

    trustMatrix = [[0,0,0,0], # p1
                   [1,0,0,0], # p2
                   [1,1,0,0], # p3
                   [1,0,1,0]  # p4
                  ]

    function trusts(a, b)
        return trustMatrix[a][b]
    end

    has_a = tzeros(4)
    #
    # Note: It seems that one have to create the extra array has_a
    #       to get the proper probabilities for has(p1..p4)
    #       Using
    #         # ...
    #         if s > 0
    #           t ~ flip(0.4)
    #         else
    #           t ~ flip(0.1)
    #         end
    #         return t
    #       don't differentiate between the p's.
    #       And this means that we can convert this to a for loop...
    #
    #=
    function has(p)
        s = sum(Int64[trusts(p,q)==1 && has(q)==1 for q in people])
        if s > 0
            has_a[p] ~ flip(0.4)
        else
            has_a[p] ~ flip(0.1)
        end
        return has_a[p]
    end
    =#

    # This is a little faster than using has(p)
    for p in people
        s = count(q->trusts(p,q)==1 && has_a[q]==1, people)
        if s > 0
            has_a[p] ~ flip(0.4)
        else
            has_a[p] ~ flip(0.1)
        end
    end

    true ~ Dirac(has_a[p3]==1)
    return has_a[p1],has_a[p2],has_a[p3],has_a[p4]

end

model = viral_marketing()

num_chns = 4

# chns = sample(model, MH(), MCMCThreads(), 100_000, num_chns)
chns = sample(model, MH(), MCMCThreads(), 10_000, num_chns)
# chns = sample(model, MH(), 10_000)

# chns = sample(model, PG(15), MCMCThreads(), 1_000, num_chns)

# chns = sample(model, SMC(1000), MCMCThreads(), 10_000, num_chns)
# chns = sample(model, SMC(1000), 10_000)
# chns = sample(model, IS(), 10_000)
#
display(chns)

chns_params = Turing.MCMCChains.get_sections(chns, :parameters)
genq = generated_quantities(model, chns_params)

show_var_dist_pct(genq)
