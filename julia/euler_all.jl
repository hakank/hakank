#=
 
  Running all 1..50 Euler problems in Julia.

  This is a file where I've collected the best variants of the 1..50
  Euler problems. Mostly it's a test to see how fast it will be
  by not handling 50 separate Julia programs.

  Running this in Julia REPL:
  * first run:
  """
  Total time: 1.39524527
  6.209198 seconds (27.90 M allocations: 1.515 GiB, 5.93% gc time)
  """

  * second run (everything JIT:ed):
  """
  ...
  Total time: 1.0628387979999998
  3.564508 seconds (20.37 M allocations: 1.121 GiB, 5.59% gc time)
  """


  This Julia model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/Julia_progs/

=#

using Memoization
include("Euler.jl")

using BenchmarkTools
const run_benchmark = false

const res = []
const total_time = [0.0]

function run_euler(p)
    if run_benchmark
        println("\nrun_euler($p)")
        # @run_euler_bench p
        # @benchmark $p
        # b = @benchmarkable $p
        # tune!(b)
        # display(run(b))
        # b = @benchmark $p
        #dump(b)
        # Note the '$p()' construct!

        b = @benchmark $p()
        display(b)
        m = mean(b)
        println("$p mean:$m\n\n")
        push!(res, [p,m])
    else 
        # GC.gc(false)
        t = @timed p()
        # GC.gc(true)
        # @printf "%s: %d: %2.8fs\n" p t.value t.time
        push!(res, [p,t.time])
        total_time[1] += t.time 
    end
end


# 0.00000276s
function euler1c()
   s = 0
   for i in 1:999
       if i % 3 == 0 || i % 5 == 0
           s+=i
       end
   end
   s
end
run_euler(euler1c)


const fibmem2b = Dict{Int,Int}()
function fib2b(n::Int)
    get!(fibmem2b, n) do
        n < 3 ? 1 : fib2b(n-1) + fib2b(n-2)
    end
end

# 8.108e-6s
function euler2b()
    n = 1
    f = 0
    s = 0
    while f < 4_000_000
        f = fib2b(n)
        if f % 2 == 0
            s += f
        end
        n += 1
    end
    return s
end
run_euler(euler2b)


# 0.00169180s
function euler3c()
    return maximum(factors(600851475143))
end
run_euler(euler3c)


# 0.00958293s
function euler4a()
    max = 0
    from = 100
    to = 999
    for i in from:to, j in i:to
        ij = i*j
        if ij > max && palindromic_number(ij)
            max = ij
        end
    end
    return max;
end
run_euler(euler4a)


# 0.00000113s
function euler5a()
    a = 1
    for i in 2:20
        a = lcm(a,i)
    end
    return a
end
run_euler(euler5a)


# 0.00000085s
function euler6b()
    sum(1:100)^2 - sum((1:100).|>i->i^2)
end
run_euler(euler6b)


# 0.00981122s
function euler7b()
    p = primes(200000) # slightly cheating...
    return p[10001];
end
run_euler(euler7b)



# digits works with this
n = 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450


# 0.04791558s
function euler8a()
    # a = split(n,"").|>i->parse(Int8,i)
    a = digits(n)
    len = length(a)
    max = 0
    for i in 1:(len-5+1)
        p = 1
        for j = 0:4
            p *= a[i+j]
            if p==0
                continue;
            end
        end
        if p > max
            max = p;
        end
    end
    return max;
end
run_euler(euler8a)



# 0.00683682s
function euler9a()
    for c in 1:500
        for b in 1:c
            for a in 1:b
                if a+b+c === 1000 && a^2 + b^2 - c^2 == 0
                    return a*b*c;
                end
            end
        end
    end
end

run_euler(euler9a)


# 0.04506652s
function euler10c()
    return sieve(2_000_000)|>sum
end
run_euler(euler10c)



m = [
     8  2 22 97 38 15  0 40  0 75  4  5  7 78 52 12 50 77 91  8;
    49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48  4 56 62  0;
    81 49 31 73 55 79 14 29 93 71 40 67 53 88 30  3 49 13 36 65;
    52 70 95 23  4 60 11 42 69 24 68 56  1 32 56 71 37  2 36 91;
    22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80;
    24 47 32 60 99  3 45  2 44 75 33 53 78 36 84 20 35 17 12 50;
    32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70;
    67 26 20 68  2 62 12 20 95 63 94 39 63  8 40 91 66 49 94 21;
    24 55 58  5 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72;
    21 36 23  9 75  0 76 44 20 45 35 14  0 61 33 97 34 31 33 95;
    78 17 53 28 22 75 31 67 15 94  3 80  4 62 16 14  9 53 56 92;
    16 39  5 42 96 35 31 47 55 58 88 24  0 17 54 24 36 29 85 57;
    86 56  0 48 35 71 89  7  5 44 44 37 44 60 21 58 51 54 17 58;
    19 80 81 68  5 94 47 69 28 73 92 13 86 52 17 77  4 89 55 40;
     4 52  8 83 97 35 99 16  7 97 57 32 16 26 26 79 33 27 98 66;
    88 36 68 87 57 62 20 72  3 46 33 67 46 55 12 32 63 93 53 69;
     4 42 16 73 38 25 39 11 24 94 72 18  8 46 29 32 40 62 76 36;
    20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74  4 36 16;
    20 73 35 29 78 31 90  1 74 31 49 71 48 86 81 16 23 57  5 54;
     1 70 54 71 83 51 54 69 16 92 33 48 61 43 52  1 89 19 67 48
]


# 0.07859598s
function euler11a()
    len = size(m)[1]
    mt = m'
    n = 4
    maxp = 0

    # Rows and columns
    for row = 1:len-1
        for col in 1:len-n
            p1 = prod(m[row,col:col+n-1])
            if p1 > maxp
                maxp = p1
            end
            p2 = prod(mt[row,col:col+n-1])
            if p2 > maxp
                maxp = p2
            end
        end
    end

    # diag_down
    for j in 1:17
        for i in 1:17
            pa = []
            for a in 0:n-1
                push!(pa,m[a+i,a+j])
            end
            p = prod(pa)
            if p > maxp
                maxp = p
            end
        end
    end

    # diag_up
    for j in 2:17
        for i = 5:20
            pa = []
            for a = 0:3
                push!(pa,m[i-a,j+a])
            end
            p = prod(pa)
            if p > maxp
                maxp = p
            end
        end
    end

    return maxp;
end

run_euler(euler11a)



# 0.13744243s
function euler12b()
    len = 0
    i = 0
    tnum = 0
    while len <= 500
        i+=1
        tnum += i
        f = factors_map(tnum)
        len = prod(values(f).|>(i->i+1))
    end
    return tnum
end


#
# This is basically factors + collect together so it's not general
# enough for placing in Euler.jl
#
function factors_map(n)
    if n == 1
        return Dict(1=>1)
    end
    m = Dict();
    while n % 2 === 0
        if !haskey(m,2)
            m[2] = 0
        end
        m[2] += 1;
        n = round(Int, n/2);
    end
    t = 3
    while n > 1 && t < ceil(Int,sqrt(n))
        while n % t == 0
            if !haskey(m,t)
                m[t] = 0
            end
            m[t] += 1
            n /= t
        end
        t += 2
    end
    if n > 1
        if !haskey(m,n)
            m[n] = 0
        end
        m[n] += 1
    end
    return m;
end

# 0.13744243s
function euler12b()
    len = 0
    i = 0
    tnum = 0
    while len <= 500
        i+=1
        tnum += i
        f = factors_map(tnum)
        len = prod(values(f).|>(i->i+1))
    end
    return tnum
end
run_euler(euler12b);

ns =[37107287533902102798797998220837590246510135740250,
     46376937677490009712648124896970078050417018260538,
     74324986199524741059474233309513058123726617309629,
     91942213363574161572522430563301811072406154908250,
     23067588207539346171171980310421047513778063246676,
     89261670696623633820136378418383684178734361726757,
     28112879812849979408065481931592621691275889832738,
     44274228917432520321923589422876796487670272189318,
     47451445736001306439091167216856844588711603153276,
     70386486105843025439939619828917593665686757934951,
     62176457141856560629502157223196586755079324193331,
     64906352462741904929101432445813822663347944758178,
     92575867718337217661963751590579239728245598838407,
     58203565325359399008402633568948830189458628227828,
     80181199384826282014278194139940567587151170094390,
     35398664372827112653829987240784473053190104293586,
     86515506006295864861532075273371959191420517255829,
     71693888707715466499115593487603532921714970056938,
     54370070576826684624621495650076471787294438377604,
     53282654108756828443191190634694037855217779295145,
     36123272525000296071075082563815656710885258350721,
     45876576172410976447339110607218265236877223636045,
     17423706905851860660448207621209813287860733969412,
     81142660418086830619328460811191061556940512689692,
     51934325451728388641918047049293215058642563049483,
     62467221648435076201727918039944693004732956340691,
     15732444386908125794514089057706229429197107928209,
     55037687525678773091862540744969844508330393682126,
     18336384825330154686196124348767681297534375946515,
     80386287592878490201521685554828717201219257766954,
     78182833757993103614740356856449095527097864797581,
     16726320100436897842553539920931837441497806860984,
     48403098129077791799088218795327364475675590848030,
     87086987551392711854517078544161852424320693150332,
     59959406895756536782107074926966537676326235447210,
     69793950679652694742597709739166693763042633987085,
     41052684708299085211399427365734116182760315001271,
     65378607361501080857009149939512557028198746004375,
     35829035317434717326932123578154982629742552737307,
     94953759765105305946966067683156574377167401875275,
     88902802571733229619176668713819931811048770190271,
     25267680276078003013678680992525463401061632866526,
     36270218540497705585629946580636237993140746255962,
     24074486908231174977792365466257246923322810917141,
     91430288197103288597806669760892938638285025333403,
     34413065578016127815921815005561868836468420090470,
     23053081172816430487623791969842487255036638784583,
     11487696932154902810424020138335124462181441773470,
     63783299490636259666498587618221225225512486764533,
     67720186971698544312419572409913959008952310058822,
     95548255300263520781532296796249481641953868218774,
     76085327132285723110424803456124867697064507995236,
     37774242535411291684276865538926205024910326572967,
     23701913275725675285653248258265463092207058596522,
     29798860272258331913126375147341994889534765745501,
     18495701454879288984856827726077713721403798879715,
     38298203783031473527721580348144513491373226651381,
     34829543829199918180278916522431027392251122869539,
     40957953066405232632538044100059654939159879593635,
     29746152185502371307642255121183693803580388584903,
     41698116222072977186158236678424689157993532961922,
     62467957194401269043877107275048102390895523597457,
     23189706772547915061505504953922979530901129967519,
     86188088225875314529584099251203829009407770775672,
     11306739708304724483816533873502340845647058077308,
     82959174767140363198008187129011875491310547126581,
     97623331044818386269515456334926366572897563400500,
     42846280183517070527831839425882145521227251250327,
     55121603546981200581762165212827652751691296897789,
     32238195734329339946437501907836945765883352399886,
     75506164965184775180738168837861091527357929701337,
     62177842752192623401942399639168044983993173312731,
     32924185707147349566916674687634660915035914677504,
     99518671430235219628894890102423325116913619626622,
     73267460800591547471830798392868535206946944540724,
     76841822524674417161514036427982273348055556214818,
     97142617910342598647204516893989422179826088076852,
     87783646182799346313767754307809363333018982642090,
     10848802521674670883215120185883543223812876952786,
     71329612474782464538636993009049310363619763878039,
     62184073572399794223406235393808339651327408011116,
     66627891981488087797941876876144230030984490851411,
     60661826293682836764744779239180335110989069790714,
     85786944089552990653640447425576083659976645795096,
     66024396409905389607120198219976047599490197230297,
     64913982680032973156037120041377903785566085089252,
     16730939319872750275468906903707539413042652315011,
     94809377245048795150954100921645863754710598436791,
     78639167021187492431995700641917969777599028300699,
     15368713711936614952811305876380278410754449733078,
     40789923115535562561142322423255033685442488917353,
     44889911501440648020369068063960672322193204149535,
     41503128880339536053299340368006977710650566631954,
     81234880673210146739058568557934581403627822703280,
     82616570773948327592232845941706525094512325230608,
     22918802058777319719839450180888072429661980811197,
     77158542502016545090413245809786882778948721859617,
     72107838435069186155435662884062257473692284509516,
     20849603980134001723930671666823555245252804609722,
     53503534226472524250874054075591789781264330331690
]

# 0.01485372s
function euler13a()
    return parse(Int,string(sum(ns))[1:10])
end

run_euler(euler13a)


function hailstone(n)
    if n % 2 == 0
        return round(Int,n / 2)
    else
        return 3*n+1;
    end
end

# Using an array instead:
# 0.04143852s
function euler14c()
    limit = 1_000_000
    hash = zeros(limit)
    maxN = 0
    maxLen = 0
    for n = 2:1_000_000
        m = n
        mlen = 1
        while m > 1
            if m <= limit && hash[m] > 0
                mlen = hash[m]+mlen-1
                m = 1
            else
                m = hailstone(m)
                mlen += 1
            end
        end
        if hash[n] == 0
            hash[n] = mlen
        end
        if mlen > maxLen
            maxN = n
            maxLen = mlen
        end
    end
    return maxN;
end

run_euler(euler14c)

#=
function prodlist(from,to)
   return prod(from:to)
end
# 0.01229874s
function euler15a()
     return prodlist(BigInt(21),40) / prodlist(2,20);
end
run_euler(euler15a)
=#

function euler15b()
    return prod(BigInt(21):40) / prod(2:20);
  end
run_euler(euler15b)

# 0.00027433s
function euler16a()
    # return split(string(BigInt(2)^1000),"").|>(i->parse(Int,i))|>sum
    return digits(BigInt(2)^1000)|>sum
end

run_euler(euler16a);


function english(n)
    divs  =      [1_000_000_000, 1_000_000,  1_000,       100]
    divnames  =  ["billion", "million", "thousand", "hundred"]
    prefixes  =  ["0", "twen", "thir", "for", "fif", "six", "seven", "eigh", "nine"]
    _ordinals  = ["first", "second", "third", "fourth", "fifth", "sixth", "seventh",
                      "eighth", "ninth", "tenth", "eleventh", "twelfth", "thirteenth",
                      "fourteenth","fifteenth", "sixteenth", "seventeenth",
                      "eighteenth", "nineteenth"]
    cardinals =  ["one", "two", "three", "four", "five", "six", "seven",
                      "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen",
                      "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"];

    s = ""
    printed = 0
    if n < 0
        s = "minus" * s;
        n = -n
    end
    d = 0
    for i in 1:length(divs)
        d = floor(Int,n / divs[i])
        n %= divs[i]
        if d != 0
            s *= english(d) * divnames[i];
            printed = 1
        end
    end

    if n > 0 && printed == 1
        s *= "and";
    end
    if n == 0
        # dummy
    elseif n > 19
        d = floor(Int,n / 10)
        n %= 10
        s *= prefixes[d] * "ty" * english(n)
    else
        s *= cardinals[n]
    end

    return s
end

# 0.00138457s
function euler17a()
    return sum((1:1000).|>i->length(english(i)))
end

run_euler(euler17a)


# Recursive function for euler18()
function pp(row, col, sum, tri, max_val)
    if sum > max_val
        max_val = sum
    end
    row += 1
    if row <= length(tri)
        for i in 0:1
            max_val = pp(row, col + i, sum + tri[row][col + i], tri, max_val)
        end
    end
    return max_val
end

# 0.00015198s
function euler18a()
    triangle  =
         [[75],
         [95,64],
         [17,47,82],
         [18,35,87,10],
         [20, 4,82,47,65],
         [19, 1,23,75, 3,34],
         [88, 2,77,73, 7,63,67],
         [99,65, 4,28, 6,16,70,92],
         [41,41,26,56,83,40,80,70,33],
         [41,48,72,33,47,32,37,16,94,29],
         [53,71,44,65,25,43,91,52,97,51,14],
         [70,11,33,28,77,73,17,78,39,68,17,57],
         [91,71,52,38,17,14,91,43,58,50,27,29,48],
         [63,66, 4,68,89,53,67,30,73,16,69,87,40,31],
         [ 4,62,98,27,23, 9,70,98,73,93,38,53,60, 4,23]];
    max_val = 0
    max_val = pp(1, 1, triangle[1][1], triangle, max_val)
    
    return max_val
end

run_euler(euler18a)


using Dates

# 0.00001321s
function euler19a()
    count = 0;
    for year in 1901:2000
        for month in 1:12
            if Dates.dayofweek(Date(year, month, 1)) == 7
                count += 1
            end
        end
    end

    return count
end

run_euler(euler19a)


# 0.00029684s
function euler20a()
    return sum(split(factorial(BigInt(100))|>string,"").|>j->parse(Int,j))
end

run_euler(euler20a)



# 0.07385759s
function euler21c()
    n = 9999
    s = zeros(Int,n)
    for i in 2:n
        s[i] = sum(all_divisors3(i))
    end
    a = []
    for i in 2:n
        # ignore perfect numbers...
        # if s[i] <= n && i==s[s[i]] && i!= s[i]
        if s[i] <= n && i!= s[i] && i==s[s[i]]
            push!(a,i)
        end
    end
    return sum(a)
end

run_euler(euler21c);


function to_code(s)
    return [Int(s[i])-64 for i in 1:length(s)]
end

# 0.00231866s
function euler22a()
    words = sort(split(readline("euler22_names.txt"),","))
    a = 1;
    s = 0;
    for word in words
        word = replace(word,"\""=>"")
        s += a*sum(to_code(word));
        a += 1;
    end
    return s;

end

# 0.02438064s
function euler22b()
    a = 1
    return sum(map(word->(word=replace(word,"\""=>"");a=a+1; (a-1)*sum(to_code(word))
                   ),
                  sort(split(readline("euler22_names.txt"),","))))

end

run_euler(euler22a)


# 0.00790154s
function euler23a()
    limit = 20161;
    arr = ones(limit)
    for i in 2:limit
        for j in i*2:i:limit
            arr[j] = arr[j] + i
        end
    end

    abundant = [i for i in 12:limit if arr[i] > i]
    for a in abundant
        for b in abundant
            if b > a || a + b >= limit
                break;
            else
                arr[a + b] = 0
            end
        end
    end

    return sum([i for i in 1:limit if arr[i] != 0])
end

run_euler(euler23a)


#
function euler24a()
    a = Array(0:9)
    c = 1
    while c < 1000000
        a = next_permutation(a)
        c += 1
    end

    return parse(Int, join(a,""))
end
run_euler(euler24a)


function fib_len(n)
    return fibmemBigInt(n)|>string|>length
end

#
# Using some heuristics to find the upper limit
# (from my Picat solution).
# 0.08765257s
function euler25b()
    target = 1000;
    foundUpper = 0;
    i = 1;
    fibLen = 0;
    step = 43;
    # Get the upper limit
    while fibLen < target && foundUpper == 0
        fibLen = fib_len(step*i);
        if fibLen > target
            foundUpper = i
            break
        end
        i += 1
    end

    # Now check all numbers from Step*(FoundUpper-1) .. Step*FoundUpper
    # The target must be in that interval.
    f = step*(foundUpper-1)
    fibLen = fib_len(f);
    while fibLen < target && f <= step*foundUpper
        f += 1
        fibLen = fib_len(f)
    end

    return f
end

run_euler(euler25b)


function get_rep_len(n)
    foundRemainders = zeros(n)
    value = 1;
    position = 1;
    while foundRemainders[value] == 0 && value != 0
        foundRemainders[value] = position
        value = round(Int,1+((value*10) % n)) # added 1
        position += 1
    end
    return position-foundRemainders[value]
end



# Checks only primes
# 0.00048207s
function euler26b()
    maxLen = 0
    maxD = 0
    for d in 2:1000
        if isPrime(d)
            len = get_rep_len(d)
            if len > maxLen
                maxLen = len
                maxD = d
            end
        end
    end
    return maxD
end

run_euler(euler26b)

# 0.04097529s
# with isPrimeCached: 0.07010393s (slower)
function euler27a()
    t = 999
    bestLen = 0
    bestA = 0
    bestB = 0
    for a in -t:t
        for b in -t:t
            len = 0;
            pp = len^2 + a*len + b
            # while pp > 1 && isPrimeCached(pp) # slower
            while pp > 1 && isPrime(pp)
                len += 1
                pp = len^2 + a*len + b;
            end
            if len > bestLen
                bestLen = len
                bestA = a
                bestB = b
            end
        end
    end

    return(bestA * bestB)
end


run_euler(euler27a)


#
function euler28a()
    s = 1
    n = 3
    while n <= 1001
        s += 4*n^2 - 6*n+6
        n+=2
    end
    return s
end

run_euler(euler28a)


# 0.00789801s
function euler29a()
    min = 2
    max = 100
    h = Dict()
    for a = min:max
        for b = min:max
            h[BigInt(a)^BigInt(b)] = 1
        end
    end

    return keys(h)|>length
end
run_euler(euler29a)


# 0.07169755ss
function euler30a()
    t = 0
    m = 5
    for n in 10:6 * (9^5)
        # nn = (split(string(n),"").|>i->parse(Int,i)^m)|>sum
        nn = (digits(n).|>i->i^m)|>sum
        if n === nn
            t += n
        end
    end
    return t
end

run_euler(euler30a)

# DP approach
function coins(c,money,m)
    len = length(c)
    if m == len
        return 1
    end

    sum1 = 0
    for i in m:len
        if money - c[i] == 0
            sum1 += 1
        end
        if  money - c[i] > 0
            sum1 += coins(c, money-c[i], i)
        end
    end
    return sum1;
end

# 0.00058022s
function euler31a()
    c = [200,100,50,20,10,5,2,1]
    return coins(c, 200, 1)
end

run_euler(euler31a)


# 0.17556319s
function euler32a()
    prod_hash = Dict{Int,Int}()
    for a in 2:98
        as = string(a)
        if occursin("0",as) continue end
        for b in a+1:9876
            p = a*b
            if occursin("0",string(p)) continue end
            l = "$as$b$p"
            if length(l) == 9 && length(Set(l)) == 9 && !occursin("0",l)
                prod_hash[p] = 1
            end
        end
    end
    return keys(prod_hash)|>sum
end

GC.gc(true)
GC.gc(false)
run_euler(euler32a)
GC.gc(true)

# 0.00000079s
function euler33a()
    s = 1;
    for y in 1:9
        for z in 1:9
            x = 9.0*y*z/(10.0*y-z)
            if floor(x)==x && y/z < 1.0 && x < 10.0
                s = (s*y)/z;
            end
        end
    end

    return 1/s;
end

run_euler(euler33a)


function factorial_sum(n)
    # return (split(string(n),"").|>i->factorial(parse(Int,i)))|>sum
    return (digits(n).|>i->factorial(i))|>sum

end


# 0.01983716s
function euler34c()
    return [n for n in 10:100000 if n == factorial_sum(n)]|>sum
end

run_euler(euler34c)



# Rotate an array/string
function rotate(n,i)
    s = string(n)
    ii = i+1
    return parse(Int,join(vcat(s[ii:end],s[1:ii-1])))
end

#
# Note: It's a little faster when using prime_set as
#       a parameter for is_circular_prime than
#       relying of the global prime_set in the function.
#       That's surprising...
#
function is_circular_prime(n, prime_set)
    # s = split(string(n),"") # num_to_list(n)
    # len = length(s)
    len = nlen(n)
    v = 0
    for i = 1:len
        v = rotate(n,i)
        if !(v in prime_set)
            return false;
        end
    end
    return v in prime_set
end

# slightly slower than is_circular_prime
function is_circular_prime2(n, prime_set)
    s = split(string(n),"") # num_to_list(n)
    len = length(s)
    v = n
    for i = 2:len
        v = parse(Int,join(vcat([s[j] for j in i:len], [s[j] for j in 1:i-1]),""))
        if !(v in prime_set)
            return false;
        end
    end
    return v in prime_set
end

# Faster if global
prime_set = Set(primes(1_000_000))

# 0.08751774s
function euler35a()
    # prime_set = Set(primes(1_000_000))
    numCircularPrimes = 0
    for n in prime_set
        if is_circular_prime(n,prime_set)
            numCircularPrimes += 1
        end
    end

    return numCircularPrimes
end


run_euler(euler35a)


# 0.06355465s
function euler36a()
    s = 0
    for n in 1:999999
        if palindromic_number(n) &&
            palindromic_list(digits(n,base=2))
            # palindromic_list(string(n,base=2))
            s += n;
        end
    end
    return s
end

run_euler(euler36a)


# 0.00016527s
function euler38a()
    for n in 9876:-1:9
        println
        s = string(n)
        i = 2
        while length(s) < 9
            s *= string(n*i)
            i += 1
        end
        if length(s) == 9 && is_pandigital(s)
            return parse(Int,s)
        end
    end
    return nothing
end

run_euler(euler38a)


# using argmax for finding the max value
# 0.05935469s
function euler39b()
    n = 1000;
    # squares = Set((1:n).|>i->i*i) # slightly slower
    squares = Dict((1:n).|>i->i*i=>1)
    valid = []
    for x in keys(squares)
        for y in keys(squares)
            if x < y && haskey(squares,x+y) && (sqrt(x) + sqrt(x) + sqrt(x+y)) < 1000
                push!(valid,[x, y])
            end
        end
    end

    counts = Dict()
    for x in valid
        c = floor(Int,sqrt(x[1]) + sqrt(x[2]) + sqrt(x[1]+x[2]))
        get!(counts,c,0)
        counts[c] += 1
   end

    # find max count
    return argmax(counts)
end



run_euler(euler39b)

# 0.00941826s
function euler40a()
    i = 1
    dlen = 1
    p = 1
    index = 10; # Index = 10, 100, 1000, ..., 1000000
    while dlen <= 1000000
        i += 1
        istr = string(i)
        istrlen = length(istr)
        if dlen+istrlen>=index
            p *= parse(Int,istr[index-dlen])
            index *= 10
        end
        dlen += istrlen
    end

    return p
end

run_euler(euler40a)


# 0.03632750s
#=
function euler41a()
    # Simplification:
    # n=9 is not possible since 1+2+3+4+5+6+7+8+9=45 is divisible by 3
    #  n=8 is not possible since 1+2+3+4+5+6+7+8=36 is divisible by 3
    n = 7
    while n >= 4
        p = Array(1:n)
        for pp in sort(all_permutations(p),rev=true)
            v = parse(Int,join(pp,""))
            if isPrime(v)
                return v
            end
        end
        n += 1
    end

    return nothing
end

run_euler(euler41a)
=#
using Combinatorics
function euler41b()
    # Simplification:
    # n=9 is not possible since 1+2+3+4+5+6+7+8+9=45 is divisible by 3
    #  n=8 is not possible since 1+2+3+4+5+6+7+8=36 is divisible by 3
    n = 7
    m = 0
    while m == 0 && n >= 4
        p = Array(1:n)
        # for pp in sort(all_permutations(p),rev=true)
        for pp in sort(collect(permutations(p)),rev=true)
            v = parse(Int,join(pp,""))
            if isPrime(v)
                return v
            end
        end
        n += 1
    end

    return nothing
end
run_euler(euler41b)



# n'th triangle number
function triangle_number(n)
    return floor(Int,(n*(n-1)) / 2)
end

# get the score for a name
function get_score(name)
    return ([name[i] for i in 1:length(name)]
           .|>c->(Int(c)-64))|>sum
end

# 0.09908583s
function euler42a()
    t20 = Set((1:20).|>i->triangle_number(i))
    words = split( (readline("euler42_words.txt").|>word->replace(word,"\""=>"")),",")
    return (words
           .|>word-> get_score(word) in t20
           )|>sum
end

run_euler(euler42a)


# Using next_permutation
# 0.05306371s
function euler43b()
    primes = [2,3,5,7,11,13,17]
    p = [1,0,2,3,4,5,6,7,8,9]
    sum = 0
    rev = Array(9:-1:0)
     while p != rev
        i = 0
        found = true
        while i < 7 && found == true
            if (100*p[i+2] + 10*p[i+3] + p[i+4]) % primes[i+1] != 0
                found = false
                break
            end
            i += 1
        end

        if found
            sum += parse(Int,join(p,""))
        end
        p = next_permutation(p)
    end

    return sum
end



run_euler(euler43b)

# 0.04486643s
function euler44b()
    n = 2500
    s = (1:2500).|>n->floor(Int,n*(3*n-1) / 2)
    t = Set(s)
    d = 10_000_000
    for j in 1:n
        for k = 1:n
            if s[j] >= s[k]
                continue;
            end
            a = s[j]+s[k]
            if a >= d
                continue
            end
            b = abs(s[j]-s[k])
            if b >= d
                continue
            end
            if a in t && b in t
                d = b
                break

            end
        end
    end
    return d
end


run_euler(euler44b)


pent(n) = floor(Int, n*(3*n-1) / 2)
tri(n) = floor(Int, n*(n+1) / 2)
hex(n) = n*(2*n-1)

# 0.00013708s
function euler45a()
    t = 285+1
    tt = tri(t)
    p = 165
    pp = pent(p)
    h = 143
    hh = hex(h)
    while tt != pp || pp != hh
        t += 1
        tt = tri(t)
        if tt > pp p+=1; pp = pent(p) end
        if pp > hh h+=1; hh = hex(h) end
        if tt > hh h+=1; hh = hex(h) end
    end

    return tt
end

run_euler(euler45a)



# 0.00057412s
function euler46a()
    res = 0
    gotit = false
    for i in 3:2:10000
        if !isPrime(i) && !gotit
            s = round(Int,sqrt(i/2))
            found = 0
            for j in 1:s
                if found === 0
                    ts = j*j*2
                    if isPrime(abs(i-ts))
                        found = 1
                    end
                end
            end
            if found == 0
                res = i
                gotit = true
                break
            end
        end
    end
    return res
end

run_euler(euler46a)


# 0.01653001s
function euler47a()
    maxn = 1_000_000
    f = zeros(maxn)
    for i in 2:maxn-1
        if f[i] == 0
            for j in 2*i:i:maxn-1
                f[j] += 1
            end
        end
    end

    goal = [4,4,4,4]
    found = 0
    for i in 1:maxn-3
        if [f[i],f[i+1],f[i+2],f[i+3]] == goal
            found = i
            break
        end
    end
    return found
end

run_euler(euler47a)


# 0.00162826s
function euler48a()
    sum = 0
    t = 10_000_000_000
    for i = 1:1_000
        n = i
        for j in 2:i
            n = (n * i) % t
        end
        sum = (sum + n) % t
    end

    return sum
end

run_euler(euler48a)


function check_perms(n, diff)
    allperms = all_permutations2(digits(n))
    if length(allperms) > 0
        p1 = get_element(n, allperms, diff)
        if p1 !== nothing
            p2 = get_element(p1, allperms, diff)
            if p2 !== nothing
                return [n, p1, p2]
            end
        end
    end

    return nothing
end

function get_element(n, ll, diff)
    for p in ll
        pp = parse(Int,join(p,""))
        # if isPrime(pp) && pp > n && pp-n == diff
        if pp > n && pp-n == diff && isPrime(pp) 
            return pp
        end
    end
    return nothing
end

# 0.10001170s
function euler49a()
    diff = 3330
    res = 0
    for n in 1001:2:9999
        if n != 1487 && isPrime(n)
            c = check_perms(n, diff)
            if c !== nothing
                res = c
                break
            end
        end
    end

    return parse(Int,join(res,""))

end


run_euler(euler49a)


# 0.00700822s
function euler50a()
    n = 10000
    p = primes(n)
    for len in 550:-1:21
        for offset in 1:549
            pp = ((offset+1:offset+len).|>i->p[i])|>sum
            if pp < 1000000 && isPrime(pp)
                return pp
            end
        end
    end

    return nothing
end

run_euler(euler50a)

println("\nResult in reverse time order:")
for r in sort(res,by=x->x[2],rev=true) 
  println("$(r[1]): $(r[2])")
end

println("Total time: $(total_time[1])")