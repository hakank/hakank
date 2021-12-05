"""
Reducing overlapping intervals in CPMpy.

From https://stackoverflow.com/questions/70036071/how-can-i-get-the-cartesian-product-of-a-set-of-intervals-with-no-overlapping
'''
How can I get the cartesian product of a set of intervals with no overlapping?

given a dictionary with a set of intervals:

    intervals = {'561801/03/08': [[1081, 1156], [1141, 1216], [1201, 1276], [1741, 1816], [1801, 1876], [1861, 1936], [1921, 1996], [1981, 2056], [2041, 2116]],
                 '563301/03/08': [[1170, 1250], [1230, 1310], [1770, 1850], [1830, 1910], [1890, 1970], [1950, 2030], [2010, 2090], [2070, 2150], [2130, 2210]],
                 '688002/03/08': [[1790, 1850], [1850, 1910], [1910, 1970], [1970, 2030], [2090, 2150], [2150, 2210], [2210, 2270], [2270, 2330], [2330, 2390], [2390, 2450], [2450, 2510], [2510, 2570], [2570, 2630], [2630, 2690], [2690, 2750]],
                 '690102/03/08': [[1900, 1960], [1960, 2020], [2020, 2080], [2080, 2140], [2200, 2260], [2260, 2320], [2320, 2380], [2380, 2440], [2440, 2500], [2500, 2560], [2560, 2620], [2620, 2680], [2680, 2740]],
                 '559402/03/08': [[2015, 2090], [2075, 2150], [2135, 2210], [2195, 2270], [2255, 2330], [2315, 2390], [2375, 2450], [2435, 2510], [2495, 2570], [2555, 2630], [2615, 2690], [2675, 2750]],
                 '561302/03/08': [[2310, 2390], [2370, 2450], [2430, 2510], [2490, 2570], [2550, 2630], [2610, 2690], [2670, 2750]],
                 '572602/03/08': [[2435, 2505], [2495, 2565], [2555, 2625], [2615, 2685], [2675, 2745]],
                 '572502/03/08': [[2560, 2640], [2620, 2700]]}

the cartesian product can be obtained using:

  prod = itertools.product(*intervals)

the size of this cartesian product is 9915131275*2 = 13,267,800

I wish to reduce it by not allowing combinations where two or more domains overlap. This combination is OK:

    [1081, 1156], [1170, 1250], [1790, 1850], [1900, 1960], [2015, 2090], [2310, 2390], [2435, 2505], [2560, 2640] OK

This combination is not OK

    [1141, 1216], [1170, 1250], [1790, 1850], [1900, 1960], [2015, 2090], [2310, 2390], [2435, 2505], [2560, 2640] not OK

and any further combinations starting with:

    [1141, 1216], [1170, 1250]

should not be considered. This excludes 15131275*2 = 163,800 combinations The purpose is to reduce 
significantly the size of the cartesian product, to only have intervals that do not overlap.
'''

This model selects exactly one interval from each interval list and accepts only those combinations
that have no interval.

There are 12201 solutions. Here are some of them:

sol #1
[7 7 2 4 5 2 4 0]
[[1981, 2056], [2070, 2150], [1910, 1970], [2200, 2260], [2315, 2390], [2430, 2510], [2675, 2745], [2560, 2640]]


sol #2
[7 7 0 4 5 2 4 0]
[[1981, 2056], [2070, 2150], [1790, 1850], [2200, 2260], [2315, 2390], [2430, 2510], [2675, 2745], [2560, 2640]]


sol #3
[7 7 1 4 5 2 4 0]
[[1981, 2056], [2070, 2150], [1850, 1910], [2200, 2260], [2315, 2390], [2430, 2510], [2675, 2745], [2560, 2640]]

...

sol #12200
[4 8 2 5 0 1 1 1]
[[1801, 1876], [2130, 2210], [1910, 1970], [2260, 2320], [2015, 2090], [2370, 2450], [2495, 2565], [2620, 2700]]


sol #12201
[6 8 1 5 0 1 1 1]
[[1921, 1996], [2130, 2210], [1850, 1910], [2260, 2320], [2015, 2090], [2370, 2450], [2495, 2565], [2620, 2700]]

ExitStatus.OPTIMAL (3.58176202 seconds)
Nr solutions: 12201
Num conflicts: 302
NumBranches: 135035
WallTime: 3.58176202


Cf with the Picat model http://hakank.org/picat/reduce_overlapping_intervals.pi

This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
See also my cpmpy page: http://hakank.org/cpmpy/

"""
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import * # See http://hakank.org/cpmpy/cpmpy_hakank.py

# For testing the solutions
def has_overlaps(a):
    """
    has_overlaps(a)

    Returns True if any pair of the intervals overlaps, otherwise return False.    
    Assunption: This assumes that the intervals are in increasing order.
    """
    n = len(a)
    for i in range(1,n):
        if a[i][0] > a[i-1][0] and a[i][0] < a[i-1][1]:
            print("overlaps:", a[i-1],"and",a[i])
            return True 
    return False


def print_solution(a):
    """
    Print the solution.
    """
    # The selected intervals, as indices in each interval list
    xval = a[0].value()
    n = len(xval)
    print(xval)
    # The selected intervals, as intervals
    sols = [intervals[i][xval[i]] for i in range(n)]
    print(sols)
    
    # Check the solution (with a sorted intervals list)
    # if not has_overlaps(sorted(sols)):
    #     print("OK")
    # else:
    #     print("NOT OK!")
    print(flush=True)
    

#
# Note: intervals is a list of list of intervals (not a dictionary)
#
def reduce_overlaps(intervals):

    # Convert the list of intervals to a list of flattened lists
    # to be able to use Element below.
    intervals_flatten = []
    for interval in intervals:
        intervals_flatten.append(cpm_array(flatten_lists(interval)))
    # intervals_flatten = cpm_array(intervals_flatten)
    
    # We need all values to create the domains of the selected interval values
    all_values = flatten_lists(intervals_flatten)
    max_val = max(all_values)
    min_val = min(all_values)
    
    n = len(intervals)
    lens = [len(interval) for interval in intervals]

    # x[i] is the selected interval for the i'th interval list
    x = intvar(0,max(lens),shape=n,name="x")

    model = Model()
    
    # Reduce the domain (the possible values) of each interval list
    # (since they have different lengths)
    for i in range(n):
        model += [x[i] < lens[i]]

    #
    # Main constraints:
    #  - Pick exactly one of the intervals from each intervals list
    #  - Ensure that there are no overlaps between any of selected intervals.
    #

    # starts[i] is the start value of the i'th selected interval
    starts = intvar(min_val,max_val,shape=n,name="starts")
    # ends[i] is the end value of the i'th selected interval    
    ends   = intvar(min_val,max_val,shape=n,name="ends")

    # get the values of the selected intervals
    for i in range(n):
        # Use Element to obtain the start and end values of the selected interval.
        # We have to use the following construct with Element since CPMPy does not (yet) support this syntax:
        #    starts[i] = intervals[x[i],0]
        #    ends[i]   = intervals[x[i],1]
        model += [starts[i] == Element(intervals_flatten[i],x[i]*2+0), # corresponds to: starts[i] = intervals[x[i],0]
                  ends[i]   == Element(intervals_flatten[i],x[i]*2+1), # corresponds to: ends[i]   = intervals[x[i],1]
                  ]

    # Ensure that the i'th selected interval don't overlap with
    # rest of the intervals (the j'th interval)
    for i in range(n):
        for j in range(i+1,n):
            
            # Either j starts after i OR i starts after j
            # Note: for some reason this constraint don't work as expected.
            # model += [(ends[j] > starts[i])
            #             |
            #           (ends[i] > starts[j])
            #           ]
            
            # Ensure that the start value of one interval is not inside the other interval
            model += [~( (starts[i] >= starts[j]) & (starts[i] <= ends[j])),
                      ~( (starts[j] >= starts[i]) & (starts[j] <= ends[i])) ]


    # Print all solutions.
    # ortools_wrapper2 is defined in http://hakank.org/cpmpy/cpmpy_hakank.py
    # ortools_wrapper2(model,[x],print_solution)
    # Here we collect all solutions
    solutions = []
    def get_solution(a):
        xval = a[0].value()
        sol = [intervals[i][xval[i]] for i in range(n)]
        # print("sol:",sol)        
        solutions.append(sol)
    ortools_wrapper2(model,[x],get_solution)
        
    return np.array(solutions)

intervals_dict = {
    '561801/03/08': [[1081, 1156], [1141, 1216], [1201, 1276], [1741, 1816], [1801, 1876], [1861, 1936], [1921, 1996], [1981, 2056], [2041, 2116]],
    '563301/03/08': [[1170, 1250], [1230, 1310], [1770, 1850], [1830, 1910], [1890, 1970], [1950, 2030], [2010, 2090], [2070, 2150], [2130, 2210]],
    '688002/03/08': [[1790, 1850], [1850, 1910], [1910, 1970], [1970, 2030], [2090, 2150], [2150, 2210], [2210, 2270], [2270, 2330], [2330, 2390], [2390, 2450], [2450, 2510], [2510, 2570], [2570, 2630], [2630, 2690], [2690, 2750]],
    '690102/03/08': [[1900, 1960], [1960, 2020], [2020, 2080], [2080, 2140], [2200, 2260], [2260, 2320], [2320, 2380], [2380, 2440], [2440, 2500], [2500, 2560], [2560, 2620], [2620, 2680], [2680, 2740]],
    '559402/03/08': [[2015, 2090], [2075, 2150], [2135, 2210], [2195, 2270], [2255, 2330], [2315, 2390], [2375, 2450], [2435, 2510], [2495, 2570], [2555, 2630], [2615, 2690], [2675, 2750]],
    '561302/03/08': [[2310, 2390], [2370, 2450], [2430, 2510], [2490, 2570], [2550, 2630], [2610, 2690], [2670, 2750]],
    '572602/03/08': [[2435, 2505], [2495, 2565], [2555, 2625], [2615, 2685], [2675, 2745]],
    '572502/03/08': [[2560, 2640], [2620, 2700]]
    }

# Convert to a list of lists since this is needed for the output
intervals = [intervals_dict[a] for a in intervals_dict]
solutions = reduce_overlaps(intervals)
print("Solutions:",solutions)
print("Num solutions:",len(solutions))

