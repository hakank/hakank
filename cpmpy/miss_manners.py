"""
Miss Manners seating problem in cpmpy.

From http://4c110.ucc.ie/cpstandards/index.php/en/standards/java/examples/27
'''
The 'Miss Manners' problem is a notorious benchmark for rule engines. 
The problem is to find an acceptable seating arrangement for guests at 
a dinner party.  It should match people with the same hobbies (at 
least one), and to seat everyone next to a member of the opposite sex. 
'''

The data is presented in the Excel file: 
http://4c110.ucc.ie/cpstandards/files/Manners.xls

Also, see 
 - http://docs.codehaus.org/display/DROOLS/Miss+Manners+Example
 - http://blog.athico.com/2009/05/miss-manners-2009-yet-another-drools.html
 - http://it.toolbox.com/blogs/thinking-out-loud/industry-analysts-and-rules-engines-2349
   Refers to OPS5 benchmark suite: 
   ftp://ftp.cs.utexas.edu/pub/ops5-benchmark-suite/


Notes: 
- This model assumes a circular table placement.
- We don't care about the preferences (order) of the hobbies.


Here is a solution for problem1 (which happen to be optimal):

   Problem problem1:
   n: 16 num_hobbies: 3 n*num_hobbies: 48
   seating: [ 0  1  7 11  3 10  4  6  9 14  5 13  8 15  2 12]
   common_hobbies: [3 2 2 3 3 3 3 3 3 3 2 2 2 2 2 2]
   num_common_hobbies: 40
   Place   0 person:  0 gender:M hobbies:2 1 3      (common w next:3)
   Place   1 person:  1 gender:F hobbies:2 1 3      (common w next:2)
   Place   2 person:  7 gender:M hobbies:3 1        (common w next:2)
   Place   3 person: 11 gender:F hobbies:3 1 2      (common w next:3)
   Place   4 person:  3 gender:M hobbies:3 2 1      (common w next:3)
   Place   5 person: 10 gender:F hobbies:1 3 2      (common w next:3)
   Place   6 person:  4 gender:M hobbies:2 1 3      (common w next:3)
   Place   7 person:  6 gender:F hobbies:1 2 3      (common w next:3)
   Place   8 person:  9 gender:M hobbies:3 2 1      (common w next:3)
   Place   9 person: 14 gender:F hobbies:2 3 1      (common w next:3)
   Place  10 person:  5 gender:M hobbies:2 3 1      (common w next:2)
   Place  11 person: 13 gender:F hobbies:1 2        (common w next:2)
   Place  12 person:  8 gender:M hobbies:2 3 1      (common w next:2)
   Place  13 person: 15 gender:F hobbies:2 3        (common w next:2)
   Place  14 person:  2 gender:M hobbies:3 2        (common w next:2)
   Place  15 person: 12 gender:F hobbies:2 3        (common w next:2)
   
   num_common_hobbies: 40


   ExitStatus.OPTIMAL (0.26118746200000004 seconds)
   Nr solutions: 1
   Num conflicts: 1
   NumBranches: 1355
   WallTime: 0.26118746200000004



Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys, math,string
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *

def match_hobbies(hobbies_flat,h1,h2,num_hobbies,num_matches):
  """
  Match hobbies between two persons seating next to each other.
  Note that hobby 0 is a dummy value and should be ignored.
  """
  return [num_matches == sum([(Element(hobbies_flat,h1*num_hobbies+i) != 0) * 
                              (Element(hobbies_flat,h1*num_hobbies+i) ==
                               Element(hobbies_flat,h2*num_hobbies+j))
                              for i in range(num_hobbies)
                              for j in range(num_hobbies)
                              ])]


def miss_manners(problem,num_procs=1):
  n = problem["n"]
  gender = cpm_array(problem["gender"])
  num_hobbies = problem["num_hobbies"] # 0..num_hobbies
  run_type = problem["type"]
    
  hobbies = cpm_array(problem["hobbies"])
  hobbies_flat = cpm_array([hobbies[i,j] for i in range(n) for j in range(num_hobbies)])

  print("n:",n,"num_hobbies:",num_hobbies,"n*num_hobbies:",n*num_hobbies)

  # variables
  seating = intvar(0,n-1,shape=n,name="seating")
  # We require at least one common hobby between neighbours
  common_hobbies = intvar(1,num_hobbies,shape=n,name="common_hobbies")
  num_common_hobbies = intvar(n,n*num_hobbies,name="num_common_hobbies")

  # constraints
  model = Model([AllDifferent(seating),
                 num_common_hobbies == sum(common_hobbies),
                 seating[0] == 0, # symmetry breaking
                 ])

  for i in range(n):
    # mix gender
    model += [gender[seating[(i-1)%n]] != gender[seating[i%n]]]
    
    # match hobbies with the neighbous: there should at least be one common hobby
    model += [match_hobbies(hobbies_flat,seating[i%n],seating[(i+1)%n],num_hobbies,common_hobbies[i%n])]

  if run_type == "maximize":
    model.maximize(num_common_hobbies)

  def print_sol():
    n = len(seating)
    seating_val = seating.value()
    print("seating:",seating_val)
    common_hobbies_val = common_hobbies.value()
    print("common_hobbies:",common_hobbies_val)
    print("num_common_hobbies:",sum(common_hobbies))
    gender_s = ["M","F"]
    for i in range(n):
      si = seating_val[i]
      hs = " ".join([str(h) for h in hobbies[si] if h > 0])
      print(f"Place {i:3} person:{si:3} gender:{gender_s[gender[si]-1]} hobbies:{hs:10} (common w next:{common_hobbies[i]})")
    print()
    print("num_common_hobbies:",sum(common_hobbies_val))      
    print("\n")
    
    
  print("Search")
  ss = CPM_ortools(model)    

  # Flags to experiment with
  if num_procs > 1:
    ss.ort_solver.parameters.num_search_workers = num_procs
      
  # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
  # ss.ort_solver.parameters.search_branching = ort.AUTOMATIC_SEARCH
  # ss.ort_solver.parameters.search_branching = ort.FIXED_SEARCH
  # ss.ort_solver.parameters.cp_model_presolve = False
  ss.ort_solver.parameters.linearization_level = 0
  ss.ort_solver.parameters.cp_model_probing_level = 0
 
  num_solutions = ss.solveAll(solution_limit=1,display=print_sol)
  print("Nr solutions:", num_solutions)
  print("Num conflicts:", ss.ort_solver.NumConflicts())
  print("NumBranches:", ss.ort_solver.NumBranches())
  print("WallTime:", ss.ort_solver.WallTime())
  print()


#
# data
#

miss_manner_problems = {

  #
  # Data Guest guests16					
  # Name  Gender	Hobbies			
  # 		Hobby 1	Hobby 2	Hobby 3
  # 1	  m	2	1	3	
  # 2	  f	2	1	3	
  # 3	  m	3	2		
  # 4	  m 	3	2	1	
  # 5 	  m	2	1	3	
  # 6	  m	2	3	1	
  # 7	  f	1	2	3	
  # 8	  m	3	1		
  # 9	  m	2	3	1	
  # 10	  m	3	2	1	
  # 11	  f	1	3	2	
  # 12	  f	3	1	2	
  # 13	  f	2	3		
  # 14	  f	1	2		
  # 15	  f	2	3	1	
  # 16	  f	2	3		
  
  # Male = 1
  # Female = 2
  "problem1" : {
  "n" : 16,
  "type" : None, # "maximize",
  "gender" : [1,2,1,1,1,1,2,1,1,1,2,2,2,2,2,2],
  "num_hobbies" :3,
  # Note: Hobby 0 is a dummy. The proper hobbies are 1..num_hobbies.
  "hobbies" : [[2,1,3],
               [2,1,3],
               [0,3,2],
               [3,2,1],
               [2,1,3],
               [2,3,1],
               [1,2,3],
               [3,1,0],
               [2,3,1],
               [3,2,1],
               [1,3,2],
               [3,1,2],
               [2,3,0],
               [1,2,0],
               [2,3,1],
               [2,3,0]]
  },

"problem2" : {
  # Male = 1
  # Female = 2
  "n" : 64,
  "type" : None,
  "gender" : [1,2,1,1,1,1,2,1,1,1,1,2,1,1,1,2,2,1,2,2,1,1,2,2,2,1,2,
              1,2,2,1,1,1,2,2,1,1,2,1,2,1,1,1,1,1,2,1,2,1,1,2,1,2,2,
              2,2,2,2,2,2,2,2,2,2], 
  "num_hobbies": 3,
  "hobbies" : [[2,1,3],
               [2,1,3],
               [3,2,0],
               [3,2,1],
               [2,1,3],
               [2,3,1],
               [1,2,3],
               [3,1,0],
               [2,3,1],
               [3,2,1],
               [1,3,2],
               [3,1,2],
               [2,3,0],
               [1,2,0],
               [2,3,1],
               [2,3,0],
               [3,2,0],
               [1,3,2],
               [3,1,0],
               [1,3,2],
               [2,3,0],
               [2,3,0],
               [1,2,0],
               [3,1,2],
               [3,1,2],
               [2,1,3],
               [2,3,1],
               [1,2,0],
               [2,3,1],
               [2,1,3],
               [1,2,3],
               [1,2,0],
               [2,3,1],
               [2,1,3],
               [2,3,0],
               [2,1,0],
               [2,1,0],
               [1,3,2],
               [3,1,2],
               [1,2,3],
               [2,1,3],
               [3,1,0],
               [1,3,2],
               [3,1,2],
               [1,2,0],
               [1,2,3],
               [1,2,0],
               [3,2,0],
               [3,2,0],
               [2,3,0],
               [2,1,3],
               [1,2,3],
               [2,1,0],
               [1,2,3],
               [1,2,3],
               [2,1,3],
               [3,2,1],
               [3,1,2],
               [1,2,3],
               [3,1,0],
               [3,2,1],
               [2,3,0],
               [3,1,2],
               [3,2,0]]
},

  
  # Data Guest guests128						
  # Male = 1
  # Female = 2
  "problem3" : {
  "n" : 128,
  "type" : None,  
  "gender" : [1,2,2,1,1,2,2,1,1,1,1,2,2,1,1,1,1,2,1,1,1,1,2,2,1,1,1,2,2,1,
              1,2,2,1,2,1,2,2,1,1,2,1,1,2,1,1,1,2,1,1,1,2,2,2,1,1,2,1,2,2,
              1,2,2,2,1,2,2,1,1,2,1,1,2,2,2,2,1,1,2,1,1,1,2,1,2,1,1,1,2,1,
              2,2,2,2,2,1,2,1,2,1,2,2,1,1,2,2,2,2,2,1,2,2,2,1,1,1,2,2,1,1,
              2,1,1,2,2,2,2,2], 
  "num_hobbies" : 5,
  "hobbies" : [[2,3,1,4,5],
               [3,2,1,4,5],
               [5,4,2,0,0],
               [3,2,1,4,0],
               [2,5,3,0,0],
               [1,4,2,5,3],
               [1,2,3,5,0],
               [3,5,0,0,0],
               [3,5,2,4,0],
               [2,3,4,5,1],
               [2,4,5,1,0],
               [3,5,2,0,0],
               [5,1,0,0,0],
               [3,5,1,4,0],
               [5,2,1,3,4],
               [2,5,4,0,0],
               [4,3,0,0,0],
               [2,4,5,1,0],
               [5,2,0,0,0],
               [2,3,0,0,0],
               [4,3,2,1,0],
               [1,2,4,0,0],
               [4,5,2,0,0],
               [4,3,2,1,5],
               [4,5,0,0,0],
               [1,2,0,0,0],
               [3,5,2,0,0],
               [4,1,3,2,5],
               [3,5,0,0,0],
               [2,1,0,0,0],
               [3,1,4,0,0],
               [2,1,5,0,0],
               [5,4,0,0,0],
               [4,5,2,0,0],
               [3,1,4,5,2],
               [1,4,2,3,0],
               [4,2,1,0,0],
               [3,1,2,4,0],
               [5,2,0,0,0],
               [2,3,0,0,0],
               [5,3,0,0,0],
               [5,4,0,0,0],
               [3,4,5,0,0],
               [2,4,3,1,0],
               [2,4,0,0,0],
               [4,1,5,0,0],
               [2,5,4,0,0],
               [3,1,0,0,0],
               [1,5,4,3,2],
               [2,4,3,0,0],
               [1,2,5,4,0],
               [5,3,1,4,0],
               [5,1,4,2,3],
               [1,3,0,0,0],
               [2,4,0,0,0],
               [2,3,0,0,0],
               [2,1,4,0,0],
               [5,3,2,1,4],
               [2,3,5,0,0],
               [4,2,3,5,0],
               [1,2,4,3,0],
               [3,2,5,4,1],
               [2,3,4,1,5],
               [2,4,1,5,3],
               [5,3,2,1,4],
               [5,3,4,0,0],
               [1,4,0,0,0],
               [4,2,1,5,0],
               [3,4,1,0,0],
               [3,5,1,4,2],
               [3,1,4,0,0],
               [4,3,1,0,0],
               [4,1,3,0,0],
               [1,4,2,5,3],
               [4,2,0,0,0],
               [5,4,3,2,1],
               [1,5,0,0,0],
               [5,1,0,0,0],
               [1,3,4,2,5],
               [5,1,2,4,0],
               [4,1,0,0,0],
               [3,4,1,2,5],
               [1,3,4,0,0],
               [3,4,0,0,0],
               [4,1,0,0,0],
               [3,4,0,0,0],
               [5,4,2,3,0],
               [2,5,3,1,4],
               [5,2,4,1,0],
               [2,4,5,0,0],
               [2,3,5,1,4],
               [3,1,2,5,0],
               [3,1,2,0,0],
               [2,5,0,0,0],
               [3,4,0,0,0],
               [1,4,2,5,3],
               [4,2,5,0,0],
               [5,4,0,0,0],
               [2,5,0,0,0],
               [2,3,5,4,0],
               [2,1,5,3,4],
               [2,1,5,3,4],
               [2,1,4,3,5],
               [5,3,4,1,2],
               [4,2,0,0,0],
               [4,5,1,0,0],
               [3,1,2,5,4],
               [4,5,1,2,3],
               [1,2,0,0,0],
               [5,4,1,2,0],
               [3,2,0,0,0],
               [4,3,5,2,1],
               [1,3,5,2,0],
               [3,2,0,0,0],
               [2,1,5,4,0],
               [3,4,5,1,0],
               [4,5,2,0,0],
               [2,4,3,0,0],
               [5,3,0,0,0],
               [5,1,3,2,4],
               [4,2,3,5,0],
               [1,2,5,0,0],
               [2,3,1,5,0],
               [2,3,4,0,0],
               [3,5,2,4,0],
               [2,3,1,4,0],
               [5,1,0,0,0],
               [2,4,1,3,0]]
  }

}

num_procs = 1
for p in miss_manner_problems:
  print(f"Problem {p}:")
  miss_manners(miss_manner_problems[p],num_procs)
