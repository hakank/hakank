"""
Run Nonogram instances in cpmpy.

Here we run the Nonogram solver testing two programs:
- nonogram_regular.py
- nonogram_regular_table.py

using my decompositions of the regular constraints:

- nonogram_regular.py: regular(.) using Element constraint
- nonogram_regular_table.py: regular_table(.) using Element constraint

Most of the instances are available at http://hakank.org/cpmpy/ 
(search for 'nonogram') and 
https://github.com/hakank/hakank/tree/master/cpmpy

However, due to copyright issues, some of the tested instances 
are not publicly available, so you have to take my word for 
the timings on these...

Here's the times running on my Intel i9-7940X 3.10GHz machine
(Linux Ubuntu 18.05LTS) with a timeout of 60s, representing
by a time of 60. Note that we try to prove unicity, i.e.
try to find two solutions. However, some of the instances
yield many solutions so we actually find two solutions.


Summary:
Total time element: 460.654733s
Total time table  : 431.218168s
(Sigificant winner is noted by '(*)')
Instance                       element       table
-------------------------------------------------
nonogram_pbn_9_dom.py          1.461557      0.828898(*)
nonogram_pbn_bucks.py          0.460687      0.467773
nonogram_pbn_cat.py            0.200635      0.202767
nonogram_pbn_dancer.py         0.014467      0.012881
nonogram_pbn_dicap.py         51.199928     17.500933(*)
nonogram_pbn_dom02.py          0.004956      0.004861
nonogram_pbn_dom03.py          0.009600      0.009799
nonogram_pbn_dom04.py          0.018164      0.016369
nonogram_pbn_dom05.py          0.039823      0.028205(*)
nonogram_pbn_dom06.py          0.105373      0.048520(*)
nonogram_pbn_dom07.py          0.286775      0.124454(*)
nonogram_pbn_dom08.py          0.724301      0.348568(*)
nonogram_pbn_dom09.py          1.466364      0.803432(*)
nonogram_pbn_dom10.py          2.729152      2.337130(*)
nonogram_pbn_dom11.py          5.933186      5.528392(*)
nonogram_pbn_dom12.py         11.496285     10.853212(*)
nonogram_pbn_dom13.py         16.23774(*)   48.280493
nonogram_pbn_dom14.py         27.28729(*)   53.526148
nonogram_pbn_dom15.py         48.66220(*)   60.000000
nonogram_pbn_dom16.py         60.000000     60.000000
nonogram_pbn_edge.py           0.022070      0.020478
nonogram_pbn_flag.py           3.063539(*)   3.446010
nonogram_pbn_forever.py        0.581731(*)   0.691116
nonogram_pbn_hot.py           15.202172(*)  26.100611
nonogram_pbn_karate.py         4.533341(*)   7.891860
nonogram_pbn_knot.py           2.666965      1.611962(*)
nonogram_pbn_light.py          2.480308      1.085201(*)
nonogram_pbn_lion.py          25.874616     21.199755(*)
nonogram_pbn_m_and_m.py       14.214592      6.992902(*)
nonogram_pbn_marley.py        60.000000     30.930257(*)
nonogram_pbn_merka.py         15.173762     11.846804(*)
nonogram_pbn_mum.py            3.540290      3.478335
nonogram_pbn_nature.py        49.091343     32.298906(*)
nonogram_pbn_petro.py          2.781776      1.641115(*)
nonogram_pbn_signed.py        16.917035      8.708115(*)
nonogram_pbn_skid.py           0.216577      0.171637(*)
nonogram_pbn_smoke.py          0.357113      0.259609(*)
nonogram_pbn_swing.py          4.377736      3.744366(*)
nonogram_pbn_tragic.py         11.22127      8.176292(*)
                               -------------------------
Winnings:                      7             23

As we can see, nonogram_regular_table is faster than nonogram_regular
on many of the problem instances, but there are cases when nonogram_regular
is better.

Both have 2 timeouts (60s) but not exactly the same instances:
 * nonogram_regular.py: nonogram_pbn_dom16 and nonogram_pbn_marley
 * nonogram using table constraint: nonogram_pbn_dom15.py nonogram_pbn_dom16.p
 

As a comparison, here are the total time when running the OR-tools CP-SAT model
with AddAutomaton and a regular decomposition using AddAllowedAssignements (table
constraint):
      Total time automaton: 211.350252s
      Total time table    : 731.020601s

See http://hakank.org/or_tools/run_nonogram_sat.py for details about these
benchmarks.

For more about benchmarking Nonogram solvers, see 
- https://webpbn.com/survey/
  (last updated in 2013) which includes some of my old 
  Nonogram solvers written in MiniZinc.


Model created by Hakan Kjellerstrand, hakank@hakank.com
See also my cpmpy page: http://www.hakank.org/cpmpy/

"""
import sys
import numpy as np
from cpmpy import *
from cpmpy.solvers import *
from cpmpy_hakank import *
import time, os, subprocess, re



def main(instances, solver="nonogram_regular.py",timeout=10):
  """
  Run the Nonogram instances using the solver `solver` 
  with a timeout of `timeout`.
  """
  print("Solver:", solver,flush=True)
  walltime_re = re.compile("WallTime: (.+)\n")
  times = {}
  total_time = 0
  for inst in instances:
    print(inst,flush=True)
    command = f"time /usr/bin/timeout {timeout} python3 {solver} {inst}  2>&1 "
    print("command:",command,flush=True)
    res = subprocess.getoutput(command)
    print("res:", res)
    walltime = walltime_re.search(res)
    if walltime != None:
      print("walltime:", walltime.group(1))
      t = float(walltime.group(1))
    else:
      print("Timeout")
      t = timeout
    times[inst] = t
    total_time += float(t)

    print(flush=True)
    
  for inst in times.keys():
    print(inst,times[inst])

  print("Total time:", total_time)

  return times, total_time

# All instances.
# However, all might not be available so we'll weed out those not available below.
instances_all = [
  "nonogram_pbn_9_dom.py",
  "nonogram_pbn_bucks.py",
  "nonogram_pbn_cat.py",
  "nonogram_pbn_dancer.py",
  "nonogram_pbn_dicap.py",
  "nonogram_pbn_dom02.py",
  "nonogram_pbn_dom03.py",
  "nonogram_pbn_dom04.py",
  "nonogram_pbn_dom05.py",
  "nonogram_pbn_dom06.py",
  "nonogram_pbn_dom07.py",
  "nonogram_pbn_dom08.py",
  "nonogram_pbn_dom09.py",
  "nonogram_pbn_dom10.py",
  "nonogram_pbn_dom11.py",
  "nonogram_pbn_dom12.py",
  "nonogram_pbn_dom13.py",
  "nonogram_pbn_dom14.py",
  "nonogram_pbn_dom15.py",
  "nonogram_pbn_dom16.py",
  "nonogram_pbn_edge.py",
  "nonogram_pbn_flag.py",
  "nonogram_pbn_forever.py",
  "nonogram_pbn_hot.py",
  "nonogram_pbn_karate.py",
  "nonogram_pbn_knot.py",
  "nonogram_pbn_light.py",
  "nonogram_pbn_lion.py",
  "nonogram_pbn_m_and_m.py",
  "nonogram_pbn_marley.py",
  "nonogram_pbn_merka.py",
  "nonogram_pbn_mum.py",
  "nonogram_pbn_nature.py",
  "nonogram_pbn_petro.py",
  "nonogram_pbn_signed.py",
  "nonogram_pbn_skid.py",
  "nonogram_pbn_smoke.py",
  "nonogram_pbn_swing.py",
  "nonogram_pbn_tragic.py",
]
instances = [inst for inst in instances_all if os.path.isfile(inst)]
timeout = 60 # seconds

print("Timeout is ",timeout,"seconds.\n")
print("nonogram_regular_py:")
times_element, total_times_element = main(instances,"nonogram_regular.py",timeout)
print("\nnonogram_table_py:")
times_table, total_times_table = main(instances,"nonogram_regular_table.py",timeout)

# times_table, total_times_table = main(instances,"nonogram_table_sat.py",timeout)
# times_regular, total_times_regular = main(instances,"nonogram_table_sat.py regular",timeout)
print("\n\nSummary:")
print("Total time element: %-4fs" % (total_times_element))
print("Total time table    : %-4fs" % (total_times_table))
print("Instance                       element  table")
print("-------------------------------------------------")
for inst in instances:
    print("%-30s %3f %3f" % (inst,times_element[inst],times_table[inst]))
    
