"""
Run Nonogram instances in cpmpy.

Here we run the Nonogram solver
- nonogram_regular.py
using my decomposition of regular constraint.

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

  nonogram_pbn_9_dom.py 1.5776850260000002
  nonogram_pbn_bucks.py 0.45897365
  nonogram_pbn_cat.py 0.214733843
  nonogram_pbn_dancer.py 0.013306874000000002
  nonogram_pbn_dicap.py 60
  nonogram_pbn_dom02.py 0.004767429
  nonogram_pbn_dom03.py 0.009985898
  nonogram_pbn_dom04.py 0.017959511
  nonogram_pbn_dom05.py 0.039782479
  nonogram_pbn_dom06.py 0.109919971
  nonogram_pbn_dom07.py 0.278626097
  nonogram_pbn_dom08.py 0.6938781890000001
  nonogram_pbn_dom09.py 1.4503053250000002
  nonogram_pbn_dom10.py 2.678018911
  nonogram_pbn_dom11.py 5.868125662000001
  nonogram_pbn_dom12.py 11.361352364
  nonogram_pbn_dom13.py 15.169244821000001
  nonogram_pbn_dom14.py 25.367685758
  nonogram_pbn_dom15.py 46.410164185000006
  nonogram_pbn_dom16.py 60
  nonogram_pbn_edge.py 0.021832844
  nonogram_pbn_flag.py 3.070413675
  nonogram_pbn_forever.py 0.581684561
  nonogram_pbn_hot.py 15.471502491
  nonogram_pbn_karate.py 4.533007863
  nonogram_pbn_knot.py 2.664306533
  nonogram_pbn_light.py 2.414399834
  nonogram_pbn_lion.py 26.995613395000003
  nonogram_pbn_m_and_m.py 13.968577192000001
  nonogram_pbn_marley.py 60
  nonogram_pbn_merka.py 14.986498651000002
  nonogram_pbn_mum.py 3.472380799
  nonogram_pbn_nature.py 47.024716492
  nonogram_pbn_petro.py 2.7167153020000003
  nonogram_pbn_signed.py 16.545099849
  nonogram_pbn_skid.py 0.204662161
  nonogram_pbn_smoke.py 0.343414425
  nonogram_pbn_swing.py 4.3543669540000005
  nonogram_pbn_tragic.py 10.474810564
  
  Total time: 461.56851957799995

As a comparison, here are the total time when running the OR-tools CP-SAT model
with AddAutomaton and a regular decomposition using AddAllowedAssignements:
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
timeout=60 # seconds

print("Timeout is ",timeout,"seconds.\n")
print("nonogram_regular_py:")
times_regular, total_times_regular = main(instances,"nonogram_regular.py",timeout)
# print("\nnonogram_table_sat_py:")
# times_table, total_times_table = main(instances,"nonogram_table_sat.py",timeout)
# times_regular, total_times_regular = main(instances,"nonogram_table_sat.py regular",timeout)
print("\n\nSummary:")
print("Total time regular: %-4fs" % (total_times_regular))
# print("Total time automaton: %-4fs" % (total_times_automaton))
# print("Total time table    : %-4fs" % (total_times_table))
# print("Instance                       automaton   table")
print("Instance                       regular")
print("-------------------------------------------------")
for inst in instances:
    # print("%-30s %3f %3f" % (inst,times_automaton[inst],times_table[inst]))
    print("%-30s %3f" % (inst,times_regular[inst]))    
    print()
    
