# Copyright 2021 Hakan Kjellerstrand hakank@gmail.com
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
"""

  Run Nonogram instances OR-tools CP-SAT Solver.

  Here we compare the two Nonogram solvers
    - nonogram_automaton_sat.py using AddAutomaton
    - nonogram_table_sat.py which use a decomposition of 
      MiniZinc style regular using AddAllowedAssignements.
  
  We would expect that the former solver is faster since it
  use the built-in AddAutomaton instead of a decomposition, 
  but how much faster?

  Most of the instances are available at http://hakank.org/or_tools/ 
  (search for "nonogram") and 
  https://github.com/hakank/hakank/tree/master/google_or_tools

  However, due to copyright issues, some of the tested instances 
  are not publicly available, so you have to take my word for 
  the timings on these...

  Here's the times running on my Intel i9-7940X 3.10GHz machine
  (Linux Ubuntu 18.05LTS) with a timeout of 60s, representing
  by a time of 60 in the result below.

      Summary:
      Total time automaton: 211.350252s
      Total time table    : 731.020601s
      Instance                       automaton   table
      -------------------------------------------------
      nonogram_pbn_9_dom.py          0.872295 33.336652
      nonogram_pbn_bucks.py          0.018069 0.101534
      nonogram_pbn_cat.py            0.004519 0.055420
      nonogram_pbn_dancer.py         0.000617 0.003696
      nonogram_pbn_dicap.py          0.205826 2.761004
      nonogram_pbn_dom02.py          0.000345 0.001544
      nonogram_pbn_dom03.py          0.005926 0.006044
      nonogram_pbn_dom04.py          0.013333 0.020526
      nonogram_pbn_dom05.py          0.023874 0.541823
      nonogram_pbn_dom06.py          0.049942 1.774315
      nonogram_pbn_dom07.py          0.157121 3.850751
      nonogram_pbn_dom08.py          0.347990 13.857369
      nonogram_pbn_dom09.py          0.883812 32.550134
      nonogram_pbn_dom10.py          1.845537 60.000000
      nonogram_pbn_dom11.py          3.405786 60.000000
      nonogram_pbn_dom12.py          7.220187 60.000000
      nonogram_pbn_dom13.py          16.631230 60.000000
      nonogram_pbn_dom14.py          26.751668 60.000000
      nonogram_pbn_dom15.py          52.803318 60.000000
      nonogram_pbn_dom16.py          60.000000 60.000000
      nonogram_pbn_edge.py           0.019300 0.040042
      nonogram_pbn_flag.py           0.094609 0.927118
      nonogram_pbn_forever.py        0.206298 0.687371
      nonogram_pbn_hot.py            4.814303 11.233480
      nonogram_pbn_karate.py         0.489679 2.081963
      nonogram_pbn_knot.py           0.017048 0.230616
      nonogram_pbn_light.py          1.100033 3.172661
      nonogram_pbn_lion.py           3.185270 60.000000
      nonogram_pbn_m_and_m.py        3.263962 7.726016
      nonogram_pbn_marley.py         15.528986 60.000000
      nonogram_pbn_merka.py          0.376428 2.236737
      nonogram_pbn_mum.py            0.362214 1.284934
      nonogram_pbn_nature.py         3.852760 60.000000
      nonogram_pbn_petro.py          0.822752 3.263342
      nonogram_pbn_signed.py         5.254635 6.327349
      nonogram_pbn_skid.py           0.003256 0.048350
      nonogram_pbn_smoke.py          0.087726 0.188815
      nonogram_pbn_swing.py          0.046900 0.562794
      nonogram_pbn_tragic.py         0.582698 2.148200

  As expected, nonogram_automaton_sat.py is significantly 
  faster (211s) with only one timeout (for nonogram_pbn_dom16), 
  while nonogram_table_sat.py takes 731s with 10 timeouts.

  For more about benchmarking Nonogram solvers, see 
  - https://webpbn.com/survey/
    (last updated in 2013) and includes some of my old 
    Nonogram solvers written in MiniZinc.
  - nonogram_table_sat.py which has some more references.
  

  This model was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my other OR-tools models: http://www.hakank.org/or_tools/

"""
from __future__ import print_function
import time, os, subprocess, re


def main(instances, solver="nonogram_automaton_sat.py",timeout=10):
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
# However, all might not be available so
# we'll weed out those not available below.
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

if __name__ == '__main__':
  print("Timeout is ",timeout,"seconds.\n")
  print("nonogram_automaton_sat_py:")
  times_automaton, total_times_automaton = main(instances,"nonogram_automaton_sat.py",timeout)
  print("\nnonogram_table_sat_py:")
  times_table, total_times_table = main(instances,"nonogram_table_sat.py",timeout)
  # times_regular, total_times_regular = main(instances,"nonogram_table_sat.py regular",timeout)
  print("\n\nSummary:")
  print("Total time automaton: %-4fs" % (total_times_automaton))
  print("Total time table    : %-4fs" % (total_times_table))
  print("Instance                       automaton   table")
  print("-------------------------------------------------")
  for inst in instances:
      print("%-30s %3f %3f" % (inst,times_automaton[inst],times_table[inst]))
  print()