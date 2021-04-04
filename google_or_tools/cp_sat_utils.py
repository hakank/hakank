#
# Utilities for OR-Tools CP-SAT solver.
#
# This package was created by Hakan Kjellerstrand (hakank@gmail.com)
# See my other Google OR-tools models: http://www.hakank.org/or_tools/
#
from ortools.sat.python import cp_model as cp


class SimpleSolutionPrinter(cp.CpSolverSolutionCallback):
    """
    SimpleSolutionPrinter: Print solution in one line.

    Example:
        # model = ...
        solution_printer = SimpleSolutionPrinter(variables)
        status = solver.SearchForAllSolutions(model, solution_printer)
        # ...
        print()
        print('Solutions found : %i' % solution_printer.SolutionCount())
        # ...
    """
    def __init__(self, variables):
        cp.CpSolverSolutionCallback.__init__(self)
        self.__variables = variables
        self.__solution_count = 0

    def OnSolutionCallback(self):
        self.__solution_count += 1
        for v in self.__variables:
            print('%s = %i' % (v, self.Value(v)), end = ' ')
        print()

    def SolutionCount(self):
        return self.__solution_count


class SimpleSolutionPrinter2(cp.CpSolverSolutionCallback):
    """
    SimpleSolutionPrinter2: Print vars in each line.

    Example:
        # model = ...
        solution_printer = SimpleSolutionPrinter([variables])
        status = solver.SearchForAllSolutions(model, solution_printer)
        # ...
        print()
        print('Solutions found : %i' % solution_printer.SolutionCount())
        # ...
    """
    def __init__(self, variables):
        cp.CpSolverSolutionCallback.__init__(self)
        self.__variables = variables
        self.__solution_count = 0

    def OnSolutionCallback(self):
        self.__solution_count += 1        
        for vars in self.__variables:
          if type(vars) in (list,):
            print([self.Value(v) for v in vars])
          else:
            print(vars,":", self.Value(vars))
        print()

    def SolutionCount(self):
        return self.__solution_count


class ListPrinter(cp.CpSolverSolutionCallback):
    """
    ListPrinter(variables)
    Print solutions just as list of integers.
    """
    def __init__(self, variables, limit=0):
        cp.CpSolverSolutionCallback.__init__(self)
        self.__variables = variables
        self.__limit = limit
        self.__solution_count = 0

    def OnSolutionCallback(self):
        self.__solution_count += 1
        print([self.Value(v) for v in self.__variables])
        if self.__limit > 0 and self.__solution_count >= self.__limit:
            self.StopSearch()

    def SolutionCount(self):
        return self.__solution_count


class SimpleSolutionCounter(cp.CpSolverSolutionCallback):    
    """
    SolutionCounter: Just count the number of solutions.

    Example:
      # model = ...
      solution_printer = SolutionCounter(variables)
      status = solver.SearchForAllSolutions(model, solution_printer)
      # ...
      print()
      print('Solutions found : %i' % solution_printer.SolutionCount())
      # ...
    """
    def __init__(self, variables):
        cp.CpSolverSolutionCallback.__init__(self)
        self.__variables = variables
        self.__solution_count = 0

    def OnSolutionCallback(self):
        self.__solution_count += 1

    def SolutionCount(self):
        return self.__solution_count


def count_vars(model, x, val, c):
    """
    count_vars(model, x, val, c)

    `c` is the number of occurrences of `val` in array `x`

    c = sum([x[i] == val for i in range(len(x))])
    """
    n = len(x)
    b = [model.NewBoolVar(f"b[{i}]")  for i in range(n)]
    for i in range(n):
        model.Add((x[i] == val)).OnlyEnforceIf(b[i])
        model.Add((x[i] != val)).OnlyEnforceIf(b[i].Not())
    model.Add(c == sum(b))


def atmost(model, val, x, n):
    """
    atmost(model,val,x,n)

    Atmost n occurrences of value val in array x
    """
    c = model.NewIntVar(0,len(x),"c")
    count_vars(model, x, val, c)
    model.Add(c <= n)

def atleast(model, val, x, n):
    """
    atleast(model,val,x,n)

    Atleast n occurrences of value val in array x
    """
    c = model.NewIntVar(0,len(x),"c")
    count_vars(model, x, val, c)
    model.Add(c >= n)


def exactly(model, val, x, n):
    """
    exactly(model,val,x,n)

    Exactly n occurrences of value val in array x
    """
    c = model.NewIntVar(0,len(x),"c")
    count_vars(model, x, val, c)
    model.Add(c == n)


def increasing(model, x):
    """
    increasing(model, x)

    Ensure that x is in increasing order
    """
    n = len(x) 
    for i in range(1,n):
        model.Add(x[i-1] <= x[i])

def increasing_strict(model, x):
    """
    increasing_strict(model, x)

    Ensure that x is in strict increasing order
    """
    n = len(x) 
    for i in range(1,n):
        model.Add(x[i-1] < x[i])

def decreasing(model, x):
    """
    decreasing(model, x)

    Ensure that x is in decreasing order
    """
    n = len(x) 
    for i in range(1,n):
        model.Add(x[i-1] >= x[i])

def decreasing_strict(model, x):
    """
    decreasing_strict(model, x)

    Ensure that x is in strict decreasing order
    """
    n = len(x) 
    for i in range(1,n):
        model.Add(x[i-1] > x[i])


def alldifferent_except_0(model, a):
    """
    alldifferent_except_0(model, a)

    Ensure that all values except 0 are distinct
    """
    n = len(a)
    # ba[i] <=> a[i] != 0
    ba = [model.NewBoolVar('ba[%i]' % (i)) for i in range(n)]
    for i in range(n):
        model.Add(a[i] != 0).OnlyEnforceIf(ba[i])
        model.Add(a[i] == 0).OnlyEnforceIf(ba[i].Not())

    for i in range(n):
        for j in range(i):
            # ba[i] && ba[j] -> x[i] != x[j]
            b = model.NewBoolVar('b[%i,%i]' % (i,j))
            model.AddBoolAnd([ba[i], ba[j]]).OnlyEnforceIf(b)
            model.AddBoolOr([ba[i].Not(), ba[j].Not()]).OnlyEnforceIf(b.Not())

            model.Add(a[i] != a[j]).OnlyEnforceIf(b)            


def reif(model, expr):
    """
    reif(model, expr, not_expr b)

    Return the boolean variable b to express
    b <=> expr
    Note that there are no negation of b here.
    """
    b = model.NewBoolVar("b " + str(expr))
    model.Add(expr).OnlyEnforceIf(b)
    return b

def reif2(model, expr, not_expr):
    """
    reif2(model, expr, not_expr b)

    Return the boolean variable b to express
    b <=> expr
    !b <=> not_expr
    """    
    b = model.NewBoolVar("b expr:" + str(expr) + " neg: " + str(not_expr))
    model.Add(expr).OnlyEnforceIf(b)
    model.Add(not_expr).OnlyEnforceIf(b.Not())
    return b

def flatten(a):
    """
    flatten(a)

    Return a flattened list of the sublists in a.
    """
    return [item for sublist in a for item in sublist]


def array_values(model, x):
    """
    array_values(model,x)

    Return the evaluated values in array x.
    model is either the model's <model> or SolutionPrinter's <self>
    """
    return [model.Value(x[i]) for i in range(len(x))]


def circuit(model, x):
  """
  circuit(model, x)
  constraints x to be an circuit
  Note: This assumes that x is has the domain 0..len(x)-1,
        i.e. 0-based.
"""

  n = len(x)
  z = [model.NewIntVar(0, n - 1, "z%i" % i) for i in range(n)]

  model.AddAllDifferent(x)
  model.AddAllDifferent(z)

  # put the orbit of x[0] in in z[0..n-1]
  model.Add(z[0] == x[0])
  for i in range(1, n - 1):
    model.AddElement(z[i - 1], x, z[i])

  #
  # Note: At least one of the following two constraint must be set.
  #
  # may not be 0 for i < n-1
  for i in range(1, n - 1):
    model.Add(z[i] != 0)

  # when i = n-1 it must be 0
  model.Add(z[n - 1] == 0)


def circuit_path(model, x,z):
  """
  circuit(model, x, z)
  constraints x to be an circuit. z is the visiting path.

  Note: This assumes that x is has the domain 0..len(x)-1,
        i.e. 0-based.
"""
  n = len(x)
  # z = [model.NewIntVar(0, n - 1, "z%i" % i) for i in range(n)]

  model.AddAllDifferent(x)
  model.AddAllDifferent(z)

  # put the orbit of x[0] in in z[0..n-1]
  model.Add(z[0] == x[0])
  for i in range(1, n - 1):
    model.AddElement(z[i - 1], x, z[i])

  #
  # Note: At least one of the following two constraint must be set.
  #
  # may not be 0 for i < n-1
  for i in range(1, n - 1):
    model.Add(z[i] != 0)

  # when i = n-1 it must be 0
  model.Add(z[n - 1] == 0)



def scalar_product(model, x, y, s):
  """
  scalar_product(model, x, y, s, ub)

  Scalar product of `x` and `y`: `s` = sum(x .* y)

  Both `x` and `y` can be decision variables.

  `lb` and `ub` are the lower/upper bound of the temporary 
  variables in the array `t`.
"""
  n = len(x)
  slb, sub = s.Proto().domain
  t = [model.NewIntVar(slb,sub,"") for i in range(n)]
  for i in range(n):
    model.AddMultiplicationEquality(t[i],[x[i],y[i]])
  model.Add(s == sum(t))



def memberOf(model, x, val):
    """
    memberOf(model, x, val)

    Ensures that the value `val` is in the array `x`.
    """
    n = len(x)
    cc = model.NewIntVar(0,n,"cc")
    count_vars(model, x, val, cc)
    model.Add(cc > 0)
    return cc

# converts a number (s) <-> an array of numbers (t) in the specific base.
def toNum(model, a, n, base=10):
  """
  toNum(model, a, n, base)

  converts a number (`n`) <-> an array of numbers (`t`) in the 
  specific base (`base`, default 10).
  """
  alen = len(a)
  model.Add(
      n == sum([(base**(alen - i - 1)) * a[i] for i in range(alen)]))


#
# Decompositon of cumulative.
#
# Inspired by the MiniZinc implementation:
# http://www.g12.csse.unimelb.edu.au/wiki/doku.php?id=g12:zinc:lib:minizinc:std:cumulative.mzn&s[]=cumulative
# The MiniZinc decomposition is discussed in the paper:
# A. Schutt, T. Feydy, P.J. Stuckey, and M. G. Wallace.
# 'Why cumulative decomposition is not as bad as it sounds.'
# Download:
# http://www.cs.mu.oz.au/%7Epjs/rcpsp/papers/cp09-cu.pdf
# http://www.cs.mu.oz.au/%7Epjs/rcpsp/cumu_lazyfd.pdf
#
#
# Parameters:
#
# s: start_times    assumption: array of IntVar
# d: durations      assumption: array of int
# r: resources      assumption: array of int
# b: resource limit assumption: IntVar or int
#
def my_cumulative(model, s, d, r, b):
  """
  my_cumulative(model, s, d, r, b)

  Enforces that for each job i, 
  `s[i]` represents the start time, `d[i] represents the 
  duration, and `r[i]` represents the units of resources needed. 
  `b` is the limit on the units of resources available at any time. 
  This constraint ensures that the limit cannot be exceeded at any time.

  Parameters:

  `s`: start_times    assumption: array of IntVar

  `d`: durations      assumption: array of int
  
  `r`: resources      assumption: array of int

  `b`: resource limit assumption: IntVar or int
  """

  max_r = max(r) # max resource

  tasks = [i for i in range(len(s)) if r[i] > 0 and d[i] > 0]

  # CP-SAT solver don't have var.Min() or var.Max() as the 
  # old SAT solver, but it does have var.Proto().domain which 
  # is the lower and upper bounds of the variable ([lb,ub])
  # See 
  # https://stackoverflow.com/questions/66264938/does-or-tools-cp-sat-solver-supports-reflection-methods-such-as-x-min-and-x
  lb = [s[i].Proto().domain[0] for i in tasks]
  ub = [s[i].Proto().domain[1] for i in tasks]

  times_min = min(lb)
  times_max = max(ub)
  for t in range(times_min, times_max + 1):
    bb = []
    for i in tasks:
      # s[i] < t
      b1 = model.NewBoolVar("")
      model.Add(s[i] <= t).OnlyEnforceIf(b1)
      model.Add(s[i] > t).OnlyEnforceIf(b1.Not())

      # t < s[i] + d[i]
      b2 = model.NewBoolVar("")
      model.Add(t < s[i] + d[i]).OnlyEnforceIf(b2)
      model.Add(t >= s[i] + d[i]).OnlyEnforceIf(b2.Not())

      # b1 and b2 (b1 * b2)
      b3 = model.NewBoolVar("")
      model.AddBoolAnd([b1,b2]).OnlyEnforceIf(b3)
      model.AddBoolOr([b1.Not(),b2.Not()]).OnlyEnforceIf(b3.Not())

      # b1 * b2 * r[i]
      b4 = model.NewIntVar(0, max_r, "b4")
      model.AddMultiplicationEquality(b4,[b3,r[i]])
      bb.append(b4)

    model.Add(sum(bb) <= b)

  # Somewhat experimental:
  # This constraint is needed to contrain the upper limit of b.
  if not isinstance(b, int):
    model.Add(b <= sum(r))


def prod(model, x, p):
  """
  prod(model, x, p)

  `p` is the product of the elements in array `x`
  p = x[0]*x[1]*...x[-1]
  Note: This trickery is needed since `AddMultiplicationEquality`
        (as of writing) don't support more than two products at a time.
  """
  n = len(x)
  lb, ub = x[0].Proto().domain
  t = [model.NewIntVar(lb,ub**(i+1),"t") for i in range(n)]
  model.Add(t[0] == x[0])
  for i in range(1,n):
    model.AddMultiplicationEquality(t[i],[t[i-1],x[i]])
  model.Add(p == t[-1])



def knapsack(model, values, weights, n):
  """
  knapsack(model, values, weights, n)

  Solves the knapsack problem.

  `x`: the selected items

  `z`: sum of values of the selected items

  `w`: sum of weights of the selected items
  """
  z = model.NewIntVar(0, sum(values), "z") 
  x = [model.NewIntVar(0, 1, "x(%i)" % i) for i in range(len(values))]
  scalar_product(model, x, values, z)
  w = model.NewIntVar(0,sum(weights), "w")
  scalar_product(model, x, weights, w)
  model.Add(w <= n)

  return x, z, w


def global_cardinality(model, x, domain, gcc):
  """
  global_cardinality(model, x, domain, gcc)

  For each value in the array `domain`, the array `gcc` 
  counts the number of occurrences in array `x`.

  The length of `domain` must the same as the length of `gcc`.
  """
  assert len(gcc) == len(domain) 
  for i in range(len(domain)):
    count_vars(model, x, domain[i], gcc[i])



#
# Global constraint regular
#
# This is a translation of MiniZinc's regular constraint (defined in
# lib/zinc/globals.mzn), via the Comet code refered above.
# All comments are from the MiniZinc code.
# '''
# The sequence of values in array 'x' (which must all be in the range 1..S)
# is accepted by the DFA of 'Q' states with input 1..S and transition
# function 'd' (which maps (1..Q, 1..S) -> 0..Q)) and initial state 'q0'
# (which must be in 1..Q) and accepting states 'F' (which all must be in
# 1..Q).  We reserve state 0 to be an always failing state.
# '''
#
# x : IntVar array
# Q : number of states
# S : input_max
# d : transition matrix
# q0: initial state
# F : accepting states
#
# Note: The difference between this approach and 
# the one in MiniZinc is that instead of element/2
# AddAllowedAssignements (i.e. table/2) is used.
# The AddElement approach is implemented in
# the regular_element method (see below).
#
# It seems that regular_table is faster than 
# regular_element.
# 
def regular_table(model, x, Q, S, d, q0, F):
    """
    regular_table(model, x, Q, S, d, q0, F)

    `x` : IntVar array

    `Q` : number of states

    `S` : input_max

    `d` : transition matrix

    `q0`: initial state

    `F` : accepting states

    `regular_table` seems to be faster than the `regular` method.
    """
    assert Q > 0, 'regular: "Q" must be greater than zero'
    assert S > 0, 'regular: "S" must be greater than zero'

    # d2 is the same as d, except we add one extra transition for
    # each possible input;  each extra transition is from state zero
    # to state zero.  This allows us to continue even if we hit a
    # non-accepted input.
    d2 = []
    for i in range(Q + 1):
        for j in range(S):
            if i == 0:
                d2.append((0, j, 0))
            else:
                d2.append((i, j, d[i - 1][j]))

    # If x has index set m..n, then a[m-1] holds the initial state
    # (q0), and a[i+1] holds the state we're in after processing
    # x[i].  If a[n] is in F, then we succeed (ie. accept the
    # string).
    x_range = list(range(0, len(x)))
    m = 0
    n = len(x)

    a = [model.NewIntVar(0, Q + 1, 'a[%i]' % i) for i in range(m, n + 1)]

    # Check that the final state is in F
    # solver.Add(solver.MemberCt(a[-1], F))
    memberOf(model,F,a[-1])

    # First state is q0
    model.Add(a[m] == q0)
    _xlb, xub = x[0].Proto().domain
    for i in x_range:
        model.Add(x[i] >= 1)
        model.Add(x[i] <= S)
        # Determine a[i+1]: a[i+1] == d2[a[i], x[i]]
        xi1 = model.NewIntVar(0,xub,"xi1")
        model.Add(xi1 == x[i]-1)
        model.AddAllowedAssignments([a[i], xi1, a[i + 1]], d2)



#
# Global constraint regular
#
# This is a translation of MiniZinc's regular constraint (defined in
# lib/zinc/globals.mzn), via Comet code.
# All comments are from the MiniZinc code.
# '''
# The sequence of values in array 'x' (which must all be in the range 1..S)
# is accepted by the DFA of 'Q' states with input 1..S and transition
# function 'd' (which maps (1..Q, 1..S) -> 0..Q)) and initial state 'q0'
# (which must be in 1..Q) and accepting states 'F' (which all must be in
# 1..Q).  We reserve state 0 to be an always failing state.
# '''
#
# x : IntVar array
# Q : number of states
# S : input_max
# d : transition matrix
# q0: initial state
# F : accepting states
#
# Cf regulat_table which use AllowedAssignments
# instead of Element.
# Note: It seems that regular_element is slower than regular_table.
#
def regular_element(model,x, Q, S, d, q0, F):
  """
  regular_element(model, x, Q, S, d, q0, F)

    `x` : IntVar array

    `Q` : number of states

    `S` : input_max

    `d` : transition matrix

    `q0`: initial state

    `F` : accepting states

  `regular` seems to be slower than the `regular_table` method.
  """
  assert Q > 0, 'regular: "Q" must be greater than zero'
  assert S > 0, 'regular: "S" must be greater than zero'

  # d2 is the same as d, except we add one extra transition for
  # each possible input;  each extra transition is from state zero
  # to state zero.  This allows us to continue even if we hit a
  # non-accepted input.
  d2 = []
  for i in range(Q + 1):
    row = []
    for j in range(S):
      if i == 0:
        row.append(0)
      else:
        row.append(d[i - 1][j])
    d2.append(row)

  # d2_flatten = flatten(d2)
  d2_flatten = [d2[i][j] for i in range(Q + 1) for j in range(S)]

  # If x has index set m..n, then a[m-1] holds the initial state
  # (q0), and a[i+1] holds the state we're in after processing
  # x[i].  If a[n] is in F, then we succeed (ie. accept the
  # string).
  x_range = list(range(0, len(x)))
  m = 0
  n = len(x)

  a = [model.NewIntVar(0, Q + 1, 'a[%i]' % i) for i in range(m, n + 1)]
  
  # Check that the final state (a[-1]) is in F (accepatble final states)
  memberOf(model, F, a[-1])

  # First state is q0
  model.Add(a[m] == q0)
  for i in x_range:
    model.Add(x[i] >= 1)
    model.Add(x[i] <= S)
  
    # Determine a[i+1]: a[i+1] == d2[a[i], x[i]]
    # AddElement(index, variables, target)
    ix = model.NewIntVar(0,len(d2_flatten)*1000,f"ix(%i)" % (i))
    model.Add(ix == ((a[i]) * S) + (x[i] - 1))
    model.AddElement(ix, d2_flatten, a[i + 1])


#
# No overlapping of tasks s1 and s2
#
def no_overlap(model, s1, d1, s2, d2):
    """
    no_overlap(model, s1, d1, s2, d2)

    Ensure that there are no overlap of task 1 (`s1` + `d1`)
    and task 2 (`s2` + `d2`)
    """
    b1 = model.NewBoolVar("b1") 
    model.Add(s1 + d1 <= s2).OnlyEnforceIf(b1)
    b2 = model.NewBoolVar("b1") 
    model.Add(s2 + d2 <= s1).OnlyEnforceIf(b2)
    model.Add(b1 + b2 >= 1)



def permutation3(model, from_a, perm_a, to_a):
  """
  Ensure that the permutation of `from_a` to `to_a` is 
  the permutation  `perm_a`
  """
  assert len(from_a) == len(perm_a), f"Length of `from_a` and `perm_a` must be the same"
  assert len(from_a) == len(to_a), f"Length of `from_a` and `to_a` must be the same"
  model.AddAllDifferent(perm_a)
  for i in range(len(from_a)):
    # to_a[i] = from_a[perm_a[i]]
    model.AddElement(perm_a[i],from_a,to_a[i])
