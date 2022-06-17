"""
  This package includes my constraints/utilities/etc for cpmpy.

  This cpmpy model was written by Hakan Kjellerstrand (hakank@gmail.com)
  See also my cpmpy page: http://hakank.org/cpmpy/
"""
import sys, math, re
import itertools
import numpy as np
from functools import reduce
from cpmpy import *
from cpmpy.expressions.globalconstraints import GlobalConstraint
from cpmpy.solvers import *
from ortools.sat.python import cp_model as ort
from cpmpy.transformations.flatten_model import flatten_constraint, flatten_model
from cpmpy.transformations.get_variables import print_variables


def AllDifferent_except_0(args):
  """
  Ensure that all arguments that are != 0 must have distinct values.
  """
  # Note: The parenthesis around (var1 != 0) are needed!
  return [ ((var1!= 0) & (var2 != 0)).implies(var1 != var2) for var1, var2 in all_pairs(args)]


def all_different_except_0(args):
  """
  Alias for AllDifferent_except_0(args).
  """
  return AllDifferent_except_0(args)


def to_num(a,n,base):
  """
  to_num(a, n, base)
  
  Ensure that the digits in array `a` corresponds to the number `n` in base `base`.
  """
  tlen = len(a)
  return n == sum([(base ** (tlen - i - 1)) * a[i] for i in range(tlen)])


def increasing(args):
  """
  Ensure that the values in args are increasing.
  """
  return [args[i-1] <= args[i] for i in range(1,len(args))]


def increasing_strict(args):
  """
  Ensure that the values in args are strict increasing.
  """  
  return [args[i-1] < args[i] for i in range(1,len(args))]

def decreasing(args):
  """
  Ensure that the values in args are decreasing.
  """    
  return [args[i-1] >= args[i] for i in range(1,len(args))]


def decreasing_strict(args):
  """
  Ensure that the values in args are strict decreasing.
  """
  return [args[i-1] >= args[i] for i in range(1,len(args))]


def all_pairs(args):
  """
  Generate all pairs from the list of lists args.

  (stolen from cmppy/globalconstraints.py)
  """
  return list(itertools.combinations(args, 2))


def get_different_solution(m,x):
  """
  Add the current solution (x) in the model to generate
  other solutions.

  Usage:
     # ...
     ss = CPM_ortools(model)
     if ss.solve():
        print(x.value())
        get_different_solution(ss, x)

  Note: The array in x must be a flattened array. If there are
        many decision variables, use flatten_lists(a) to
        flatten out the array. E.g.

     # ...
     ss = CPM_ortools(model)
     while ss.solve():
        print(x.value()) # an array
        print(y.value()) # a variable
        print(z.value()) # another variable        
        get_different_solution(ss,flatten_lists([x,[y,z]])

  Note that this might be slow for larger models or models with
  many solutions. If so, try to use
  - ortools_wrapper()
  or the simple solution printers such as
  - ORT_simple_printer
  - ORT_arrays_printer
  - ORT_simple_printer_matrix
  - ORT_simple_function_printer
  or define a similiar solution printer.

  """
  # n = len(x)
  # m += [any([x[i].value() != x[i] for i in range(n)])]
  m += [any([t.value() != t for t in x])]


def flatten_lists(a):
  """
  Flatten a list of lists.

  Note: a must be an array of arrays (list of lists).
  
  See get_different_solution for examples.
  """
  return [item for sublist in a for item in sublist]


class ORT_simple_printer(ort.CpSolverSolutionCallback):
  """
  A simple printer callback for single array printing.
  """
  def __init__(self, varmap, a, num_solutions=0):
    super().__init__()
    self.solcount = 0
    self.varmap = varmap
    self.vars = (a)
    self.num_solutions=num_solutions

  def on_solution_callback(self):
    self.solcount += 1 # I always start at 1. :-) 

    # populate values before printing
    # For array of arrays (Tias' original)
    # for wm in self.vars:
    #     for cpm_var in wm:
    #         cpm_var._value = self.Value(self.varmap[cpm_var])
    
    # For single arrays:
    for cpm_var in self.vars:
      cpm_var._value = self.Value(self.varmap[cpm_var])
      
    (a) = self.vars            
    print(f"#{self.solcount}: {a.value()}")

    if self.num_solutions > 0 and self.solcount >= self.num_solutions:
      self.StopSearch()


class ORT_arrays_printer(ort.CpSolverSolutionCallback):
  """
  A simple printer callback for array of arrays.
  """
  def __init__(self, varmap, a, num_solutions=0):
    super().__init__()
    self.solcount = 0
    self.varmap = varmap
    self.vars = (a)
    self.num_solutions=num_solutions

  def on_solution_callback(self):
    self.solcount += 1 # I always start at 1. :-) 

    # populate values before printing
    # For array of arrays (Tias' original)
    for wm in self.vars:
      for cpm_var in wm:
        cpm_var._value = self.Value(self.varmap[cpm_var])
   
    # For single arrays:
    for cpm_var in self.vars:
      cpm_var._value = self.Value(self.varmap[cpm_var])
      
    (a) = self.vars            
    print(f"#{self.solcount}: {a.value()}")

    if self.num_solutions > 0 and self.solcount >= self.num_solutions:
      self.StopSearch()


class ORT_simple_printer_matrix(ort.CpSolverSolutionCallback):
  """
  A simple printer callback for printing a matrix.
  """
  def __init__(self, varmap, a, rows,cols, num_solutions=0):
    super().__init__()

    self.solcount = 0
    self.varmap = varmap
    self.vars = (a)
    self.rows = rows
    self.cols = cols
    self.num_solutions=num_solutions

  def on_solution_callback(self):
    self.solcount += 1
   
    for cpm_var in self.vars:
      cpm_var._value = self.Value(self.varmap[cpm_var])

    (a) = self.vars
    print(f"#{self.solcount}:")    
    for i in range(self.rows):
      for j in range(self.cols):
        print("%3d" % a[i*self.cols+j].value(), end=" ")
      print()
    print()

    if self.num_solutions > 0 and self.solcount >= self.num_solutions:
      self.StopSearch()

class ORT_simple_function_printer(ort.CpSolverSolutionCallback):
  """
  A printer callback with a callback (cb_fun) for printing
  the array a, which should be structured by the user and
  including .value() for the variables.

  Note that the data array a must be a flattening array
  to be used with this printer callback.

  Example of a printer function:
    def f(a):
        print(a[0].value(),"+",a[1].value(),"=",a[2].value())
  which will print a solution such as
       2 + 3 = 5

  """
  def __init__(self, varmap, a, cb_fun,num_solutions=0):
    super().__init__()
    self.solcount = 0
    self.varmap = varmap
    self.vars = (a)
    self.cb_fun = cb_fun
    self.num_solutions=num_solutions

  def on_solution_callback(self):
    self.solcount += 1
   
    # For single arrays:
    for cpm_var in self.vars:
      cpm_var._value = self.Value(self.varmap[cpm_var])
      
    (a) = self.vars
    print(f"\n#{self.solcount}:")
    self.cb_fun(a)

    if self.num_solutions > 0 and self.solcount >= self.num_solutions:
      self.StopSearch()


class ORT_simple_solution_counter(ort.CpSolverSolutionCallback):
  """
  This is a solution 'printer' that just count the solutions.
  """
  def __init__(self, varmap, a):
    super().__init__()
    self.solcount = 0
    self.varmap = varmap
    self.vars = (a)

  def on_solution_callback(self):
    self.solcount += 1
   
    for wm in self.vars:
      for cpm_var in wm:
        cpm_var._value = self.Value(self.varmap[cpm_var])
      
    (a) = self.vars



class ORT_function_printer_arrays(ort.CpSolverSolutionCallback):
  """
  A printer callback with a callback (cb_fun) for printing
  the array of arrays a, which should be structured by the user and
  including .value() for the variables.

  This version t prints solution number.
  
  Example of a printer function:

  def print_solution(a):
    print('x:', a[0].value())
    print('y:', a[1].value())

  """
  def __init__(self, varmap, a, cb_fun,num_solutions=0):
    super().__init__()
    self.solcount = 0
    self.varmap = varmap
    self.vars = (a)
    self.cb_fun = cb_fun
    self.num_solutions=num_solutions

  def on_solution_callback(self):
    self.solcount += 1
    
    for wm in self.vars:
      for cpm_var in wm:
        cpm_var._value = self.Value(self.varmap[cpm_var])

    (a) = self.vars
    print(f"sol #{self.solcount}")
    self.cb_fun(a)
    print()

    if self.num_solutions > 0 and self.solcount >= self.num_solutions:
      self.StopSearch()

class ORT_function_printer_arrays2(ort.CpSolverSolutionCallback):
  """
  A printer callback with a callback (cb_fun) for printing
  the array of arrays a, which should be structured by the user and
  including .value() for the variables.

  This version don't print solution number.
  
  Example of a printer function:

  def print_solution(a):
    print('x:', a[0].value())
    print('y:', a[1].value())

  """
  def __init__(self, varmap, a, cb_fun,num_solutions=0):
    super().__init__()
    self.solcount = 0
    self.varmap = varmap
    self.vars = (a)
    self.cb_fun = cb_fun
    self.num_solutions=num_solutions

  def on_solution_callback(self):
    self.solcount += 1
    
    for wm in self.vars:
      for cpm_var in wm:
        cpm_var._value = self.Value(self.varmap[cpm_var])

    (a) = self.vars
    self.cb_fun(a)

    if self.num_solutions > 0 and self.solcount >= self.num_solutions:
      self.StopSearch()



def print_solution(a):
    """
    print_solution(a)

    Default callback method for printing the solution in a printer callback.
    Note: a must be an array of arrays to be used with ortools_wrapper
    (defined below).
    """
    for x in a:
        print(x.value())


def ortools_wrapper(model,var_array,print_solution=print_solution,num_sols=0,num_procs=1):
    """
    ortools_wrapper((model,var_array,print_solution=print_solution,num_sols=0,num_procs=1)

    This is a simple wrapper for printing the solutions of a model and tends
    to be (significantly) faster than using

        ss = CPM_ortools(model)
        while ss.solve():
           # ...
           get_different_solution(ss,flatten_lists(var_array))

    Parameters:
    - model    : the model
    - var_array: the array of arrays of the decision variables to be printed
                 with print_solution(var_array)
    - print_solution: the method used to do the actual printing of the solution.
                Default is print_solution(a) defined above. The function
                can be overwritten / defined in the current constraint model.
    - num_sols : number of solutions. Default 0, all solutions.
    - num_procs: number of processes. Note: Only works if num_sols = 1.

    Note: For optimality problems, use ortools_wrapper_opt(.) instead.
    
    """
    ss = CPM_ortools(model)    
    cb = ORT_function_printer_arrays(ss._varmap,var_array,print_solution,num_sols)


    # Flags to experiment with    
    # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
    # ss.ort_solver.parameters.cp_model_presolve = False
    ss.ort_solver.parameters.linearization_level = 0
    ss.ort_solver.parameters.cp_model_probing_level = 0

    if num_sols == 1 and num_procs > 1:
      print(f"Using {num_procs} procs")
      ss.ort_solver.parameters.num_search_workers = num_procs
      ort_status = ss.ort_solver.Solve(ss.ort_model, cb)
    else:
      ort_status = ss.ort_solver.SearchForAllSolutions(ss.ort_model, cb)
    # ss._after_solve(ort_status)
    print(ss.status())
    print("Nr solutions:", cb.solcount)
    print("Num conflicts:", ss.ort_solver.NumConflicts())
    print("NumBranches:", ss.ort_solver.NumBranches())
    print("WallTime:", ss.ort_solver.WallTime())
    print()


def ortools_wrapper2(model,var_array,print_solution=print_solution,num_sols=0,num_procs=1):
    """
    ortools_wrapper((model,var_array,print_solution=print_solution,num_sols=0,num_procs=1)

    This is a simple wrapper for printing the solutions of a model and tends
    to be (significantly) faster than using

        ss = CPM_ortools(model)
        while ss.solve():
           # ...
           get_different_solution(ss,flatten_lists(var_array))

   This version don't print the solution number.

    Parameters:
    - model    : the model
    - var_array: the array of arrays of the decision variables to be printed
                 with print_solution(var_array)
    - print_solution: the method used to do the actual printing of the solution.
                Default is print_solution(a) defined above. The function
                can be overwritten / defined in the current constraint model.
    - num_sols : number of solutions. Default 0, all solutions.
    - num_procs: number of processes. Note: Only works if num_sols = 1.    

    Note: For optimality problems, use ortools_wrapper_opt(.) instead.
    
    """
    ss = CPM_ortools(model)    
    cb = ORT_function_printer_arrays2(ss._varmap,var_array,print_solution,num_sols)

    # Flags to experiment with
    # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
    # ss.ort_solver.parameters.cp_model_presolve = False
    ss.ort_solver.parameters.linearization_level = 0
    ss.ort_solver.parameters.cp_model_probing_level = 0

    if num_sols == 1 and num_procs > 1:
      ss.ort_solver.parameters.num_search_workers = num_procs # Don't work together with SearchForAllSolutions
      ort_status = ss.ort_solver.Solve(ss.ort_model, cb)
    else:
      ort_status = ss.ort_solver.SearchForAllSolutions(ss.ort_model, cb)
      
    print()
    # ss._after_solve(ort_status) # post-process after solve() call...
    print(ss.status())
    print("Nr solutions:", cb.solcount)
    print("Num conflicts:", ss.ort_solver.NumConflicts())
    print("NumBranches:", ss.ort_solver.NumBranches())
    print("WallTime:", ss.ort_solver.WallTime())
    print()



def ortools_wrapper_opt(model,var_array,print_solution=print_solution,num_sols=1,num_procs=1):
    """
    ortools_wrapper_opt((model,var_array,print_solution=print_solution,num_sols=0)

    This is a simple wrapper for printing the _optimal_ solution of a model.
    This tends to be (significantly) faster than using
        if model.solve():
           # ...


    Parameters:
    - model    : the model
    - var_array: the array of arrays of the decision variables to be printed
                 with print_solution(var_array)
    - print_solution: the method used to do the actual printing of the solution.
                Default is print_solution(a) defined above. The function
                can be overwritten / defined in the current constraint model.
    - num_sols : number of solutions. Default 0, all solutions.
    """
    ss = CPM_ortools(model)    
    cb = ORT_function_printer_arrays(ss._varmap,var_array,print_solution,1)

    # Flags to experiment with
    if num_procs > 1:
      ss.ort_solver.parameters.num_search_workers = num_procs
      
    # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
    # ss.ort_solver.parameters.cp_model_presolve = False
    ss.ort_solver.parameters.linearization_level = 0
    ss.ort_solver.parameters.cp_model_probing_level = 0

    # Note: This is the real difference between this method and ortool_wrapper.
    # For optimal problems one cannot use SearchForAllSolutions. Instead
    # one must use ss.ort_solver.Solve(,)
    # ort_status = ss.ort_solver.SearchForAllSolutions(ss.ort_model, cb)
    ort_status = ss.ort_solver.Solve(ss.ort_model, cb)
    
    # ss._after_solve(ort_status) # post-process after solve() call...
    print(ss.status())
    print("Nr solutions:", cb.solcount)
    print("Num conflicts:", ss.ort_solver.NumConflicts())
    print("NumBranches:", ss.ort_solver.NumBranches())
    print("WallTime:", ss.ort_solver.WallTime())
    print()


def ortools_wrapper_count_solutions(model,var_array):
    """
    ortools_wrapper((model,var_array,print_solution=print_solution,num_sols=0)

    This is a simple wrapper for just counting the solutions of a model.
    
    Parameters:
    - model    : the model
    - var_array: the array of arrays of the decision variables to be printed
                 with print_solution(var_array)
    
    """
    ss = CPM_ortools(model)    
    cb = ORT_simple_solution_counter(ss._varmap,var_array)

    # Flags to experiment with
    # ss.ort_solver.parameters.num_search_workers = 8 # Don't work together with SearchForAllSolutions
    # ss.ort_solver.parameters.search_branching = ort.PORTFOLIO_SEARCH
    # ss.ort_solver.parameters.cp_model_presolve = False
    ss.ort_solver.parameters.linearization_level = 0
    ss.ort_solver.parameters.cp_model_probing_level = 0
    
    ort_status = ss.ort_solver.SearchForAllSolutions(ss.ort_model, cb)
    # ss._after_solve(ort_status)
    return cb.solcount


def base_array(n):
    """
    Returns an array of length `n` with base coefficients.
    
    Example: `base_array(4)` returns the array [1000,100,10,1]
    """
    return np.array([10**i for i in range(n-1,-1,-1)])

def scalar_product(a,b):
    """
    `scalar_product(a,b)`

    Returns the scalar product of the arrays `a` and `b`.
    Assumption: `len(a) == len(b)`
    """
    assert len(a) == len(a), f"len(a) == len(b)"
    # return np.dot(a,b)
    return sum(a*b)

def scalar_product1(a):
    """
    `scalar_product1(a)`

    Returns the scalar product of the array `a` and a base_array of appropriate length.
    Assumption: `len(a) == len(b)`
    """
    assert len(a) == len(a), f"len(a) == len(b)"
    # return np.dot(a,base_array(len(a)))
    return sum(a*base_array(len(a)))


def my_circuit(x):
    """
    circuit(x)

    Exsures that x is a circuit.
    
    Note: This assumes that x is has the domain 0..len(x)-1,
          i.e. 0-based.
    """
    assert x[0].lb == 0, f"circuit: lb is {x[0].lb}, but must be 0"
    
    n = len(x)
    z = intvar(0, n-1,shape=n,name='z')

    constraints = [
        AllDifferent(x),
        AllDifferent(z),

        # put the orbit of x[0] in in z[1..n]        
        z[0] == x[0],
        [ z[i] == x[z[i-1]] for i in range(1, n-1)],
        
        # may not be 0 for i < n-1
        [ z[i] != 0 for i in range(1, n-1)],
        
        # when i = n-1 it must be 0
        z[n-1] == 0
        ]
    
    return constraints




def my_circuit_path(x,z):
    """
    circuit(x,z)
    
    Ensures that x is an circuit and z is the path.
    
    Note: This assumes that x is has the domain 0..len(x)-1,
    i.e. 0-based.   
    """
    assert x[0].lb == 0, f"circuit: x[0].lb is {x[0].lb}, but must be 0"
   
    n = len(x)
    constraints = [
        AllDifferent(x),
        AllDifferent(z),

        # put the orbit of x[0] in in z[1..n]        
        z[0] == x[0],
        [ z[i] == x[z[i-1]] for i in range(1, n-1)],
        
        # may not be 0 for i < n-1
        [ z[i] != 0 for i in range(1, n-1)],
        
        # when i = n-1 it must be 0
        z[n-1] == 0
        ]
    
    return constraints

def count(a,val,c):
    """
    count(a,val,c)

    c is the number of occurrences of val in array a.
    """
    return [c == sum([a[i] == val for i in range(len(a))])
        ]

def atmost(a,val,c):
    """
    atmost(a,val,c)

    Ensure that the number of occurrences of val in a is atmost c.
    """
    return [sum([a[i] == val for i in range(len(a))]) <= c]

def atleast(a,val,c):
    """
    atleast(a,val,c)

    Ensure that the number of occurrences of val in a is atmost c.
    """
    return [sum([a[i] == val for i in range(len(a))]) >= c]

def exactly(a,val,c):
    """
    exactly(a,val,c)

    Ensure that the number of occurrences of val in a is exactly c.
    """
    return [sum([a[i] == val for i in range(len(a))]) == c]



def global_cardinality_count(a,gcc):
    """
    global_cardinality_count(a,gcc)

    Global cardinality count: Collect the number of occurrences of each value 0..a.ub
    in gcc. The array gcc must be of length 0..ub.
    """
    n = len(a)
    ub = max([a[i].ub for i in range(n)])
    constraints = []
    for i in range(ub+1):
        constraints += [count(a,i,gcc[i])]

    return constraints



def inverse(x,y):
    """
    inverse(x,y)

    Ensures that:
       x[i] == j #<=> y[j] == i

    Note: inverse(x,y) is sometimes called assignment(x,y).
    There is an alternative version: inverse(x) which can
    be simulated by inverse(x,x)
    
    """
    n = len(x)
    assert n == len(y), "x and y must be of equal length"
    constraints = []
    for i in range(n):
        for j in range(n):
            constraints += [(x[i] == j) == (y[j] == i)]
    return constraints



def my_cumulative(s, d, r, b):
    """
    Decompositon of cumulative.
    
    Inspired by the MiniZinc implementation.
    
    The MiniZinc decomposition is discussed in the paper:
    A. Schutt, T. Feydy, P.J. Stuckey, and M. G. Wallace.
    'Why cumulative decomposition is not as bad as it sounds.'

    Parameters:

    s: start_times    assumption: array of varint
    d: durations      assumption: array of int
    r: resources      assumption: array of int
    b: resource limit assumption: varint or int
    """
    
    constraints = []
    max_d = max(d)
    tasks = [i for i in range(len(s)) if r[i] > 0 and d[i] > 0]
    times_min = min([s[i].lb for i in tasks])
    times_max = max([s[i].ub + max_d for i in tasks])
    for t in range(times_min, times_max + 1):
        constraints += [ b >= sum([((s[i] <= t) & (t < s[i] + d[i])) * r[i] for i in tasks])]
            
    # Somewhat experimental:
    # This constraint is needed to contrain the upper limit of b.
    if not isinstance(b, int):
        constraints += [b <= sum(r)]
        
    return constraints 



def member_of(x, val):
    """
    member_of(x, val)

    Ensures that the value `val` is in the array `x`.
    """
    n = len(x)
    # cc = intvar(0,n)    
    # constraints = [count(x, val, cc), cc >  0]
    constraints = [sum([x[i] == val for i in range(n)]) > 0]    
    return constraints


def regular(x, Q, S, d, q0, F):
    """
    Global constraint regular

    This is a translation of MiniZinc's regular constraint (defined in
    lib/zinc/globals.mzn), via the Comet code refered above.
    All comments are from the MiniZinc code.
    '''
    The sequence of values in array 'x' (which must all be in the range 1..S)
    is accepted by the DFA of 'Q' states with input 1..S and transition
    function 'd' (which maps (1..Q, 1..S) -> 0..Q)) and initial state 'q0'
    (which must be in 1..Q) and accepting states 'F' (which all must be in
    1..Q).  We reserve state 0 to be an always failing state.
    '''

    x : IntVar array
    Q : number of states
    S : input_max
    d : transition matrix
    q0: initial state
    F : accepting states

    Note: As mentioned above the states must start at 1 since 0 is
          represents a failed state.
    Note: Compare with regular_table which use the Table constraints
          instead of Element constraint in the main loop.
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

    d2_flatten = [d2[i][j] for i in range(Q + 1) for j in range(S)]

    # If x has index set m..n, then a[m-1] holds the initial state
    # (q0), and a[i+1] holds the state we're in after processing
    # x[i].  If a[n] is in F, then we succeed (ie. accept the
    # string).
    x_range = list(range(0, len(x)))
    m = 0
    n = len(x)

    a = [intvar(0, Q + 1) for i in range(m, n + 1)]

    constraints = []

    # Check that the final state is in F
    constraints += [member_of(F,a[-1])]
  
    # First state is q0
    constraints += [a[m] == q0]
    for i in x_range:
        constraints += [x[i] >= 1]
        constraints += [x[i] <= S]
        # Determine a[i+1]: a[i+1] == d2[a[i], x[i]]
        constraints += [
            a[i + 1] == Element(d2_flatten,(a[i]) * S + (x[i] - 1))
            ]

    return constraints


def regular_table(x, Q, S, d, q0, F):
    """
    Global constraint regular_table

    This is a translation of MiniZinc's regular constraint (defined in
    lib/zinc/globals.mzn), via the Comet code refered above.
    All comments are from the MiniZinc code.
    '''
    The sequence of values in array 'x' (which must all be in the range 1..S)
    is accepted by the DFA of 'Q' states with input 1..S and transition
    function 'd' (which maps (1..Q, 1..S) -> 0..Q)) and initial state 'q0'
    (which must be in 1..Q) and accepting states 'F' (which all must be in
    1..Q).  We reserve state 0 to be an always failing state.
    '''

    x : IntVar array
    Q : number of states
    S : input_max
    d : transition matrix
    q0: initial state
    F : accepting states

    Note: As mentioned above the states must start at 1 since 0 is
          represents a failed state.


    The difference between this version (regular_table) and 
    regular is that this version use Table constraint instead
    of Element constraint.
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
                # This is different from regular(.)
                row.append((0,j,0))
            else:
                # This is different from regular(.)
                row.append((i,j, d[i - 1][j]))
        d2.append(row)

    d2_flatten = [d2[i][j] for i in range(Q + 1) for j in range(S)]

    # If x has index set m..n, then a[m-1] holds the initial state
    # (q0), and a[i+1] holds the state we're in after processing
    # x[i].  If a[n] is in F, then we succeed (ie. accept the
    # string).
    x_range = list(range(0, len(x)))
    m = 0
    n = len(x)

    a = [intvar(0, Q + 1) for i in range(m, n + 1)]

    constraints = []

    # Check that the final state is in F
    constraints += [member_of(F,a[-1])]
  
    # First state is q0
    constraints += [a[m] == q0]
    x_lb, x_ub = get_min_max_domain(x)
    for i in x_range:
        constraints += [x[i] >= 1]
        constraints += [x[i] <= S]
        # Determine a[i+1]: a[i+1] == d2[a[i], x[i]]
        xi1 = intvar(0,x_ub)
        constraints += [
            # These two constraints are different
            # from regular(.)
            xi1 == x[i]-1,
            Table((a[i], xi1, a[i + 1]), d2_flatten)
            ]

    return constraints



def lex_less(x,y):
    """
    lex_less(x,y)

    Ensures that the array 'x' is strictly lexicographically less than array 'y'.
    Compares them from first to last element, regardless of indices

    This is a port of MiniZinc's definition lex_less_int
    https://github.com/MiniZinc/libminizinc/blob/master/share/minizinc/std/fzn_lex_less_int.mzn
    Note that we simplify the calculation of lx and ly since cpmpy has start index 0 (in MiniZinc
    the start index can be user defined).  
    """
    xlen = len(x)
    ylen = len(y)
    ux = xlen
    uy = ylen 
    size = min([ux,uy])
    # Do not name variables in global constraints
    # since then the variables are not unique.
    # b = boolvar(shape=size+1,name="b")
    b = boolvar(shape=size+1)
    constraints = []
    constraints += [b[0] == 1 ]
    for i in range(size):
        constraints +=  [b[i] == ((x[i] <= y[i]) & 
                                  ((x[i] <  y[i]) | (b[i+1] == 1)) )]
    constraints += [b[size] == (ux < uy)]
    return constraints


def lex_greater(x,y):
    """
    lex_greater(x,y)

    Ensures that the array 'x' is strictly lexicographically greater than array 'y'.
    Compares them from first to last element, regardless of indices.
    This constraint is defined by lex_less(y,x) defined above .
    """
    return lex_less(y,x)

def lex2(x):
    """
    lex2(x)

    Ensures that the rows and columns in the matrix `x` are increasing,
    using lex_less.
    """
    x_t = x.transpose()
    return [[lex_less(x[i],x[i+1]) for i in range(len(x)-1)],
            [lex_less(x_t[i],x_t[i+1]) for i in range(len(x_t)-1)]]


#
# Somewhat general definition of knapsack.
#
def knapsack(values, weights, n):
    """
    knapsack(values, weights, n)

    Creates a model for the knapsack problem with the values, weights and limit n.
    See knapsack.py for usage of this.
    """
    z = intvar(0, 10000,name="z")
    x = intvar(0,1,shape=len(values),name="x")
    model = Model(
        [
        z >= 0,
        z == sum(x*values),
        sum(x*weights) <= n,
        ],
        maximize=z
        )
    return [model, x, z]


def my_abs(x,y,d):
  """
  A decomposition of abs() for experimentation.
  """
  constraints = []
  b = boolvar()
  constraints += [b == (x >= y)]
  constraints += [(b).implies(d == x - y)]
  constraints += [(~b).implies(d == y - x)]
  return constraints

def my_abs2(x,y):
  """
  A decomposition of abs() for experimentation.
  """
  constraints = []
  b = boolvar()
  d = intvar(0,1000000)
  constraints += [b == (x >= y)]
  constraints += [(b).implies(d == x - y)]
  constraints += [(~b).implies(d == y - x)]
  return d



def prod(x,res):
  """
  prod(x,res)

  res is the product of the values in x.
  """
  return [reduce(lambda a, b: a * b, x) == res]

def prod1(x):
  """
  prod1(x)

  return the product of the values in x.
  """
  return reduce(lambda a, b: a * b, x)


def among(m,x,v):
  """
  among(m,x,v)

  Requires exactly m variables in x to take one of the values in v.
  """
  return [m == sum([x[i] == j for i in range(len(x)) for j in v])]


#
# Symmetry breaking
#
# From
# http://en.wikipedia.org/wiki/Fr#C3#A9nicle_standard_form
# """
# A magic square is in Frénicle standard form, named for 
# Bernard Frénicle de Bessy, if the following two conditions apply:
#  - the element at position [1,1] (top left corner) is the smallest 
#    of the four corner elements; and
#  - the element at position [1,2] (top edge, second from left) is 
#    smaller than the element in [2,1].
# """
#
def frenicle(x,n):
  constraints = [x[(0,0)] == min([x[0,0], x[0,n-1], x[n-1,0], x[n-1,n-1]])]
  constraints += [x[0,1] < x[1,0]]
  return constraints



def distribute(card, value, base):
  """
  distribute(card, value, base)
  
  Requires that 'card[i]' is the number of occurences of 'value[i]' in 'base'.
  
  Note: card, value, and base are assumed to be intvar arrays.
  """
  card_len = len(card)
  value_len = len(value)
  assert card_len == value_len, "`card` and `value` must have the same length"
   
  base_len = len(base)

  constraints = []
  constraints += [AllDifferent(value)]
  for i in range(card_len):
    constraints += [
      card[i] == sum([value[i] == base[j] for j in range(base_len)])
      ]
  
  return constraints


def fill_array(x,x_val):
  """
  fill_array(x,x_val)

  If x_val[i] != None then x[i] == x_val[i].
  """
  constraints = []
  for i in range(len(x)):
    if x_val[i] != None:
      constraints += [x[i] == x_val[i]]

  return constraints




def all_different_pairs(a, s):
  """
  all_different_pairs(a, s)
  
  all pairs must be different
  """
  return [AllDifferent([p for p in pairs(a,s)])]


def increasing_pairs(a, s):
  """
  increasing_pairs(a, s)
  
  Ensure that the pairs are in increasing order.
  """
  return [increasing(pairs(a,s))]
  
def decreasing_pairs(a, s):
  """
  decreasing_pairs(a, s)
  
  Ensure that the pairs are in decreasing order.
  """
  return [decreasing(pairs(a,s))]

def pairs(a, s):
  """
  return the pairs of a in the 'integer representation': a[k,0]*(n-1) + a[k,1]
  s is the size of max value of n
  """
  n = len(a)
  return [ a[(k,0)]*(s-1) + a[(k,1)] for k in range(n)]


def all_min_dist(min_dist, x, n):
  """
  all_min_dist(min_dist, x, n)
  
  Ensures that the differences of all pairs (i !=j) are >= min_dist.
  """
  constraints = []
  for i in range(n):
    for j in range(i):
      constraints += [abs(x[i]-x[j]) >= min_dist] # Nope!
      
  return constraints



def all_different_on_intersection(x, y):
  """
  all_different_on_intersection(x, y)
  
  Ensure that the values that are common in x and y are distinct (in each array).
  """
  return [count_a_in_b(x,y), count_a_in_b(y,x)]


def count_a_in_b(ass,bss):
  """
  count_a_in_b(ass,bss)
  
  helper for all_different_on_intersection
  """
  constraints = []
  for a in ass:
    constraints += [sum([a == b for b in bss]) <= 1]
  return constraints

def all_different_modulo(x, m):
  """
  all_different_modulo(x, m)

  Ensure that all elements in x (modulo m) are distinct
  """
  print("x2:",x)
  n = len(x)
  constraints = []
  mods = intvar(0,m-1,shape=n)
  for i in range(n):
     constraints += [mods[i] == x[i] % m]
  constraints += [AllDifferent(mods)]
  
  return constraints


def all_different_cst(xs, cst):
  """
  all_different_cst(xs, cst)
  
  Ensure that all elements in xs + cst are distinct
  """
  return [AllDifferent([(x + c) for (x,c) in zip(xs,cst)])]



def arith(x, relop, val):
  """
  arith(x, relop, val)
  
  Ensure that all elements in x are <relop> val.
  """
  constraints = []
  for i in range(len(x)):
    constraints += [arith_relop(x[i],relop, val)]
  return constraints


def arith_relop(a, t, b):
  """
  arith_relop(a, t, b)
  
  This is (arguably) a hack.
  Represents each function as an integer 0..5.
  """
  return [(t == 0).implies(a  < b),
          (t == 1).implies(a <= b),
          (t == 2).implies(a == b),
          (t == 3).implies(a >= b),
          (t == 4).implies(a  > b),
          (t == 5).implies(a != b)
          ]



#
# diffn ported from MiniZinc's fzn_diffn:
# 
def diffn(x,y,dx,dy):
    """
    diffn(x,y,dx,dy)
    
    Constrains rectangles i, given by their origins x[i], y[i])
    and sizes (dx[i], dy[i]), to be non-overlapping. Zero-width
    rectangles can still not overlap with any other rectangle.
    """
    n = len(x)
    constraints = []
    for i in range(n):
        for j in range(i+1,n):
            constraints += [(x[i] + dx[i] <= x[j]) |
                            (y[i] + dy[i] <= y[j]) |
                            (x[j] + dx[j] <= x[i]) |
                            (y[j] + dy[j] <= y[i])
                            ]
    return constraints



def nvalue(m, x):
  """
  nvalue(m, x)
     
  Requires that there is exactly m distinct values in x
  (min_val and max_val are the minimum and maximum value
  in x, respectively)
  """
  n = len(x)
  min_val = min([x[i].lb for i in range(n)])
  max_val = max([x[i].ub for i in range(n)])  
  return (m == sum([ sum([ x[j] == i for j in range(n)]) > 0 for i in range(min_val, max_val+1)]))


#
# nvalues(x,op,n)
#
# Requires that the number of distinct values in the array x is 
#    op n 
# where
# op is either one of 
#   =, <m, =<, >=, >
#
def nvalues(x, op, n):
    xlen = len(x)
    m = intvar(1,xlen)
    return [nvalue(m,x),
            arith_relop(m,op,n)
            ]

def clique(g, clique, card):
  """
  clique(g, clique, card)

  Ensure that the boolean array 'clique' (of Integer Array type) 
  represents a clique in the graph g with the cardinality card.

  Note: This is kind of backward, but it is the whole thing:
  If there is a connection between nodes I and J (I != J) then
  there should be a node from I to J in G. If it's not then
  both c1 and c2 is not in the clique.
  """
  n = len(g)
  constraints = []
  constraints += [card == sum([clique[i] for i in range(n)])]
  for (c1,i) in zip(clique, range(n)):
    for (c2,j) in zip(clique, range(n)):
      if i != j and g[i][j] == 0:
        constraints += [(c1 == 0) | (c2 == 0)]
  return constraints


def assignment_model(cost, tasks=None,people=None,print_solution=None,opt="min"):
    """
    assignment_model(cost, rows, cols, tasks=None,people=None,print_solution=None,opt='min'):
    
    Fairly general implementation of the assignment problem:
    Minimize total cost of assign all task to one person given
    the cost of assigning a person to the tasks.

    For problems were 'task' and 'people' does not applies, a used-defined
    method 'print_solution' can be used.
    
    For maximization problems, use opt='max'.
    """

    rows = len(cost)
    cols = len(cost[0])
    
    max_cost = np.sum(np.array(cost))
    
    total_cost = intvar(0,max_cost,name='cost')
    x = boolvar(shape=(rows,cols),name="x")
    
    model = Model(
        total_cost >= 0,
        total_cost == np.sum([ x_row*cost_row for (x_row, cost_row) in zip(x, cost)]),
        
        # exacly one assignment per row, all rows (tasks) must be assigned.
        [sum(row) == 1 for row in x],
        
        # zero or one assignments per column (people)
        [sum(col) <= 1 for col in x.transpose()],
        )
    
    if opt == "max":
      model.maximize(total_cost)
    else:
      model.minimize(total_cost)


    ss = CPM_ortools(model)    
    if ss.solve():
        print("total_cost: ", total_cost.value())
        print("x:")
        print(x.value())
        print()

        if tasks == None and people == None:
            for i in range(rows):
                print("Task", i, end="")
                for j in range(cols):
                    if x[i][j].value() == 1:
                        print(" is done by ", j)
            print()
        else:
          if print_solution != None:
            print_solution(x.value(),tasks,people)
          else:
            for i in range(rows):
                print("Task", tasks[i], end="")
                for j in range(cols):
                    if x[i][j].value() == 1:
                        print(" is done by", people[j])
            print()


def latin_square(x):
  """
  latin_square(x)

  The matrix x is a Latin square.
  """
  return [[AllDifferent(row) for row in x],
          [AllDifferent(col) for col in x.transpose()]]




#
# reverses an array from -> to
#
def reverse(xfrom, xto):
  """
  reverse(xfrom, xto)

  xto is reverse of xfrom.
  """
  n = len(xfrom)
  return [xto[i] == xfrom[n-i-1] for i in range(n)]


def print_model_and_variables(model):
  """
  print_model_and_variables(model)

  Prints the following:
  - the unflattened model (via print(model))
  - the flattened model
  - the variables and the domains in the flattened model

  (From Tias Guns when he debugged one of my models. Thanks, Tias!)
  """
  print("Model:")
  print(model)
  print("\nFlattened model and variables:")
  mf = flatten_model(model)
  print_variables(mf)
  print(mf)
  print()




def argmax(x,p):
  """
  argmax(x,p)

  Ensure that p is the argmax, i.e. the position of the maximum value
  in x.
  Note: If there are many maximum values then argmax(x,p) will find
  all these values.
  """
  n = len(x)
  constraints = []
  for i in range(n):
    constraints += [(p != i).implies(x[p] > x[i]) ]
  return constraints

def argmin(x,p):
  """
  argmin(x,p)

  Ensure that p is the argmin, i.e. the position of the minimum value
  in x.
  Note: If there are many minimum values then argmin(x,p) will find
  all these values.
  """
  n = len(x)
  constraints = []
  for i in range(n):
    constraints += [(p != i).implies(x[p] < x[i]) ]
  return constraints



def argmin_except_c(x,p,c):
  """
  argmin_except_c(x,p,c)

  Ensure that p is the argmin, i.e. the position of the minimum value
  in x, but ignores any value of c.
  
  Note:
  - If there are many minimum values then argmin_except_c(x,p,c) will find
    all these values.
  - We assume that there are at least one value != c.
  """
  n = len(x)
  constraints = [x[p] != c]  
  for i in range(n):
    constraints += [(p != i).implies((x[i] == c) | (x[p] < x[i])) ]
    
  return constraints

def argmin_except_0(x,p):
  """
  argmin_except_0(x,p)

  Ensure that p is the argmin, i.e. the position of the minimum value
  in x, but ignores any value of 0.
  
  Note:
  - If there are many minimum values then argmin_except_0(x,p) will find
    all these values.
  - We assume that there are at least one value > 0.
  """
  return argmin_except_c(x,p,0)


def argmax_except_c(x,p,c):
  """
  argmax_except_c(x,p,c)

  Ensure that p is the argmax, i.e. the position of the minimum value
  in x, but ignores any value of c.
  
  Note:
  - If there are many maximum values then argmax_except_c(x,p,c) will find
    all these values.
  - We assume that there are at least one value != c.
  """
  n = len(x)
  constraints = [x[p] != c]  
  for i in range(n):
    constraints += [(p != i).implies((x[i] == c) | (x[p] > x[i])) ]
  return constraints



def permutation3(x,p,y):
  """
  permutation(x,p,y)

  Ensure that the array y is a permutation of array x with the permutation
  operations in array p.

  Example:
    x = [2,0,1,3]
    p = [2,1,3,0]

  What is y?
    y[0] = x[p[0]] = x[2] = 1
    y[1] = x[p[1]] = x[1] = 0
    y[2] = x[p[2]] = x[3] = 3
    y[3] = x[p[3]] = x[0] = 2

  Thus:
    y = [1,0,3,2]

  Assumptions:
  - We assume that x, p, and y has distinct values, i.e. constrained by
    AllDifferent.

  We check that:
  - p has the domain of 0..len(p)-1
  """
  n = len(x)
  assert n == len(p) and n == len(y), f"Length of x, p, and y must be the same"
  p_lb, p_ub = get_min_max_domain(p)
  assert p_lb == 0 and p_ub == n-1, "Domain value of p must be 0..n-1"
  
  constraints = []
  for i in range(n):
    constraints += [y[i] == x[p[i]] ]
  return constraints


def permutation(x,y):
  """
  permutation(x,y)

  Ensure that the array y is a permutation of array x,
  connected with some unknown permutation.

  permutation3(x,p,y) is used (which see).
  """
  n = len(x)
  p = intvar(0,n-1,shape=n)
  return permutation3(x,p,y)
  

def get_min_max_domain(x):
  """
  get_min_max_domain(x)

  Return the minimum and maximum domain of an array x.
  """
  n = len(x)
  x_lb = min([x[i].lb for i in range(n)])
  x_ub = max([x[i].ub for i in range(n)])

  return [x_lb,x_ub]


def chain(op,x):
  """
  chain(op,x)

  Ensure that all elements pairwise satisfies the binary operator op.

  Note: In order for this to work the operator must be from the
        operator library, e.g. operator.lt, operator.ne, e.g:
          chain(operator.lt,x)
  Note: Many of the binary operator.* has a definition already, e.g.
      (from cpmpy_hakank.py):
      increasing, increasing_strict, decreasing, descreasing_strict
      and
      AllDifferent, AllEqual

  """
  n = len(x)
  constraints = []
  for i in range(1,n):
    constraints += [ op(x[i-1], x[i]) ]
    
  return constraints



def minimum_except_c(x,min_val,c,allow_all_c=False):
  """
  minimum_except_c(x,min_val,c,allow_all_c)

  Ensures that min_val is the minimum value in array x, ignoring the value of c.

  The flag allow_all_c:
  - If True: allow an array with only c values: min_val is thus c.
  - If False: assume that there is at least one non c value. min_val must be != c.
  """
  n = len(x)
  ix = intvar(0,n-1)
  
  # Ensure that min_val is in x
  constraints = [min_val == x[ix]]
  for j in range(n):
    constraints += [(min_val <= x[j]) | (x[j] == 0)]
  if allow_all_c:
    max_val = max(x) # To be able to handle the case when there is only 0s
    constraints += [(max_val == c)==(min_val == c)]
  else:
    constraints += [min_val != c]
  

  return constraints 

def minimum_except_0(x,min_val,allow_all_0s=False):
  """
  minimum_except_0(x,min_val,allow_all_0s)

  Ensures that min_val is the minimum value in array x, ignoring 0s.
  
  The flag allow_all_0s:
  - If True: allow an array with only 0 values: min_val is thus 0.
  - If False: assume that there is at least one non 0 value. min_val must be != 0.
  """
  return minimum_except_c(x,min_val,0,False)



def value_precede(s,t,x):
    """
    value_precede(s,t, x)
    
    Ensures that the (first occurrence) of the value s precedes
    the (first occurrence) of the value t in array x if both
    s and t are in x.

    This means that for t to occur in x then s has to precede t.

    This definition is inspired by MiniZinc's definition
    value_precede.mzn
    """
    n = len(x)
    bs = boolvar(shape=n+1)
    constraints = []
    for i in range(n):
        xis = boolvar()
        constraints += [(xis ==1)==(x[i] == s),
                        (xis ==1).implies(bs[i+1]==1),
                        (xis == 0).implies(bs[i]==bs[i+1]),
                        (bs[i] == 0).implies(x[i] != t)
                        ]
    constraints += [bs[0] == 0]
    return constraints

def value_precede_chain(c,x):
    """
    value_precede_chain(c, x)

    Ensures that the value c[i-1] precedes the value c[i] is the array x
    if both c[i-1] and c[i] are in x.

    See value_precede().
    """
    n=len(c)
    constraints = []
    for i in range(1,n):
        constraints += [value_precede(c[i-1],c[i],x)]
        
    return constraints


def sliding_sum(low, up, seq, x):
  """
  sliding_sum(low, up, seq, x)

  Ensure that all sequences of length seq in x sums to between low and up.
  """
  vlen = len(x)
  constraints = []
  for i in range(vlen-seq+1):
    s = intvar(low,up)    
    constraints += [s == sum([x[j] for j in range(i,i+seq)])]
  return constraints


def no_overlap(s1, d1, s2, d2):
  """
  no_overlap(s1, d1, s2, d2)

  Ensures that task 1 (start time s1 with duration d1) does not overlap with
  task2 (start time s2 with duration d2)
  """
  return [(s1 + d1 <= s2) | (s2 + d2 <= s1)]

def is_prime(n):
  """
  is_prime(n)

  Returns True if the number n is a prime number, otherwise return False.
  """
  if n < 2: return False
  if n == 2: return True
  if not n & 1:
    return False
  for i in range(3, 1+int(math.sqrt(n)), 2):
    if n % i == 0:
      return False
  return True

def primes(limit):
  """
  primes(limit)

  Returns the prime numbers below limit.
  """
  primes = [2]
  i = 3
  for i in range(3, limit, 2):
    if is_prime(i):
      primes.append(i)
  return primes


def all_different_reif(x,b):
    """
    all_different_reif(x,b)

    b == 1 if all values in x are different, else 0.
    """
    n = len(x)
    m = intvar(1,n)
    return [nvalue(m,x),
            (m==n)==(b==1)
            ]


def all_different_reif_m(model,x):
    """
    all_different_reif(x,b)

    b == 1 if all values in x are different, else 0.
    This version returns b.
    
    Note that the model is a parameter so it must be
    created first:

        x = intvar(...)
        b = boolvar()
        model = Model(...)
        model += [b == all_different_reif_m(model,x)]
        
    """
    n = len(x)
    m = intvar(1,n)
    b = boolvar()
    model += [nvalue(m,x),
              (m==n)==(b==1)]
    return b


def lex_chain_less(x):
  """
  lex_chain_less(x)
  
  Require that all the rows are lexicographically sorted
  (but not the columns as in lex2).
  See: http://www.emn.fr/z-info/sdemasse/gccat/Clex_chain_less.html
  """
  n = len(x)
  m = len(x[0])
  constraints = []
  for i in range(1,n):
    constraints += [lex_less([x[i-1,j] for j in range(m)], [x[i,j] for j in range(m)])]

  return constraints
  

def soft_alldifferent(x,p):
  """
  soft_alldifferent(x,p)

  p is the number of pairs that have the same value.
  
  See http://www.emn.fr/z-info/sdemasse/gccat/Csoft_alldifferent_ctr.html
  """
  n = len(x)
  return [p == sum([x[i] == x[j] for i in range(n) for j in range(i+1,n)])]



def among_seq(low,high,seqlen,x,v):
    """
    among_seq(low, high, seqlen, x, v)

    Ensures that all sequences of length SeqLen in the list X 
    contains at least Low and atmost High occurrences of V.
    """
    n = len(x)
    size = n-seqlen+1
    constraints = []
    for i in range(size):
       seq = [x[j] for j in range(i,i+seqlen)]
       constraints += [among_range(low, high, seq, v)]

    return constraints


def among_range(low, high,x,v):
    """
    among_range(low, high, x, v)

    Ensures that the list x contains at least low and atmost high
    occurrences of v.
    Used by among_seq.
    """
    xs = intvar(0,len(x))
    vlen = len(v)
    return [
      xs == sum([sum([el == v[i] for i in range(vlen)])>0 for el in x]),
      xs >= low,
      xs <= high]



def sequence(x,seq_length, lbound,ubound):
    """
    sequence(,length,lbound,ubound)
     
    Ensures that all sums of every subsequence of length length
    in array x is between lbound and ubound
    """
    n = len(x)
    xs = intvar(lbound.lb,ubound.ub)
    constraints = []
    for i in range(n-seq_length+1):
       constraints += [xs == sum([x[j] for j in range(i,i+seq_length)]),
                       xs >= lbound,
                       xs <= ubound
                       ]

    return constraints


def sort_array(x,y):
    """
    sort_array(x,y)

    Ensure that y is a sorted version of x.

    Note: If x contains duplicate values then there will be
          multiple solutions.

          Example: for x = [2,1,2] there are two identical solutions,
                   since the two 2 are not distinct:
                     x: [2 1 2]
                     y: [1 2 2]  (permutation: 1,0,2)
                     
                     x: [2 1 2]
                     y: [1 2 2]  (permutation: 2,0,1)

    If x is distinct, however, then there there will be the expected len(x)!
    solutions.
    """
    n = len(x)
    p = intvar(0,n-1,shape=n) # the permutation array
    return [permutation3(x,p,y),  
            AllDifferent(p),
            increasing(y)
            ]



def all_different_consecutive_values(x):
    """
    all_different_consecutive_values(x)
    
    Ensure:
    - that all variables of x to take distinct values and 
    - that the difference between the largest and the smallest values 
      of the x collection is equal to the number of variables 
      minus one (i.e., there is no holes at all within the used values).
    (http://www.emn.fr/z-info/sdemasse/gccat/Calldifferent_consecutive_values.html)
    """
    return [AllDifferent(x),
            max(x) - min(x) == len(x) - 1]



def all_different_explain(x, d):
    """
    all_different_explain(x, d)
   
    Ensure that d[i] == 0 if there is atmost 1 occurrences of the value i
    in x.  d[i] == 1 of there are more than one occurrence in x, and is
    thus the 'explanation' that the all_different constraint fails.
    """
    n = len(x)
    xmin, xmax = get_min_max_domain(x) # domain of x
    constraints = []
    for i in range(xmin,xmax+1):
        # remove values not in x
        constraints += [(sum([x[j] == i for j in range(n)]) == 0).implies(d[i] == 0)]
        # mark duplicates in s
        constraints += [(sum([(x[a] == i) & (x[b] == i)
                              for a in range(n)
                              for b in range(a+1,n) ]) > 0) == (d[i] == 1)]
    return constraints
                             

