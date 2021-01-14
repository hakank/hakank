#=

  Constraint utils in Julia ConstraintSolver.jl

  Many of these decompositions were inspired/ported from
  my Picat implementations (http://hakank.org/picat/)

  This model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

#
# resize_matrix(grid)
#
# Resize a (column) matrix of the form (3,)
#    [[1, 2,3],
#     [4,5,6],
#     [7,8,9]
#     ]
# To a 3x3 (row) matrix
#    [ 1 2 3;
#      4 5 6;
#      7 8 9
#    ]
#
function resize_matrix(grid)
    rows = size(grid)
    # reduce(hcat,grid)'
    transpose(reduce(hcat,grid)) # Suggestion by @Mason Protter @ Zulip
end

#
# Print a grid (row matrix)
#
function print_grid(grid)
    rows, cols = size(grid)
    for row in 1:rows
        println(grid[row,:])
    end
    println()
end


#
# print_matrix_str(x,zstring)
#
# Print each row and convert each element to the corresponding position
# in zstring. "z" is for zero-based, i.e. first char corresponds to 0, etc
#
function print_matrix_str(x,zstring="")
    for row in eachrow(x)
        if zstring == ""
            println(row)
        else
            println(join(row.|>c->zstring[c+1]))
        end
    end
    println()
end


#
# Scalar product(model, s,x,v)
#
# Ensure that
#    s == x.*v
# Where x or v might be an array of decision variables
#
function scalar_product(model,x,v,s)
    @constraint(model, s == sum(x.*v))
end

#
# scalar_product(model,x,v,cond,x)
#
# Ensures that x.*v op s
#
function scalar_product(model,x,v,cond,s)
    if cond == :(>=)
        @constraint(model, sum(x.*v) >= s)
    elseif cond == :(<=)
        @constraint(model, sum(x.*v) <= s)
    elseif cond == :(==)
        @constraint(model, sum(x.*v) == s)
    else
        error("scalar_product(model,s,x,cond,v): unknown cond: $cond")
    end

end


#
# to_num(model, x, base, num)
#
# Converts a number num to/from a list of integer list (x) given a base base
#
function to_num(model, x, base, num)
   len = length(x)
   @constraint(model, num == sum([x[i]*base^(len-i) for i in 1:len]))
end

#
# increasing(model, x)
#
# Ensure that array x in increasing order
#
function increasing(model, x)
    len = length(x)
    for i in 2:len
        @constraint(model, x[i-1] <= x[i])
    end
end

function increasing_strict(model, x)
    len = length(x)
    for i in 2:len
        @constraint(model, x[i-1] <= x[i])
        @constraint(model, x[i-1] != x[i])
    end
end


#
# decreasing(model, x)
#
# Ensure that array x in decreasing order
#
function decreasing(model, x)
    len = length(x)
    for i in 2:len
        @constraint(model, x[i-1] >= x[i])
    end
end

function decreasing_strict(model, x)
    len = length(x)
    for i in 2:len
        @constraint(model, x[i-1] >= x[i])
        @constraint(model, x[i-1] != x[i])
    end
end


#
# all_different_except_c
#
# Ensure that all values (except c) are distinct
# Thanks to Ole who fixed some initial problems I had.
# (See https://github.com/Wikunia/ConstraintSolver.jl/issues/202 for
# details.)
#
function all_different_except_c(model, x, c=0)
    n = length(x)

    # Define the variables we'll use
    b_len = length([1 for i in 2:n for j in 1:i-1 for k in 1:3])
    bs = @variable(model, [1:b_len], Bin) # "Anonymous" variables
    c = 1
    for i in 2:n, j in 1:i-1
        b1 = bs[c]
        b2 = bs[c+1]
        b3 = bs[c+2]
        @constraint(model, b1 := {x[i] != 0})
        @constraint(model, b2 := {x[j] != 0})
        @constraint(model, b3 := {b1 + b2 == 2})
        @constraint(model, b3 => {x[i] != x[j]})
        c += 3
    end
    # return bs so we can print it in the main function
    return bs
end



#
# count_ctr(model, x, val, op, s)
#
# Ensure that there the exactly s occurrences of
#    x[i] op val
#
# TODO: handle < and >
#       i.e. <= && !=  and >= && !=
#
function count_ctr(model, x, op, val, s)
    xflat = vcat(x...)
    len = length(xflat)
    b = @variable(model, [1:len], Bin)
    for i in 1:len
        if op == :(==)
            @constraint(model, b[i] := { xflat[i] == val})
        elseif op == :(<=)
            @constraint(model, b[i] := { xflat[i] <= val})
        elseif op == :(>=)
            @constraint(model, b[i] := { xflat[i] >= val})
        elseif op == :(!=)
            @constraint(model, b[i] := { xflat[i] != val})
        end
    end
    @constraint(model, s == sum(b))
end


#
# my_abs(model, x, y, d)
#
#  d = abs(x-y)
#
function my_abs(model, x, y, d)
    b = @variable(model, [1:1], Bin)
    @constraint(model, b[1] := {x >= y})
    @constraint(model, b[1] => {d == x - y})
    @constraint(model, !b[1] => {d == y - x})
end


# my_abs(model, x, a)
#
#  a = abs(x)
#
function my_abs(model, x, a)
    my_abs(model,x, 0, a)
    #=
    b = @variable(model, [1:1], Bin)
    @constraint(model, b[1] := {x >= 0})
    @constraint(model, b[1] => {a == x})
    @constraint(model, !b[1] => {a == -x})
    =#
end


#
# my_element(model, a, ix, val)
#
# Ensure that a[ix] = val
#
function my_element(model, ix, a, val)
    n = length(a)
    b = @variable(model, [1:n], Bin)
    for i in 1:n
        @constraint(model,b[i] => {a[i] == val})
        @constraint(model,b[i] := {ix == i})
    end
end


#
# Both a and gcc are lists.
#
# This version is bidirectional but limited:
#
# The list a can contain only values 1..gcc_len (i.e. the length of gcc).
# This means that the caller must know the max values of a.
# Or rather: if a contains another values they will not be counted.
#
function global_cardinality_count(model, a, gcc)
    len = length(a)
    gcc_len = length(gcc)
    for i in 1:gcc_len
       # count_ctr(model, x, op, val, s)
       count_ctr(model, a,:(==), i, gcc[i])
    end
    # This works as well
    # (1:length(gcc)).|>i->count_ctr(model, a,:(==),i,gcc[i])
end



#
# modulo(model, x, y, z)
#
# Ensures that  x mod y == z.
#
# Note: experimental.
# For large domains the table will blow up so handle with care!
#
function modulo(model, x, y, z)
    lbx = round(Int, JuMP.lower_bound(x))
    ubx = round(Int, JuMP.upper_bound(x))
    lby = round(Int, JuMP.lower_bound(y))
    uby = round(Int, JuMP.upper_bound(y))

    table = resize_matrix([ [i,j,i % j] for i in lbx:ubx, j in lby:uby if j != 0])
    @constraint(model, [x, y, z] in CS.TableSet(table))
end


#
# my_argmin(model, x, m, ix)
#
# Ensure that m is the minimum element and m = x[ix]
#
function my_argmin(model, x, m, ix)
    n = length(x)
    my_element(model, ix,x, m)
    for i in 1:n
        @constraint(model, m <= x[i] )
    end
end

#
# my_argmax(model, x, m, ix)
#
# Ensure that m is the maximum element and m = x[ix]
#
function my_argmax(model, x, m, ix)
    n = length(x)
    my_element(model, ix,x, m)
    for i in 1:n
        @constraint(model, m >= x[i] )
    end
end




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
function cumulative(model, start, duration, resource, limit)
    tasks = [i for i in 1:length(start) if resource[i] > 0 && duration[i] > 0]
    num_tasks = length(tasks)

    times_min = minimum(round.(Int,[JuMP.lower_bound(start[i]) for i in tasks]))
    times_max = maximum(round.(Int,[JuMP.upper_bound(start[i])+duration[i] for i in tasks]))
    println("times: $(times_min)..$(times_max)")
    for t in times_min:times_max
        bs = @variable(model, [1:num_tasks], Bin)
        bt = @variable(model, [1:num_tasks], Bin)
        b  = @variable(model, [1:num_tasks], Bin)
        for i in tasks
            # The following don't work since ConstraintSolver don't
            # support nonlinear constraints
            # @constraint(model,sum([(start[i] <= t) * (t <= start[i] + duration[i])*resource[i] for i in tasks])  <= b)

            # is this task active during this time t?
            @constraint(model, bs[i] := {start[i] <= t})
            @constraint(model, bt[i] := {t <= start[i]+duration[i]-1}) # should be '<'
            @constraint(model, b[i] := { bs[i] + bt[i] == 2}) # is this task active in time t ?
        end
        # Check that there's no conflicts in time t
        @constraint(model,sum([b[i]*resource[i] for i in tasks]) <= limit)
  end

end


#
# circuit(x) succeeds for the array x if it's a circuit.
#
# This implementation use an extra array (z) for the orbit of x[1].
#
function circuit(model, x)
    n = length(x)
    z = @variable(model, 1 <= z[1:n] <= n, Int)

    #
    # The main constraint is that Z[I] must not be 1
    # until I = N, and for I = N it must be 1.
    #
    @constraint(model, x in CS.AllDifferentSet())
    @constraint(model, z in CS.AllDifferentSet())

    # put the orbit of x[1] in z[1..n]
    @constraint(model, x[1] == z[1])

    # when i = n it must be 1
    @constraint(model, z[n] == 1)

    # Get the orbit for z
    for i in 2:n
       my_element(model, z[i-1],x,z[i])
    end

end


#
# circuit_path(model,x,path) succeeds for the array x if it's a circuit
# and path is the corresponding path (orbit).
#
# This implementation use an extra array (z) for the orbit of x[1].
#
function circuit_path(model, x, path)
    n = length(x)

    #
    # The main constraint is that Z[I] must not be 1
    # until I = N, and for I = N it must be 1.
    #
    # @constraint(model, x in CS.AllDifferentSet())
    # @constraint(model, path in CS.AllDifferentSet())

    # put the orbit of x[1] in z[1..n]
    @constraint(model, x[1] == path[1])

    # when i = n it must be 1
    @constraint(model, path[n] == 1)

    # Get the orbit for path
    for i in 2:n
       my_element(model, path[i-1],x,path[i])
    end

end



#
# inverse(model, x)
#
#  From Global Constraint Catalog
#  http://www.emn.fr/z-info/sdemasse/gccat/Cinverse.html
#  """
#  inverse(NODES)
#
# Enforce each vertex of a digraph to have exactly one predecessor and
# one successor. In addition the following two statements are equivalent:
#    - The successor of the ith node is the jth node.
#    - The predecessor of the jth node is the ith node.
# """
#
#  This means that for each element X[I] either
#     - X[I] = I
#     or
#     - X[I] = J <=> X[J] = I
#
# Cf the 2d version assignment_ctr(model, x, y) below
#
function inverse(model, x)
    len = length(x)
    b = @variable(model, [1:len,1:len], Bin)
    # Picat:
    # foreach(I in 1..Len, J in 1..Len)
    #  J = X[I] <=> I = X[J]
    # end
    for i in 1:len, j in 1:len
       @constraint(model, b[i,j] := { x[i] == j})
       @constraint(model, b[j,i] := { x[j] == i})
       @constraint(model, (b[i,j] + b[j,i]) != 1 )
    end
end


#=
From the Picat documentation
"""
assignment(FDVars1,FDVars2): This constraint ensures that FDVars2 is a dual
assignment of FDVars1, i.e., if the ith element of FDVars1 is j,
then the jth element of FDVars2 is i.
"""

The constraint can be defined as:
  foreach(I in 1..N, J in 1..N)
     X[I] = J <=> Y[J] = I
  end.

Note: We assume that both x and y are distinct arrays (respectively).
Also: Compare with the single array constraint inverse (tested in inverse.jl)

=#
function assignment_ctr(model, x, y)
    n = length(x)
    b1 = @variable(model, [1:n,1:n], Bin)
    b2 = @variable(model, [1:n,1:n], Bin)
    for i in 1:n, j in 1:n
       @constraint(model, b1[i,j] := { x[i] == j})
       @constraint(model, b2[j,i] := { y[j] == i})
       @constraint(model, b1[i,j] + b2[j,i] != 1 )
    end
end


#=
   matrix_element(model, x, i ,j, val)

   Ensures that x[i,j] = val

=#
function matrix_element(model, x,i,j,val)
    n, m = size(x)
    bi = @variable(model,[1:n], Bin) # row
    bj = @variable(model,[1:m], Bin) # column
    bij = @variable(model, [1:n,1:m], Bin) # matrix
    @constraint(model, sum(bi) == 1)
    @constraint(model, sum(bj) == 1)
    @constraint(model, sum(bij[:]) == 1)
    for k in 1:n, l in 1:m
        @constraint(model,bi[k] := { k == i }) # fix row
        @constraint(model,bj[l] := { l == j }) # fix column
        @constraint(model,bij[k,l] := { bi[k] + bj[l] == 2 })
        # Connection constraints
        # These don't work now (ConstraintSolver.jl v0.5.3)
        # since for  b => {....} b must be a boolen variable
        # See https://github.com/Wikunia/ConstraintSolver.jl/issues/224
        # @constraint(model, val == x[k,l]  => {bi[k] + bj[l] == 2} )
        # @constraint(model, (bi[k] + bj[l] == 2) => { val == x[k,l] } )

        @constraint(model, bij[k,l] => { val == x[k,l] } )
    end
end


#=
  regular(model,x,q,s,q0,f)

  This is a translation of MiniZinc's regular constraint (defined in
  lib/zinc/globals.mzn), via the Comet code (via Picat).
  All comments are from the MiniZinc code.
  """
  The sequence of values in array 'x' (which must all be in the range 1..S)
  is accepted by the DFA of 'Q' states with input 1..S and transition
  function 'd' (which maps (1..Q, 1..S) -> 0..Q)) and initial state 'q0'
  (which must be in 1..Q) and accepting states 'F' (which all must be in
  1..Q).  We reserve state 0 to be an always failing state.
  """

  x : IntVar array
  Q : number of states
  S : input_max
  d : transition matrix
  q0: initial state
  F : accepting states
=#
function regular(model,x,q,s,d,q0,f)
    # """
    # If x has index set m..n-1, then a[m] holds the initial state
    # (q0), and a[i+1] holds the state we're in after  processing
    # x[i].  If a[n] is in F, then we succeed (ie. accept the string).
    # """
    m = 1
    n2 = length(x)+1
    # The selected states (which is returned)
    a = @variable(model, 1 <= a[1:n2] <= q, Int)

    @constraint(model, a[m] == q0) # Set a[0], initial state
    for i in 1:length(x)
        @constraint(model, 1 <= x[i])      # Do this in case it's a var.
        @constraint(model, x[i] <= s)      # Do this in case it's a var.
        # Here is MiniZinc's infamous matrix element
        #     a[i+1] = d[a[i], x[i]]
        matrix_element(model, d, a[i], x[i], a[i+1])
    end
    # """Check the final state is in F."""
    f_ix = @variable(model, [1:1],CS.Integers(1:length(f))) # index in accepting states
    my_element(model,f_ix[1],f,a[n2])

    return a
end



#
# atmost(model,x,v,m)
#
# Ensures that the number of elements with value v is <= m
#
function atmost(model,x,v,m)
    n = length(x)
    b = @variable(model, [1:n],Bin)
    for i in 1:n
        @constraint(model,b[i]:={x[i] == v})
    end
    @constraint(model, sum(b) <= m)
end

#
# atleast(model,x,v,m)
#
# Ensures that the number of elements with value v is >= m
#
function atleast(model,x,v,m)
    n = length(x)
    b = @variable(model, [1:n],Bin)
    for i in 1:n
        @constraint(model,b[i]:={x[i] == v})
    end
    @constraint(model, sum(b) >= m)
end

#
# exactly(model,x,v,m)
#
# Ensures that the number of elements with value v is == m
#
function exactly(model,x,v,m)
    n = length(x)
    b = @variable(model, [1:n],Bin)
    for i in 1:n
        @constraint(model,b[i]:={x[i] == v})
    end
    @constraint(model, sum(b) == m)
end

#
# latin_square(model, x)
#
# Ensures that the square matrix is a Latin Square.
#
function latin_square(model, x)
    n,_ = size(x)
    for i in 1:n
        @constraint(model, x[i,:] in CS.AllDifferentSet())
        @constraint(model, x[:,i] in CS.AllDifferentSet())
    end
end

#
# no_overlap(model, begins,durations)
#
# Ensure that there is no overlap between the tasks.
#
function no_overlap(model, begins,durations)
    n = length(begins)
    for i in 1:n, j in i+1:n
        b = @variable(model,[1:2], Bin)
        @constraint(model,b[1] := {begins[i] + durations[i] <= begins[j]})
        @constraint(model,b[2] := {begins[j] + durations[j] <= begins[i]})
        @constraint(model, sum(b) >= 1)
    end
end

#
# mult_table2(model,lb=1,ub=9) # 2D
#
# Generate a multiplication table of i*j and i*j*k
# for i,j,.. in lb:ub
#
# i*j
function mult_table2(lb=1,ub=9)
    resize_matrix([[i,j,i*j] for i in lb:ub, j in lb:ub if i != j])
end

# i*j*k
function mult_table3(lb=1,ub=9)
    resize_matrix([[i,j,k,i*j*k] for i in lb:ub, j in lb:ub, k in lb:ub if i != j && i != k && j != k])
end

# i*j*k*l
function mult_table4(lb=1,ub=9)
    resize_matrix([[i,j,k,l,i*j*k*l] for i in lb:ub, j in lb:ub, k in lb:ub, l in lb:ub
                                     if i != j && i != k && i != l &&
                                        j != k && j != l &&
                                        k != l
                                        ])
end


#
# global_contiguity_regular(model, x)
#
# Ensures that x contains 0 and 1s and that the
# 1s are in a contiguous sequence.
#
function global_contiguity_regular(model, x)
    n = length(x)

    # Transition function (MiniZinc style)
    # This use the regular expression "0*1*0*" to
    # require that all 1's (if any) in an array appear contiguously.
    transition = resize_matrix([
                  [1,2], # state 1 (start) input 0 -> state 1, input 1 -> state 2 i.e. 0*
                  [3,2], # state 2: 1*
                  [3,0]  # state 3: 0*
                 ])
    n_states = 3
    input_max = 2
    initial_state = 1
    accepting_states = [1,2,3]

    reg_input = @variable(model, [1:n], CS.Integers(1:input_max)) # 1..2

    # Translate x's 0..1 to reg_input's 1..2
    # (since regular use 0 as an invalid state)
    for i in 1:n
       @constraint(model,reg_input[i] == x[i]+1)
    end

    return regular(model,reg_input,n_states,input_max,transition,initial_state, accepting_states)

end
