#=

  Constraint utils in Julia ConstraintSolver.jl

  Many of these decompositions were inspired/ported from
  my Picat implementations (http://hakank.org/picat/)

  This model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my Julia page: http://www.hakank.org/julia/

=#

using LinearAlgebra

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
    # @constraint(model, s == sum(x.*v))
    @constraint(model, s == dot(x, v))
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
        @constraint(model, x[i-1] < x[i])
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
        @constraint(model, x[i-1] > x[i])
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
function all_different_except_c(model,x, c=0)
    len = length(x)
    for i in 2:len, j in 1:i-1
        b = @variable(model, binary=true)
        @constraint(model, b := {x[i] != c && x[j] != c})
        @constraint(model, b => {x[i] != x[j]} ) 
    end
end


#
# count_ctr(model, x, val, op, s)
#
# Ensure that there the exactly s occurrences of
#    x[i] op val
#
function count_ctr(model, x, op, val, s)
    len = length(x)
    b = @variable(model, [1:len], Bin)
    for i in eachindex(x)
        if op == :(==)
            @constraint(model, b[i] := { x[i] == val})
        elseif op == :(<=)
            @constraint(model, b[i] := { x[i] <= val})
        elseif op == :(>=)
            @constraint(model, b[i] := { x[i] >= val})
        elseif op == :(!=)
            @constraint(model, b[i] := { x[i] != val})
        elseif op == :(<)
            @constraint(model, b[i] := { x[i] < val})
        elseif op == :(>)
            @constraint(model, b[i] := { x[i] > val})
        end
    end
    @constraint(model, s == sum(b))
end

#
# Ensure that there the exactly s occurrences of
#    x[i] op y[i]
#
function count_ctr2(model, x, op, y, s)
    len = length(x)
    len2 = length(y)
    @assert len == len2 "Lengths must be the same!"
    b = @variable(model, [1:len], Bin)
    for i in eachindex(x)
        if op == :(==)
            @constraint(model, b[i] := { x[i] == y[i]})
        elseif op == :(<=)
            @constraint(model, b[i] := { x[i] <= y[i]})
        elseif op == :(>=)
            @constraint(model, b[i] := { x[i] >= y[i]})
        elseif op == :(!=)
            @constraint(model, b[i] := { x[i] != y[i]})
        elseif op == :(<)
            @constraint(model, b[i] := { x[i] < y[i]})
        elseif op == :(>)
            @constraint(model, b[i] := { x[i] > y[i]})
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
    b = @variable(model, [1:1], Bin)
    @constraint(model, b[1] := {x >= 0})
    @constraint(model, b[1] => {a == x})
    @constraint(model, !b[1] => {a == -x})
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
# Thanks Ole for suggestions on this (https://github.com/Wikunia/ConstraintSolver.jl/issues/235)
function modulo(model, x, y, z)
    if x isa Integer
        x = @variable(model, lower_bound=x, upper_bound=x, integer=true)
    end
    if y isa Integer
        y = @variable(model, lower_bound=y, upper_bound=y, integer=true)
    end
    if z isa Integer
        z = @variable(model, lower_bound=z, upper_bound=z, integer=true)
    end

    lbx = round(Int, JuMP.lower_bound(x))
    ubx = round(Int, JuMP.upper_bound(x))
    lby = round(Int, JuMP.lower_bound(y))
    uby = round(Int, JuMP.upper_bound(y))

    table = resize_matrix([ [i,j,i % j] for i in lbx:ubx, j in lby:uby if j != 0])
    @constraint(model, [x, y, z] in CS.TableSet(table))
end

#
# mult(model, x, y, z)
#
# ensures that z = x * y
#
# This is experimental!
# Note: This don't scale but works in smaller models.
#
function mult(model, x, y, z)
    # println("$mult(model, $x, $y, $z)")
    x_fixed = false
    if x isa Integer
        x = @variable(model, lower_bound=x, upper_bound=x, integer=true)
        x_fixed = true
    end

    y_fixed = false
    if y isa Integer
        y = @variable(model, lower_bound=y, upper_bound=y, integer=true)
        y_fixed = true
    end

    z_fixed = false 
    if z isa Integer
        z = @variable(model, lower_bound=z, upper_bound=z, integer=true)
        z_fixed = true
    end

    lbx = round(Int, JuMP.lower_bound(x))
    ubx = round(Int, JuMP.upper_bound(x))
    lby = round(Int, JuMP.lower_bound(y))
    uby = round(Int, JuMP.upper_bound(y))
    lbz = round(Int, JuMP.lower_bound(z))
    ubz = round(Int, JuMP.upper_bound(z))

    # Handle z:
    #  min z = lbx*lby 
    #  max_z = ubx*uby
    if !z_fixed 
        if lbz < lbx*lby 
            lbz = lbx*lby
        end

        if ubz > ubx*uby
            ubz = ubx*uby
        end 
        @constraint(model, lbz <= z)
        @constraint(model, z <= ubz)
        
    end

    table = resize_matrix([ [i,j,i * j] for i in lbx:ubx, j in lby:uby])
    # println("table:$table")
    @constraint(model, [x, y, z] in CS.TableSet(table))

end

#
# either_eq(model,c1,c2,c3,c4)
#
# ensures  c1 = c2 \/ c3 = c4
#
# Example (from jobs_puzzle.jl):
#   either_eq(model, Nurse,Steve, Nurse,Pete)
#
function either_eq(model, c1, c2, c3,c4)
    b = @variable(model, [1:2], Bin)
    @constraint(model, b[1] := {c1 == c2})
    @constraint(model, b[2] := {c3 == c4})
    @constraint(model, sum(b) == 1)
end



#
# is_member_of(model,el, a)
#
# Ensure that element el is in the array a
# 
# Example (from jobs_puzzle.jl):
#   is_member_of(model,Nurse, [Steve,Pete])
#
function is_member_of(model, el, a)
    @constraint(model, [el] in CS.TableSet(resize_matrix(a)) )
end

#
# my_min(model, x, m, ix)
#
# Ensure that m is the minimum element in x
# TODO: Test this more
function my_min(model, x, m)
    n = length(x)
    ix = @variable(model, integer=true, lower_bound=1, upper_bound=n)
    my_element(model, ix,x, m)
    @constraint(model, m .<= x[i] )

end

#
# my_max(model, x, m, ix)
#
# Ensure that m is the maximum element in x
# TODO: Test this more
function my_max(model, x, m)
    n = length(x)
    ix = @variable(model, integer=true, lower_bound=1, upper_bound=n)
    my_element(model, ix,x, m)
    @constraint(model, m .>= x)
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
        # bs = @variable(model, [1:num_tasks], Bin)
        # bt = @variable(model, [1:num_tasks], Bin)
        b  = @variable(model, [1:num_tasks], Bin)
        for i in tasks
            # The following don't work since ConstraintSolver don't
            # support nonlinear constraints
            # @constraint(model,sum([(start[i] <= t) * (t <= start[i] + duration[i])*resource[i] for i in tasks])  <= b)

            #= 
            # is this task active during this time t?
            @constraint(model, bs[i] := {start[i] <= t})
            @constraint(model, bt[i] := {t < start[i]+duration[i]}) 
            @constraint(model, b[i] := { bs[i] + bt[i] == 2}) # is this task active in time t ?
            =# 
            @constraint(model, b[i] := {start[i] <= t && t < start[i]+duration[i]})

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
    @constraint(model, x in CS.AllDifferent())
    @constraint(model, z in CS.AllDifferent())

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
    # @constraint(model, x in CS.AllDifferent())
    # @constraint(model, path in CS.AllDifferent())

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
        @constraint(model, x[i,:] in CS.AllDifferent())
        @constraint(model, x[:,i] in CS.AllDifferent())
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
        #=
        b = @variable(model,[1:2], Bin)
        @constraint(model,b[1] := {begins[i] + durations[i] <= begins[j]})
        @constraint(model,b[2] := {begins[j] + durations[j] <= begins[i]})
        @constraint(model, sum(b) >= 1)
        =#
        @constraint(model,(begins[i] + durations[i] <= begins[j]) || (begins[j] + durations[j] <= begins[i]))

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

#
# lex_less_eq(model, x, y)
#
# Ensures that the array x is lexicographically less or equal to array y
#
# This is a port of MiniZinc's lex_lesseq_int 
# (via my Picat port in http://hakank.org/picat/lex.pi)
# TODO: This is either wrong or too slow!
#=
function lex_less_eq(model, x, y)
    lx = 1
    ux = length(x)
    ly = 1
    uy = length(y)
    size = max(ux-lx,uy-ly)
    
    # "b[i] is true if the lexicographical order holds from position i on"
    b  = @variable(model, [1:size+2], binary=true)
    b1 = @variable(model, [1:size+2], binary=true)
    b2 = @variable(model, [1:size+2], binary=true)
    b3 = @variable(model, [1:size+2], binary=true)
    @constraint(model,b[1] == 1)
    for i in 1:size+1
        # B[I] == ( X[I] <= Y[I] /\ (X[I] < Y[I] \/ B[I+1] == 1) ) # Picat version
        @constraint(model, b1[i] := {x[i] <= y[i]})
        @constraint(model, b2[i] := {x[i] <  y[i]})
        @constraint(model, b3[i] := {b2[i] + b[i+1] >= 1}) 
        @constraint(model, b[i] := {b1[i] + b3[i] == 2})
    end
    # @constraint(model, b[size + 2] := {size - 1 < size - 1})
    @constraint(model, b[size + 2] == 1)
end
=#



#=

  among(model,n, x, v)

  From MiniZinc globals.mzn:
  """
  Requires exactly 'n' variables in 'x' to take one of the values in 'v'.
  """
=#
function among(model, n, x, v)
    x_len = length(x)
    v_len = length(v)
    b = @variable(model, [1:x_len], Bin)
    for i in 1:x_len 
        bv = @variable(model, [1:v_len], Bin)
        for j in 1:v_len
            @constraint(model, bv[j] := {x[i] == v[j]})
        end
        @constraint(model, b[i] := {sum(bv) > 0})
    end
    @constraint(model, n == sum(b))
end


# 
# square(model, x,y, table=nothing)
# 
# y = x * x
# 
# If this is used for the same table repeatedly,
# one might to use a precalculated table.
# Note: Then use resize_matrix() to get it in the
# proper form (see below how it's done)
#
# Beware: This might blow up on largish domains.
#
function square(model, x,y, table=[])
    if x isa Integer
        x = @variable(model, lower_bound=x, upper_bound=x, integer=true)
    end
    lbx = round(Int, JuMP.lower_bound(x))
    ubx = round(Int, JuMP.upper_bound(x))
    if table !== []
        table = resize_matrix([ [i,j,i * j] for i in lbx:ubx, j in lbx:ubx])
    end
    @constraint(model, [x,x,y] in CS.TableSet(table))
end
