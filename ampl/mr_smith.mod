/*

  Mr Smith logical problem in AMPL+CP.

  From an IF Prolog example (http://www.ifcomputer.de/)
  """
  The Smith family and their three children want to pay a visit but they
  do not all have the time to do so. Following are few hints who will go
  and who will not:
      o If Mr Smith comes, his wife will come too.
      o At least one of their two sons Matt and John will come.
      o Either Mrs Smith or Tim will come, but not both.
      o Either Tim and John will come, or neither will come.
      o If Matt comes, then John and his father will
        also come.
   """

  The answer should be:
   Mr_Smith_comes      =  0
   Mrs_Smith_comes     =  0
   Matt_comes          =  0
   John_comes          =  1
   Tim_comes           =  1

  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/


*/

var Mr_Smith binary;
var Mrs_Smith binary;
var Matt binary;
var John binary;
var Tim binary;

#
# constraints
#

# If Mr Smith comes then his wife will come too.
s.t. c1: Mr_Smith ==> Mrs_Smith;

# At least one of their two sons Matt and John will come.
s.t. c2: Matt or John;

# Either Mrs Smith or Tim will come but not both.
# bool2int(Mrs_Smith) + bool2int(Tim) = 1
# Mrs_Smith xor Tim
s.t. c3: Mrs_Smith + Tim = 1;

# Either Tim and John will come or neither will come.
s.t. c4: Tim <==> John;

# If Matt comes then John and his father will also come.
s.t. c5: Matt ==> (John and Mr_Smith);


data;


option solver gecode;
# option gecode_options "var_branching=degree_size_min val_branching=min outlev=1 outfreq=1 timelimit=30";
# option solver ilogcp;

solve;

display Mr_Smith, Mrs_Smith, Matt, John, Tim;