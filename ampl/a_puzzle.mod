/*

  "A puzzle" in AMPL+CP.

  From "God plays dice"
  "A puzzle"
  http://gottwurfelt.wordpress.com/2012/02/22/a-puzzle/
  And the sequel "Answer to a puzzle"
  http://gottwurfelt.wordpress.com/2012/02/24/an-answer-to-a-puzzle/

  This problem instance was taken from the latter blog post.
  """
  8809 = 6
  7111 = 0
  2172 = 0
  6666 = 4
  1111 = 0
  3213 = 0
  7662 = 2
  9312 = 1
  0000 = 4
  2222 = 0
  3333 = 0
  5555 = 0
  8193 = 3
  8096 = 5
  7777 = 0
  9999 = 4
  7756 = 1
  6855 = 3
  9881 = 5
  5531 = 0

  2581 = ?
  """

  Note: 
  This model yields 10 solutions, since x4 is not 
  restricted in the constraints. 
  All solutions has x assigned to the correct result. 
  

  The problem stated in "A puzzle"
  http://gottwurfelt.wordpress.com/2012/02/22/a-puzzle/
  is
  """
  8809 = 6
  7662 = 2
  9312 = 1
  8193 = 3
  8096 = 5
  7756 = 1
  6855 = 3
  9881 = 5

  2581 = ?
  """
  This problem instance - using the same principle - yields 
  two different solutions of x, one is the same (correct) as 
  for the above problem instance, and one is not.
  This is because here both x4 and x1 are underdefined.
  

  Note: 
  This problem has another non-algebraic and - let us say - topological
  approach which yield the same solution as the first problem and one
  of the solutions of the second problem.


  Later note (2013-05-14)  
  Erwin Kalvelagen blogged about this model and wrote a neater 
  (and more general) variant in his
  "Scalar models"
  http://yetanothermathprogrammingconsultant.blogspot.se/2013/05/scalar-models.html



  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

param n;

# decision variables
var x0 >= 0 <= 9 integer;
var x1 >= 0 <= 9 integer;
var x2 >= 0 <= 9 integer;
var x3 >= 0 <= 9 integer;
var x4 >= 0 <= 9 integer;
var x5 >= 0 <= 9 integer;
var x6 >= 0 <= 9 integer;
var x7 >= 0 <= 9 integer;
var x8 >= 0 <= 9 integer;
var x9 >= 0 <= 9 integer;

var x >= 0 <= 9 integer;

#
# constraints
#
s.t. c1:
  x8+x8+x0+x9 = 6 and
  x7+x1+x1+x1 = 0 and
  x2+x1+x7+x2 = 0 and
  x6+x6+x6+x6 = 4 and
  x1+x1+x1+x1 = 0 and
  x3+x2+x1+x3 = 0 and
  x7+x6+x6+x2 = 2 and
  x9+x3+x1+x2 = 1 and
  x0+x0+x0+x0 = 4 and
  x2+x2+x2+x2 = 0 and
  x3+x3+x3+x3 = 0 and
  x5+x5+x5+x5 = 0 and
  x8+x1+x9+x3 = 3 and
  x8+x0+x9+x6 = 5 and
  x7+x7+x7+x7 = 0 and
  x9+x9+x9+x9 = 4 and
  x7+x7+x5+x6 = 1 and
  x6+x8+x5+x5 = 3 and
  x9+x8+x8+x1 = 5 and
  x5+x5+x3+x1 = 0 and

  x2+x5+x8+x1 = x
;

# This smaller variant yields two different values for x.
# s.t. c2:
#  x8+x8+x0+x9 = 6 and
#  x7+x6+x6+x2 = 2 and
#  x9+x3+x1+x2 = 1 and
#  x8+x1+x9+x3 = 3 and
#  x8+x0+x9+x6 = 5 and
#  x7+x7+x5+x6 = 1 and
#  x6+x8+x5+x5 = 3 and
#  x9+x8+x8+x1 = 5 and

#  x2+x5+x8+x1 = x
# ;

data;


# option presolve 0;
option show_stats 2;

option solver gecode;
option gecode_options "var_branching=size_min val_branching=min outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=4 debugexpr=0 logperiod=10 logverbosity=0";

solve;

display x;
display x0,x1,x2,x3,x4,x5,x6,x7,x8,x9;
