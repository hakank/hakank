/*
  AMPL variant av ~/eclipse_me/dinner.ecl

  """
  My son came to me the other day and said, "Dad, I need help with a
  'math problem'. The problem went like this:
    * We're going out to dinner taking 1-6 grandparents, 1-10 parents and/or 1-40 children
    * Grandparents cost $3 for dinner, parents $2 and children $0.50
    * There must be 20 total people at dinner and it must cost $20
    * How many grandparents, parents and children are going to dinner?
  """

Eclipse:
[grandparents : 1, parents : 5, children : 14]

Minis ;
MINOS 5.5: optimal solution found.
1 iterations, objective 20
Grandparents = 4
Parents = 0
Children = 16

CPLEX 10.1.0: optimal integer solution; objective 20
0 MIP simplex iterations
0 branch-and-bound nodes
Grandparents = 4
Parents = 0
Children = 16

If at least one of each:

CPLEX 10.1.0: optimal integer solution; objective 20
0 MIP simplex iterations
0 branch-and-bound nodes
Grandparents = 1
Parents = 5
Children = 14


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/


*/

var Grandparents integer >= 1 <= 6;
var Parents      integer >= 1 <= 10;
var Children     integer >= 1 <= 40;

maximize z: 3*Grandparents + 2*Parents + 0.5*Children;

c1: Grandparents * 3 + Parents * 2 + Children * 0.5  = 20;
c2: Grandparents + Parents + Children = 20 ;     # amount = 20

#       % at least one of each
#        Grandparents #> 0,
#        Parents      #> 0,
#        Children     #> 0,

option solver cplex;
solve;

display Grandparents, Parents, Children;