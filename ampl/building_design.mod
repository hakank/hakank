/*

Fri Dec 28 21:47:14 2007/hakank@bonetmail.com

From Practial Optimization with Mathematica (PDF-fil), page 16

""""
To save energy costs for heating and cooling, an architect is considering de- 
signing a partially buried rectangular building. The total floor space needed 
is 20,000 m^2. 
Lot size limits the building plan dimension to 50 m. It has already 
been decided that the ratio between the plan dimensions must be equal to 
the golden ratio (1.618) and that each story must be 3.5 m high. The heating 
and cooling costs are estimated at $100 per m^2 of the exposed surface area of 
the building. The owner has specified that the annual energy costs should not 
exceed $225,000. Formulate the problem of determining building dimensions 
to minimize cost of excavation. 
""""

Answer:
   - page 78f (graphical with 2 variables)
   - 175 f

Correct answer should be
  d* = 80.03 m h* = 13.31m w* = 21.53 m f* = 60020.5 m^3 

No solver likes this problem as it stands.
For bonmin:
              Num      Status      Obj             It       time
IpOp0009I     1        OPT         60020.4        63       0.06899
...

and the answer:
  f: 15994.4

Why?

loqo gives the correct answer direct.

If d, h, and w is given the start values (80, 13,21), then 
minos solves the problem directly

And then back to the original problem with start values about
as the part solution.
Both minos, donlp2, and snopt solves the problem directly.

It was enought for loqo to get the clue n := 3.
bonmin still gives the solution 15994.4...


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/

*/

var n >=1 integer := 3; # number of stories. Should be integer
var d >=0;# := 80; # depth of building below ground
var h >=0;# := 13; # height of building above ground
var l >=0;# := 10; # length of building in plan
var w >=0;# := 21; # width of building in plan

minimize z: 
        d*l*w;

subject to
        c1: (d+h)/n = 3.5;
        c2: l = 1.618*w;
        c3: n*l*w >= 20000;
        c4: l <= 50;
        c5: w <= 50;
        c6: 100*(2*h*l+2*h*w + l*w) <= 225000;

/*

# First rewrite the problem to
minimize f:
        1.618*d*w^2;

subject to
        c1: 100*(5.236*h*w + 1.618*w^2) <= 225000;
        c2: 1.618*w <= 50;
        c3: 0.4622857*(d+h)*w^2 >= 20000;
*/


# data;

# option cplex_option "sensitivity";
# option solver cplex;
option solver loqo; # gives the correct solution
# option solver snopt; 
# option bonmin_option 'bonmin.algorithm B-BB'; # simple branch-and-bound algorithm,
# option bonmin_option 'bonmin.algorithm B-OA'; # OA Decomposition algorithm,
# option bonmin_option 'bonmin.algorithm B-QG'; # Quesada and Grossmann branch-and-cut algorithm,
# option bonmin_option 'bonmin.algorithm B-Hyb'; # hybrid outer approximation based branch-and-cut,
# option bonmin_option 'bonmin.algorithm B-Ecp'; # ecp cuts based branch-and-cut a la FilMINT.

# option solver bonmin;
# option solver donlp2;
# option solver LaGO;

solve;

# display f;
# display d,h,w;
# display 1.618*d*w^2;
display _obj;
display _varname, _var, _var.rc, _var.lb, _var.ub, _var.slack;
display _conname, _con, _con.lb, _con.ub, _con.slack;
# display _varname, _var, _var.rc, _var.lb, _var.ub, _var.slack, _var.down, _var.current, _var.up;
#display _conname, _con, _con.lb, _con.ub, _con.slack, _con.down, _con.current, _con.up;

option nl_comments 1;
write gbuilding_design;
