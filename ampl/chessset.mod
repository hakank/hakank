/*
   Applications of Optimization with XPress-MP.pdf,
   page 11 (the problem is presented from page 7)

   Run as

     ampl chessset.mod

     (glpk:
     glpsol -m chessset.mod -o chessset.out --wtxt chessset.txt
     )

chesset.out:

Problem:    chessset
Rows:       3
Columns:    2
Non-zeros:  6
Status:     OPTIMAL
Objective:  profit = 1333.333333 (MAXimum)

   No.   Row name   St   Activity     Lower bound   Upper bound    Marginal
------ ------------ -- ------------- ------------- ------------- -------------
     1 profit       B        1333.33
     2 boxwood      NU           200                         200       6.66667
     3 lathe        B        133.333                         160

   No. Column name  St   Activity     Lower bound   Upper bound    Marginal
------ ------------ -- ------------- ------------- ------------- -------------
     1 smallset     NL             0             0                    -1.66667
     2 largeset     B        66.6667             0

Karush-Kuhn-Tucker optimality conditions:

KKT.PE: max.abs.err. = 1.14e-13 on row 1
        max.rel.err. = 8.52e-17 on row 1
        High quality

KKT.PB: max.abs.err. = 0.00e+00 on row 0
        max.rel.err. = 0.00e+00 on row 0
        High quality

KKT.DE: max.abs.err. = 1.78e-15 on column 2
        max.rel.err. = 8.46e-17 on column 2
        High quality

KKT.DB: max.abs.err. = 0.00e+00 on row 0
        max.rel.err. = 0.00e+00 on row 0
        High quality

End of output


 This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
 See also my AMPL page: http://www.hakank.org/ampl/


*/

# 
# If there shoule be integers (which seems to be reasonable in this case)
# it's just to add 'integer' to the attributes.
#
var smallset >= 0, integer; 
var largeset >= 0, integer;

maximize profit: 5*smallset + 20*largeset;

s.t. boxwood: 1*smallset + 3*largeset <= 200;
s.t. lathe:   3*smallset + 2*largeset <= 160;

option solver cplex;
solve;

display smallset, largeset;
display _varname, _var;

end;
