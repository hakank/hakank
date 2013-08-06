/*

  Twin letters problem in AMPL+CP.

  From
  http://www.comp.nus.edu.sg/~henz/projects/puzzles/digits/index.html
  """
  Twin Letters    

  In the following puzzle, there are ten pairs of
  letters to be assigned to the same digit so that the multiplication
  (including intermediate results) is correct. Can you find out the
  pairs and their values?

          A B C
   *      D E F
   ____________
          G H I
        J K L
      M N O
   ____________
      P Q R S T
  """


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

set letters;

# decision variables
var x{letters} >= 0 <= 9 integer;

var C1 binary;
var C2 >= 0 <= 2 integer;
var C3 binary;


#
# constraints
#

# exactly 2 occurrences of each digit
s.t. c1{d in 0..9}: 
    exactly 2 {i in letters} (x[i] = d)
;

s.t. c2a:
                                 100*x['G'] + 10*x['H'] + x['I'] +
                   1000*x['J'] + 100*x['K'] + 10*x['L'] +
    10000*x['M'] + 1000*x['N'] + 100*x['O'] =
    10000*x['P'] + 1000*x['Q'] + 100*x['R'] + 10*x['S'] + x['T'];

s.t. c2b:
    (100*x['D'] + 10*x['E'] + x['F'])*x['C'] = 100*x['G'] + 10*x['H'] + x['I'];

s.t. c2c:
    (100*x['D'] + 10*x['E'] + x['F'])*x['B'] = 100*x['J'] + 10*x['K'] + x['L'];

s.t. c2d:
    (100*x['D'] + 10*x['E'] + x['F'])*x['A'] = 100*x['M'] + 10*x['N'] + x['O'];

s.t. c2e:    
    (100*x['A'] + 10*x['B'] + x['C']) * (100*x['D'] + 10*x['E'] + x['F']) =
    10000*x['P'] + 1000*x['Q'] + 100*x['R'] + 10*x['S'] + x['T']
;

# carry restrictions
s.t. c3:
    x['T']         = x['I']                         and
    x['S'] + 10*C1 = x['H'] + x['L']                and
    x['R'] + 10*C2 = x['G'] + x['K'] + x['O'] + C1  and
    x['Q'] + 10*C3 = x['J'] + x['N'] + C2           and
    x['P']         = x['M'] + C3
;


data;


set letters := A B C D E F G H I J K L M N O P Q R S T;


option show_stats 2;
# option presolve 0;

option solver gecode;
option gecode_options "var_branching=regret_min_min val_branching=min outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=auto alldiffinferencelevel=1 debugexpr=0 logperiod=1 logverbosity=0";

solve;

display x;
display C1,C2,C3;