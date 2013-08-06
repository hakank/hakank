/*

   A fourth variant with the same approach of all_different as in
   magic_square_taha.mod
 
   The drawback with this approach is that it requires a lot of constraints, 203,
   compared with send_more_money3.mod which just requires 33.

   This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
   See also my AMPL page: http://www.hakank.org/ampl/

*/

set letters :=  {"s","e","n","d","m","o","r","y","dummy1","dummy2"};
var x{letters} >= 0 <= 9    integer;
var y{letters, letters} binary; # for all_different

# M must be exactly 10 (i.e. the largest number + 1) unless
# minimize is done on sum y[i],
# or - perhaps better - has <= 9 as a constraint on x
param M := 10;

# dummy_objective
minimize z: 
        0
        # sum{i in letters} x[i]; #  if M > 10
;

subject to problem:
                  1000*x['s'] + 100*x['e'] + 10*x['n'] + x['d'] + 
                  1000*x['m'] + 100*x['o'] + 10*x['r'] + x['e'] =
   10000*x['m'] + 1000*x['o'] + 100*x['n'] + 10*x['e'] + x['y']
;

# special for SMM problem
subject to s_not_zero: x['s'] >= 1;
subject to m_not_zero: x['m'] >= 1;


#
# all different, see magic_square_taha.mod for a little more about
# this representation
#
all_different1{i in letters, j in letters}: 
   M*y[i,j]       + (x[i] - x[j]) >= (if i <> j then 1);

all_different2{i in letters, j in letters}: 
   M*(1 - y[i,j]) + (x[j] - x[i]) >= (if i <> j then 1);


#option cplex_options 'writeprob=send_more_money4.lp';
option show_stats 1;
option solver cplex;

solve;

display y;
display x;

printf "Should be: S:9 E:5 N:6 D:7 M:1 O:0 R:8 Y:2\n";
printf "           S:%d E:%d N:%d D:%d M:%d O:%d R:%d Y:%d \n", x["s"], x["e"],x["n"],x["d"],x["m"],x["o"],x["r"],x["y"] ;

printf "SEND:\t\t%10d\n",                 1000*x["s"] + 100*x["e"] + 10*x["n"] + x["d"];
printf "MORE:\t\t%10d\n",                 1000*x["m"] + 100*x["o"] + 10*x["r"] + x["e"];
printf "SEND+MORE:\t%10d\n", 1000*x["s"] + 100*x["e"] + 10*x["n"] + x["d"] + 1000*x["m"] + 100*x["o"] + 10*x["r"] + x["e"]; 
printf "MONEY:\t\t%10d\n", 10000*x["m"] + 1000*x["o"] + 100*x["n"] + 10*x["e"] + x["y"];
printf "RESULT:\t\t%10d\n",               1000*x["s"] + 100*x["e"] + 10*x["n"] + x["d"] + 
                      1000*x["m"] + 100*x["o"] + 10*x["r"] + x["e"] - 
        (10000*x["m"] + 1000*x["o"] + 100*x["n"] + 10*x["e"] + x["y"]);

# expand;


