/*

Thu Dec 27 08:47:14 2007/hakank@bonetmail.com

Winston OR, sid 191ff

"""
The Leon Burnit Advertising Agency is trying to determine a
TV advertisin schedule for Priceles Auto Company. Priceler has
three goals:
Goal 1: Its ads should be seen by at least 40 million high-income men (HIM)
Goal 2:                                    60         low-income person (LIP) 
Goal 3:                                    35         hight-incom women (HIW) 

Leon Burnit can purchase two types of ad: those shon during football games
and those shown during soap operas. At most, $6000000 can be spent on ads.
The advertising costs and potential audience of a one-minute ad is:

                 Million of 
                   views  
Ad           HIM   LIP   HIW  Cost
Football     7      10    5    100000
Soap-opera   3       5    4     60000

"""

The original version is infeasible.
Therefore: add goal (priorities) to decide what to do...


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/


*/

var x1 >= 0;  # football game
var x2 >= 0;  # soap operas
var s1_minus >= 0; # "punishment" for x1 < limit
var s2_minus >= 0; 
var s3_minus >= 0;
var s4_minus >= 0; # punishment for increasing budget
var s1_plus >= 0;  # punishment for x1 > limit
var s2_plus >= 0;  # 
var s3_plus >= 0;
var s4_plus >= 0;
param s1_penalty := 200;
param s2_penalty := 100;
param s3_penalty := 50;



/*
# original version
minimize z:
        0*x1+0*x2;

subject to 
        c1:   7*x1 +  3*x2 >=  40; # HIW
        c2:  10*x1 +  5*x2 >=  60; # LIP
        c3:   5*x1 +  4*x2 >=  35; # HIW
        c4: 100*x1 + 60*x2 <= 600;
*/

# Then minimize the punishments
minimize z:
        s1_penalty*s1_minus + 
        s2_penalty*s2_minus + 
        s3_penalty*s3_minus + 
        s4_plus;

subject to 
        c1:   7*x1 +  3*x2 + s1_minus - s1_plus >=  40; # HIW
        c2:  10*x1 +  5*x2 + s2_minus - s2_plus >=  60; # LIP
        c3:   5*x1 +  4*x2 + s3_minus - s3_plus >=  35; # HIW
        c4: 100*x1 + 60*x2 + s4_minus - s4_plus <= 600; # Budget constraint


# data;

option solver cplex;
solve;
# display _varname, _var, _obj;
display z;
display x1,x2;
display s1_minus, s1_plus;
display s2_minus, s2_plus;
display s3_minus, s3_plus;
display s4_minus, s4_plus;

