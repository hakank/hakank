/*

  From http://diuflx71.unifr.ch/lpl/GetModel?name=/demo/demo2
  LPL code: http://www.virtual-optima.com/en/demo.html
  """
  ** Mr. Greenfan wants to give a dress party where the male guests
  * must wear green dresses. The following rules are given:
  * 1 If someone wears a green tie he has to wear a green shirt.
  * 2 A guest may only wear green socks and a green shirt 
  *   if he wears a green tie or a green hat.
  * 3 A guest wearing a green shirt or a green hat or who does
  *   not wear green socks must wear a green tie.
  * 4 A guest who is not dressed according to rules 1-3 must
  *   pay a $11 entrance fee.
  * Mr Greenguest wants to participate but owns only a green shirt 
  * (otherwise he would have to pay one for $9). He could buy 
  * a green tie for $10, a green hat (used) for $2 and green socks
  * for $12.
  * What is the cheapest solution for Mr Greenguest to participate?
  *)
  MODEL Dressing "Dressing Party";


  FORMULATION:The problem can be formulated as a mathematical model by imposing
  Boolean (logical) constraints. Let's introduce the following propositions:
    k  "Mr Greenguest wears a green tie"
    b  "Mr Greenguest wears a green hat"
    h  "Mr Greenguest wears a green shirt"
    s  "Mr Greenguest wears a green socks"
    n  "Mr Greenguest is not costumed according the three rules 1-3"

  ref:
  Suhl U. (private communication)
  """

  Solutions should be:

  Mr Greenguest must buy a tie.

  Total costs are:  10

  i.e. k should be 1.
      
  What I can see cplex cannot handle or|then directly so
  one have to rewrite it with boolean algebra.
  

  Notes:
   1. Since n is no tie, all lhs must have n
   2. If a then b is translated to
        b >= a


   This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
   See also my AMPL page: http://www.hakank.org/ampl/



*/

var k binary; 
var b binary; 
var h binary; 
var s binary; 
var n binary;


#   CONSTRAINT
#    Rule1: (k->h) OR n;
subject to rule1: 
        # (if k then h) or n;
        n + h >= k - 1
;
        

# Rule2: ((s AND h) -> (k OR b)) OR n;
subject to rule2:
        #(if s and h then k or b) or n;
        n + k + b >= s + h - 2
;

#  Rule3: ((h OR b OR ~s) -> k) OR n;
subject to rule3:
       # (if h or b or not s then k) or n;
        n + k >= h + b - s
;

# exactly one choice
subject to rule4:
   k + b + h + s + n  = 1
;

# Guest: h;

#  MINIMIZE  Cost: 10*k+2*b+12*s+11*n;
minimize Cost:
        10*k+2*b+12*s+11*n;
 
option solver cplex;
# option solver cbc;
# option solver donlp2;

solve;

display _varname, _var, _obj;
display Cost;
       

/*
  WRITE 'Mr Greenguest must buy a tie.\n' IF k; 
  WRITE 'Mr Greenguest must buy a hat.\n' IF b;
  WRITE 'Mr Greenguest must buy socks.\n' IF s;
  WRITE 'Mr Greenguest must pay the entrance fee.\n' IF n;
  WRITE '\n   Total costs are: %3d' : Cost;
*/
