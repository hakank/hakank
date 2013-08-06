/*

  Cryptarithmetic puzzle in AMPL+CP.
  
  Prolog benchmark problem GNU Prolog (crypta.pl)
  """
  Name           : crypta.pl                                              
  Title          : crypt-arithmetic                                       
  Original Source: P. Van Hentenryck's book                               
  Adapted by     : Daniel Diaz - INRIA France                             
  Date           : September 1992                                         
                                                                         
  Solve the operation:                                                    
                                                                        
     B A I J J A J I I A H F C F E B B J E A                              
   + D H F G A B C D I D B I F F A G F E J E                              
   -----------------------------------------                              
   = G J E G A C D D H F A F J B F I H E E F                              
  """
  
  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/

*/

set Letters;

var x{Letters} >= 0 <= 9 integer;
var Sr1 binary;
var Sr2 binary;


#
# constraints
#
s.t. c1: alldiff{i in Letters} x[i];

s.t. c2:  x['B'] >= 1 and x['D'] >= 1 and x['G'] >= 1;
         
s.t. c3:   x['A']+10*x['E']+100*x['J']+1000*x['B']+10000*x['B']+100000*x['E']+1000000*x['F']+
	   x['E']+10*x['J']+100*x['E']+1000*x['F']+10000*x['G']+100000*x['A']+1000000*x['F']
	= x['F']+10*x['E']+100*x['E']+1000*x['H']+10000*x['I']+100000*x['F']+1000000*x['B']+10000000*Sr1
 ;
      
s.t. c4:
	   x['C']+10*x['F']+100*x['H']+1000*x['A']+10000*x['I']+100000*x['I']+1000000*x['J']+
	   x['F']+10*x['I']+100*x['B']+1000*x['D']+10000*x['I']+100000*x['D']+1000000*x['C']+Sr1
	= x['J']+10*x['F']+100*x['A']+1000*x['F']+10000*x['H']+100000*x['D']+1000000*x['D']+10000000*Sr2
;

s.t. c5:
	   x['A']+10*x['J']+100*x['J']+1000*x['I']+10000*x['A']+100000*x['B']+
	   x['B']+10*x['A']+100*x['G']+1000*x['F']+10000*x['H']+100000*x['D']+Sr2
	= x['C']+10*x['A']+100*x['G']+1000*x['E']+10000*x['J']+100000*x['G']
;

data;

set Letters := A B C D E F G H I J;

option solver gecode;
option gecode_options "var_branching=size_min val_branching=min outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=cp alldiffinferencelevel=4 debugexpr=0 logperiod=10 logverbosity=0";

solve;

display Letters;
display x,Sr1,Sr2;
