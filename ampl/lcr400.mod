# From
# http://www.maths.ed.ac.uk/~gondzio/software/nlp/modeles/lcr400.mod
#
#TITLE SAVINGS MODEL BY RAMSEY  (RAMSEY,SEQ=63)
#  THIS SIMPLE GROWTH MODEL IS A VERY POPULAR MODEL FOR
#  NLP CODE TESTING. ONLY THE SET T NEEDS TO BE REDEFINED
#  IN ORDER TO GENERATE MODELS OF DIFFERENT SIZES. VARIOUS
#  OPTIONS ON CONSTRAINT TYPES ARE AVAILABLE AS WELL.
#
#  REFERENCES: RAMSEY F P, A MATHEMATICAL THEORY OF SAVING,
#              ECONOMICS JOURNAL, DECEMBER 1928.
#
#              MURTAGH M AND SAUNDERS M, A PROJECTED LAGRANGIAN ALGORITHM
#              AND ITS IMPLEMENTATION FOR SPARSE NONLINEAR CONSTRAINTS,
#              MATHEMATICAL PROGRAMMING STUDY  16, PP 84-117,1982,
#              SECTION 5.12 ECONOMIC GROWTH MODEL
#
#   This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
#   See also my AMPL page: http://www.hakank.org/ampl/
#


option presolve 0;
# set T ordered:=1..400;
set T ordered:=1..100; # hakank: Has to decrease this


param    BET    := .95;				#DISCOUNT FACTOR
param    B      := .25;				#SHARE PARAMETER
param    G      := .03;				#LABOR GROWTH
param    K0     :=3.00;				#INITIAL CAPITAL
param    I0     := .05;				#INITIAL INVESTMENT
param    C0     := .95;				#INITIAL CONSUMPTION
param    ALPH   :=(C0+I0)/K0**B;      		#CALIBRATION CONSTANT
param    ALPHA{t in T}:=ALPH*(1+G)**((1-B)*ord(t));#PRODUCTION FUNCTION CONSTANT 
param BETA{t in T}  := if (ord(t) < card(T)) then BET**ord(t)
					     else BET**ord(t)/(1-BET); #DISCOUNT FACTOR


var K{t in T} >= I0 + K0, := (I0+K0+I0) ;# CAPITAL
var I{t in T} >= I0, <= 1.04**ord(t)*I0 := if (ord(t)<card(T)) then I0 else 3*I0; 	# INVESTMENT



minimize UTIL:
	-sum{t in T} BETA[t]*log(ALPHA[t]*K[t]**B - I[t]);

subject to K1{t in T:ord(t)>1}:
K[t] <= K[t-1] + I[t-1] ;

subject to TC{t in T: ord(t)=card(T)}:
G*K[t] <= I[t];

subject to KFIX{t in T: ord(t)=1}:
K[t] = I0 + K0;

solve;
display _varname, _var;
option auxfiles rc;

write glcr400;
