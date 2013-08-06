/*

  Crew allocation problem in AMPL+CP.

  
  From Gecode example crew
  examples/crew.cc
  
  (Original text from crew.cc)
  """
  * Example: Airline crew allocation
  *
  * Assign 20 flight attendants to 10 flights. Each flight needs a certain
  * number of cabin crew, and they have to speak certain languages.
  * Every cabin crew member has two flights off after an attended flight.
  *
  """

  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/


*/

param numPersons;
param attributes{1..numPersons, 1..5};
param numFlights;
param requiredCrew{1..numFlights, 1..6};

# Might be minimized
var z >= 1 <= numPersons*numPersons integer;
var crew{1..numFlights, 1..numPersons} binary;

# Gecode and ilogcp finds min number of people (19) immediately, 
# but then it takes "forever" to prove it...
# minimize obj: z;

#
# constraints
#
s.t. c1{f in 1..numFlights}:
   # size of crew
   sum{i in 1..numPersons} crew[f,i] = requiredCrew[f,1]
   and
   # attributes and requirements
   forall{j in 1..5} (
     sum{i in 1..numPersons} (attributes[i,j]*crew[f,i]) >=  requiredCrew[f,j+1]
   )
;

# after a flight, break for two flights
s.t. c2{f in 1..numFlights-2, i in 1..numPersons}:
     (crew[f,i] + crew[f+1,i] + crew[f+2,i]) <= 1
;

# number of persons working
s.t. c3: z = count{p in 1..numPersons}
                  (count{f in 1..numFlights} (crew[f,p] > 0) > 0)
;

data;

param numPersons := 20;
param attributes: 1 2 3 4 5 :=
  1  1 0 0 0 1    # Tom     = 1
  2  1 0 0 0 0    # David   = 2
  3  1 0 0 0 1    # Jeremy  = 3
  4  1 0 0 0 0    # Ron     = 4
  5  1 0 0 1 0    # Joe     = 5
  6  1 0 1 1 0    # Bill    = 6
  7  1 0 0 1 0    # Fred    = 7
  8  1 0 0 0 0    # Bob     = 8
  9  1 0 0 1 1    # Mario   = 9
 10  1 0 0 0 0    # Ed      = 10
 11  0 1 0 0 0    # Carol   = 11
 12  0 1 0 0 0    # Janet   = 12
 13  0 1 0 0 0    # Tracy   = 13
 14  0 1 0 1 1    # Marilyn = 14
 15  0 1 0 0 0    # Carolyn = 15
 16  0 1 0 0 0    # Cathy   = 16
 17  0 1 1 1 1    # Inez    = 17
 18  0 1 1 0 0    # Jean    = 18
 19  0 1 0 1 1    # Heather = 19
 20  0 1 1 0 0    # Juliet  = 20
;

param numFlights := 10;
param requiredCrew :  1 2 3 4 5 6 :=
  1   4 1 1 1 1 1  # Flight 1
  2   5 1 1 1 1 1  # Flight 2
  3   5 1 1 1 1 1  # ..
  4   6 2 2 1 1 1 
  5   7 3 3 1 1 1 
  6   4 1 1 1 1 1 
  7   5 1 1 1 1 1 
  8   6 1 1 1 1 1 
  9   6 2 2 1 1 1 
 10   7 3 3 1 1 1  # Flight 10
;

option solver gecode;
option gecode_options "var_branching=size_min val_branching=min outlev=1 outfreq=1";

# option solver ilogcp;
# option ilogcp_options "optimizer=auto debugexpr=0 logperiod=1 logverbosity=0";

solve;

printf "z: %d\n", z;
for{f in 1..numFlights} {
  for{p in 1..numPersons} {
     printf "%2d ", crew[f,p];
  }
  printf "\n";
}

printf "\n";
