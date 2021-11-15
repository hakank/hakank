/*
https://stackoverflow.com/questions/69753921/minizinc-constraint-formulation
Minizinc Constraint Formulation
"""
I am a beginner to OR and Minizinc, I am trying to modify a example provided by 
Håkan Kjellerstrand, crew.mzn

I would like to add a constraint stating that a pilot can not start a flight if the previous one is 
not finished. I have created an array "FDP" containing for each flight, start timestamp, end timestamp, 
duration. (rest is considered included between start and end).

I am stuck writing my constraint and a bit lost linking flight done by a person to 
flight characteritics.

Could you please confirm if what I am trying to do is doable. 
Thanks !

NB: constraints @ line72
"""

*/

% Pilots list

int: Tom     = 1;
int: David   = 2;
int: Jeremy  = 3;
int: Ron     = 4;
int: Joe     = 5;
int: Bill    = 6;
int: Fred    = 7;
int: Bob     = 8;
int: Mario   = 9;
int: Ed      = 10;

int: Carol   = 11;
int: Janet   = 12;
int: Tracy   = 13;
int: Marilyn = 14;
int: Carolyn = 15;
int: Cathy   = 16;
int: Inez    = 17;
int: Jean    = 18;
int: Heather = 19;
int: Juliet  = 20;

int: numPersons = 20; % number of persons
array[1..numPersons, 1..2] of int: attributes =  
array2d(1..numPersons, 1..2, [ %Capt or First Officer
  1,0,   % Tom     = 1
  1,0,  % David   = 2
  1,0,   % Jeremy  = 3
  1,0,   % Ron     = 4
  1,0,   % Joe     = 5
  1,0,  % Bill    = 6
  0,1,   % Fred    = 7
  0,1,   % Bob     = 8
  0,1,   % Mario   = 9
  0,1,   % Ed      = 10
  0,1,   % Carol   = 11
  0,1,   % Janet   = 12
  0,1,   % Tracy   = 13
  0,1,   % Marilyn = 14
  0,1,   % Carolyn = 15
  0,1,   % Cathy   = 16
  0,1,   % Inez    = 17
  0,1,   % Jean    = 18
  0,1,   % Heather = 19
  0,1   % Juliet  = 20
 ])
 ;

int: numFlights = 10;                           % number of flights
array[1..numFlights,1..3] of int: requiredCrew; % required crew per flight 
array[1..numFlights,1..3] of float: FDP; %flight characteristics
array[1..numFlights, 1..numPersons] of var 0..1: crew; 

% objective to minimize: standard deviation of flown hours between pilots
var 1..numPersons: z = sum(p in 1..numPersons) (bool2int(sum(f in 1..numFlights) (crew[f,p]) > 0));

% solve satisfy;
solve minimize z;

constraint
  forall(f in 1..numFlights) (
     % size of crew
     sum(i in 1..numPersons) (crew[f,i]) = requiredCrew[f, 1] /\ 
     % attribute and requirements
     forall(j in 1..2) (sum(i in 1..numPersons) (attributes[i,j]*crew[f,i]) >=  requiredCrew[f,j+1])) ;


% for each pilot doing a flight, end of flight i must not overlap beginning of flight i+1
% hakank: If two flights overlap, then a person cannot be assigned to both
constraint
forall(f1,f2 in 1..numFlights where f1 < f2  /\
           (
            ( FDP[f1,1] >= FDP[f2,1] /\ FDP[f1,2] <= FDP[f2,2] )
             \/
           ( FDP[f2,1] >= FDP[f1,1] /\ FDP[f2,2] <= FDP[f1,2] )
          ))
(
   trace("f1::\(f1) f2:\(f2)\n") /\
    not(exists(p in 1..numPersons) (
        crew[f1,p] = 1 /\ crew[f2,p] = 1
    ))
)
;
%data

requiredCrew = 
  array2d(1..numFlights,1..3,   %Number of pilots required, Capt, FO
       [2,1,1, % Flight 1
        2,1,1, % Flight 2
        2,1,1, % ..
        2,1,1,
        3,2,1,
        2,1,1,
        2,1,1,
        3,1,2,
        2,1,1,
        2,1,1  % Flight 10
]);

FDP = 
  array2d(1..numFlights,1..3,  % flight start , flight end, flown hours
  [44531.58333,44531.70833,1.2,
   44533.16667,44533.5,2,
   44534.33333,44534.54167,1.1,
   44258.33333,44533.45833,1.5,
   44536.72917,44536.79167,2.3,
   44534.33333,44534.54167,3.1,
   44258.33333,44533.45833,0.2,
   44538.75,44538.95833,1.5,
   44539.625,44539.79167,2.2,
   44534.33333,44534.54167,1.8
 ]);

% Show the table
constraint 
  forall(f in 1..numFlights) (
     trace("f: \(f): start:\(FDP[f,1])  end: \(FDP[f,2])  flown:\(FDP[f,3]) diff: \(FDP[f,2] - FDP[f,1]) \n") 
  )
;

output [

       if i = 1 /\ j = 1 then
       "number of person: " ++ show(z) ++ "\n"
       else "" endif ++
       if j mod numPersons = 1 then show(i) ++ ": " else "" endif ++
       show(crew[i,j]) ++ if j mod numPersons = 0 then "\n" else " " endif 
       | i in 1..numFlights, j in 1..numPersons
] ++ ["\n"]
;


        