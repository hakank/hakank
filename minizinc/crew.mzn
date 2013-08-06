%
% Crew allocation in Minizinc
% 
% From Gecode example crew
% examples/crew.cc
%
% (Original text from crew.cc)
% * Example: Airline crew allocation
% *
% * Assign 20 flight attendants to 10 flights. Each flight needs a certain
% * number of cabin crew, and they have to speak certain languages.
% * Every cabin crew member has two flights off after an attended flight.
% *
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

%
% Somewhat talkative model
%
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
array[1..numPersons, 1..5] of int: attributes =  
% steward, hostess, french, spanish, german
array2d(1..numPersons, 1..5, [
  1,0,0,0,1,   % Tom     = 1
  1,0,0,0,0,   % David   = 2
  1,0,0,0,1,   % Jeremy  = 3
  1,0,0,0,0,   % Ron     = 4
  1,0,0,1,0,   % Joe     = 5
  1,0,1,1,0,   % Bill    = 6
  1,0,0,1,0,   % Fred    = 7
  1,0,0,0,0,   % Bob     = 8
  1,0,0,1,1,   % Mario   = 9
  1,0,0,0,0,   % Ed      = 10
  0,1,0,0,0,   % Carol   = 11
  0,1,0,0,0,   % Janet   = 12
  0,1,0,0,0,   % Tracy   = 13
  0,1,0,1,1,   % Marilyn = 14
  0,1,0,0,0,   % Carolyn = 15
  0,1,0,0,0,   % Cathy   = 16
  0,1,1,1,1,   % Inez    = 17
  0,1,1,0,0,   % Jean    = 18
  0,1,0,1,1,   % Heather = 19
  0,1,1,0,0    % Juliet  = 20
 ])
 ;

int: numFlights = 10;                           % number of flights
array[1..numFlights,1..6] of int: requiredCrew; % required crew per flight 

array[1..numFlights, 1..numPersons] of var 0..1: crew; % the crew schedule

% stewards = {Tom, David, Jeremy, Ron, Joe, Bill, Fred, Bob, Mario, Ed};
% hostesses = {Carol, Janet, Tracy, Marilyn, Carolyn, Cathy, Inez,
%                                     Jean, Heather, Juliet};
% frenchSpeaking = {Bill, Inez, Jean, Juliet};
% germanSpeaking = {Tom, Jeremy, Mario, Cathy, Juliet};
% spanishSpeaking = {Joe, Bill, Fred, Mario, Marilyn, Inez, Heather};


% objective to minimize: number of persons working
var 1..numPersons: z = sum(p in 1..numPersons) (
         bool2int(sum(f in 1..numFlights) (crew[f,p]) > 0)
      )
;

% solve satisfy;
% solve minimize z;
solve :: int_search(
       [crew[f,p] | f in 1..numFlights, p in 1..numPersons ], 
       first_fail, 
       indomain_min, 
      complete) 
   minimize z;
% the eclipse ic/fd solvers gives a solution with this heuristic: 
% solve :: int_search([ crew[f,p] | f in 1..numFlights, p in 1..numPersons ], first_fail, indomain_min, credit(5,bbs(5))) minimize z;

constraint
%  z = 19  % for solve satisfy
%  /\
  forall(f in 1..numFlights) (
     % size of crew
     sum(i in 1..numPersons) (crew[f,i]) = requiredCrew[f, 1] /\ 
     % attribute and requirements
     forall(j in 1..5) (
       sum(i in 1..numPersons) (attributes[i,j]*crew[f,i]) >=  requiredCrew[f,j+1]
     )
  ) 
  /\ % after a flight, break for two flights
  forall(f in 1..numFlights-2, i in 1..numPersons) (
     (crew[f,i] + crew[f+1,i] + crew[f+2,i]) <= 1
  )
% /\ % extra contraint: must work at least two times
% forall(i in 1..numPersons) ( 
%    sum(f in 1..numFlights) (crew[f,i]) >= 2
% )
% /\ forall(i in 1..numPersons) (
%   flightsPerPerson[i] = sum(f in 1..numFlights) (bool2int(crew[f, i] = 1))
% )
;


%
% data
%

% The columns are in the following order
% staff;     % Overall number of cabin crew needed
% stewards;  % How many stewards are required
% hostesses; % How many hostesses are required
% french;    % How many French speaking employees are required
% spanish;   % How many Spanish speaking employees are required
% german;    % How many German speaking employees are required
requiredCrew = 
  array2d(1..numFlights,1..6,
       [4,1,1,1,1,1, % Flight 1
        5,1,1,1,1,1, % Flight 2
        5,1,1,1,1,1, % ..
        6,2,2,1,1,1,
        7,3,3,1,1,1,
        4,1,1,1,1,1,
        5,1,1,1,1,1,
        6,1,1,1,1,1,
        6,2,2,1,1,1,
        7,3,3,1,1,1  % Flight 10
]);


output [

       if i = 1 /\ j = 1 then
       "number of person: " ++ show(z) ++ "\n"
       else "" endif ++
       if j mod numPersons = 1 then show(i) ++ ": " else "" endif ++
       show(crew[i,j]) ++ if j mod numPersons = 0 then "\n" else " " endif 
       | i in 1..numFlights, j in 1..numPersons
] ++ ["\n"]
;
