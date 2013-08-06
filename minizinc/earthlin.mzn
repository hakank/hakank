% 
% Earthlings puzzle in MiniZinc.
%
% From Martin Chlond Integer Programming Puzzles
% http://www.chlond.demon.co.uk/puzzles/puzzles3.html, puzzle nr. 11
% Description  : Earthlings
% Source       : Poniachik, J. & L., (1998), Hard-to-solve Brainteasers, Sterling

%
% This model was inspired by the XPress Mosel model created by Martin Chlond.
% http://www.chlond.demon.co.uk/puzzles/sol3s11.html

%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: team = 3;   % 1 = Zaire, 2 = Uruguay, 3 = Spain
int: place = 3;  % 1st, 2nd or 3rd 
int: ttype = 3;  % 1 = truth-teller, 2 = alternator, 3 = liar
int: state = 3;  % statements 1 = x(1,1)+x(2,2)+x(3,3)=3
                 %            2 = x(1,1)+x(3,2)+x(2,3)=3
                 %            3 = x(2,1)+x(3,2)+x(1,3)=3 

set of 1..team: T = 1..team;
set of 1..place: P = 1..place;
set of 1..ttype: E = 1..ttype;
set of 1..state: S = 1..state;

% x(i,j) = 1 if team i in place j , 0 otherwise 
array[T,P] of var 0..1: x; % 
% y(k,l) = 1 if statement k made by type l
array[S, E] of var 0..1: y;
% d(k) = number of truths in statement k
array[S] of var 0..3: d;

solve satisfy;

constraint 
   % each place one team
   forall(j in P) (
        sum(i in T) (x[i,j]) = 1
   )
   /\
   % each team one place
   forall(i in T) (
        sum(j in P) (x[i,j]) = 1
   )
   /\
   % each type makes one statement
   forall(k in E) (
        sum(l in S) (y[k,l]) = 1
   )
  /\
  % each statement made by one type
  forall(l in S) (
        sum(k in E) (y[k,l]) = 1
  )
  /\
  % d[i] = number of truths in statement i
  x[1,1]+x[2,2]+x[3,3] = d[1]
  /\
  x[1,1]+x[3,2]+x[2,3] = d[2]
  /\
  x[2,1]+x[3,2]+x[1,3] = d[3]
  /\

  forall(k in S) (
   % if statement k made by truthteller (i.e. d[k]=3 ] then y[k,1] = 1, else 0
        d[k] - 3*y[k,1] >= 0
  )
  /\
  forall(k in S) (
        d[k] - 3*y[k,1] <= 2
  )
  /\
  % if statement k made by liar (i.e. d[k]=0 ] then y[k,3] = 1, else 0
  forall(k in S) (
        d[k] + 3*y[k,3] <= 3
  )
  /\
  forall(k in S) (
    d[k] + y[k,3] >= 1
  )
  /\
  % assertion 1 and 3 either both true or both false for all statements
  x[1,1]=x[3,3]
  /\
  x[1,1]=x[2,3]
  /\
  x[2,1]=x[1,3]
;


output [
   if i = 1 /\ j = 1 then
   "x:" else "" endif ++
   if j = 1 then 
   "\n" else " " endif ++
   show(x[i,j])
   
   | i in T, j in T
] ++
[

   if i = 1 /\ j = 1 then
   "\ny:" else "" endif ++
   if j = 1 then 
   "\n" else " " endif ++
   show(y[i,j])
   
   | i in S, j in E
] ++ 
[
  if i = 1 then
   "\nd:\n" else "\n" endif ++
  show(d[i]) 
  | i in S
];
