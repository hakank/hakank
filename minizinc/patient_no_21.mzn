% 
% Patient No. 21 problem in MiniZinc.
% 
% From PuzzlOR
% Patient No. 21
% http://www.analytics-magazine.org/may-june-2010/149-the-puzzlor-patient-no-21.html
% """
% by John Toczek
% 
% Making a correct diagnosis on a patient is a very challenging task for 
% a doctor. A complex disease can manifest through any number of symptoms that 
% often leave "trial and error" as the only method for arriving at the correct 
% diagnosis. To aid doctors in this daunting task, mathematical modeling can be 
% used to help guide a diagnosis.
%
% Table 1 shows 20 patients with varying symptoms and corresponding diagnoses. 
% For example, Patient No. 1 reported symptoms of sneezing and a sore throat 
% and was found to have a cold. Patient No. 20 exhibited symptoms of fatigue 
% and sneezing that was found to be allergies. All diagnoses were confirmed through lab tests.
% 
% 
% Table 1
% [
%             Stuffy            Sore 
%    Fatigue  Nose    Sneezing  Throat    Diagnosis
% 
%    0,       0,      1,        1,        Cold, % 1
%    1,       1,      1,        0,        Cold, % 2
%    1,       1,      0,        1,        Cold, % 3
%    1,       0,      0,        1,        Cold, % 4
%    1,       1,      0,        1,        Cold, % 5
%    0,       1,      1,        1,        Cold, % 6
%    1,       0,      0,        0,        Cold, % 7
%    1,       1,      0,        0,        Cold, % 8
%    0,       1,      0,        0,        Cold, % 9
%    0,       0,      1,        1,        Cold, % 10
%
%    1,       0,      0,        1,        Flu, % 11
%    1,       0,      0,        1,        Flu, % 12
%    0,       1,      1,        0,        Flu, % 13
%    1,       1,      0,        0,        Flu, % 14
%    1,       0,      1,        0,        Flu, % 15
%    1,       0,      0,        0,        Flu, % 16
%
%    1,       0,      0,        1,        Allergies, % 17
%    0,       1,      1,        0,        Allergies, % 18
%    0,       0,      1,        0,        Allergies, % 19
%    1,       0,      1,        0,        Allergies, % 20
%
%    0,       1,      1,        1,        ???, % 21
%
% ]

% Question: Patient No. 21 has not been diagnosed yet is exhibiting symptoms of 
% stuffy nose, sneezing and sore throat. Using only the data in Table 1, rank the 
% three diagnoses (cold, flu and allergies) in order of how likely 
% Patient No. 21 has each.
% """

% This is the same approach as in http://www.hakank.org/minizinc/movie_stars.mzn
% which is an approach similar to the nearest neighbour principle:
%   - for all the known ratings of #21
%        calculate the distance between #21 and the other
%   - select the minimum distance (i.e. the one most like #21)
%     and pick that symptom.
%
%     Here we conclude that #21 is the one most like #6 (distance 0) and
%     thus also have Cold.
% 
% 
% Solution
% x: [0, 1, 1, 1, 1]
% dists: [1, 2, 2, 3, 2, 0, 4, 3, 2, 1, 3, 3, 1, 3, 3, 4, 3, 1, 2, 3]
% min_ix: 6
% #21 is most like 6
% probable diagnose: 1 (Cold)


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 
int: num_p; % number of persons (excluding #21)
int: num_r; % number of symptoms

array[1..num_p, 1..num_r] of int: data;
array[1..num_r-1] of int: testcase; % #21's data
int: the_case; % the index of the test case


% decision variables
% Note: we could use a single var 1..5: rate here
%       since we can compare using the testcase array.
%       However, I prefer to have all #21's rating
%       collected...
array[1..num_r] of var 0..1: x;

% The distances between #21 and the others
array[1..num_p] of var int: dists;

% the minimum distance
var int: min_dist = min(dists);

% index of the minimum in min_dist
var 1..num_p: min_ix; 
% var int: min_ix; 

% solve satisfy;
solve minimize min_dist;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

% 
% Calculate the distance between two persons.
% 
% Note: d is the the sum of squared distances but should be
%       the the square root of that sum. It doesn't matter here...
%
predicate dist(array[int] of int: a, array[int] of var int: v, var int: d) = 

      d = sum(i in index_set(a)) (
              (a[i]-v[i])*(a[i]-v[i])
          )
      /\ d >= 0
;

%
% min_index(ix, array)
%
% ix is the index of the minimum value in x (i.e. argmin).
% (I assume that the values are distinct...)
predicate min_index(var int: mi, array[int] of var int: x) =
  exists(i in index_set(x)) (
     x[i] = min(x) % minimum(x[i], x)
     /\
     mi = i 
  )
;


constraint
  % Fill in the known values of #21
  forall(i in 1..num_r-1) (
     x[i] = testcase[i]
  )

  /\ % Calculate the distances between #21 (testcase, x) 
     % and the other persons.
  forall(p in 1..num_p) (
     dist([data[p,i] | i in 1..num_r-1], testcase, dists[p])
     % dists[p] = sum([(testcase[i]-data[p,i])*(testcase[i]-data[p,i]) | i in 1..num_r-1])
  )

  /\ % get the index of the person with the minimum distance 
  min_index(min_ix, dists)
  /\ % assign the value of that person's rating for movie 5
  x[the_case] = data[min_ix, the_case]

;

output [
 "x: " ++ show(x) ++ "\n" ++
 "dists: " ++ show(dists) ++ "\n" ++
 "min_ix: " ++ show(min_ix) ++ "\n" ++ 
 "#21 is most like " ++ show(min_ix) ++ "\n" ++
 "probable diagnose: " ++ show(x[the_case]) ++ " (" ++ show(diagnoses_str[fix(x[the_case])]) ++ ")\n"
]
 ++ ["\n"]
;

%
% Data
%
num_p = 20;
num_r = 5;
data = array2d(1..num_p, 1..num_r,
[
   0,       0,      1,        1,        1, % Cold, % 1
   1,       1,      1,        0,        1, % Cold, % 2
   1,       1,      0,        1,        1, % Cold, % 3
   1,       0,      0,        1,        1, % Cold, % 4
   1,       1,      0,        1,        1, % Cold, % 5
   0,       1,      1,        1,        1, % Cold, % 6
   1,       0,      0,        0,        1, % Cold, % 7
   1,       1,      0,        0,        1, % Cold, % 8
   0,       1,      0,        0,        1, % Cold, % 9
   0,       0,      1,        1,        1, % Cold, % 10

   1,       0,      0,        1,        2, % Flu, % 11
   1,       0,      0,        1,        2, % Flu, % 12
   0,       1,      1,        0,        2, % Flu, % 13
   1,       1,      0,        0,        2, % Flu, % 14
   1,       0,      1,        0,        2, % Flu, % 15
   1,       0,      0,        0,        2, % Flu, % 16

   1,       0,      0,        1,        3, % Allergies, % 17
   0,       1,      1,        0,        3, % Allergies, % 18
   0,       0,      1,        0,        3, % Allergies, % 19
   1,       0,      1,        0,        3, % Allergies, % 20

%  0,       1,      1,        1,        ???, % 21
]);

testcase = [0,1,1,1]; % #21's data
the_case = 5; % I.e. the diagnosis

array[1..3] of string: diagnoses_str = ["Cold", "Flu", "Allergies"];