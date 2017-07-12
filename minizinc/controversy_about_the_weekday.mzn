% 
% Controversy about the weekday in MiniZinc.
% 
% From the German https://www.logisch-gedacht.de/logikraetsel/wochentag/
% translated by Google Translate
% """
% Seven friends met and disagreed, which day of the week was:
% Klaus: Today is Tuesday. 
% Sabine: No, that is not correct. Tomorrow is Tuesday. 
% Lutz: It is both wrong. Yesterday was Tuesday. 
% Claudia: No, Lutz, yesterday was Saturday! 
% Susanne: Today is either Thursday or Friday. 
% Fritz: I do not believe that. Today is Sunday. 
% Sophia:  No, today is not Sunday. 

% Only one person is correct with their statement.
%
% Determine the day of the week when the friends met! 
% """

% Solution: sat

% The solution comments:
% """
% The meeting took place on a Saturday because two statements are correct for every other day of the week.
% """

% Without the sum(truth) = 1 the solutions are
% today: mon
% truth: [0, 1, 0, 0, 0, 0, 1]
% ----------
% today: tue
% truth: [1, 0, 0, 0, 0, 0, 1]
% ----------
% today: wed
% truth: [0, 0, 1, 0, 0, 0, 1]
% ----------
% today: thu
% truth: [0, 0, 0, 0, 1, 0, 1]
% ----------
% today: fri
% truth: [0, 0, 0, 0, 1, 0, 1]
% ----------
% today: sat
% truth: [0, 0, 0, 0, 0, 0, 1]
% ----------
% today: sun
% truth: [0, 0, 0, 1, 0, 1, 0]
% ----------
% ==========
%
% Where we see that all days except Saturdat is covered by two statements. Only the negative 
% statement by Sophia (No, today is not Sunday) is correct. 
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 7;

enum days = {mon,tue,wed,thu,fri,sat,sun};

% decision variables
var days: today;
array[1..n] of var 0..1: truth; % truthfulness of the speakers


solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint

  %   1   2   3   4   5   6   7
  % mon,tue,wed,thu,fri,sat,sun

  % Klaus: Today is Tuesday. 
  (truth[1] = 1 <-> today = tue) /\

  % Sabine: No, that is not correct. Tomorrow is Tuesday. 
  % (truth[2] = 1 <-> today = tue-1) /\
  (truth[2] = 1 <-> today = enum_prev(days,tue)) /\

  % Lutz: It is both wrong. Yesterday was Tuesday. 
  % (truth[3] = 1 <-> today = tue+1) /\
  (truth[3] = 1 <-> today = enum_next(days,tue)) /\

  % Claudia: No, Lutz, yesterday was Saturday! 
  % (truth[4] = 1 <-> today = sat+1) /\
  (truth[4] = 1 <-> today = enum_next(days,sat)) /\

  % Susanne: Today is either Thursday or Friday. 
  (truth[5] = 1 <-> today in {thu,fri}) /\

  % Fritz: I do not believe that. Today is Sunday. 
  (truth[6] = 1 <-> today = sun) /\

  % Sophia:  No, today is not Sunday. 
  (truth[7] = 1 <-> today != sun) /\

  % Only one person is correct with their statement.
  sum(truth) = 1

;

output [
  "today: \(today)\n",
  "truth: \(truth)\n"
];

