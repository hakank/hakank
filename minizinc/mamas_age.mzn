% 
% Mama' age problem in MiniZinc.
% 
% 
% Mamma's Age from "Amusements in Mathematics, Dudeney", number 40.
% """
% Tommy: "How old are you, mamma?"
% Mamma: "Our three ages add up to exactly seventy years."
% Tommy: "And how old are you, papa?"
% Papa: "Just six times as old as you, my son."
% Tommy: "Shall I ever be half as old as you, papa?"
% Papa: "Yes, Tommy; and when that happens our three ages will add up to
% exactly twice as much as today."
%
% Can you find the age of Mamma?
% """

% Compare with the following models:
% * SICStus Prolog: http://www.hakank.org/sicstus/mamas_age.pl
% * ECLiPSe: http://www.hakank.org/eclipse/mamas_age.ecl
% * Gecode: http://www.hakank.org/gecode/mamas_age.cpp

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

var 1..500: m; % mamma's age
var 1..500: p; % papa's age
var 1..500: t; % tommy's age
var 1..500: i; % temp

% the real ages
var 1..100: m2; % mamma's age
var 1..100: p2; % papa's age
var 1..100: t2; % tommy's age



solve satisfy;

constraint
   m + p + t = 70 * 12 /\
   6 * t = p /\
   (t + i) * 2 = p + i /\
   m + i + p + i + t + i = 2 * (m + p + t)

   /\
   m2 = m div 12 /\
   p2 = p div 12 /\
   t2 = t div 12

;

output
[
  show([m,p,t,i,   m2, p2, t2])
];