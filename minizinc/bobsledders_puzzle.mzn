% 
% Bobsledders puzzle in MiniZinc.
% 
% From https://aminoapps.com/c/school/page/blog/word-puzzle/bYRr_nncoumrlB5Z71q5nj812DNWmnj2zk
% """
% Person: Zane, Bobby, Ted, Lance, Phil, Connie, Jane, Oprah, Betsy, Lulu
%
% Sleds: Red, blue, black, green, white
%
% Country: Jamaica, Germany, Canada, USA, Romanian
%
% Hints:
%   1. Betsy and Phil are the Green sled duo. Though, in unrelated news, Phil doesn't like Betsy much.
%   2. The Romanian team doesn't use the Red Sled.
%   3. Bobby doesn't use the Black Sled.
%   4. Phil doesn't sled for Canada or Romanian.
%   5. Connie is German. She would never be caught dead in the Red Sled or the Blue Sled.
%   6. Ted and his partner drive the White Sled. They are not from USA, Germany, or Romanian.
%   7. Lulu is from USA.
%   8. Jane does not use the Blue Sled.
%   9. Lance is not from Germany, and he doesn't use the Red Sled.
%
% Will you find out which person is partner with?
% """

% It might be the same problem instance that is mentioned the or-tools-discuss by
% Fletcher Cocquyt
%  https://groups.google.com/forum/#!topic/or-tools-discuss/tarmhBizp0g
% 
% This problem is also shown in 
% http://ghvmath.com/new-events/2016/10/17/section-22-logic-contd
% with a video explaining the puzzle (though with a bad sound and a bad video).

% The solution is here as well:
% http://www.rossmcnamara.com/apps-and-games/cryptograms-word-games-brain-training/logic-grid-puzzle-answers/

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

enum names = {Zane, Bobby, Ted, Lance, Phil, Connie, Jane, Oprah, Betsy, Lulu};
enum sled_colors =  {red, blue, black, green, white};
enum country_names = {Jamaica, Germany, Canada, USA, Romanian};
enum sex = {female, male};

int: num_people = card(names);
int: num_pairs = num_people div 2;

                                   %  Zane, Bobby, Ted, Lance, Phil,   Connie, Jane, Oprah, Betsy, Lulu
array[1..num_people] of sex: gender = [male, male, male,male, male,    female, female, female,female,female];

% decision variables
array[1..num_people] of var 1..num_pairs: pairs; % what pair does a person belong to?
array[1..num_pairs]  of var country_names: countries;
array[1..num_pairs]  of var sled_colors: sleds;
array[1..num_people] of var names: partners;

solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

% general constraints
constraint
  all_different(countries) /\
  all_different(sleds) /\
  all_different(partners) /\
  % there are 2 people of each team
  global_cardinality(pairs, [i | i in 1..num_pairs], [2 | i in 1..num_pairs]) /\
  % a team consists of a female and a male
  forall(p1,p2 in 1..num_people where p1 != p2) (
    if pairs[p1] = pairs[p2] then gender[p1] != gender[p2] else true endif
    /\
    (pairs[p1] = pairs[p2] <-> partners[p1] = p2 /\ partners[p2] = p1)
  )
  /\ % symmetry breaking: Fix the pairs number of the guys
  forall(p in 1..num_pairs) (
     pairs[p] = p
  )
;

% The specific constraints
constraint
   % 1. Betsy and Phil are the Green sled duo. Though, in unrelated news, Phil doesn't like Betsy much.
   pairs[Betsy] = pairs[Phil] /\
   sleds[pairs[Betsy]] = green /\ % sleds[pairs[Phil]] = green /\

   % 2. The Romanian team doesn't use the Red Sled.
   % sleds[Romanian] != red /\
   % note: without this constraint, there are 3 different solutions
   forall(p in 1..num_pairs) ( if sleds[pairs[p]] = red then countries[pairs[p]] != Romanian else true endif ) /\
   
   % 3. Bobby doesn't use the Black Sled.
   sleds[pairs[Bobby]] != black /\

   % 4. Phil doesn't sled for Canada or Romanian.
   countries[pairs[Phil]] != Canada /\ countries[pairs[Phil]] != Romanian /\

   % 5. Connie is German. She would never be caught dead in the Red Sled or the Blue Sled.
   countries[pairs[Connie]] = Germany /\
   sleds[pairs[Connie]] != red /\ sleds[pairs[Connie]] != blue /\

   % 6. Ted and his partner drive the White Sled. They are not from USA, Germany, or Romanian.
   sleds[pairs[Ted]] = white /\
   countries[pairs[Ted]] != USA /\ countries[pairs[Ted]] != Germany /\ countries[pairs[Ted]] != Romanian /\ 

   % 7. Lulu is from USA.
   countries[pairs[Lulu]] = USA /\

   % 8. Jane does not use the Blue Sled.
   sleds[pairs[Jane]] != blue /\

   % 9. Lance is not from Germany, and he doesn't use the Red Sled.
   countries[pairs[Lance]] != Germany /\ sleds[pairs[Lance]] != red

;

output 
[
  "partners: \(partners)\n",
  "pairs: \(pairs)\n",
  "countries: \(countries)\n",
  "sleds: \(sleds)\n",
]
++
[
 "\(p):\t\(partners[p])\t\(sleds[pairs[p]])\t\(countries[pairs[p]]) \n"
 | p in names
] 
;

