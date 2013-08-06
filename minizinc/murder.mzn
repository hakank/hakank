% 
% Murder puzzle in MiniZinc.
% 
% From NSolver 
% http://www.cs.cityu.edu.hk/~hwchun/nsolver/content/show.shtml?Murder
% """
% Who was the Murderer?
% 
% Someone was murdered last night, and you are summoned to investigate the 
% murder. The objects found on the spot that do not belong to the victim 
% include: a pistol, an umbrella, a cigarette, a diary, and a threatening 
% letter. There are also witnesses who testify that someone had argued 
% with the victim, someone left the house, someone rang the victim, and 
% some walked past the house several times about the time the murder 
% occurred. 
% 
% The suspects are: Miss Linda Ablaze, Mr. Tom Burner, Ms. Lana Curious, 
% Mrs. Suzie Dulles, and Mr. Jack Evilson. Each suspect has a different 
% motive for the murder, including: being harassed, abandoned, sacked, 
% promotion and hate. Other clues are given below.
% 
% The cigarette belongs to Mr. Burner. Neither Ms. Curious nor the person 
% who was sacked by the victim is the author of the threatening letter. 
% Also, Ms. Curious does not own the pistol and she did not hate the 
% victim. In fact, the person who hated the victim is the one who owns 
% the diary that disclosed this information. The person who owns the 
% umbrella is the one who left the victim's house without it. It is 
% Mrs. Dulles who walked past the house several times. The person who 
% argued with the victim is the man who stands a good chance of being 
% promoted to the victim's position. As for Miss Ablaze, she had been 
% often harassed by the victim, but she did not write the threatening 
% letter and did not commit the murder. Finally, it is established that 
% the people heard or seen by the witnesses are different people among 
% the suspects and that they did not commit the murder; also each 
% evidence-object belongs to a different suspect.
% 
% Who was the murderer? And what was the motive, the evidence-object, 
% and the activity associated with each suspect. 
% """
% 
% Note: There are 4 solutions.

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: n = 5;
set of int: r = 1..n;
% the evidence objects
var r: pistol;
var r: umbrella;
var r: cigarette;
var r: diary;
var r: letter;
array[r] of var r: objects = [pistol, umbrella, cigarette, diary, letter];

% the actions
var r: argue;
var r: leave;
var r: rang;
var r: walk;
var r: murder;
array[r] of var r: actions = [argue, leave, rang, walk, murder];

% the motives
var r: harassed;
var r: abandoned;
var r: sacked;
var r: promotion;
var r: hate;
array[r] of var r: motives = [harassed, abandoned, sacked, promotion, hate];

% the suspects
r: Ablaze = 1;
r: Burner = 2;
r: Curious = 3;
r: Dulles = 4;
r: Evilson = 5;

solve satisfy;


% additonal clues
constraint
  cigarette = Burner /\ 
  letter != Curious /\ 
  sacked != letter /\ 
  pistol != Curious /\ 
  hate != Curious /\ 
  hate = diary /\ 
  umbrella = leave /\ 
  walk = Dulles /\ 
  argue = promotion /\ 
  harassed = Ablaze /\ 
  letter != Ablaze /\ 
  murder != Ablaze /\ 

  all_different(actions) /\ 
  all_different(objects) /\ 
  all_different(motives)

;


output [
  "actions: ", show(actions), "\n",
  "objects: ", show(objects), "\n",
  "motives: ", show(motives), "\n",
  

];