% 
% Enigma 1576 (The holly and the ivy and the...) in MiniZinc.
% 
% From 
% http://www.newscientist.com/article/mg20427391.200-enigma-number-1576.html
% """
% 16 December 2009 by Susan Denham
% 
% The holly and the ivy and the...
% 
% This year I have designed my own Christmas card. It consists of 16 
% squares pasted onto a card in a closely packed 4-by-4 array. There are 
% four pictures of holly, four of ivy, four of mistletoe and four of 
% fir trees. There are four background colours of gold, silver, red or 
% white, and every combination of plant and background colour (such as 
% holly on a red background) can be found.
% 
% In each row and in each column of the array there is one square of 
% each plant and one square of each background colour. In the top row 
% the colours (from left to right) are gold, silver, red and white.
% 
% The holly on a gold background touches the ivy on a silver background, 
% whereas no two mistletoe squares are touching, not even at their corners. 
% One row begins with a fir tree on a white background. What (in order) 
% are the other three squares in that row?
% """
% 
% Solution:
% colors:
% 1 2 3 4
% 4 3 2 1 <--
% 3 4 1 2
% 2 1 4 3
% 
% plants:
% 2 3 4 1
% 4 1 2 3 <--
% 3 2 1 4
% 1 4 3 2
% 
% The row that begins with Fir tree on White is the following
%  4 4 plant: Fir       color: White
%  1 3 plant: Holly     color: Red
%  2 2 plant: Ivy       color: Silver
%  3 1 plant: Mistletoe color: Gold
%

% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 4;
set of int: Plants = 1..4;
set of int: Colors = 1..4;

% plants
int: Holly     = 1;
int: Ivy       = 2;
int: Mistletoe = 3;
int: FirTree   = 4;

% colors
int: Gold   = 1;
int: Silver = 2;
int: Red    = 3;
int: White  = 4;

array[1..n,1..n] of var Plants: plants;
array[1..n,1..n] of var Colors: colors;

array[1..n,1..n, 1..2] of var 1..n: pairs;
% array[1..n,1..n] of var 1..n: positions;

predicate latin_square(array[int,int] of var int: x) =
   forall(i in index_set_1of2(x)) (
       all_different([ x[i, j] | j in index_set_2of2(x)]) /\
       all_different([ x[j, i] | j in index_set_2of2(x)])
   )
;

%
% ensure that we have all pairs of x (plants) and y (colors)
%
predicate all_pairs(array[int,int] of var int: x,
                    array[int,int] of var int: y) =
   let {
     int: nn = card(index_set_1of2(x)),
     array[1..nn,1..nn] of var 1..n: pos
   } in
  forall(k in 1..nn) (
    forall(i in 1..nn) (
      let {
        var 1..nn: j
      } in 
        x[i,j] = k /\ pos[k,i] = j
    )
  )
  /\
  forall(i in 1..nn) (
      all_different([y[j,pos[i,j]] | j in 1..nn])
  )

;

% arrays of 1d where both arguments are var int
predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 


% solve satisfy;
solve :: int_search(
        [plants[i,j] | i,j in 1..n] ++
        [colors[i,j] | i,j in 1..n]
        , 
        first_fail, 
        indomain_min, 
        complete) 
    satisfy;

constraint
  latin_square(plants) /\
  latin_square(colors)

  /\ % this is just to show the pairs
  forall(i,j in 1..n) (
     pairs[i,j,1] = plants[i,j] /\
     pairs[i,j,2] = colors[i,j] 
  )
  /\ % all combinations of plants and colors can be found
  all_pairs(plants, colors)

  % the clues 

  /\ % top row  
  cp1d([colors[1,j] | j in 1..n], [Gold,Silver,Red,White])

  /\ % one row begins with Fir tree on White (4,4)
  exists(i in 1..n) (
    plants[i,1] = FirTree /\ colors[i,1] = White
  )

   /\ % Holly on Gold (1,1) touches Ivy on Silver (2,2)
   exists(i,j in 1..n, a,b in {-1,0,1} where  
           i+a > 0  /\ j+b >  0 /\ i+a <= n /\ j+b <= n) (
     plants[i,j]     = Holly /\ colors[i,j]     = Gold /\
     plants[i+a,j+b] = Ivy   /\ colors[i+a,j+b] = Silver
     % /\ (a = 0 \/ b = 0)
   )

  /\ % No mistletoe (3) square are touching, not even at their corners
  forall(i in 2..n) (
    exists(j,k in 1..n) (
      plants[i,j] = Mistletoe /\ plants[i-1,k] = Mistletoe /\ 
      abs(j-k) > 1
    )
  )

;


output
[ "Plants:" ]
++
[
  if j = 1 then "\n" else " " endif ++ 
    show(plants[i,j])
  | i,j in 1..n
]
++
[
  "\nColors: "
]
++
[
  if j = 1 then "\n" else " " endif ++ 
    show(colors[i,j])
  | i,j in 1..n

];
