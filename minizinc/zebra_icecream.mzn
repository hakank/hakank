/* 

  Zebra Icecream in MiniZinc.

  From https://www.ahapuzzles.com/logic/zebra/ice-creams/
  """ 
  Ice Creams Zebra Puzzle

  Four boys are side by side talking about their favorite ice cream flavor. 
  Find out who likes mint ice cream.

  - The oldest boy is wearing the Blue shirt.
  - Nicholas is next to the boy who likes Strawberry ice cream.
  - Harris is at the fourth position.
  - Miller's favorite ice cream is Chocolate.
  - The youngest boy is next to the boy that is wearing the Green shirt.
  - Young is at the third position.
  - The 9-year-old boy is exactly to the left of Young.
  - Kevin is next to the 9-year-old boy.
  - At the second position is the boy wearing the Black shirt.
  - Daniel likes Strawberry ice cream.
  - At the fourth position is the boy who likes Vanilla ice cream.
  - The 7-year-old boy is somewhere between the boy wearing the Blue shirt and the 8-year-old boy, in that order.
  - Nicholas is at the third position.
  """

  This model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my MiniZinc page: http://www.hakank.org/minizinc/

*/
include "globals.mzn"; 

int: n = 4;

var 1..n: Daniel;
var 1..n: Joseph;
var 1..n: Kevin;
var 1..n: Nicholas;
array[1..n] of var 1..n: FirstName = [Daniel,Joseph,Kevin,Nicholas];

var 1..n: Harris;
var 1..n: Miller;
var 1..n: Taylor;
var 1..n: Young;
array[1..n] of var 1..n: LastName = [Harris,Miller,Taylor,Young];  
  
var 1..n: Black;
var 1..n: Blue;
var 1..n: Green;
var 1..n: Red;
array[1..n] of var 1..n: Shirt = [Black,Blue,Green,Red];  

var 1..n: Chocolate;
var 1..n: Mint;
var 1..n: Strawberry;
var 1..n: Vanilla;
array[1..n] of var 1..n: Icecream = [Chocolate,Mint,Strawberry,Vanilla];  

array[1..n] of var 7..10: Age;  
  
array[1..n] of var 1..n: Position;    

solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  let {
    var 1..n: Age7;
    var 1..n: Age8;
    var 1..n: Age9;
  } in
  all_different(FirstName) /\
  all_different(LastName)  /\
  all_different(Shirt)  /\
  all_different(Icecream)  /\
  all_different(Age)  /\
  all_different(Position)  /\

  increasing(Position) /\  % order of presentation

  % - The oldest boy is wearing the Blue shirt.
  Age[Blue] = 10 /\

  % - Nicholas is next to the boy who likes Strawberry ice cream.
  abs(Position[Nicholas]- Position[Strawberry]) = 1 /\
  
  % - Harris is at the fourth position.
  Position[Harris] = 4 /\
  
  % - Miller's favorite ice cream is Chocolate.
  Miller = Chocolate /\
  
  % - The youngest boy is next to the boy that is wearing the Green shirt.
  Age[Age7] = 7 /\
  abs(Position[Green] - Position[Age7]) = 1 /\
  
  % - Young is at the third position.
  Position[Young] = 3 /\
  
  % - The 9-year-old boy is exactly to the left of Young.
  Age[Age9] = 9 /\
  Position[Age9] + 1 = Position[Young] /\
  
  % - Kevin is next to the 9-year-old boy.
  abs(Position[Kevin] - Position[Age9]) = 1 /\
  
  % - At the second position is the boy wearing the Black shirt.
  Position[Black] = 2 /\
  
  % - Daniel likes Strawberry ice cream.
  Daniel = Strawberry /\
  
  % - At the fourth position is the boy who likes Vanilla ice cream.
  Position[Vanilla] = 4 /\
  
  % - The 7-year-old boy is somewhere between the boy wearing the Blue shirt and the 8-year-old boy, in that order.
  Age[Age8] = 8 /\
  Position[Blue] < Position[Age7] /\
  Position[Age7] < Position[Age8] /\
  
  % - Nicholas is at the third position.
  Position[Nicholas] = 3
;

output [
        "firstName: \(FirstName)\n",
        "lastName : \(LastName)\n",
        "Shirt    : \(Shirt)\n",
        "Icecream : \(Icecream)\n",
        "Age      : \(Age)\n", 
        "Position : \(Position)\n",       


];

