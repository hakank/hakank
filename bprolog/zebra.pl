/*

  Zebra problem in B-Prolog.

  Lewis Carrol's classical puzzle with five houses and a zebra:
  
  Five men with different nationalities live in the first five houses
  of a street.  They practise five distinct professions, and each of
  them has a favourite animal and a favourite drink, all of them
  different.  The five houses are painted in different colours.
  
  The Englishman lives in a red house.
  The Spaniard owns a dog.
  The Japanese is a painter.
  The Italian drinks tea.
  The Norwegian lives in the first house on the left.
  The owner of the green house drinks coffee.
  The green house is on the right of the white one.
  The sculptor breeds snails.
  The diplomat lives in the yellow house.
  Milk is drunk in the middle house.
  The Norwegian's house is next to the blue one.
  The violinist drinks fruit juice.
  The fox is in a house next to that of the doctor.
  The horse is in a house next to that of the diplomat.
  
  Who owns a Zebra, and who drinks water?
  

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

go :-

        % we use 5 lists of 5 variables each
	Nat = [English, Spaniard, Japanese, Italian, Norwegian],
	Color = [Red, Green, White, Yellow, Blue],
	Profession = [Painter, Sculptor, Diplomat, Violinist, Doctor],
	Pet = [Dog, Snails, Fox, Horse, Zebra],
	Drink = [Tea, Coffee, Milk, Juice, Water],

        % domains: all the variables range over house numbers 1 to 5
	Nat :: 1..5,
	Color :: 1..5,
	Profession :: 1..5,
	Pet :: 1..5,
	Drink :: 1..5,

        % the values in each list are exclusive
	alldifferent(Nat),
	alldifferent(Color),
	alldifferent(Profession),
	alldifferent(Pet),
	alldifferent(Drink),

        % and here follow the actual constraints
	English = Red,
	Spaniard = Dog,
	Japanese = Painter,
	Italian = Tea,
	Norwegian = 1,
	Green = Coffee,
	Green #= White + 1,
	Sculptor = Snails,
	Diplomat = Yellow,
	Milk = 3,
	Dist1 #= Norwegian - Blue, Dist1 :: [-1, 1],
	Violinist = Juice,
	Dist2 #= Fox - Doctor, Dist2 :: [-1, 1],
	Dist3 #= Horse - Diplomat, Dist3 :: [-1, 1],

        % put all the variables in a single list
	flatten([Nat, Color, Profession, Pet, Drink], List),

        % search: label all variables with values
	labeling(List),
	
        % print the answers: we need to do some decoding
	NatNames = [English-english, Spaniard-spaniard, Japanese-japanese,
		    Italian-italian, Norwegian-norwegian],
	member(Zebra-ZebraNat, NatNames),
	member(Water-WaterNat, NatNames),
	format("The ~w owns the zebra~n", [ZebraNat]),
	format("The ~w drinks water~n", [WaterNat]).

