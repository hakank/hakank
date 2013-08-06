/*

  Problem 6
  
  http://projecteuler.net/index.php?section=problems&id=6
  
  The sum of the squares of the first ten natural numbers is,
  12 + 22 + ... + 102 = 385

  The square of the sum of the first ten natural numbers is,
  (1 + 2 + ... + 10)2 = 552 = 3025

  Hence the difference between the sum of the squares of the first ten
  natural numbers and the square of the sum is 3025 385 = 2640.

  Find the difference between the sum of the squares of the first one
  hundred natural numbers and the square of the sum.

  Solution: 25164150

*/

go :-
        time(problem6).

% 0.004s
problem6 :-
        foreach(I in 1..100,
                [ac(List1,[]), ac(List2,[])],
                (
                  List1^1 = [I**2|List1^0],
                  List2^1 = [I|List2^0]
                )
               ),
        sumlist(List1, SumSquares), 
        sumlist(List2, Sum), 
        SquaresSum is Sum**2, 
        Diff is SquaresSum - SumSquares,
        writeln(Diff).
