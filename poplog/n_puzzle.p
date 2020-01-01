/*

   (n x n)-1 puzzle, e.g. 8-puzzle, 15-puzzle, etc in Pop-11.

   This program uses the SOLVEMS library which handles the 
   backtracking etc. See
     * TEACH SOLVEMS

   Notes: 
     - The position n*n is the blank space in the goal. This simplifies
       the estimate procedure somewhat, which use the distance from
       a cell to its goal position.


   This Pop-11 program was created by Hakan Kjellerstrand (hakank@gmail.com
   See also my Pop-11/Poplog page: http://www.hakank.org/poplog/

*/
compile('/home/hakank/Poplib/init.p');
uses teaching;
lib solvems;

;;; (8,15,24,...)-puzzle, or rather (n x n)-1-puzzle
;;; The position n*n is the blank space in the goal.
;;; 
vars n = 3;
;;; vars n = 4;
;;; vars n = 5;


;;; For n = 3
;;; This is the puzzle in TEACH SOLVEMS:
vars puzzle = [[6 8 7] [3 5 1] [4 2 9]]; ;;; astar: 310 states, length 27
                                             ;;; best: 41 states, length 31
;;; vars puzzle = [[9 8 7] [3 6 1] [4 2 5]]; ;;; astar: 2847 states, length 33
                                             ;;; best: 44 states: length 33
;;; vars puzzle = [[9 8 7] [6 5 4] [3 2 1]]; ;;; 28 states, length 29
;;; vars puzzle = [[1 9 3] [4 2 5] [7 8 6]]; ;;; simple
;;; vars puzzle = [[3 2 1] [4 5 6] [7 8 9]];


;;; For n = 4;
;;; astar: 1136 states, length 22
;;; vars puzzle = [[1 2 3 4] [5 6 7 8] [9 10 11 12] [13 14 15 16]];
;; vars puzzle = [[1 2 3 4] [5 6 7 8] [9 10 11 12] [16 14 15 13]];
;;; vars puzzle = [[1 2 3 4] [5 6 7 8] [9 10 11 12] [13 16 15 14]]; ;;; is this valid?


/*
   print_puzzle(puzzle)

*/
define print_puzzle(p);
    lvars i,j;
    lvars len = p.length;
    for i to len do
        for j to len do
            pr(p(i)(j));
            pr(' ');
        endfor;
        pr('\n');
    endfor;  
enddefine;


/*

  make_goal(n)

  Create the goal positions for a (n x n)-1 puzzle,
  where the position n x n contains the blank
  (represented by the number n x n).

  Example for 8-puzzle (3 x 3 - 1):
    [ 
      [1 2 3] 
      [4 5 6] 
      [7 8 9]
    ]

  9 is the blank.

*/
define make_goal(n);
    lvars i,j;
    vars counter = 0;
    [%
      for i to n do
          [% for j to n do counter + 1 ->> counter; endfor;%]
      endfor 
      %];
enddefine;

/*
  make_puzzle(n, t)

  creates a random puzzle.
  n = the dimension: n x n
  t = number of swaps

*/
define make_puzzle(n, t);
    [make_puzzle ^n ^t]=>;
    lvars puz = make_goal(n);
    lvars zero = [^n ^n]; 
    lvars a,b,i,j,c,t1;
    dl(zero) -> (a,b);
    lvars valid = [ [-1 0] [0 -1] [1 0] [0 1] ];
    0 -> t1;
    while t1 < t do
        oneof(valid) -> c;
        dl(c)->(i,j);
        lvars newi = a+i;
        lvars newj = b+j;
        if newi > 0 and newi <= n and
                newj > 0 and newj <= n then
            puz(a)(b), puz(newi)(newj) -> (puz(newi)(newj), puz(a)(b));
            (newi,newj) -> (a,b);
            t1 + 1 -> t1;
        endif;
    endwhile;

    return(puz);

enddefine;



/*
  make_puzzle_estimate(n, estimate_value)

  creates a random puzzle with a certain 
  estimate value

  n = the dimension: n x n
  est = target estimate

*/
define make_puzzle_estimate(n, est);
    [make_puzzle_estimate ^n ^est]=>;
    lvars puz = make_goal(n);
    lvars zero = [^n ^n]; 
    lvars a,b,i,j,c,est1;
    dl(zero) -> (a,b);
    lvars valid = [ [-1 0] [0 -1] [1 0] [0 1] ];
    0 -> est1;
    while est /= est1 do
        oneof(valid) -> c;
        dl(c)->(i,j);
        lvars newi = a+i;
        lvars newj = b+j;
        if newi > 0 and newi <= n and
                newj > 0 and newj <= n then
            puz(a)(b), puz(newi)(newj) -> (puz(newi)(newj), puz(a)(b));
            (newi,newj) -> (a,b);
            estimate(puz)->est1
        endif;
    endwhile;

    return(puz);

enddefine;


/*
    find_blank(puzzle, num)

    Finds the blank in the puzzle where num is the last 
    position in the grid (n x n).

    TODO (maybe):
     - Maybe the position of the blank should be a part of the
       puzzle structure.

*/
define find_blank(p, num); 
   lvars i,j,len = p.length; 
   for i to len do 
       for j to len do 
           if p(i)(j) == num then
               return([^i ^j]);
           endif; 
       endfor; 
   endfor; 
enddefine;    


/*

   adjacent(this_state) 
   
   Creates the adjacent states of this state.

   Note: This is a redefinition of the adjacent procedure in solvems.p .
 
*/
define adjacent(old) -> result;
    vars i,j, a,b, new;
    vars n = old.length;
    lvars valid = [ [-1 0] [0 -1] [1 0] [0 1] ];
    [] -> result;
    lvars zero = find_blank(old, n*n);
    dl(zero) -> (a,b);
    lvars c, newi, newj;
    for c in valid do
        a+c(1) -> newi;
        b+c(2) -> newj;
        ;;; assure that this is a valid move
        if newi > 0 and newi <= n and
                newj > 0 and newj <= n then
	    copydata(old) -> new;
            ;;; swap
            new(a)(b), new(newi)(newj) -> (new(newi)(newj), new(a)(b));
            ;;; add to queue
	    [^new ^^result] -> result;
        endif;
    endfor;
enddefine;


vars goal = make_goal(n);

/*
   isgoal(state)

   Is this a goal state?

   Note: This is a redefition of procedure in solvems.p

*/
define isgoal(state);
    state = goal;                 
enddefine;

/*

  estimate(state)

  This is the estimate of how good this state is, 
  or rather: how far this state is to the goal state.

  The principle: Sum the difference of the value in 
  "this" cell from it's cell number.

  Note: This is a redefition of procedure in solvems.p

*/
define estimate(state) -> result;
    lvars i;
    0 -> result;
    lvars len = state.length;
    for i to len do
        for j to len do
            result + abs(state(i)(j) - ((i - 1)*len+j)) -> result;
        endfor;
    endfor;

enddefine;

/*
   create a random puzzle

   There are two ways of creating 
   a random puzzle:

   
   * make_puzzle(n, t) 
     where t is number of swaps

   * make_puzzle_estimate(n, est)
     where est is the estimate_value 
     of the puzzle.
     
*/

;;; create puzzle with 24 swaps
;;; make_puzzle(n, 28)->puzzle;

;;; create puzzle with estimate value 28
make_puzzle_estimate(n, 28)->puzzle;


'puzzle:'=>;
print_puzzle(puzzle);

vars orig_estimate = estimate(puzzle);
'orig_estimate:', orig_estimate=>;


;;; adjacent(puzzle)=>;
puzzle -> initial;

true -> debug;
;;; true -> verbose;

/*
  The following are the comparison strategies
  defined in solevms.p . Defauls is astar.

*/
;;; depth -> compare;
;;; breadth -> compare;
;;; hill -> compare;
;;; best -> compare;
;;; astar -> compare; ;;; best + length

go;

'\norig_estimate:', orig_estimate=>;
