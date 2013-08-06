/*

  Rabbits problem in ECLiPSe.

  From Pascal Van Hentenryck "The OPL Optimization Programming Language",
  page 9.

  Compare with the Comet model http://www.hakank.org/comet/rabbits.co


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).


go :-
        
        N = 20,
        NbRabbits :: 0..N,
        NbPheasants :: 0..N,
        
        20 #= NbRabbits + NbPheasants,
        56 #= 4*NbRabbits + 2*NbPheasants,

        labeling([]),

        writeln(nbRabbits:NbRabbits),
        writeln(nbPheasants:NbPheasants).
