/*

  Rabbits problem in B-Prolog.


  From Pascal Van Hentenryck "The OPL Optimization Programming Language",
  page 9.

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

go :-
        go1,go2,go3,go4,go5.

% CLP
go1 :-
        writeln(clp),
        N = 20,
        NbRabbits in 0..N,
        NbPheasants in 0..N,
        
        20 #= NbRabbits + NbPheasants,
        56 #= 4*NbRabbits + 2*NbPheasants,

        labeling([],[NbRabbits,NbPheasants]),

        writeln(nbRabbits:NbRabbits),
        writeln(nbPheasants:NbPheasants),
        nl.

% CP solve
go2 :-
        writeln(cp_solve),
        N = 20,
        NbRabbits in 0..N,
        NbPheasants in 0..N,
        
        20 $= NbRabbits + NbPheasants,
        56 $= 4*NbRabbits + 2*NbPheasants,

        cp_solve([NbRabbits,NbPheasants]),

        writeln(nbRabbits:NbRabbits),
        writeln(nbPheasants:NbPheasants),
        nl.

% IP solve
go3 :-
        writeln(ip_solve),
        N = 20,
        NbRabbits in 0..N,
        NbPheasants in 0..N,
        
        20 $= NbRabbits + NbPheasants,
        56 $= 4*NbRabbits + 2*NbPheasants,

        ip_solve([NbRabbits,NbPheasants]),

        writeln(nbRabbits:NbRabbits),
        writeln(nbPheasants:NbPheasants),
        nl.

% LP solve
go4 :-
        writeln(lp_solve),
        N = 20,
        lp_domain(NbRabbits,0,N),
        lp_domain(NbPheasants,0,N),
        
        20 $= NbRabbits + NbPheasants,
        56 $= 4*NbRabbits + 2*NbPheasants,

        lp_solve([NbRabbits,NbPheasants]),

        writeln(nbRabbits:NbRabbits),
        writeln(nbPheasants:NbPheasants),
        nl.

% SAT solve
go5 :-
        writeln(sat_solve),
        N = 20,
        NbRabbits in 0..N,
        NbPheasants in 0..N,
        
        20 $= NbRabbits + NbPheasants,
        56 $= 4*NbRabbits + 2*NbPheasants,

        sat_solve([NbRabbits,NbPheasants]),

        writeln(nbRabbits:NbRabbits),
        writeln(nbPheasants:NbPheasants),
        nl.

