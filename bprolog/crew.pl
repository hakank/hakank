/*

  Crew allocation problem in B-Prolog.

  From Gecode example crew
  examples/crew.cc
  """ 
  (Original text from crew.cc)
  * Example: Airline crew allocation
  *
  * Assign 20 flight attendants to 10 flights. Each flight needs a certain
  * number of cabin crew, and they have to speak certain languages.
  * Every cabin crew member has two flights off after an attended flight.
  *
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/



go :-
        crew.




crew :-

        attributes(Attributes),
        required(RequiredCrew),
        length(RequiredCrew, NumFlights),

        names(Names),
        length(Names,NumPersons),

        format("Number of flights: ~d Number of persons: ~d\n",[NumFlights,NumPersons]),

        % split this table
        foreach(R in RequiredCrew,
                [ac(TotalRequired,[]),ac(Required,[])],
                [R,Tot,Rest],
                (
                    R = [Tot|Rest],
                    TotalRequired^1 = [Tot|TotalRequired^0],
                    Required^1 = [Rest|Required^0]
                )
        ),

        % the crew schedule
        new_array(Crew,[NumFlights, NumPersons]),
        array_to_list(Crew,CrewList),
        CrewList :: 0..1,

        % objective to (perhaps) minimize:
        % total number of persons working during these flights
        Z :: 0..NumPersons,
        Z #= sum([(T#>0) : P in 1..NumPersons,[T],
                  T #= sum([S : F in 1..NumFlights, [S], S @= Crew[F,P]])
                 ]),

        % calculate the persons for a specific flight
        % that has the proper requirements.
        % Also the number of persons for a flight.
        transpose(Attributes,AttributesTransposed),

        CrewRows @= Crew^rows,
        foreach((C,Tot2,Req) in (CrewRows,TotalRequired,Required),
                  (% number of persons for a flight
                   sum(C)#=Tot2,
                   % require the number of persons with 
                   % the proper attributes
                   foreach((R,Attribute) in (Req,AttributesTransposed),
                           scalar_product(Attribute,C,#>=,R)))
               ),

        % after a flight, break for two flights, i.e.
        % for three consecutive flights there can be
        % work for max 1 of these flights.
        transpose(Crew,CrewTransposed),
        foreach(C in CrewTransposed, consecutive_sum_to_less_than(C,3,1)),
        

        % extra contraint: all persons (if at all) 
        % must work at least two times
        foreach(C1 in CrewTransposed, (sum(C1) #>=2 #\/ sum(C1) #= 0)),

        %
        % search
        % 
        append(CrewList,[Z],Vars),
        labeling([ff], Vars),
        % optimization
        % minof(labeling([ff],Vars), Z),

        format('Number of persons working (Z): ~d\n',[Z]),
        foreach((Flight,C) in (1..NumFlights,CrewRows), format("Flight ~2d: ~w\n",[Flight, C])),

        nl,
        writeln('Persons working during the flights:'),
        foreach((I,Flight) in (1..NumFlights,CrewRows),[Assign,Assign2],
                (foreach((F,Name) in (Flight,Names), ac(Assign,[]),
                         (F #= 1 -> 
                              Assign^1 = [Name|Assign^0]
                         ;
                              Assign^1 = Assign^0
                         )),
                 reverse(Assign,Assign2),
                 format("Flight ~2d: ~w\n",[I,Assign2])
                )
               ),
        
        writeln('\nPersonell working at flights:'),

        foreach((P,Person) in (1..NumPersons, CrewTransposed),
                [PFlights,Name],
                ( element(P,Names,Name),
                  boolean_to_set(Person,PFlights),
                  format("Person ~d (~w) Flights: ~w\n",[P,Name,PFlights])
                 )
                ),
        nl.
        

transpose(Matrix,Transposed):-
    N is Matrix^length,
    M is Matrix[1]^length,
    Transposed @= [MJI : J in 1..M, [MJI],
                (MJI @= [Matrix[I,J] : I in 1..N])].


boolean_to_set(List,Set) :-
        length(List, Len),
        foreach((I,C) in (1..Len, List), ac(Set2,[]),
                (C #= 1 -> Set2^1 = [I|Set2^0] ; Set2^1 = Set2^0)
         ),
        reverse(Set2,Set).


% Require that all D consecutive numbers sums to =< Sum.
consecutive_sum_to_less_than(X,D,Sum) :-
        length(X,Len),
        foreach(I in 1..Len-D, sum([X[J] : J in I..I+D-1 ]) #=< Sum).
        

matrix_element(X, I, J, Val) :-
        nth1(I, X, Row),
        element(J, Row, Val).


/* 
  stewards  = {Tom, David, Jeremy, Ron, Joe, Bill, Fred, Bob, Mario, Ed};
  hostesses = {Carol, Janet, Tracy, Marilyn, Carolyn, Cathy, Inez,
               Jean, Heather, Juliet};
  frenchSpeaking = {Bill, Inez, Jean, Juliet};
  germanSpeaking = {Tom, Jeremy, Mario, Cathy, Juliet};
  spanishSpeaking = {Joe, Bill, Fred, Mario, Marilyn, Inez, Heather};
*/
attributes(
        % steward, hostess, french, spanish, german
        [[1,0,0,0,1], % Tom     = 1
         [1,0,0,0,0], % David   = 2
         [1,0,0,0,1], % Jeremy  = 3
         [1,0,0,0,0], % Ron     = 4
         [1,0,0,1,0], % Joe     = 5
         [1,0,1,1,0], % Bill    = 6
         [1,0,0,1,0], % Fred    = 7
         [1,0,0,0,0], % Bob     = 8
         [1,0,0,1,1], % Mario   = 9
         [1,0,0,0,0], % Ed      = 10

         [0,1,0,0,0], % Carol   = 11
         [0,1,0,0,0], % Janet   = 12
         [0,1,0,0,0], % Tracy   = 13
         [0,1,0,1,1], % Marilyn = 14
         [0,1,0,0,0], % Carolyn = 15
         [0,1,0,0,0], % Cathy   = 16
         [0,1,1,1,1], % Inez    = 17
         [0,1,1,0,0], % Jean    = 18
         [0,1,0,1,1], % Heather = 19
         [0,1,1,0,0]  % Juliet  = 20
        ]).


names([
          'Tom',     % 1
          'David',   % 2
          'Jeremy',  % 3
          'Ron',     % 4
          'Joe',     % 5
          'Bill',    % 6 
          'Fred',    % 7
          'Bob',     % 8
          'Mario',   % 9
          'Ed',      % 10
          
          'Carol',   % 11
          'Janet',   % 12
          'Tracy',   % 13
          'Marilyn', % 14 
          'Carolyn', % 15 
          'Cathy',   % 16
          'Inez',    % 17
          'Jean',    % 18
          'Heather', % 19 
          'Juliet'   % 20
      ]).


/* required crew per flight 
   The columns are in the following order
     staff:     Overall number of cabin crew needed
     stewards:  How many stewards are required
     hostesses: How many hostesses are required
     french:    How many French speaking employees are required
     spanish:   How many Spanish speaking employees are required
     german:    How many German speaking employees are required
*/

required(
        [[4, 1,1,1,1,1], % Flight 1
         [5, 1,1,1,1,1], % Flight 2
         [5, 1,1,1,1,1], % Flight 3
         [6, 2,2,1,1,1], % Flight 4
                        [7, 3,3,1,1,1], % Flight 5
         [4, 1,1,1,1,1], % Flight 6
         [5, 1,1,1,1,1], % Flight 7
         [6, 1,1,1,1,1], % Flight 8
         [6, 2,2,1,1,1], % Flight 9
         [7, 3,3,1,1,1]  % Flight 10
        ]
).