konsult :- write ( 'What is your problem?'),
                    read(X),
                    (X),
                    repeat,
                    seeing(user).



'car wont start' :- write( 'Is the battery voltage low?' ),
                    affirm,
                    nl,
                    write( 'Check battery' ).

'car wont start' :- write( 'Smell gasoline?' ),
                    affirm,
                    nl ,
                    'fuel system'.

'fuel system'    :- write( 'Try full throttle cranking' ).

'fuel system'    :- write( 'Are there fuel line leaks?' ),
                    affirm,
                    nl,
                    write( 'Replace fuel line' ).

'fuel system'    :- write( 'Check carburator' ).

'car wont start' :- write( 'Is spark present?' ),
                    not( affirm ),
                    nl,
                    'no spark'.

'no spark'       :- write( 'Do points open and close?' ),
                    not( affirm ),
                    nl,
                    write( 'Adjust or replace points' ).

'no spark'       :- write( 'Is the spark off the coil good?' ),
                    affirm,
                    write( 'Check plug wires and cap' ).

'no spark'       :- write( 'What is the voltage on the primary of the coil: ' ),
                    read( Volts ),
                    Volts < 10,
                    nl,
                    write( 'Check wiring and ballast resistor' ).

'no spark'       :- write( 'Does the capacitor leak?' ),
                    affirm,
                    write( 'Replace the capacitor' ).

'no spark'       :- not( 'primary circuit' ).

'primary circuit' :- write( 'Open the points. Voltage across coil?:' ),
                     nl,
                     read( Openvolts ),
                     Openvolts < 1,
                     write( 'Close the points. Voltage across coil?:' ),
                     read( Closevolts ), Closevolts > 10,
                     nl,
                     write( 'primary circuit is OK' ).


'no spark'       :- write( 'Consider a hidden fault. Swap cap, rotor, points, capacitor' ).

'car wont start' :- write( 'Get a tow truck!!!' ).












