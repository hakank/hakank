/*
   Turbo Prolog 2.0 Chapter 7, Example Program 1
   
   Copyright (c) 1986, 88 by Borland International, Inc
   
*/

domains

land, leader = symbol
population = integer   

predicates
   country(land, population, leader)
   print_countries

clauses
   country(england, 21, thatcher).
   country(france, 13, mitterand ).
   country(germany, 43, schultz).
   country(denmark, 49, jensen).
   
   print_countries :- write("These are the countries"), nl,
                      write("their population"), nl,
                      write("and their leaders:"), nl,
                      fail.
                           
   print_countries :- country(X, Y, Z),
                     write(X, " ", Y, " ", Z),
                     nl,                                  /* start a new line */
                     fail.

   print_countries.