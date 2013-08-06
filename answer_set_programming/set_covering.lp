%
% Simple set covering problem in ASP.
% 
% Placing of firestations, from Winston "Operations Research", page 486
%
%
% This was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also http://www.hakank.org/answer_set_programming/
%

% The answer should be that firestation 2 and 4 should
% have a firestation

#const num_cities = 6.
#const min_distance = 15.

cities(1..num_cities).

% distances between the cities
distance(1, 2, 10).
distance(1, 3, 20).
distance(1, 4, 30).
distance(1, 5, 30).
distance(1, 6, 20).
distance(2, 1, 10).
distance(2, 3, 25).
distance(2, 4, 35).
distance(2, 5, 20).
distance(2, 6, 10).
distance(3, 1, 20).
distance(3, 2, 25).
distance(3, 4, 15).
distance(3, 5, 30).
distance(3, 6, 20).
distance(4, 1, 30).
distance(4, 2, 35).
distance(4, 3, 15).
distance(4, 5, 15).
distance(4, 6, 25).
distance(5, 1, 30).
distance(5, 2, 20).
distance(5, 3, 30).
distance(5, 4, 15).
distance(5, 6, 14).
distance(6, 1, 20).
distance(6, 2, 10).
distance(6, 3, 20).
distance(6, 4, 25).
distance(6, 5, 14).

% Add distance to self.
distance(City,City,0) :- cities(City).

%
% x(City): city City has a fire station
% 

% There should be at least one firestation.
1 { x(City) : cities(City) } .

% for all cities: ensure that there is at least one city with
% a firestation within minimum distance.
% x_sum(City1, Sum) :- Sum = [  x(City2) : cities(City2) : distance(City1, City2, Distance) : Distance <= min_distance ], cities(City1).  
% :- x_sum(City1, Sum), cities(City1), Sum = 0.

% Simpler:
:- [ x(City2) : cities(City2) : distance(City1, City2, Distance) : Distance <= min_distance ] 0, cities(City1).  

% minimize the number of cities with fire stations
#minimize [ x(City) : cities(City) = 1].

#hide.
#show x(City).