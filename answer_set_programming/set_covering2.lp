%
% Set covering in ASP.
%
% Example 9.1-2, page 354ff, from Taha "Operations Research - An Introduction"
% Minimize the number of security telephones in street corners on a campus.
%
% AMPL model: http://taha.ineg.uark.edu/setcover.txt
%
% This was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also http://www.hakank.org/answer_set_programming/
%
%

% Two answers are found with
%    clingo --opt-all set_covering2.lp
%  Corners: 1,2,5,7
%  Corners: 2,5,6,7

#const num_corners = 8.
#const num_streets = 11.

% corners of each streets
% street(Stree, Corner1, Corner2)
street(1,1,2).
street(2,2,3).
street(3,4,5).
street(4,7,8).
street(5,6,7).
street(6,2,6).
street(7,1,6).
street(8,4,7).
street(9,2,4).
street(10,5,8).
street(11,3,5).

street(Street,Corner1, Corner2) :- street(Street, Corner2, Corner1), 
                                   corners(Corner1), corners(Corner2).

% domains
corners(1..num_corners).
streets(1..num_streets).

% at least one gorner must be selected
% 1 { selected(Corner) : corners(Corner)}.

% all streets must be covered by at least one corner
1 [ selected(Corner1) : street(Street, Corner1, Corner2) : corners(Corner1) : corners(Corner2) : Corner1 != Corner2 ] :- streets(Street).  

% minimize the number of corners
#minimize [ selected(Corner) : corners(Corner) ].

#hide.
#show selected(Corner).