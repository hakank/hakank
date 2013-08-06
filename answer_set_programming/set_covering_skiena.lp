%
% Set covering problem in ASP.
%
% Example from Steven Skiena, The Stony Brook Algorithm Repository
% http://www.cs.sunysb.edu/~algorith/files/set-cover.shtml
% """
% Input Description: A set of subsets S_1, ..., S_m of the 
% universal set U = {1,...,n}.
% 
% Problem: What is the smallest subset of subsets T subset S such that \cup_{t_i in T} t_i = U?
% """
% Data is from the pictures INPUT/OUTPUT.

% Solution: Sets 3,6,7. Total elements choosen: 15
% Another solution with three sets is {4,6,7}, but the total choosen elements is 17.

%
% This was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also http://www.hakank.org/answer_set_programming/
%

#const num_sets = 7.
#const num_elements = 12.

% We can change this to a set partition problem by setting
% set_covering to 0, e.g.
%  clingo -c set_covering=0 set_covering_skiena.lp
%
#const set_covering=1.

% 1 2 3 4 5 6 7 8 9 0 1 2  elements
%
% 1,1,0,0,0,0,0,0,0,0,0,0, % set 1
% 0,1,0,0,0,0,0,1,0,0,0,0, % set 2
% 0,0,0,0,1,1,0,0,0,0,0,0, % set 3
% 0,0,0,0,0,1,1,0,0,1,1,0, % set 4
% 0,0,0,0,0,0,0,0,1,1,0,0, % set 5
% 1,1,1,0,1,0,0,0,1,1,1,0, % set 6
% 0,0,1,1,0,0,1,1,0,0,1,1, % set 7
%
% belongs(Set, Element)
belongs(1, 1).
belongs(1, 2).
belongs(2, 2).
belongs(2, 8).
belongs(3, 5).
belongs(3, 6).
belongs(4, 6).
belongs(4, 7).
belongs(4, 10).
belongs(4, 11).
belongs(5, 9).
belongs(5, 10).
belongs(6, 1).
belongs(6, 2).
belongs(6, 3).
belongs(6, 5).
belongs(6, 9).
belongs(6, 10).
belongs(6, 11).
belongs(7, 3).
belongs(7, 4).
belongs(7, 7).
belongs(7, 8).
belongs(7, 11).
belongs(7, 12).

%
% domains
%
sets(1..num_sets).
elements(1..num_elements).

%
% set covering: all elements must be selected by _at least_ one sets
%
1 { selected(Set) : sets(Set) : belongs(Set, Element) } num_sets :- elements(Element), set_covering == 1.

% set partition (set_covering = 0)
1 { selected(Set) : sets(Set) : belongs(Set, Element) } 1 :- elements(Element), set_covering != 1.

:- #sum [selected(Set) : sets(Set) ] 0.

% experimental: get the sum of the number of elements (the overlaps)
% { selected_element(Element) : belongs(Set, Element) } :- elements(Element).

%
% minimize the number of selected sets
%
#minimize [ selected(Set) : sets(Set) ].

% experimental: also try to minimize the total number of sets (the overlaps)
% #minimize [ selected_element(Element) : sets(Set) : belongs(Set, Element)].



#hide.
#show selected(Set).
% #show selected_element(Element).