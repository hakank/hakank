%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Background knowledge
% path(G,N1,N2) - node N1 is path to node N2 in graph G iff
%	there is a non-empty path formed from arcs from N1 to N2.

% We try to invent path/3
% path(G,N1,N2) :- arc(G,N1,N2).
% path(G,N1,N2) :- arc(G,N1,N3), path(G,N3,N2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Type information

graph(g1). graph(g2). graph(g3). graph(g4). graph(g5).
node(a1).  node(b1).  node(c1).  node(d1).  node(e1).  node(f1).
node(a2).  node(b2).  node(c2).  node(d2).  node(e2).  node(f2).
node(a3).  node(b3).  node(c3).  node(d3).  node(e3).  node(f3).
node(a4).  node(b4).  node(c4).  node(d4).  node(e4).  node(f4).
node(a5).  node(b5).  node(c5).  node(d5).  node(e5).  node(f5).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Graph 1
%
%	   -> b
%	  /   |
%	 /    v
%	a <-- c	    -> e
%	 \         /
%	  ------> d
%		   \
%		    -> f

arc(g1,a1,b1).
arc(g1,a1,d1).
arc(g1,b1,c1).
arc(g1,c1,a1).
arc(g1,d1,e1).
arc(g1,d1,f1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Graph 2
%
%	   -> b
%	  /  /|
%	 /  / v
%	a <-  c	-----> e
%	 \         /
%	  ------> d
%		   \
%		    -> f

arc(g2,a2,b2).
arc(g2,a2,d2).
arc(g2,b2,a2).
arc(g2,b2,c2).
arc(g2,c2,e2).
arc(g2,d2,e2).
arc(g2,d2,f2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Graph 3
%
%	   -> b
%	  /  /|
%	 /  / v
%	a <-- c	-----> e
%	 \         /
%	  ------> d
%		   \
%		    -> f

arc(g3,a3,b3).
arc(g3,a3,d3).
arc(g3,b3,a3).
arc(g3,b3,c3).
arc(g3,c3,a3).
arc(g3,c3,e3).
arc(g3,d3,e3).
arc(g3,d3,f3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Graph 4
%
%	   -> b
%	  /   |
%	 /    v
%	a     c	    -> e
%	 \         /
%	  ------> d
%		   \
%		    -> f

arc(g4,a4,b4).
arc(g4,a4,d4).
arc(g4,b4,c4).
arc(g4,a4,b4).
arc(g4,d4,e4).
arc(g4,d4,f4).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Graph 5
%
%	   -> b
%	  /   |
%	 /    v
%	a     c	-----> e
%	 \         /
%	  ------> d
%		   \
%		    -> f

arc(g5,a5,b5).
arc(g5,a5,d5).
arc(g5,b5,c5).
arc(g5,c5,e5).
arc(g5,d5,e5).
arc(g5,d5,f5).
