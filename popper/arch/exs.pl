% Arch problem

% From Progol 4.1 examples/arch.pl  
% 
% """
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Patrick Winston's arch problem. Learns a definition for an arch
%	in terms of support and touching relationships.
%
%							   / \
%	 ______				 _______	  /   \
%	|  11  |			|   31  |	 /  41 \
%	 ~~~~~~				 ~~~~~~~ 	 ~~~~~~~
%	|~|  |~|	|~|   |~|	  | | |		|~|   |~|
%	12|  |13	22|   |23	  32|33		42|   |43
%	| |  | |	|_|___|_|	  | | |		| |   | |
%	| |  | |	|   21  |	  | | |		| |   | |
%	 ~    ~          ~~~~~~~	   ~ ~		 ~     ~
%
%							   / \
%	 ______		    _______	_______  	  /   \
%	|  51  |	   |   61  |   |   71  |	 /  81 \
%	 ~~~~~~		    ~~~~~~~     ~~~~~~~  	 ~~~~~~~
%	|~|  |~|	|~|   |~|	  |~|   |~|	|~|   |~|
%	52|  |53	62|   |63	  72|   |73	82|   |83
%	| |  | |	| |   | |	  | |   | |	| |   | |
%	| |  | |	| |   | |	  | |   | |	| |   | |
%	 ~    ~          ~     ~	   ~     ~	 ~     ~
% """
% 
pos(arch(b11,b12,b13)).
pos(arch(b41,b42,b43)).
pos(arch(b51,b52,b53)).
pos(arch(b81,b82,b83)).

neg(arch(b22,b23,b24)).
neg(arch(b31,b32,b33)).
neg(arch(b61,b62,b63)).
neg(arch(b71,b72,b73)).
