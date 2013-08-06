% 
% Global constraint precedence in MiniZinc.
% 
% From Toby Walsh Sliding Constraints (presentation)
% www.cse.unsw.edu.au/~tw/samos/slide.ppt
% page 43ff
% """
%   precedence([X1,...Xn]) iff
%     min({i | Xi=j or i=n+1}) <
%     min({i | Xi=k or i=n+2}) for all j < k
% In other words
%   * The first time we use j is before the first time we use k
% E.g. 
%   precedence([1,1,2,1,3,2,4,2,3])
% but not
%    precedence([1,1,2,1,4])
% 
% """
%
% Note1: Cf int_value_precede.mzn.
% Note2: there is now a standard definition of precendence in 
% the MiniZinc distribution (which is much faster than this).

% 
% This model also contain a variant: precedence_all with
% the added constraint that x must contain all values 
% in lb_array(x)..ub_array(x).
%


% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/

include "globals.mzn"; 

int: n = 9;
array[1..n] of var 1..4: x;


%
% does a contains e?
%
predicate contains(var int: e, array[int] of var int: a) =
   exists(i in index_set(a)) (
      a[i] = e
   )
;

%
% precedence
% 
% Note: This implementation is different from Walsh's suggestion.
% We can't assume that all values in lb_array(x)..ub_array(x) 
% is in x, though if a value (i) exist, then all 1..i-1 must
% be in x.
predicate precedence_me(array[int] of var int: x) =
   let {
      int: lba = lb_array(x),
      int: uba = ub_array(x),
      int: len = length(x)
   }
   in
   % must contain 1 (lb)
   contains(lba, x)
   /\
  forall(i in lba..uba) (
    contains(i, x) ->
    (
       exists(j in lba..len) (
         x[j] = i
         /\ 
         forall(k in lba..j-1) (x[k] <= i)
      )
    )
    /\
    forall(k in lba..i-1) (contains(k, x))
  )
;


% As precedence with the added constraint that all
% values in lb_array(x)..ub_array(x) must be in x.
% Which is the same as saying that is must conform
% to precedence/1 and that the ub_array(x) must be
% in x.
predicate precedence_all(array[int] of var int: x) =
  precedence_me(x)
  /\
  contains(ub_array(x), x)
;


%
% A variant of precedence_all using my precedence_me/1. 
% Slower than precedence_all.
%
predicate precedence_all2(array[int] of var int: x) =
  precedence_me(x)
  /\
  contains(ub_array(x), x)
;


predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 


solve satisfy;

constraint

  % cp1d(x,[1,1,2,1,3,2,4,2,3])
  % cp1d(x, [1,1,2,1,4,2,3,4,2])
  % /\

  % /\
  % precedence(x) % the built-in predicate
  precedence_me(x) % my own implementation of precedence

  % precedence_all(x) % x must contain all values in its domain
  % precedence_all2(x) % x must contain all values in its domain (slower)



;

output [show(x)];