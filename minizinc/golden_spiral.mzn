% 
% Golden spiral in MiniZinc.
% 
% From Gecode's golden-spiral.cpp
% """
% \brief %Example: Golden spiral
% 
% The Golden Spiral is a logarithmic spiral whose growth factor
% is the golden ratio \f$\phi=1,618\f$.
% 
% It is defined by the polar equation:
% \f[
% r = ae^{b\theta}
% \f]
% where
% \f[
% \operatorname{abs}(b) = \frac{\operatorname{ln}(\phi)}{\frac{\pi}{2}}
% \f]
% 
% To get cartesian coordinates, it can be solved for \f$x\f$
% and \f$y\f$ in terms of \f$r\f$ and \f$\theta\f$.
% By setting \f$a=1\f$, it yields to the equation:
% 
% \f[
% r = e^{0.30649\times\theta}
% \f]
% with
% \f[
% x=r\operatorname{cos}(\theta), \quad y=r\operatorname{sin}(\theta)
% \f]
% 
% The tuple \f$(r,\theta)\f$ is related to the position for
% \f$x\f$ and \f$y\f$ on the curve.  \f$r\f$ and \f$\theta\f$
% are positive numbers.  
% 
% To get reasonable interval starting sizes, \f$x\f$ and \f$y\f$
% are restricted to \f$[-20;20]\f$.
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 

var -20.0..20.0: theta;
var -20.0..20.0: r;
var -20.0..20.0: x;
var -20.0..20.0: y;

% array[1..4] of var -20.0..20.0: f;


% solve satisfy;
solve :: float_search([x,y,theta,r], 0.01, input_order, indomain_split, complete) satisfy;

constraint
    theta >= 0.0 /\
    r >= 0.0 /\
    r*cos(theta) = x /\
    r*sin(theta) = y /\
    exp(0.30649*theta) = r

;

output [
  % "theta: " ++ show(theta) ++ "\n" ++ 
  % "r: " ++ show(r) ++ "\n" ++ 
  % "x: " ++ show(x) ++ "\n" ++ 
  % "y: " ++ show(y) ++ "\n"
  "xy: ", show([x,y])
  
];

