% 
% Arbitrage loops in MiniZinc.
% 
% This program shows how to detecting arbitrage loops (and don't care about 
% broker's fees etc).
%
% The problem was inspired inspired by Dan Goldin's Prolog code at
% http://dangoldin.com/2013/06/07/fun-with-prolog-priceonomics-puzzle/
% Also see the Picat version (http://hakank.org/picat/arbitrage_loops.pi.%
%
%
% Problem description and some examples from
% http://priceonomics.com/jobs/puzzle/
% """
% Daily trading volume in currency exchange markets often exceeds $1 
% trillion. With the advent of new crypto-currencies, your knowledge of 
% algorithms, and a good pair of sound-canceling headphones, you're 
% convinced that there could be some profitable arbitrage opportunities to 
% exploit.

% Sometimes, these currency pairs drift in a way that creates arbitrage 
% loops where you can convert through a certain sequence of currencies to 
% return a profit in your base currency. This is referred to as an arbitrage 
% loop. For example, you could do the following trades with $100 US and the 
% exchange data below:
%
%
%       USD 	EUR 	JPY 	  BTC
% USD 	- 	0.7779 	102.4590  0.0083
% EUR 	1.2851 	- 	131.7110  0.01125
% JPY 	0.0098 	0.0075 	- 	  0.0000811
% BTC 	115.65 	88.8499 12325.44  -
%
% ...
% 
% Trade $100 to €77.79
% Trade €77.79 to .8751375 BTC
% Trade .8751375 BTC for $101.20965.
% """

%
% Notes: 
% - MiniZinc don't support HTTP calls, so I just went to 
%     http://fx.priceonomics.com/v1/rates/
%   and extracted the entries.
% - This program don't care at all about such reality stuff like
%   broker's fee etc.
% - This program is for fun and not for profit. :-)
% - This model has a fix length (parameter "m") of the number of currencies to 
%   involve in the arbitrage loop. m = n (the number of currencies) may be
%   the overall optimal chain, but not necessary.
% - The solver must support non linear float operations. As far as I know,
%   the only solvers supporting this is:
%     * Gecode
%     * JaCoP
%     * ECLiPSe/ic
%
% Also, see
% - https://en.wikipedia.org/wiki/Triangular_arbitrage
% - "Two-Currency, Three-Currency and Multi-Currency Arbitrage"
%   http://www.fem.uniag.sk/mefi/pdf/arbitraz.pdf
%
% A warning:
% From Triangular Arbitrage
% http://www.nusinvest.com/wp-content/uploads/2013/01/Triangular-Arbitrage.pdf
% """
% As a matter of fact, triangular arbitrage opportunities do actually exist in 
% the forex trading market. However, it is important to note that these opportunities 
% are very rare and often exist only for a few seconds. Why? One has to realize 
% that these arbitrage opportunities will not last forever. Once people start to 
% engage in these profit taking activities, the market will correct itself and bring 
% the foreign exchange rate to the equivalent level. Furthermore, with the presence 
% of several high-frequency-trading (HFT) firms today, which uses advance and 
% complicated computer programs to execute trades automatically, the time for the market to
% correct itself is made a lot faster as compared to a century ago. These complex computer
% soft wares are programmed to specifically sift out such arbitraging opportunities and will
% profit from these imbalance at the very split second that these opportunities present
% themselves.
% """

% Some data files:
%   http://hakank.org/minizinc/arbitrage_loops1.dzn
%   http://hakank.org/minizinc/arbitrage_loops2.dzn
%   http://hakank.org/minizinc/arbitrage_loops3.dzn
%   http://hakank.org/minizinc/arbitrage_loops4.dzn
%   http://hakank.org/minizinc/arbitrage_loops5.dzn

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n; % total number of currencies
int: m; % number of involved currencies
array[1..n,1..n] of float: currencies;
array[1..n] of string: currencies_s;
float: start;

% decision variables
array[1..m+1] of var 0..n: y; % currency to use
array[1..m+1] of var 0.0..max(array1d(currencies)): profit;


% var 0.0..1000000.0: z;
var float: z;

solve maximize z;
% solve :: float_search(y ++ profit,0.1, input_order, indomain_split, complete) maximize z;

constraint
  z >= 0.0 /\
  all_different([y[i] | i in 1..m]) /\
  y[1] = y[m+1] /\
  forall(i in 1..m) (
     profit[i] = currencies[y[i],y[i+1]]
  )
  /\
  z = product([profit[i] | i in 1..m])
;

output [
  "z: \(z)\n",
  "profit: \([profit[i] | i in 1..m])\n",
  "y: \(y)\n",
  "starts with \(start) \(currencies_s[fix(y[1])]) -> \(start*z) \(currencies_s[fix(y[1])])\n",
  "real profit: \(start*z) - \(start) = \(start*z - start) \(currencies_s[fix(y[1])])\n\n",
]
++
[
  "\(currencies_s[fix(y[i])]) -> \(currencies_s[fix(y[i+1])]) \(profit[i])\n"
  | i in 1..m
]
;


%% From the example above:
%%  Trade $100 to €77.79
%%  Trade €77.79 to .8751375 BTC
%%  Trade .8751375 BTC for $101.20965.
%%
%%  USD  EUR     JPY            BTC
% n = 4;
% m = 4;
% start = 1000.0;
% currencies = array2d(1..n,1..n,
% [
% 0,      0.7779,  102.4590,  0.0083,
% 1.2851, 0,       131.7110,  0.01125,
% 0.0098, 0.0075,  0,         0.0000811,
% 115.65, 88.8499, 12325.44,  0,
% ]);
% currencies_s = ["USD","EUR","JPY","BTC"];









