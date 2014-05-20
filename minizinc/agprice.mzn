% 
% Agricultural pricing in MiniZinc.
% 
% From H. Paul Williams "Model Building in Mathematical Programming", 4th edition
% Agricultural Pricing example, sections 12.21, 13.21, 14.21
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: n_point = 35;
set of int: point = 1..n_point;
array[point] of float: grid = [0.06,0.1,0.15,0.2,0.25,0.3,0.3125,0.325,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.6625,0.66875,0.7,0.75,0.8,0.85,0.9,0.95,1.0,1.025,1.05,1.1,1.15,1.2,1.25,1.3,1.35,1.4,1.45,1.5];

  	 
var float: milk;
var float: milksq;
var float: butt;
var float: buttsq;
var float: cha;
var float: chasq;
var float: chb;
var float: chbsq;
var float: xm;
var float: xb;
var float: xca;
var float: xcb;
var float: q; % free variable
var float: qsq;

array[point] of var float: lmilk;
array[point] of var float: lbutt;
array[point] of var float: lcha;
array[point] of var float: lchb;
array[point] of var float: lq;
array[point] of var float: mq;

var float: revenue = -6492.0*milksq-1200.0*buttsq-194.0*chasq-8.0*chbsq-qsq +6748.0*milk+1185.0*butt+420.0*cha+70.0*chb;

solve maximize revenue;
% solve :: float_search(lmilk++lbutt++lcha++lchb++lq++mq, 0.001, input_order, indomain_split, complete)  maximize revenue;

constraint
   milk >= 0.0 /\
   milksq >= 0.0 /\
   butt >= 0.0 /\
   buttsq >= 0.0 /\
   cha >= 0.0 /\
   chasq >= 0.0 /\
   chb >= 0.0 /\
   chbsq >= 0.0 /\
   xm >= 0.0 /\
   xb >= 0.0 /\
   xca >= 0.0 /\
   xcb >= 0.0 /\
   % q >= 0.0 /\
   qsq >= 0.0 /\

   forall(i in point) (lmilk[i] >= 0.0)  /\
   forall(i in point) (lbutt[i] >= 0.0)  /\
   forall(i in point) (lcha[i] >= 0.0)  /\
   forall(i in point) (lchb[i] >= 0.0)  /\
   forall(i in point) (lq[i] >= 0.0)  /\
   forall(i in point) (mq[i] >= 0.0)  /\
   
   (1.0/4.82)*xm+(0.4/0.297)*milk = 1.4 /\
   (1.0/0.32)*xb+(2.7/0.720)*butt = 3.7 /\
   (1.0/0.21)*xca+(1.1/1.05)*cha -(0.1/0.815)*chb = 2.0 /\
   (1.0/0.07)*xcb+(0.4/0.815)*chb -(0.4/1.05)*cha = 1.0 /\
   0.04*xm+0.8*xb+0.35*xca+0.25*xcb<=0.600 /\
   0.09*xm+0.02*xb+0.3*xca+0.4*xcb<=0.750 /\
   4.82*milk+0.32*butt+0.21*cha+0.07*chb <= 1.939 /\
   cha-chb-0.195*q = 0.0 /\
   milk - sum(i in point) (grid[i]*lmilk[i])=0.0 /\
   butt - sum(i in point) (grid[i]*lbutt[i])=0.0 /\
   cha - sum(i in point) (grid[i]*lcha[i])=0.0 /\
   chb - sum(i in point) (grid[i]*lchb[i])=0.0 /\
   q + sum(i in point) (grid[i]*mq[i])- sum(i in point) (grid[i]*lq[i])=0.0 /\
   milksq-sum (i in point) (grid[i]*grid[i] * lmilk[i])=0.0 /\
   buttsq-sum (i in point) (grid[i]*grid[i] * lbutt[i])=0.0 /\
   chasq-sum (i in point) (grid[i]*grid[i] * lcha[i])=0.0 /\
   chbsq-sum (i in point) (grid[i]*grid[i] * lchb[i])=0.0 /\
   qsq-sum(i in point) (grid[i]*grid[i] * mq[i])-sum (i in point) (grid[i]*grid[i] * lq[i])=0.0 /\
   sum (i in point) (lmilk[i])<=1.0 /\
   sum (i in point) (lbutt[i])<=1.0 /\
   sum (i in point) (lcha[i])<=1.0 /\
   sum (i in point) (lchb[i])<=1.0 /\
   sum (i in point) (mq[i]+lq[i])<=1.0
;


output [
   "revenue: ", show(revenue), "\n",
   "milk price: ", show(milk*1000.0), "\n",
   "butter price: ", show(butt*1000.0), "\n",
   "cheese 1 price: ", show(cha*1000.0), "\n",
   "cheese 2 price: ", show(chb*1000.0), "\n",

   "milk prod: ", show(xm*1000.0), "\n",
   "butter prod: ", show(xb*1000.0), "\n",
   "cheese 1 prod: ", show(xca*1000.0), "\n",
   "cheese 2 prod: ", show(xcb*1000.0), "\n",

];
