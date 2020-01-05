/*

  Euler problem 39
  """
  If p is the perimeter of a right angle triangle with integral length sides, 
  {a,b,c}, there are exactly three solutions for p = 120.
  
  {20,48,52}, {24,45,51}, {30,40,50}
  
  For which value of p <= 1000, is the number of solutions maximised?
  """ 

  This Pop-11 program was created by Hakan Kjellerstrand (hakank@gmail.com).
  See also my Pop-11 / Poplog page: http://www.hakank.org/poplog/

*/

compile('/home/hakank/Poplib/init.p');

;;;
;;; 3.71s
;;;
define problem39;

    lvars m = 1000,
         best_p = 0,
         best_cc = 0;

    lvars p a b;
    fast_for p from 1 to m do 
        lvars cc = 0;
        fast_for a from 1 to p div 2 do
            fast_for b from a to (p-a) div 2 do
                ;;; if a+b > p then
                ;;;     nextloop;
                ;;; endif;
                lvars c = sqrt(a**2+b**2);
                if a + b + c = p then
                    cc+1->cc;
                else 
                    nextloop;
                endif;

                if best_cc < cc then 
                    cc->best_cc;
                    p->best_p;
                    ;;; [p ^p cc ^cc]=>
                endif;

            endfast_for;
        endfast_for;
    endfast_for;

    best_p=>

enddefine;

;;;
;;; 0.05s
;;;
define problem39b;
    lvars n=1000-1;
    lvars squares = newmapping([], 100, 0, true);
    lvars i,squares_list;
    for i from 1 to n do
        1->squares(i*i);
    endfor;
    [%explode(squares)%] -> squares_list;
    lvars x, y, x1,y1,c, valid, counts;
    [% fast_for x in squares_list do
           x(1)->x1;
           fast_for y in squares_list do
               y(1)->y1;
               if x1 < y1 and squares(x1+y1) > 0 then
                   round(sqrt(x1)+sqrt(y1) + sqrt(x1+y1))->c;
                   if c < 1000 then
                       c;
                   endif;
               endif;
           endfast_for;
       endfast_for %] -> valid;
    
    lvars counts = newmapping([], 100, 0, true);
    for c in valid do
        1+counts(c)->counts(c);
    endfor;
    lvars v, maxv = 0, maxc = 0, counts_list=[%explode(counts)%];
    fast_for v in counts_list  do
        if v(2) > maxc then
            v(2)->maxc;
            v(1)->maxv;
        endif;
    endfast_for;
    maxv=>;
enddefine;


;;; 'problem39()'=>
;;; problem39();
;;; timediff()=>;

'problem39b()'=>
problem39b();
timediff()=>;



