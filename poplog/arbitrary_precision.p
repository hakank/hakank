;;; http://rosettacode.org/wiki/Arbitrary-precision_integers_%28included%29
;;; 
;;; Using the in-built capabilities of your language, calculate the integer value of:
;;;
;;;         5^{4^{3^2}} 
;;;
;;;  Confirm that the first and last twenty digits of the answer are: 
;;;      62060698786608744707...92256259918212890625
;;;  Find and show the number of decimal digits in the answer. 


vars x = (5**(4**(3**2))).unpackitem;
vars len = x.length;
vars i;
[% for i from 1 to 20 do x(i) endfor; %].packitem;
'...';
[% for i from len-19 to len do x(i) endfor; %].packitem;
' (',len, ' digits)';
=>
