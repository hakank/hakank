/*
   Mon Dec  1 18:18:21 2008/hakank@bonetmail.com

   From mailing list
Date: Mon, 11 Oct 2004 01:16:49 +0000 (UTC)
Subject: string library question (fwd)
From: bfulgham


*/
compile('/home/hakank/Poplib/init.p');



define fast_ack (m, n) -> result;
    if m == 0 then
       n + 1 -> result
    elseif n == 0 then
       fast_ack(m - 1, 1) -> result
    else
       fast_ack(m - 1, fast_ack(m, n - 1)) -> result
    endif
enddefine;

define get_args() -> result;
    if null(poparglist) then
       1 -> result
    else
       strnumber( hd(poparglist) ) -> result
    endif
enddefine;

vars n = 3;
;;;;get_args() -> n;

;;; newmemo(fast_ack,100)->fast_ack; ;;; gives weird results!

;;;format_print('Ack(3,~D): ~D\n', [^n ^(fast_ack(3, n))]);
[Ack(3, ^n) ^(fast_ack(3, n))]=>
