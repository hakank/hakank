x:+.'0:"1.txt"            / read two columns
a:x@0;b:x@1;              / assign the columns to a and b
+/{x|-x}'(a[<a])-(b[<b])  / part 1
+/a*+/'{x=b}'a            / part 2
