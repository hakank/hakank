%
% Magic squares water retention in MiniZinc
%
% This model is in part inspired by the Numberjack model MagicWater.py


% http://www.knechtmagicsquare.paulscomputing.com/topographical.html
% """
% a) there are 880 different order 4 magic squares
% b) 137 of the 880 squares retain no water according to the topographical model
% """


% Also see:
%    http://www.knechtmagicsquare.paulscomputing.com/
%    http://www.knechtmagicsquare.paulscomputing.com/topographical.html
%    http://en.wikipedia.org/wiki/Associative_magic_square
%    http://en.wikipedia.org/wiki/Water_retention_on_mathematical_surfaces
%    http://www.azspcs.net/Contest/MagicWater
% 
%    Johan Öfverstedt's CBLS solver (C++ and Comet): http://sourceforge.net/projects/wrmssolver/
%    

% 
% Some results:
% 
% Using associative and maximizing water retension, Gecode/fz
% finds the optimal value (59) for n=5 in 16.9s (695803 failures)
% 
% Without the associative requirement the optimal value is higher, 
% at least >= 69 (found by Gecode/fz using most_constraint/indomain_random 
% in about one minute, but proving optimality - if it's 69 - takes much longer).
%
% magic: [3, 12, 22, 17, 11, 16, 19, 8, 4, 18, 25, 2, 13, 5, 20, 7, 23, 1, 24, 10, 14, 9, 21, 15, 6]
% water: [3, 12, 22, 17, 11, 16, 19, 17, 17, 18, 25, 17, 17, 17, 20, 7, 23, 17, 24, 10, 14, 9, 21, 15, 6]
% Total: 65
% z:     69
% sum(water): 394
% (n*n*(n*n+1) div 2): 325
%
% magic:
%
%   3  12  22  17  11 
%  16  19   8   4  18 
%  25   2  13   5  20 
%   7  23   1  24  10 
%  14   9  21  15   6 
% water:
%
%   3  12  22  17  11 
%  16  19  17  17  18 
%  25  17  17  17  20 
%   7  23  17  24  10 
%  14   9  21  15   6 
% Retention:
%
% ___ ___ ___ ___ ___ 
% ___ ___   9  13 ___ 
% ___  15   4  12 ___ 
% ___ ___  16 ___ ___ 
% ___ ___ ___ ___ ___ 
% ----------


% Note: The following results are for 
% _Associative_ Magic Square Water Retention
% (AMSWR problem), which has (much) smaller 
% value for the water retention than without 
% the associative requirements (MSWR problem).
%
% Here is a summary of times for the size, after that some 
% results for larger sizes. The times is for Gecode/fz.
% 
% Note: For some N, there is no associative magic squares. These
% impossible size are 6, 10, 14, 18 (and 22 and all even values after?)
% See http://www.hakank.org/minizinc/magic_square_associate.mzn 
% for more about this.
% 
%       1x1: -
%       2x2: -
%       3x3: 0.105s (z: 0, proven optimal)
%       4x4: 0.127s (z: 15, proven optimal)
%       5x5: 20s (z: 59, proven optimal) 
%       6x6: - (there is no associative magic square for n=6)
%       7x7: (found z=315, not proved optimal)
%       8x8: 
%       9x9: (z=685, not proved optimal)
%     10x10: -
%     11x11: (1217, not proved optimal, 11h)
%     12x12: 
%     13x13: 50:48min (z=2197, proven optimal )
%     14x14: 
%     15x15: ?
%     16x16: 
%     17x17: (z=2443, not proven optimal)
%     18x18: 
%     19x19: 29.2s (z=6859, proven optimal
%     20x20: 
%     21x21: 
%     22x22: 
%     23x23: 
%     24x24: 
%     25x25: 
%     26x26: 
%     27x27: 
%     28x28: 
%     29x29: 
%     30x30:
%     31x31: 
%     32x32: 
%     33x33: 

%
% Some more detailed results:
%

% 7x7 (AMSWR) This solution (z=315) is _not_ proved 
%             to be optimal. Gecode/fz took about 7h to get
%             this solution.
%
% magic total: 175
% assoc: 50
% z:     315
% sum(water): 1540
% (n*n*(n*n+1) div 2): 1225
%
% magic:
%   4  23  37  44  20  15  32 
%  31  39   7   2  45  34  17 
%  41  10  26  29   8  47  14 
%  12  49  28  25  22   1  38 
%  36   3  42  21  24  40   9 
%  33  16   5  48  43  11  19 
%  18  35  30   6  13  27  46 
%
% water:
%   4  23  37  44  20  15  32 
%  31  39  37  37  45  34  17 
%  41  37  37  37  37  47  14 
%  12  49  37  37  37  37  38 
%  36  30  42  37  37  40   9 
%  33  30  30  48  43  19  19 
%  18  35  30   6  13  27  46 
%
% retention:
% ___ ___ ___ ___ ___ ___ ___ 
% ___ ___  30  35 ___ ___ ___ 
% ___  27  11   8  29 ___ ___ 
% ___ ___   9  12  15  36 ___ 
% ___  27 ___  16  13 ___ ___ 
% ___  14  25 ___ ___   8 ___ 
% ___ ___ ___ ___ ___ ___ ___ 





% 9x9: (pending: Gecode/fz z=685 after about 8h:16min)
% Note: This is _not_ proved optimal!
%
% magic total: 369
% assoc: 82
% z:     685
% sum(water): 4006
% (n*n*(n*n+1) div 2): 3321
%
% magic:
%   7  38  30  54  17  25  71  77  50 
%  51  63  14   9  36  62  34  55  45 
%  47  24  42  61   3  78  74   1  39 
%  29  76  67  26  70  16   2  60  23 
%  64  33  69  72  41  10  13  49  18 
%  59  22  80  66  12  56  15   6  53 
%  43  81   8   4  79  21  40  58  35 
%  37  27  48  20  46  73  68  19  31 
%  32   5  11  57  65  28  52  44  75 
%
% water:
%   7  38  30  54  17  25  71  77  50 
%  51  63  30  30  36  62  55  55  45 
%  47  42  42  61  36  78  74  39  39 
%  29  76  67  61  70  49  49  60  23 
%  64  59  69  72  49  49  49  49  18 
%  59  59  80  66  49  56  49  49  53 
%  43  81  48  48  79  49  49  58  35 
%  37  27  48  48  48  73  68  31  31 
%  32   5  11  57  65  28  52  44  75 
%
% retention:
% ___ ___ ___ ___ ___ ___ ___ ___ ___ 
% ___ ___  16  21 ___ ___  21 ___ ___ 
% ___  18 ___ ___  33 ___ ___  38 ___ 
% ___ ___ ___  35 ___  33  47 ___ ___ 
% ___  26 ___ ___   8  39  36 ___ ___ 
% ___  37 ___ ___  37 ___  34  43 ___ 
% ___ ___  40  44 ___  28   9 ___ ___ 
% ___ ___ ___  28   2 ___ ___  12 ___ 
% ___ ___ ___ ___ ___ ___ ___ ___ ___ 
% ----------


% 11x11
% For 11x11 (AMSWR) the following is not proved
% optimal value after running over 11h with Gecode/fz
% 
% magic total: 671
% assoc: 122
% z:     1217
% sum(water): 8598
% (n*n*(n*n+1) div 2): 7381
%
% magic:
%  11  57  27  29  36  21  92  99 116 109  74 
%  75  94   5  15  18 121  55 100  35  81  72 
%  77 120  62  51 114  56   4  54 112   7  14 
%  63  31  88  42  84 106  44  25  26 113  49 
%  64  53  98 119  46  39  79  20  12  89  52 
%  32 105  82 103  37  61  85  19  40  17  90 
%  70  33 110 102  43  83  76   3  24  69  58 
%  73   9  96  97  78  16  38  80  34  91  59 
% 108 115  10  68 118  66   8  71  60   2  45 
%  50  41  87  22  67   1 104 107 117  28  47 
%  48  13   6  23  30 101  86  93  95  65 111 
%
% water:
%  11  57  27  29  36  21  92  99 116 109  74 
%  75  94  27  27  27 121  60 100  81  81  72 
%  77 120  62  51 114  60  60  60 112  14  14 
%  63  63  88  51  84 106  60  60  60 113  49 
%  64  63  98 119  78  78  79  60  60  89  52 
%  32 105  98 103  78  78  85  60  60  60  90 
%  70  70 110 102  78  83  76  60  60  69  58 
%  73  70  96  97  78  67  67  80  60  91  59 
% 108 115  68  68 118  67  67  71  60  45  45 
%  50  41  87  23  67  67 104 107 117  45  47 
%  48  13   6  23  30 101  86  93  95  65 111 
%
% retention:
% ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ 
% ___ ___  22  12   9 ___   5 ___  46 ___ ___ 
% ___ ___ ___ ___ ___   4  56   6 ___   7 ___ 
% ___  32 ___   9 ___ ___  16  35  34 ___ ___ 
% ___  10 ___ ___  32  39 ___  40  48 ___ ___ 
% ___ ___  16 ___  41  17 ___  41  20  43 ___ 
% ___  37 ___ ___  35 ___ ___  57  36 ___ ___ 
% ___  61 ___ ___ ___  51  29 ___  26 ___ ___ 
% ___ ___  58 ___ ___   1  59 ___ ___  43 ___ 
% ___ ___ ___   1 ___  66 ___ ___ ___  17 ___ 
% ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ 



% For 13x13 (AMSWR) the optimal value found is 2197, 
% found by Gecode/fz in 50:48 minutes (68206330 failures):
% (using most_constraint/indomain_random and -f)
% z:     2197
% 
% magic:
%  15  80  52  30  94  63 119   8  50 157 169 165 103 
% 104 130  75 143 121  53 110 168  29  32   3 113  24 
% 115 159  87 132 163  22  28   9  23 125 154  68  20 
% 158  98  37  59  93  79  88  46 139  36  26 149  97 
%  86 136  81 100  64  56  99  17 109 126  35  69 127 
%  96  33  47 145  18  39 156  41 160  54  58  92 166 
%  48 105 151   6  42  62  85 108 128 164  19  65 122 
%   4  78 112 116  10 129  14 131 152  25 123 137  74 
%  43 101 135  44  61 153  71 114 106  70  89  34  84 
%  73  21 144 134  31 124  82  91  77 111 133  72  12 
% 150 102  16  45 147 161 142 148   7  38  83  11  55 
% 146  57 167 138 141   2  60 117  49  27  95  40  66 
%  67   5   1  13 120 162  51 107  76 140 118  90 155 
%
% water:
%  15  80  52  30  94  63 119   8  50 157 169 165 103 
% 104 130  75 143 121  53 110 168  50  50  50 113  24 
% 115 159  87 132 163  50  50  50  50 125 154  68  20 
% 158  98  87  87  93  79  88  50 139 122 122 149  97 
%  86 136  87 100  79  79  99  50 109 126 122 122 127 
%  96  87  87 145  79  79 156  50 160 122 122 122 166 
%  48 105 151  79  79  79  85 108 128 164 122 122 122 
%   4  78 112 116  79 129  79 131 152  89 123 137  74 
%  43 101 135  79  79 153  79 114 106  89  89  72  84 
%  73  73 144 134  79 124  82  91  77 111 133  72  12 
% 150 102 102 102 147 161 142 148  76  76  83  55  55 
% 146  57 167 138 141  60  60 117  76  76  95  55  66 
%  67   5   1  13 120 162  51 107  76 140 118  90 155 
% 
% retention:
% ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ 
% ___ ___ ___ ___ ___ ___ ___ ___  21  18  47 ___ ___ 
% ___ ___ ___ ___ ___  28  22  41  27 ___ ___ ___ ___ 
% ___ ___  50  28 ___ ___ ___   4 ___  86  96 ___ ___ 
% ___ ___   6 ___  15  23 ___  33 ___ ___  87  53 ___ 
% ___  54  40 ___  61  40 ___   9 ___  68  64  30 ___ 
% ___ ___ ___  73  37  17 ___ ___ ___ ___ 103  57 ___ 
% ___ ___ ___ ___  69 ___  65 ___ ___  64 ___ ___ ___ 
% ___ ___ ___  35  18 ___   8 ___ ___  19 ___  38 ___ 
% ___  52 ___ ___  48 ___ ___ ___ ___ ___ ___ ___ ___ 
% ___ ___  86  57 ___ ___ ___ ___  69  38 ___  44 ___ 
% ___ ___ ___ ___ ___  58 ___ ___  27  49 ___  15 ___ 
% ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ 
%



% 
% 17x17: not proved optimal z=2443  after 9h with Gecode/fz
% magic total: 2465
% assoc: 290
% z:     2443
% sum(water): 44348
% (n*n*(n*n+1) div 2): 41905
%
% magic:
%  26 136 200  15  34  42 288 129 242 203  88 187 289 114 282  16 174 
% 175 221 201 273 224 280  37  47  11  39 218 159 267  31  84 191   7 
% 172  57 148 166 209  75  79 240 133 152  64  40 271 147 263 121 128 
% 170  94 134 102   5 287 167 146 193 231 252  29 185  62 168  30 210 
% 286  73  41 272 111 244 208  85 227 220 232  44 184  53 139  24  22 
% 241 278 258  98  25  68 236 101 165 110 112  71  52  77 182 214 177 
% 107  95 171  78 276  13  91 254 163 117  55 229  35 234  93 207 247 
%  60 135 149  65 284 137 100 160  74 109 198 262   9 257  86 186 194 
%  67  45 140  51 269  20 158 164 145 126 132 270  21 239 150 245 223 
%  96 104 204  33 281  28  92 181 216 130 190 153   6 225 141 155 230 
%  43  83 197  56 255  61 235 173 127  36 199 277  14 212 119 195 183 
% 113  76 108 213 238 219 178 180 125 189  54 222 265 192  32  12  49 
% 268 266 151 237 106 246  58  70  63 205  82  46 179  18 249 217   4 
%  80 260 122 228 105 261  38  59  97 144 123   3 285 188 156 196 120 
% 162 169  27 143  19 250 226 138 157  50 211 215  81 124 142 233 118 
% 283  99 206 259  23 131  72 251 279 243 253  10  66  17  89  69 115 
% 116 274   8 176   1 103 202  87  48 161   2 248 256 275  90 154 264 
%
% water:
%  26 136 200  15  34  42 288 129 242 203  88 187 289 114 282  16 174 
% 175 221 201 273 224 280 129 129 129 129 218 159 267  84  84 191   7 
% 172  94 148 166 209 129 129 240 133 152  64  40 271 147 263 121 128 
% 170  94 134 102 102 287 167 146 193 231 252  40 185  62 168  30 210 
% 286  73  73 272 111 244 208 146 227 220 232  44 184  62 139  24  22 
% 241 278 258  98  68  68 236 146 165 112 112  71  52  77 182 214 177 
% 107 107 171  78 276  68  91 254 163 117 112 229  35 234 150 207 247 
%  60 135 149  65 284 137 100 160 109 109 198 262  21 257 150 186 194 
%  67  67 140  51 269 137 158 164 145 126 132 270  21 239 150 245 223 
%  96 104 204  51 281 137 137 181 216 130 190 153  14 225 141 155 230 
%  43  83 197  56 255 137 235 173 127 127 199 277  14 212 119 195 183 
% 113  83 108 213 238 219 178 180 125 189  82 222 265 192  32  32  49 
% 268 266 151 237 106 246  58  70  70 205  82  74 179 179 249 217   4 
%  80 260 122 228 105 261  58  59  97 144 123  74 285 188 156 196 120 
% 162 169 122 143  23 250 226 138 157 144 211 215  81 124 142 233 118 
% 283 169 206 259  23 131 131 251 279 243 253  66  66  66  89  89 115 
% 116 274   8 176   1 103 202  87  48 161   2 248 256 275  90 154 264 
%
% retention:
% ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ 
% ___ ___ ___ ___ ___ ___  92  82 118  90 ___ ___ ___  53 ___ ___ ___ 
% ___  37 ___ ___ ___  54  50 ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ 
% ___ ___ ___ ___  97 ___ ___ ___ ___ ___ ___  11 ___ ___ ___ ___ ___ 
% ___ ___  32 ___ ___ ___ ___  61 ___ ___ ___ ___ ___   9 ___ ___ ___ 
% ___ ___ ___ ___  43 ___ ___  45 ___   2 ___ ___ ___ ___ ___ ___ ___ 
% ___  12 ___ ___ ___  55 ___ ___ ___ ___  57 ___ ___ ___  57 ___ ___ 
% ___ ___ ___ ___ ___ ___ ___ ___  35 ___ ___ ___  12 ___  64 ___ ___ 
% ___  22 ___ ___ ___ 117 ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ 
% ___ ___ ___  18 ___ 109  45 ___ ___ ___ ___ ___   8 ___ ___ ___ ___ 
% ___ ___ ___ ___ ___  76 ___ ___ ___  91 ___ ___ ___ ___ ___ ___ ___ 
% ___   7 ___ ___ ___ ___ ___ ___ ___ ___  28 ___ ___ ___ ___  20 ___ 
% ___ ___ ___ ___ ___ ___ ___ ___   7 ___ ___  28 ___ 161 ___ ___ ___ 
% ___ ___ ___ ___ ___ ___  20 ___ ___ ___ ___  71 ___ ___ ___ ___ ___ 
% ___ ___  95 ___   4 ___ ___ ___ ___  94 ___ ___ ___ ___ ___ ___ ___ 
% ___  70 ___ ___ ___ ___  59 ___ ___ ___ ___  56 ___  49 ___  20 ___ 
% ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ 


% 19x19 (AMSWR): z=6859 optimal value found by Gecode/fs in 29.2 seconds
% (most_constrained/indomain_random)
%
% magic total: 3439
% assoc: 362
% z:     6859
% sum(water): 72200
% (n*n*(n*n+1) div 2): 65341
%
% magic:
%  33 170  43  52 360 163 302 253 111 233 361 144 228 353 250 154   7   5 217 
% 219 276 349  46  58  16  48 275 201 334  91 259 341 220  70 209 132 238  57 
%  92  98 184 301 166 190  79  47 339 185 308 175 212 116 167   8 328 351  93 
% 206 182 240 128 288 317  36 231 207 324 323  75  42 359  95  78  40 256  12 
% 303 260 107 280 139 273 289  62  90 286 205  18 266 307 229 252  14  53   6 
% 335 120  25 135 282  85 188 262  17 330 118 210 127  88 342 285 164 232  94 
% 254 245  99 221 290  67 114 215 338  97 312 321  69 140 123 211  65  44 214 
%  56 159 202 279 104 336  64 200 168 171 125 136 216 105 138 299 193 197 291 
%  10 204  66 173 119 176 333 179 247 261  19 236  49  81 241 249 213 225 358 
%  15  68  22 311  30 327 278  31 325 181  37 331  84  35 332  51 340 294 347 
%   4 137 149 113 121 281 313 126 343 101 115 183  29 186 243 189 296 158 352 
%  71 165 169  63 224 257 146 226 237 191 194 162 298  26 258  83 160 203 306 
% 148 318 297 151 239 222 293  41  50 265  24 147 248 295  72 141 263 117 108 
% 268 130 198  77  20 274 235 152 244  32 345 100 174 277  80 227 337 242  27 
% 356 309 348 110 133  55  96 344 157  76 272 300  73  89 223  82 255 102  59 
% 350 106 322 284 267   3 320 287  39  38 155 131 326  45  74 234 122 180 156 
% 269  11  34 354 195 246 150 187  54 177  23 315 283 172 196  61 178 264 270 
% 305 124 230 153 292 142  21 103 271  28 161  87 314 346 304 316  13  86 143 
% 145 357 355 208 112   9 134 218   1 129 251 109  60 199   2 310 319 192 329 
%
% water:
%  33 170  43  52 360 163 302 253 111 233 361 144 228 353 250 154   7   5 217 
% 219 276 349  52  58  48  48 275 201 334 259 259 341 220 167 209 132 238  57 
%  92  98 184 301 166 190  79  79 339 308 308 175 212 167 167  78 328 351  93 
% 206 182 240 240 288 317  79 231 207 324 323 175 175 359  95  78  40 256  12 
% 303 260 107 280 273 273 289 207 207 286 205 175 266 307 229 252  40  53   6 
% 335 120  99 135 282 135 188 262 207 330 205 210 127 127 342 285 164 232  94 
% 254 245  99 221 290 135 135 215 338 171 312 321 127 140 138 211 164 164 214 
%  56 159 202 279 119 336 135 200 171 171 125 136 216 105 138 299 193 197 291 
%  10 204  66 173 119 176 333 179 247 261 115 236  81  81 241 249 213 225 358 
%  15  68  66 311 119 327 278 126 325 181 115 331  84  81 332 189 340 294 347 
%   4 137 149 113 121 281 313 126 343 115 115 183  84 186 243 189 296 203 352 
%  71 165 169 113 224 257 226 226 237 191 194 162 298 186 258 141 160 203 306 
% 148 318 297 151 239 239 293 226 226 265 147 147 248 295  80 141 263 117 108 
% 268 198 198  77  77 274 235 226 244 155 345 147 174 277  80 227 337 242  27 
% 356 309 348 110 133  55  96 344 157 155 272 300 172 172 223 223 255 102  59 
% 350 230 322 284 267  55 320 287 155 155 155 155 326 172 172 234 178 180 156 
% 269 230 230 354 246 246 150 187 155 177 155 315 283 172 196 178 178 264 270 
% 305 230 230 208 292 142 103 103 271 129 161 109 314 346 304 316  86  86 143 
% 145 357 355 208 112   9 134 218   1 129 251 109  60 199   2 310 319 192 329 
%
% retention:
% ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ 
% ___ ___ ___   6 ___  32 ___ ___ ___ ___ 168 ___ ___ ___  97 ___ ___ ___ ___ 
% ___ ___ ___ ___ ___ ___ ___  32 ___ 123 ___ ___ ___  51 ___  70 ___ ___ ___ 
% ___ ___ ___ 112 ___ ___  43 ___ ___ ___ ___ 100 133 ___ ___ ___ ___ ___ ___ 
% ___ ___ ___ ___ 134 ___ ___ 145 117 ___ ___ 157 ___ ___ ___ ___  26 ___ ___ 
% ___ ___  74 ___ ___  50 ___ ___ 190 ___  87 ___ ___  39 ___ ___ ___ ___ ___ 
% ___ ___ ___ ___ ___  68  21 ___ ___  74 ___ ___  58 ___  15 ___  99 120 ___ 
% ___ ___ ___ ___  15 ___  71 ___   3 ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ 
% ___ ___ ___ ___ ___ ___ ___ ___ ___ ___  96 ___  32 ___ ___ ___ ___ ___ ___ 
% ___ ___  44 ___  89 ___ ___  95 ___ ___  78 ___ ___  46 ___ 138 ___ ___ ___ 
% ___ ___ ___ ___ ___ ___ ___ ___ ___  14 ___ ___  55 ___ ___ ___ ___  45 ___ 
% ___ ___ ___  50 ___ ___  80 ___ ___ ___ ___ ___ ___ 160 ___  58 ___ ___ ___ 
% ___ ___ ___ ___ ___  17 ___ 185 176 ___ 123 ___ ___ ___   8 ___ ___ ___ ___ 
% ___  68 ___ ___  57 ___ ___  74 ___ 123 ___  47 ___ ___ ___ ___ ___ ___ ___ 
% ___ ___ ___ ___ ___ ___ ___ ___ ___  79 ___ ___  99  83 ___ 141 ___ ___ ___ 
% ___ 124 ___ ___ ___  52 ___ ___ 116 117 ___  24 ___ 127  98 ___  56 ___ ___ 
% ___ 219 196 ___  51 ___ ___ ___ 101 ___ 132 ___ ___ ___ ___ 117 ___ ___ ___ 
% ___ 106 ___  55 ___ ___  82 ___ ___ 101 ___  22 ___ ___ ___ ___  73 ___ ___ 
% ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ 
%


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "globals.mzn";
% include "gecode.mzn"; % For Gecode!

int: n = 5; 
int: total = (n*(n*n+1)) div 2;

% var 0..n*n*n: total;
array[1..n,1..n] of var 1..n*n: magic;
array[1..n,1..n] of var 1..n*n: water;

% the difference between water and magic
% array[1..n,1..n] of var 0..n*n: diffs;

int: assoc = n*n+1;
% var 1..n*n+1: assoc;

% objective (to maximize)
var 0..n*n*n: z = sum(water) - (n*n*(n*n+1) div 2) ;
% var 0..n*n*n: z2 = sum(diffs);

% ann: var_select;
% ann: val_select;
% solve :: int_search(
%         % [water[i,j] | i in 1..n, j in 1..n] ++
%         [magic[i,j] | i in 1..n, j in 1..n] % ++ [z]
%         % ++[water[i,j] | i in 1..n, j in 1..n] 
%         % [magic[i,j] | i in 1..n, j in 1..n] % ++ [z]
%         % [diffs[i,j] | i in 1..n, j in 1..n]
%         % ++[water[i,j] | i in 1..n, j in 1..n] % ++
%         % ++[magic[i,j] | i in 1..n, j in 1..n]
%         , 
%         most_constrained,
%         indomain_random, % indomain_split, % 
%         complete) 
%     % satisfy;
%     maximize z;
%     % maximize z2;

% solve :: seq_search(
%        [

%         int_search([magic[i,j] | i in 1..n, j in 1..n], most_constrained, indomain_random, complete),

%          % this (as the singular int_search) is best at 5x5
%          % int_search([magic[i,j] | i in 1..n, j in 1..n], most_constrained, indomain_max, complete),


%          % int_search([magic[i,j] | i in 1..n, j in 1..n], most_constrained, indomain_max, complete),
%          % int_search([water[i,j] | i in 1..n, j in 1..n], first_fail, indomain_max, complete),


%          % int_search([diffs[i,j] | i in 1..n, j in 1..n], first_fail, indomain_max, complete),
%          % int_search([magic[i,j] | i in 1..n, j in 1..n], most_constrained, indomain_max, complete),
         
%        ]
%      ) 
%      maximize z;

% solve satisfy;
solve maximize z;


% constraint
%   % trace("n: " ++ show(n) ++ " total: " ++ show(total) ++ " assoc: " ++ show(assoc) ++ "\n", 1=1)
%   trace("n: " ++ show(n) ++ " total: " ++ show(total) ++ "\n", 1=1)
% ;

constraint
  all_different([magic[i,j] | i,j in 1..n]) :: domain
  /\
  forall(k in 1..n) (
     sum(i in 1..n) (magic[k,i]) = total /\
     sum(i in 1..n) (magic[i,k]) = total
  )
  /\ % diagonal
  sum(i in 1..n) (magic[i,i]) = total
  /\ % diagonal
  sum(i in 1..n) (magic[i,n-i+1]) = total

  /\ % "associative value"
  forall(i,j in 1..n) (
     magic[i,j] + magic[n-i+1,n-j+1] = assoc
  )

  % /\ z = 0 % optimal value for n=4
  % /\ z >= 69 % testing n=5

  % TESTING for n=5
  % /\ if n = 5 then
  %    magic[1,2] > magic[2,2] /\ magic[1,3] > magic[2,3] /\ magic[1,4] > magic[2,4] /\ 
  %    magic[n,2] > magic[n-1,2] /\   magic[n,3] > magic[n-1,3] /\ magic[n,4] > magic[n-1,4] 
  %    /\ 
  %    magic[2,1] > magic[2,2] /\ magic[3,1] > magic[3,2] /\ magic[4,1] > magic[4,2] /\
  %    magic[2,n] > magic[2,n-1] /\ magic[3,n] > magic[3,n-1] /\ magic[4,n] > magic[4,n-1]
  %    else 
  %      true
  %    endif
  
  % Testing
  % /\ sum(i, j in 1..n) ( bool2int(water[i,j] > magic[i,j]) ) > n


;

% Water retention
% This is from the Numberjack model (MagicWater.py)
constraint
   % first, the rim
   forall(i in 1..n) (
      % rows
      water[i,1] = magic[i,1] /\
      water[i,n] = magic[i,n] /\
      % columns
      water[1,i] = magic[1,i] /\
      water[n,i] = magic[n,i]
   )
   /\
   % % then the inner cells (max between their own height and of 
   % % the water level around)
   forall(a in 2..n-1, b in 2..n-1) (
      % water[a,b] = max(magic[a,b], min([water[a-1,b], water[a,b-1], 
      %                                   water[a+1,b], water[a,b+1]]))
      % /\
      maximum(water[a,b], [magic[a,b], min([water[a-1,b], water[a,b-1], 
                                            water[a+1,b], water[a,b+1]])]) % :: domain

      % let {
      %   var 1..n*n: t = min([water[a-1,b], water[a,b-1], 
      %                        water[a+1,b], water[a,b+1]])
      % } in
      % maximum(water[a,b], [magic[a,b], t]) :: domain

   )

   % /\
   % forall(i,j in 1..n) (
   %   water[i,j] >= magic[i,j]
   % )

     % Testing
   % /\ 
   % forall(i,j in 1..n) (
   %   diffs[i,j] = water[i,j] - magic[i,j]
   % )


   % /\  % Testing: Each hole must have a neigbouring hole
   % forall(i,j in 1..n) (
   %   diffs[i,j] > 0 ->
   %          sum(a,b in {-1,0,1} where 
   %             i+a > 0  /\ j+b >  0 /\
   %             i+a <= n /\ j+b <= n /\
   %             (abs(a) + abs(b) > 0)
   %          ) (bool2int(diffs[i+a,j+b] > 0)) > 0
   % )


;

% Frénicle standard form
% See hakank.org/minizinc/magic_square_frenicle_form.mzn
% For n=4 this yields the 48 squares that's shown at
% http://en.wikipedia.org/wiki/Associative_magic_square#4_x_4_associative_magic_square_-_complete_listing
% 
constraint
  minimum(magic[1,1], [magic[1,1], magic[1,n], magic[n,1], magic[n,n]]) :: domain
  /\
  magic[1,2] < magic[2,1]
;

% Symmetry breaking as the Numberjack model 
% (which is not exactly the same as Frénicle standard form)
% constraint
%    magic[1,1] < magic[1,n] /\
%    magic[1,1] < magic[n,n] /\
%    magic[1,n] < magic[n,1]
% ;


output [
  "magic: " ++ show(magic) ++ "\n" ++
  "water: " ++ show(water) ++ "\n" ++
  "magic total: " ++ show(total) ++ "\n" ++ 
  "assoc: " ++ show(assoc) ++ "\n" ++
  "z:     " ++ show(z) ++ "\n" ++
  % "z2:     " ++ show(z2) ++ "\n" ++
  "sum(water): " ++ show(sum(water)) ++ "\n" ++
  "(n*n*(n*n+1) div 2): " ++ show((n*n*(n*n+1) div 2)) ++ "\n"
] 
++ ["\nmagic:"] ++
[
  if j = 1 then "\n" else "" endif ++
    show_int(3, magic[i,j]) ++ " "
  | i,j in 1..n
] 
++ ["\n\nwater:"] ++
[
  if j = 1 then "\n" else "" endif ++
    show_int(3, water[i,j]) ++ " "
  | i,j in 1..n
] 
% ++ ["\ndiffs:\n"] ++
% [
%   if j = 1 then "\n" else "" endif ++
%     show_int(3, diffs[i,j]) ++ " "
%   | i,j in 1..n
% ] 
++ ["\n\nretention:"] ++
[
  if j = 1 then "\n" else "" endif ++
    if fix(water[i,j]) != fix(magic[i,j]) then 
        show_int(3, water[i,j]-magic[i,j]) ++ " "
    else 
        "___ " 
    endif
  | i,j in 1..n
] 
++ 
[ "\nz:     " ++ show(z) ++ "\n"]
++
["\n"];
