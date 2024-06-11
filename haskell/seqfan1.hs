{- 
  
  Touching number product sequence in Haskell.

  From the seqfan mailing list:
  """
  To: Sequence Fanatics Discussion list <seqfan@list.seqfan.eu>
  From: jsk <jskcmg@gmail.com>
  Date: Wed, 4 Jan 2012 01:43:34 +1100
  Subject: [seqfan] Re: 10 different digits, 9 products

  Hello Seqfans,

  On Tue, Jan 3, 2012 at 11:08 PM, Eric Angelini <Eric.Angelini@kntv.be> wrote:
  >
  > Hello SeqFans, 
  > I'm looking for all D numbers with 10 digits (digits must be
  > different one from another) having this property :
  > when you multiply two touching digits of D, the result is
  > visible in D (as a character string).

  Here are my 58 solutions (found by brute force over 10!):

  3207154869
  3205486917
  4063297185
  4063792185
  4230567819
  4230915678
  4297630518
  4297631805
  5042976318
  5063297184
  5079246318
  5093271486
  5094236718
  5148609327
  5180429763
  5180792463
  5180942367
  5184063297
  5420796318
  5420976318
  5486913207
  5630187924
  5630241879
  5630418792
  5630421879
  5630429718
  5630792418
  5630924187
  5678142309
  6320184597
  6320459718
  6320718459
  6320791845
  6320971845
  6324079185
  6324097185
  6329705184
  6329718405
  7091532486
  7132054869
  7153248609
  7183092465
  7924063185
  7924630518
  7924631805
  7963205418
  9071532486
  9142305678
  9153248607
  9246518307
  9305614278
  9308142765
  9327051486
  9327148605
  9423670518
  9423671805
  9872305614

  Thanks,
  Jason.
  """ 

  I tried to use patterns but it does not work!


  This program was created by Hakan Kjellerstrand (hakank@gmail.com)
  Also see my Haskell page: http://hakank.org/haskell/
  
  
-}

import Data.List
import Data.Char
-- import HakankUtils

-- findIx a lst: return the index of the element a in the list lst
-- here we assume that a is in the list (in contrast to the built-in elemIndex)
findIx :: Eq a => a -> [a] -> Int
findIx a lst = findIx' a lst 0

-- findIx' a [] p = p
findIx' a (x:xs) p = if a == x then p else findIx' a xs (p+1)

check :: [Int] -> [Int] -> Int -> Int
check _ []  c      = c
check _ [_] c      = c
check lst (a:b:xs) c = if (a*b) <= 9 || eq == (ep+1) then check lst (b:xs) (c+1) else 0
                     where
                        [p,q] = map digitToInt $ show (a*b)
                        ep = findIx p lst
                        eq = findIx q lst

seq1 = -- print $ length res
       (res, length res) 
          where res = filter (\p -> 9 == check p p 0) $ permutations [0..9]

--
-- Interestingly, KiCS2 Curry takes about 6s for this.
--
main = do
         print seq1 -- (22.96 secs, 24,131,343,400 bytes)
