-----------------------------------------------------------------------------
-- |
-- Module         : Sort3
-- Copyright      :  (c) Some description... 2018
--
-- License        : MIT
-- Author         : Sampath Singamsetty
-- Maintainer     : Singamsetty.Sampath@gmail.com
-- Description    :
--
-----------------------------------------------------------------------------
module Sort3 (sort3) where


sort3 :: Ord a => (a, a, a) -> (a, a, a)
sort3 (a0, a1, a2) =
  if a0 > a1
  then if a0 > a2
       then if a2 < a1
               then (a2, a1, a0)
               else (a1, a2, a0)
       else (a1, a0, a2)
  else if a1 > a2
       then if a0 > a2
            then (a2, a0, a1)
            else (a0, a2, a1)
       else (a0, a1, a2)
