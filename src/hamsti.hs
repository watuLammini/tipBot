applyMe :: (Floating a, RealFrac a, Ord a) => a -> Maybe a
applyMe a
  | a < (-2) = Nothing
  | (a > (-2)) && (a <= 0) = Just 1
  | otherwise = (Just (a*)) <*> (applyMe (a-2))
--  where it 0 = 1
--        it (-1) = 1
--        it a = 0

sqrtMe :: (Floating a, Ord a, RealFrac a) => a -> Maybe Int
sqrtMe a
  | a > 1000 = Nothing
  | otherwise = Just $ floor (sqrt a)

hamsti1 = applyMe 9 >>= sqrtMe
hamsti2 = applyMe 9.01 >>= sqrtMe