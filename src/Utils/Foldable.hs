module Utils.Foldable where



-- | Check if the collection is monotonic provided a given equivalence relation

isMonotonic :: (a -> a -> Bool) -> [a] -> Bool
isMonotonic rel xs = and $ zipWith rel xs (tail xs)

