ring x = (head.dropWhile (\i -> i^2 <= x)) [1,3..]

corners x = map (\i -> (x^2) - ((x - 1) * i)) [0..3]

cross x = map (\i -> i - (x `div` 2)) (corners x)

distance x = (ring x `div` 2) + (minimum.map (\i -> abs $ i - x)) (cross $ ring x)
