{-
[X] Julian is at the fifth position.
[X] The boss with the Red tie is going on vacation on June.
[X] The man wearing the Black tie is somewhere between who earns $3,000 and Adam, in that order.
[X] The 51 years old boss makes $2,000 monthly.
[X] Michael is at the first position.
[X] The boss who is going on vacation on March is in one of the ends.
[X] Adam sitting is next to who earns $4,000 each month.
[X] In the fourth position is the boss who is going on vacation on August.
[X] The man wearing the Black tie earns $4,000 monthly.
[X] In one of the ends is the boss who makes $5,000 monthly.
[X] The 46 years old boss is somewhere to the left of the boss who works at HR department.
[X] The man from HR department is somewhere between who is 40 years old and Thomas, in that order.
[X] Nathan is going on vacation on December.
[X] The Sales department's boss is exactly to the right of the 46 years old boss.
[X] The boss wearing the Red tie is somewhere to the left of the boss wearing the Yellow tie.
[X] In the first position is the boss of R&D department.
[X] The oldest boss is wearing Blue tie.
[X] The man wearing the Yellow tie is in one of the ends.
[X] The youngest boss is at the fifth position.
[X] The boss of the Marketing department is 51 years old.
-}

import Control.Monad (guard)
import Data.List (permutations, intersect, delete, zip6, tails)
import Data.Semigroup ((<>))

data Boss = Julian | Michael | Adam | Nathan | Thomas
  deriving (Eq, Show, Enum, Bounded)

data Tie = Yellow | Black | Blue | Green | Red
  deriving (Eq, Show, Enum, Bounded)

data Department = RD | HR | IT | Marketing | Sales
  deriving (Eq, Show, Enum, Bounded)

data Vacation = August | December | January | June | March
  deriving (Eq, Show, Enum, Bounded)

-- General helpers for constructing seqeunces.

allOf :: (Eq a, Enum a, Bounded a) => a -> [a]
allOf _ = [minBound..]

inPos :: Eq a => Int -> a -> [a] -> [[a]]
inPos n a as = map (\l -> take n l <> [a] <> drop n l) $ (permutations . delete a) as

onOneEnd :: Eq a => a -> [a] -> [[a]]
onOneEnd a as = inBeginning a as <> inPos ((succ.length) as) a as

inBeginning :: Eq a => a -> [a] -> [[a]]
inBeginning a as = map (a:) $ (permutations . delete a) as

-- I hand-crafted all the sequences rather than using guards because
-- they all had something interesting about them that would reduce the
-- search space.

salaries :: [[Int]]
salaries = onOneEnd 5000 [2000, 3000, 4000, 5000, 6000]

ages :: [[Int]]
ages = inPos 4 34 [40, 46, 51, 55]

bosses :: [[Boss]]
bosses = map (\l -> Michael : l <> [Julian]) $ permutations [Adam ..]

vacations :: [[Vacation]]
vacations = inPos 3 August [December ..] `intersect` onOneEnd March [minBound..]

departments :: [[Department]]
departments = inBeginning RD [minBound..]

ties :: [[Tie]]
ties = onOneEnd Yellow [Black ..]

-- General helpers for constructing guards

-- Match two different properties for one position.
has :: (Eq a, Eq b) => [a] -> a -> [b] -> b -> Bool
has as a bs b = (a,b) `elem` zip as bs

-- Match a property of a neighbor position.
neighbor :: (Eq a, Eq b) => [a] -> a -> [b] -> b -> Bool
neighbor as a bs b = (a,b) `elem` (zip as (tail bs) <> zip (tail as) bs)

-- Match a property of some other property that falls somewhere to the right.
leftOf :: (Eq a, Eq b) => [a] -> a -> [b] -> b -> Bool
leftOf [] _ [] _ = False
leftOf (a':as) a (_:bs) b
  | a' == a = b `elem` bs
  | otherwise = leftOf as a bs b

-- The specific solution

solutions :: [[(Boss, Tie, Department, Int, Vacation, Int)]]
solutions = do
  b <- bosses
  t <- ties
  d <- departments
  a <- ages
  s <- salaries
  v <- vacations

  guard $ m t Red v June
  guard $ m a 51 s 2000
  guard $ n b Adam s 4000
  guard $ m t Black s 4000
  -- 3000 Black Adam
  guard $ l s 3000 t Black
  guard $ l t Black b Adam
  guard $ l a 46 d HR
  -- 40 HR Thomas
  guard $ l a 40 d HR
  guard $ l d HR b Thomas
  guard $ m b Nathan v December
  guard $ n d Sales a 46 -- to the right -- maybe another guard
  guard $ l t Red t Yellow
  guard $ m a 55 t Blue
  guard $ m d Marketing a 51

  pure $ zip6 b t d a v s

  -- A few aliases to make the guards lighter.
  where m :: (Eq a, Eq b) => [a] -> a -> [b] -> b -> Bool
        m = has
        n :: (Eq a, Eq b) => [a] -> a -> [b] -> b -> Bool
        n = neighbor
        l :: (Eq a, Eq b) => [a] -> a -> [b] -> b -> Bool
        l = leftOf
