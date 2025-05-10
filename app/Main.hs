{-# LANGUAGE TupleSections #-}
module Main
  (
    main
  ) where

import Data.List (tails, group)
import Data.Maybe (fromMaybe)
import Control.Monad
import qualified Data.Map as M
import Math.Combinat.Classes (HasDuality(dual))
import Math.Combinat.Partitions.Integer (Partition, fromPartition, mkPartition)
import qualified Data.IntMap.Strict as I

----------------------------------------------------------------------
-- Dyck paths and some properties.
----------------------------------------------------------------------

newtype DyckPath = DyckPath { getDyckPath :: [Bool] }
  deriving (Ord, Eq)

instance Show DyckPath where
  show = show . areaSequence

-- | All Dyck paths of size n.
--
-- >>> mapM_ print (dyckPaths 3)
-- [0,0,0]
-- [0,0,1]
-- [0,1,0]
-- [0,1,1]
-- [0,1,2]
dyckPaths :: Int -> [DyckPath]
dyckPaths n = DyckPath <$> go n n
  where
    go 0 0 = return []
    go n0 n1 = do
      guard (n1 <= n0 && n0 >= 0 && n1 >= 0)
      step <- [False, True]
      rest <- go (if not step then n0-1 else n0)
                 (if step then n1-1 else n1)
      return (step : rest)

-- | Area under Dyck path.
--
-- >>> ghci> area (fromAreaSequence' [0,1,2,3,1,0])
-- 7
area :: DyckPath -> Int
area = sum . areaSequence

-- | Bounce statistic for a Dyck path.
--
-- >>> bounce (fromAreaSequence' [0,1,2,3,1,0])
-- 3
bounce :: DyckPath -> Int
bounce = sum . bouncePath

dim :: DyckPath -> Int
dim = (`div` 2) . length . getDyckPath

-- | The area sequence of a Dyck path.
--
-- >>> areaSequence (fromAreaSequence' [0,1,2,3,1,0])
-- 7
areaSequence :: DyckPath -> [Int]
areaSequence (DyckPath xs)
  | null xs = []
  | otherwise = (0:) . go 0 . tail $ xs
  where
    go _ [] = []
    go area (step:rest)
      | step = (area+1) : go (area+1) rest
      | otherwise = go (area-1) rest

-- | Construct a Dyck path from its area sequence.
--
-- >>> fromAreaSequence [0,1,2,3,1,0] :: Maybe DyckPath
-- Just [0,1,2,3,1,0]
fromAreaSequence :: MonadPlus m => [Int] -> m DyckPath
fromAreaSequence [] = return (DyckPath [])
fromAreaSequence (a:as) = do
  guard (a == 0)
  guard (all (>=0) as)
  guard (all (<=1) $ zipWith (-) as (a:as))
  return $ DyckPath (True : go a as)
  where
    go prev [] = replicate (prev+1) False
    go prev (a:as)
      | a == prev + 1 = True : go a as
      | otherwise = replicate (prev-a+1) False ++ [True] ++ go a as

-- | Construct a Dyck path from the N,E steps.
--
-- >>> fromSequence [True,True,True,True,False,False,False,True,False,False,True,False]
-- Just [0,1,2,3,1,0]
fromSequence :: [Bool] -> Maybe DyckPath
fromSequence bs | valid (0::Int) bs = Just (DyckPath bs)
                | otherwise = Nothing
  where valid 0 (False:_) = False
        valid n (False:xs) = valid (n-1) xs
        valid n (True:xs) = valid (n+1) xs
        valid 0 [] = True
        valid _ _ = False

fromAreaSequence' :: [Int] -> DyckPath
fromAreaSequence' xs = fromMaybe
  (error $ "Cannot create path from area sequence " ++ show xs)
  (fromAreaSequence xs)

-- | The sequence of bounce points for a Dyck path.
--
-- >>> bouncePath (fromAreaSequence' [0,1,2,3,1,0])
-- [2,1,0]
bouncePath :: DyckPath -> [Int]
bouncePath d = go . tagged 1 1 . getDyckPath $ d
  where tagged _ _ [] = []
        tagged nF nT (False:xs) = (nF, False) : tagged (nF+1) nT xs
        tagged nF nT (True:xs) = (nT, True) : tagged nF (nT+1) xs
        go [] = []
        go xs = let (trues, rest) = span snd xs
                    value = fst (last trues)
                in n-value : go (filter ((> value) . fst) rest)
        n = length (getDyckPath d) `div` 2

-- | The heights at which the Dyck path is in each column.
--
-- >>> heights (fromAreaSequence' [0,1,2,3,1,0])
-- [4,4,4,5,5,6]
heights :: DyckPath -> [Int]
heights d = go 0 (getDyckPath d)
  where
    go _ [] = []
    go height (False:xs) = height : go height xs
    go height (True:xs) = go (height+1) xs

-- | Construct a Dyck path from its heights.
--
-- >>> fromHeights [4,4,4,5,5,6]
-- Just [0,1,2,3,1,0]
fromHeights :: [Int] -> Maybe DyckPath
fromHeights = fromSequence . go 0
  where
    go _ [] = []
    go prev (h:hs) = replicate (h-prev) True ++ [False] ++ go h hs

-- | Returns information at the column at each bounce point.
--
-- At each such column, it returns (column, (width,height), heights).
--
-- >>> mapM_ print (bounceMaps (fromAreaSequence' [0,1,2,3,1,0]))
-- (1,(4,1),fromList [(1,0),(2,0),(3,0),(4,1)])
-- (5,(1,1),fromList [(5,0)])
-- (6,(1,0),fromList [(6,0)])
bounceMaps :: DyckPath -> [(Int, (Int, Int), M.Map Int Int)]
bounceMaps d = do
  (b1:b2:b3:_) <- tails . fmap (n-) . (++[0]) . (n:) $ bouncePath d
  let width = b2 - b1
      height = b3 - b2
      startCol = b1+1
      hMap = M.fromList [(col, hs !! (col-1) - b2) | col <- [b1+1..b2]]
  return (startCol, (width, height), hMap)
  where hs = heights d
        n = dim d

-- | The composition due to the bounce points.
--
-- >>> composition (fromAreaSequence' [0,1,2,3,1,0])
-- [4,1,1]
composition :: DyckPath -> [Int]
composition d = zipWith (-) (tail xs) xs
  where
    xs = 0 : [dim d - b | b <- bouncePath d]

----------------------------------------------------------------------
-- The Bounce incrementing operator.
--
-- 
----------------------------------------------------------------------

newtype BounceIndex = BounceIndex { getBounceIndex :: Int }
  deriving (Show)

-- | Counts of repetitions of parts in a parititons.
--
-- >>> dMap $ mkPartition [4,3,3,1,1,1]
-- fromList [(1,3),(2,2),(3,1)]
dMap :: Partition -> M.Map Int Int
dMap = M.fromList . zip [1..] . fmap length . group . reverse . fromPartition

-- | The map bnc_lambda(i,r).
--
-- >>> bounceIndex (mkPartition [6,3,3,1]) (1,1)
-- Just (BounceIndex {getBounceIndex = 3})
-- >>> bounceIndex (mkPartition [6,3,3,1]) (1,2)
-- Just (BounceIndex {getBounceIndex = 2})
bounceIndex :: Partition -> (Int, Int) -> Maybe BounceIndex
bounceIndex p (i, r) = do
  guard (1 <= i && i < l)
  guard (1 <= r && r <= d M.! (i+1))
  return $ BounceIndex $ 1 - r + sum (M.filterWithKey (\j _ -> j > i) d)
  where d = dMap p
        l = M.size d

-- | The operator S_i.
--
-- >>> shift (BounceIndex 1) (fromAreaSequence' [0,1,2,0,1,1,1])
-- Just [0,1,0,1,2,1,1]
shift :: BounceIndex -> DyckPath -> Maybe DyckPath
shift (BounceIndex i) x = shiftMove (bounceMaps x !! (i-1)) x

shiftMany :: BounceIndex -> [Int] -> DyckPath -> Maybe DyckPath
shiftMany _ [] path = return path
shiftMany b@(BounceIndex i) (c:cs) path = do
  path' <- foldM (flip shift) path (replicate c b)
  shiftMany (BounceIndex (i+1)) cs path'

type BounceCells = M.Map Int Int

-- | Add a cell to the given location, space permitting.
addExtraCell :: (Int, (Int, Int), BounceCells)
             -> DyckPath
             -> Maybe DyckPath
addExtraCell (startCol, (width, height), hMap) x = do
  guard (width > 1)
  guard (last (M.elems hMap) < height)
  let (lhs, r:rhs) = splitAt (startCol + width - 2) (heights x)
  fromHeights (lhs ++ (r+1):rhs)

shiftMove :: (Int, (Int, Int), BounceCells)
          -> DyckPath
          -> Maybe DyckPath
shiftMove conf@(col, _, _) x = do
  x' <- addExtraCell conf x
  let (conf':_) = filter (\(col',_,_) -> col == col') $ bounceMaps x'
  upMove conf' x'

-- | The operator U_i.
--
-- Not important for Phi as S_i can be implemented without this.
upMove :: (Int, (Int, Int), BounceCells)
       -> DyckPath
       -> Maybe DyckPath
upMove (startCol, (width, height), hMap) x = do
  -- the column at startCol-1 must have smaller height
  guard (startCol-2 < 0 || hs !! (startCol-1) > hs !! (startCol-2))
  case checkUp startCol (width, height) hMap of
    Nothing -> mzero
    Just delta ->
      case fromHeights $ fmap (\(c,h) -> h + M.findWithDefault 0 c delta) (zip [1..] hs) of
        Nothing -> mzero
        Just x' -> return x'
  where hs = heights x

checkUp :: Int -> (Int, Int) -> M.Map Int Int -> Maybe (M.Map Int Int)
checkUp startCol (width, height) hMap = do
  let lastCol = startCol + width - 1
  lastLen <- M.lookup lastCol hMap
  let gap = height - lastLen
      vW = min (width-gap-1) $
           if lastLen == 0 then 1 else length (filter (==lastLen) $ M.elems hMap)
      delta = [(c, (hMap M.! c) + 1) | c <- [startCol+1 .. startCol + gap]]
      delta' = transposePartition . fmap snd $ delta
  guard (1+gap <= width)
  guard (null delta || snd (last delta) <= vW)

  return $ M.fromList $
    -- remove 1 from the first column
    (startCol, -1):
    -- remove 1 + height of columns for the next gap columns
    fmap (fmap negate) delta ++
    -- add the delta to the columns
    zip [lastCol,lastCol-1..] delta'

transposePartition :: [Int] -> [Int]
transposePartition xs
  | null xs || null nonzero = []
  | otherwise = length nonzero : transposePartition (subtract 1 <$> nonzero)
  where nonzero = filter (>=1) xs

----------------------------------------------------------------------
-- The bijection Phi
----------------------------------------------------------------------

type CountMap = M.Map (Int, Int) Int

-- | Possibly maps a Dyck path (having area > bounce) via the Phi map.
--
-- Given d, returns (composition of d, count map of d, Phi(d)).
--
-- >>> aCountMap (fromAreaSequence' [0,1,2,3,2,2])
-- Just ([4,2],fromList [((1,1),2),((1,2),1)],[0,0,1,0,0,1])
--
-- >>> aCountMap (fromAreaSequence' [0,1,2,1,2,3])
-- Nothing
aCountMap :: DyckPath -> Maybe ([Int], CountMap, DyckPath)
aCountMap d
  | or (zipWith (<) comp (tail comp)) = Nothing
  | otherwise = do
      (p, f) <- ((comp,) . M.fromList . filter ((> 0) . snd)) <$>
                go 1 0 comp (areaSequence d)
      d' <- bfPath (fromPartition . dual . mkPartition $ p) f
      return (p, f, d')
  where
    comp = composition d
    go i row (c1 : c2 : cs) xs
      | c1 == c2 && head xs' > 0 = Nothing
      | c1 == c2 = go i (row + c1) (c2 : cs) xs'
      | otherwise =
          (++) <$> pure (zip indices counts) <*> go (i + 1) (row + c1) (c2 : cs) xs'
      where
        xs' = drop c1 xs
        indices = [(i, r) | r <- [1 .. c2]]
        counts = zipWith (-) (take c2 xs') [0 ..]
    go _ _ _ _ = pure []

-- | Constructs the Dyck path having a <= b given the count map and partition.
--
-- >>> bfPath [2,2,1,1] $ Data.Map.fromList [((1,1),2),((1,2),1)]
-- Just [0,0,1,0,0,1]
bfPath :: [Int] -> CountMap -> Maybe DyckPath
bfPath parts = foldM go bpath . M.toList
  where
    p = mkPartition parts
    go path (index, count) = do
      b <- bounceIndex p index
      let (c : cs) = drop (getBounceIndex b - 1) (composition path)
          diffs = map (c -) cs
          (lhs, rhs) = span (/= count) . scanl1 (+) $ diffs
      guard (all (> 0) lhs && not (null rhs))
      shiftMany b (take (length lhs + 1) diffs) path
    bpath = fromAreaSequence' . concat $ [[0 .. c - 1] | c <- parts]

----------------------------------------------------------------------
-- Writes the bijection into a file for a given n.
--
-- Each line consists of two paths isMin a b d d'
-- where isMin=True if d is area-minimal
--       a is the area of d
--       b is the bounce of d
--       area(d) <= bounce(d)
--       d' = Phi(d)
----------------------------------------------------------------------

minimals = I.fromListWith min . fmap (\d -> (area d, bounce d)) .
           filter (\d -> area d >= bounce d) . dyckPaths

example n = writeFile ("examples/n" ++ show n ++ ".txt") .
  unlines . fmap unwords $ do
  d <- dyckPaths n
  let (a,b) = (area d, bounce d)
  guard (a >= b)
  case aCountMap d of
    Just (_, _, d') -> return [show (isMin a b), show (area d), show (bounce d), show d, show d']
    _ -> mzero
  where mins = minimals n
        isMin a b = I.findWithDefault 0 a mins == b

writeExamples = do
  writeFile "examples/minimal_counts.txt" .
    unlines .
    map (\n -> let m = minimals n
               in show n ++ " " ++
                  (show . length $
                    filter (\d -> I.findWithDefault 0 (area d) m == bounce d) $
                    dyckPaths n)) $
    [1..15]
  mapM_ example [1..15]

----------------------------------------------------------------------
-- Given path d having area >= bounce, returns Phi(d), if possible.
----------------------------------------------------------------------

-- main = writeExamples

main :: IO ()
main = interact $
  unlines .
  fmap (show .
        fmap (\(_, _, d') -> d') .
        aCountMap .
        fromAreaSequence' .
        read) .
  lines
