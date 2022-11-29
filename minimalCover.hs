import Data.List

type Attribute = Char

type Determinant = [Attribute]
type Dependent = [Attribute]

data FD = FD Determinant Dependent

instance Show FD where
  show fd = determinant fd ++ " -> " ++ dependent fd

determinant :: FD -> Determinant
determinant (FD x _) = x

dependent :: FD -> Dependent
dependent (FD _ y) = y

-- Splits a functional dependency such that the RHS
-- only contains one attribute
single :: FD -> [FD]
single fd@(FD x [y]) = [fd]
single (FD x y)      = [FD x [c] | c <- y]

-- Check if a set of Attributes is a subset of another
contain :: Determinant -> Determinant -> Bool
contain x y = all (`elem` x) y

-- Finds closure of a determinant in a set of functional dependencies
-- by looping through the set n times since worst case it adds 
-- one fd to closure each loop
closure :: Determinant -> [FD] -> Dependent
closure k fds = sort (close (length fds) k fds)
  where
    close :: Int -> Determinant -> [FD] -> Dependent
    close 0 k _   = k
    close n k fds = close (n-1) k' fds
      where
        k' = nub (k ++ concat [y | (FD x y) <- fds, contain k x])

-- Checks for redundant determinant in a functional dependency
rDeterminant :: FD -> [FD] -> FD
rDeterminant (FD x y) fds = FD (sort (newdet [] x fds)) y
  where
    newdet :: Determinant -> Determinant -> [FD] -> Determinant
    newdet n [] _ = n
    newdet n (x : xs) fds
      | contain (closure (n ++ xs) fds) [x] = newdet n xs fds
      | otherwise                           = newdet (x : n) xs fds

-- minimizes a set of functional dependencies (sequentially)
-- by removing redundant determinants (cannot use map)
minDet :: [FD] -> [FD] -> [FD]
minDet nfds [] = nfds
minDet nfds (fd : fds) = minDet ((rDeterminant fd (nfds ++ fds)) : nfds) fds

-- Checks for redundant dependencies
-- Pre: FD was a part of [FD] and is now removed
rDependency :: FD -> [FD] -> Bool
rDependency (FD x y) fds = contain (closure x fds) y

-- minimizes a set of functional dependencies (sequentially)
-- by removing redundant dependencies
minDep :: [FD] -> [FD] -> [FD]
minDep nfds [] = nfds
minDep nfds (fd : fds)
  | rDependency fd (nfds ++ fds) = minDep nfds fds
  | otherwise                    = minDep (fd : nfds) fds

minimalCover :: [FD] -> [FD]
minimalCover fds = minDep [] (minDet [] singled)
  where
    singled = concat (map single fds)

fdset1 = [
  FD "AB" "DEH",
  FD "BEF" "A",
  FD "FGH" "C",
  FD "D" "EG",
  FD "EG" "BF",
  FD "F" "BH"]

fdset2 = [
  FD "A" "B",
  FD "B" "C",
  FD "A" "C"]

