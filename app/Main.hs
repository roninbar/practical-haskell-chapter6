{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main where

import           Control.Monad.State    (MonadState (get), State, gets, modify)
import           Data.Foldable          (minimumBy)
import qualified Data.Map               as M
import           Lens.Micro             (Lens, Lens', lens, (^.))
import           Lens.Micro.TH          (makeLenses)
import           System.Posix.Internals (c_link)

data Client i
  = GovOrg
      { _identifier :: i
      , _name       :: String
      } -- i String
  | Company
      { _identifier :: i
      , _name       :: String
      , _person     :: Person
      , _duty       :: String
      } -- i String Person String
  | Individual
      { _identifier :: i
      , _person     :: Person
      } -- i Person
  deriving (Show)

data Person =
  Person
    { _firstName :: String
    , _lastName  :: String
    } -- String String
  deriving (Show)

-- firstName :: Lens' Person String
-- firstName = lens (\(Person f _) -> f) (\(Person _ l) newF -> Person newF l)
-- lastName :: Lens' Person String
-- lastName = lens (\(Person _ l) -> l) (\(Person f _) newL -> Person f newL)
-- person :: Lens' (Client i) Person
-- person =
--   lens
--     (\case
--        (Company _ _ p _) -> p
--        (Individual _ p) -> p
--        c -> error "Attempting to read `person` from ...")
--     (\client p ->
--        case client of
--          Company i n _ r -> Company i n p r
--          Individual i _  -> Individual i p
--          _ -> error "Attempting to write `person` to ...")
-- identifier :: Lens (Client i) (Client j) i j
-- identifier =
--   lens
--     (\case
--        (GovOrg i _) -> i
--        (Company i _ _ _) -> i
--        (Individual i _) -> i)
--     (\client newId ->
--        case client of
--          GovOrg _ n      -> GovOrg newId n
--          Company _ n p r -> Company newId n p r
--          Individual _ p  -> Individual newId p)
fullName :: Lens' Person String
fullName =
  lens
    (\(Person f l) -> f ++ " " ++ l)
    (\_ newFullName ->
       case words newFullName of
         f:l:_ -> Person f l
         _     -> error "Incorrect name")

makeLenses ''Client

makeLenses ''Person

class Ord v => Vector v where
  distance :: v -> v -> Double
  centroid :: [v] -> v

instance Vector (Double, Double) where
  distance :: (Double, Double) -> (Double, Double) -> Double
  distance (a, b) (c, d) = sqrt $ (c - a) ^ 2 + (d - b) ^ 2
  centroid :: [(Double, Double)] -> (Double, Double)
  centroid vs =
    let (u, v) = foldr (\(a, b) (c, d) -> (a + c, b + d)) (0, 0) vs
        n = fromIntegral $ length vs
     in (u / n, v / n)

class Vector v =>
      Vectorizable e v
  where
  toVector :: e -> v

data KMeansState v =
  KMeansState
    { centroids :: [v]
    , threshold :: Double
    , steps     :: Int
    }

newCentroids :: (Vector v, Vectorizable e v) => M.Map v [e] -> [v]
newCentroids = M.elems . fmap (centroid . map toVector)

clusterAssignments :: (Vector v, Vectorizable e v) => [v] -> [e] -> M.Map v [e]
clusterAssignments centrs points =
  let initialMap = M.fromList $ zip centrs (repeat [])
   in foldr
        (\p m ->
           let chosenC = minimumBy (compareDistance p) centrs
            in M.adjust (p :) chosenC m)
        initialMap
        points
  where
    compareDistance p x y =
      compare (distance x $ toVector p) (distance y $ toVector p)

kMeans' :: (Vector v, Vectorizable e v) => [e] -> State (KMeansState v) [v]
kMeans' points = do
  prevCentrs <- gets centroids
  let assignments = clusterAssignments prevCentrs points
      newCentrs = newCentroids assignments
  modify (\s -> s {centroids = newCentrs})
  modify (\s -> s {steps = steps s + 1})
  t <- gets threshold
  let err = sum $ zipWith distance prevCentrs newCentrs
  if err < t
    then return newCentrs
    else kMeans' points

main :: IO ()
main =
  let p = Person "John" "Smith"
      client = Individual 3 p
   in print $ client ^. person . firstName
