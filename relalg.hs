{-# LANGUAGE TupleSections #-}

module RelAlg where

import Data.Function (on)
import Data.List (elemIndex, elemIndices, intersect, nub, transpose, (\\), sort)
import Data.Maybe (fromJust)

import Prelude hiding (join, (-), (*))

data Obj = S String
         | I Int
         | D Double
         | Null
         deriving (Show, Eq)

data Table = Table
             { getHeader :: [String]
             , getBody   :: [[Obj]]
             } deriving (Show, Eq)

h = nub . getHeader
b = nub . getBody

pack :: Table -> Table
pack t = project (sort . nub $ getHeader t) t

project :: [String] -> Table -> Table
project names (Table header body) = case body of
  [] -> Table names []
  _  -> Table names $ nub newCols
    where
      newCols = transpose $ map (transpose body !!) indices
      indices = map (fromJust . flip elemIndex header) names

select :: ([String] -> [Obj] -> Bool) -> Table -> Table
select f (Table header body) = Table header $ filter (f header) body

prefix :: String -> Table -> Table
prefix p table = rho (zip (getHeader table) $ map ((p++".")++) $ getHeader table) table

unprefix :: Table -> Table
unprefix table = rho (zip (getHeader table) $ map (drop 1 . snd . break (=='.')) $ getHeader table) table

-- | Renaming
replace a b = map (\x -> if x == a then b else x)

rho :: [(String, String)] -> Table -> Table
rho ren table = foldr (\f acc -> f acc) table $ map (uncurry rrho) ren

rrho :: String -> String -> Table -> Table
rrho oldName newName (Table header body) = Table (replace oldName newName header) body

-- | classic algebra
same :: Table -> Table -> Bool
same = (==) `on` (sort . getHeader)

union :: Table -> Table -> Table
union t1 t2
  | not $ same t1 t2 = error "Cannot append different tables"
  | otherwise = let Table h1 b1 = pack t1
                    Table h2 b2 = pack t2
                 in Table h1 . nub $ b1 ++ b2
infixl 3 \/
(\/) = union

intersectt :: Table -> Table -> Table
intersectt t1 t2
  | not $ same t1 t2 = error "Cannot intersect different tables"
  | otherwise = let Table h1 b1 = pack t1
                    Table h2 b2 = pack t2
                 in Table h1 . nub $ intersect b1 b2
infixl 4 /\
(/\) = intersectt

sub :: Table -> Table -> Table
sub t1 t2
  | not $ same t1 t2 = error "Cannot subtract different tables"
  | otherwise = let Table h1 b1 = pack t1
                    Table h2 b2 = pack t2
                 in Table h1 $ b1 \\ b2
(-) = sub

-- | Joins
infixr 9 .:
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.).(.)

cross :: Table -> Table -> Table
cross (Table h1 b1) (Table h2 b2) = Table (h1 ++ h2) $ (++) <$> b1 <*> b2
infixl 5 *
(*) = cross

join :: ([String] -> [Obj] -> Bool) -> Table -> Table -> Table
join f = select f .: cross

equijoin :: [String] -> Table -> Table -> Table
equijoin names t1 t2 = pack $ join (\header body -> all (\x -> all (head x==) x) $ map (map (body !!)) $ map (flip elemIndices header) names) t1 t2

naturaljoin :: Table -> Table -> Table
naturaljoin t1 t2 = equijoin (intersect (getHeader t1) (getHeader t2)) t1 t2
infixl 4 |><|
(|><|) = naturaljoin

-- | sub: outer joins
nullentry :: Table -> Table
nullentry = nulltable . getHeader

nulltable :: [String] -> Table
nulltable l = Table l [map (const Null) l]

leftjoin :: Table -> Table -> Table
leftjoin t s = (t |><| s) \/ (t - (project (h t) (t |><| s))) * (nulltable (sort $ h s \\ h t))
infixl 4 =|><|
(=|><|) = leftjoin

rightjoin :: Table -> Table -> Table
rightjoin t s = leftjoin s t
infixl 4 |><|=
(|><|=) = rightjoin

cities = Table ["Name", "State"]
              [[S "Zurich",  S "ZH"]
              ,[S "Bern",    S "BE"]
              ,[S "Locarno", S "TI"]
              ,[S "Geneva",  S "GE"]]

cities2 = Table ["Name", "State"]
              [[S "Zurich",  S "ZH"]
              ,[S "Chur",  S "GR"]]

stations = Table ["Name", "NoPlatforms", "CityName", "State"]
                [[S "HB",         I 44, S "Zurich",  S "ZH"]
                ,[S "Bern Bf",    I 1,  S "Bern",    S "BE"]
                ,[S "Locarno Bf", I 2,  S "Locarno", S "TI"]
                ,[S "Geneva Bf",  I 10, S "Geneva",  S "GE"]]

itinerary = Table ["ItNr", "Length", "StartStation", "DestinationStation"]
                 [[I 1, I 2, S "HB",        S "Geneva Bf"]
                 ,[I 2, I 3, S "HB",        S "Bern Bf"]
                 ,[I 3, I 5, S "Bern Bf",   S "Locarno Bf"]
                 ,[I 4, I 3, S "Bern Bf",   S "Geneva Bf"]
                 ,[I 5, I 6, S "HB",        S "Locarno Bf"]
                 ,[I 6, I 1, S "Geneva Bf", S "Locarno Bf"]]

connections = Table ["FromStation", "ToStation", "ItNr", "Departure", "Arrival"]
                   [[S "HB",        S "Geneva Bf" , I 1, I 1, I 1]
                   ,[S "HB",        S "Bern Bf"   , I 2, I 1, I 1]
                   ,[S "Bern Bf",   S "Locarno Bf", I 3, I 1, I 1]
                   ,[S "Bern Bf",   S "Geneva Bf" , I 4, I 1, I 1]
                   ,[S "HB",        S "Locarno Bf", I 5, I 1, I 1]
                   ,[S "Geneva Bf", S "Locarno Bf", I 6, I 1, I 1]]

t = rrho "Name" "CityName" cities
t2 = rrho "Name" "CityName" cities2
s = stations
