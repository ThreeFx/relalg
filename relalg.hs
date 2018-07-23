{-# LANGUAGE TupleSections #-}

module RelAlg where

import Control.Applicative ((<$>), (<*>))
import Data.Function (on)
import Data.List (elemIndex, elemIndices, intersect, nub, transpose, (\\), sort)
import Data.Maybe (fromJust)

data Obj = ObjStr String
         | ObjInt Int
         | ObjDbl Double
         | Null
         deriving (Show, Eq)

data Table = Table
             { getHeader :: [String]
             , getBody   :: [[Obj]]
             } deriving (Show, Eq)


replace a b = map (\x -> if x == a then b else x)

pack :: Table -> Table
pack t = project (sort . nub $ getHeader t) t

project :: [String] -> Table -> Table
project names (Table header body) = Table names newCols
  where
    newCols = transpose $ map (transpose body !!) indices
    indices = map (fromJust . flip elemIndex header) names

select :: ([String] -> [Obj] -> Bool) -> Table -> Table
select f (Table header body) = Table header $ filter (f header) body

cross :: Table -> Table -> Table
cross (Table h1 b1) (Table h2 b2) = Table (h1 ++ h2) $ (++) <$> b1 <*> b2

prefix :: String -> Table -> Table
prefix p table = rho (zip (getHeader table) $ map ((p++".")++) $ getHeader table) table

rho :: [(String, String)] -> Table -> Table
rho ren table = foldr (\f acc -> f acc) table $ map (uncurry rrho) ren

rrho :: String -> String -> Table -> Table
rrho oldName newName (Table header body) = Table (replace oldName newName header) body

same :: Table -> Table -> Bool
same = (==) `on` (sort . getHeader)

union :: Table -> Table -> Table
union t1 t2
  | not $ same t1 t2 = error "Cannot append different tables"
  | otherwise = let Table h1 b1 = pack t1
                    Table h2 b2 = pack t2
                 in Table h1 . nub $ b1 ++ b2
(\/) = union

sub :: Table -> Table -> Table
sub t1 t2
  | not $ same t1 t2 = error "Cannot subtract different tables"
  | otherwise = let Table h1 b1 = pack t1
                    Table h2 b2 = pack t2
                 in Table h1 $ b1 \\ b2
(-) = sub

infixr 9 .:
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.).(.)

jjoin :: ([String] -> [Obj] -> Bool) -> Table -> Table -> Table
jjoin f = select f .: cross

equijoin :: [String] -> Table -> Table -> Table
equijoin names = jjoin (\header body -> (\x -> all (==head x) x) (map (body !!) $ concatMap (flip elemIndices header) names))

naturaljoin :: Table -> Table -> Table
naturaljoin t1 t2 = equijoin (intersect (getHeader t1) (getHeader t2)) t1 t2
(|><|) = naturaljoin

cities = Table ["Name", "State"]
              [[ObjStr "Zurich",  ObjStr "ZH"]
              ,[ObjStr "Bern",    ObjStr "BE"]
              ,[ObjStr "Locarno", ObjStr "TI"]
              ,[ObjStr "Geneva",  ObjStr "GE"]]

stations = Table ["Name", "NoPlatforms", "CityName", "State"]
                [[ObjStr "HB",         ObjInt 44, ObjStr "Zurich",  ObjStr "ZH"]
                ,[ObjStr "Bern Bf",    ObjInt 1,  ObjStr "Bern",    ObjStr "BE"]
                ,[ObjStr "Locarno Bf", ObjInt 2,  ObjStr "Locarno", ObjStr "TI"]
                ,[ObjStr "Geneva Bf",  ObjInt 10, ObjStr "Geneva",  ObjStr "GE"]]

itinerary = Table ["ItNr", "Length", "StartStation", "DestinationStation"]
                 [[ObjInt 1, ObjInt 2, ObjStr "HB",        ObjStr "Geneva Bf"]
                 ,[ObjInt 2, ObjInt 3, ObjStr "HB",        ObjStr "Bern Bf"]
                 ,[ObjInt 3, ObjInt 5, ObjStr "Bern Bf",   ObjStr "Locarno Bf"]
                 ,[ObjInt 4, ObjInt 3, ObjStr "Bern Bf",   ObjStr "Geneva Bf"]
                 ,[ObjInt 5, ObjInt 6, ObjStr "HB",        ObjStr "Locarno Bf"]
                 ,[ObjInt 6, ObjInt 1, ObjStr "Geneva Bf", ObjStr "Locarno Bf"]]

connections = Table ["FromStation", "ToStation", "ItNr", "Departure", "Arrival"]
                   [[ObjStr "HB",        ObjStr "Geneva Bf" , ObjInt 1, ObjInt 1, ObjInt 1]
                   ,[ObjStr "HB",        ObjStr "Bern Bf"   , ObjInt 2, ObjInt 1, ObjInt 1]
                   ,[ObjStr "Bern Bf",   ObjStr "Locarno Bf", ObjInt 3, ObjInt 1, ObjInt 1]
                   ,[ObjStr "Bern Bf",   ObjStr "Geneva Bf" , ObjInt 4, ObjInt 1, ObjInt 1]
                   ,[ObjStr "HB",        ObjStr "Locarno Bf", ObjInt 5, ObjInt 1, ObjInt 1]
                   ,[ObjStr "Geneva Bf", ObjStr "Locarno Bf", ObjInt 6, ObjInt 1, ObjInt 1]]
