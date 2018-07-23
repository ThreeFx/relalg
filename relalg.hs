{-# LANGUAGE TupleSections #-}

module RelAlg where

import Data.Function (on)
import Data.List (elemIndex, elemIndices, intersect, nub, transpose, (\\), sort)
import Data.Maybe (fromJust)

import Prelude hiding (join, (-), (*), (/), div)

import qualified Prelude

data Obj = S String
         | I Int
         | D Double
         | Null
         deriving (Show, Eq)

getS (S s) = s
getS x = error $ "Type error: Not a string: " ++ show x
getI (I i) = i
getI x = error $ "Type error: Not an integer: " ++ show x
getD (D d) = d
getD x = error $ "Type error: Not a double: " ++ show x

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
pi = project

get :: String -> [(String, Obj)] -> Obj
get name = snd . head . filter ((==name) . fst)
gs n = getS . get n
gi n = getI . get n
gd n = getD . get n

sigma :: ([(String, Obj)] -> Bool) -> Table -> Table
sigma f (Table header body) = Table header $ filter (f . zip header) body

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
  | not $ same t1 t2 = error $ "Cannot subtract different tables: " ++ show (h t1) ++ " and " ++ show (h t2)
  | otherwise = let Table h1 b1 = pack t1
                    Table h2 b2 = pack t2
                 in Table h1 $ b1 \\ b2
infixl 5 -
(-) = sub

div :: Table -> Table -> Table
div s t
  | not $ all (\x -> elem x (h s)) $ h t = error $ "Cannot divide " ++ show (h s) ++ " by " ++ show (h t)
  | otherwise = t1 - t2
  where
    otherCols = h s \\ h t
    t1 = project otherCols s
    t2 = project otherCols (t * t1 - s)
infixl 6 /
(/) = div

-- | Joins
infixr 9 .:
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.).(.)

cross :: Table -> Table -> Table
cross (Table h1 b1) (Table h2 b2) = Table (h1 ++ h2) $ (++) <$> b1 <*> b2
infixl 6 *
(*) = cross

join :: ([String] -> [Obj] -> Bool) -> Table -> Table -> Table
join f = select f .: cross

equijoin :: [String] -> Table -> Table -> Table
equijoin names t1 t2 = pack $ join (\header body -> all (\x -> all (head x==) x) $ map (map (body !!)) $ map (flip elemIndices header) names) t1 t2

naturaljoin :: Table -> Table -> Table
naturaljoin t1 t2 = equijoin (intersect (getHeader t1) (getHeader t2)) t1 t2
infixl 5 |><|
(|><|) = naturaljoin

-- | sub: outer joins
nullentry :: Table -> Table
nullentry = nulltable . getHeader

nulltable :: [String] -> Table
nulltable l = Table l [map (const Null) l]

leftjoin :: Table -> Table -> Table
leftjoin t s = (t |><| s) \/ (t - (project (h t) (t |><| s))) * (nulltable (sort $ h s \\ h t))
infixl 5 =|><|
(=|><|) = leftjoin

rightjoin :: Table -> Table -> Table
--rightjoin t s = (t |><| s) \/ (nulltable (sort $ h t \\ h s)) * (s - (project (h s) (t |><| s)))
rightjoin = flip leftjoin
infixl 5 |><|=
(|><|=) = rightjoin

outerjoin :: Table -> Table -> Table
outerjoin t1 t2 = t1 =|><| t2 \/ t1 |><|= t2
infixl 5 =|><|=
(=|><|=) = outerjoin

-- | Printing
pt = putStrLn . st
st :: Table -> String
st (Table header body) = sh lens header ++ "\n" ++ replicate (sum lens + 3 Prelude.* length lens + 1) '-' ++ "\n" ++ sb lens body
  where
    lens = zipWith max (map length header) $ map (maximum . map (length . s)) $ transpose body
sh lens header = ('|' :) $ concat $ zipWith (\l s -> " " ++ fill l s ++ " |") lens header
sb lens body = unlines $ map ('|' :) . map (\row -> concat $ zipWith (\l v -> " " ++ fill l (s v) ++ " |") lens row) $ body
fill :: Int -> String -> String
fill l s = s ++ replicate (l Prelude.- length s) ' '
s (S s) = s
s (I i) = show i
s (D d) = show d
s Null = "NULL"

cities = Table ["Name", "State"]
              [[S "Zurich",  S "ZH"]
              ,[S "Bern",    S "BE"]
              ,[S "Locarno", S "TI"]
              ,[S "Geneva",  S "GE"]]

cities2 = Table ["CityName", "State"]
              [[S "Zurich",  S "ZH"]
              ,[S "Chur",  S "GR"]]

stations = Table ["Name", "NoPlatforms", "CityName", "State"]
                [[S "HB",         I 44, S "Zurich",  S "ZH"]
                ,[S "Bern Bf",    I 1,  S "Bern",    S "BE"]
                ,[S "Locarno Bf", I 2,  S "Locarno", S "TI"]
                ,[S "Geneva Bf",  I 10, S "Geneva",  S "GE"]]

stations2 = Table ["Name", "NoPlatforms", "CityName", "State"]
                [[S "HB",   I 44, S "Zurich",  S "ZH"]
                ,[S "HB2",  I 10, S "Basel",   S "BA"]]

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

proj = Table ["Name", "Project"]
            [[S "Ben",    S "DMDB"]
            ,[S "Robin",  S "DMDB"]
            ,[S "Nicole", S "DMDB"]
            ,[S "Oli",    S "DMDB"]
            ,[S "Yann",   S "DMDB"]
            ,[S "Robin",  S "FMFP"]
            ,[S "Oli",    S "FMFP"]
            ,[S "Nicole", S "FMFP"]
            ,[S "Robin",  S "CN"]
            ,[S "Oli",    S "CN"]
            ,[S "Yann",   S "CN"]
            ,[S "Ben",    S "CN"]]

fmfpcn = Table ["Project"] [[S "FMFP"], [S "CN"]]
dmdbfmfp = Table ["Project"] [[S "DMDB"], [S "FMFP"]]
dmdbcn = Table ["Project"] [[S "DMDB"], [S "CN"]]
dmdbfmfpcn = Table ["Project"] [[S "DMDB"], [S "FMFP"], [S "CN"]]
