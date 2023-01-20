module Board where  -- do NOT CHANGE export of module

-- IMPORTS HERE
--import Data.List
import Data.List.Split
-- import qualified Data.Text as T
-- import Data.Array
-- import Text.Read
import qualified Data.Char as C
-- Note: Imports allowed that DO NOT REQUIRE TO CHANGE package.yaml, e.g.:



-- #############################################################################
-- ############# GIVEN IMPLEMENTATION                           ################
-- ############# Note: "deriving Show" may be deleted if needed ################
-- #############       Given data types may NOT be changed      ################
-- #############################################################################

data Player = Black | White deriving Show
data Cell = Piece Player Int | Empty deriving Show
data Pos = Pos { col :: Char, row :: Int } deriving Show
type Board = [[Cell]]

instance Eq Pos where
  (==) (Pos c1 r1) (Pos c2 r2) = c1 == c2 && r1 == r2

instance Eq Player where
  (==) Black Black = True
  (==) White White = True
  (==) _ _ = False

instance Eq Cell where
  (==) Empty Empty = True
  (==) (Piece p1 i1) (Piece p2 i2) = p1 == p2 && i1 == i2
  (==) _ _ = False

-- #############################################################################
-- ################# IMPLEMENT validateFEN :: String -> Bool ###################
-- ################## - 2 Functional Points                  ###################
-- ################## - 1 Coverage Point                     ###################
-- #############################################################################

countCom :: String -> Int
countCom [] = 0
countCom (x:xs)
 | x == ',' = 1 + countCom xs
 | otherwise = countCom xs

comCheck :: String -> Bool
comCheck str = countCom str == 8

comCheckList :: [String] -> Bool
comCheckList [] = True
comCheckList (x:xs)
 | not (comCheck x) = False
 | otherwise = comCheckList xs


validateFEN :: String -> Bool
validateFEN str
 | length (splitSlash str) /= 9 = False
 | not (comCheckList (splitSlash str)) = False
 | not (checkTransform (transform (splitSlash str))) = False
 | otherwise = True

transform :: [String] -> [String]
transform = concatMap commaSlash

checkTransform :: [String] -> Bool
checkTransform [] = True
checkTransform (x:xs)
 | not (legitFigure x) = False
 | otherwise = checkTransform xs

checkBorW :: Char -> Bool
checkBorW c
 | c == 'w' || c == 'b' = True
 | otherwise = False

splitSlash :: String -> [String]
splitSlash = wordsWhen (== '/')

commaSlash :: String -> [String]
commaSlash = wordsWhen (== ',')


legitFigure :: String -> Bool
legitFigure [] = False
legitFigure [_] = False
legitFigure (_:'0':_) = False
legitFigure (x:xs)
 | not (checkBorW x) = False
 | not (isNumber xs) = False
 | let a = read xs::Int in a > 255 || a <= 0 = False
 | otherwise = True

legitFigureRow :: [String] -> Bool
legitFigureRow [] = True
legitFigureRow (x:xs)
 | not (legitFigure x) = False
 | otherwise = legitFigureRow xs



isNumber :: String -> Bool
isNumber str =
    case reads str :: [(Double, String)] of
      [(_, "")] -> True
      _         -> False


wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

-- #############################################################################
-- ####################### buildBoard :: String -> Board #######################
-- ####################### - 2 Functional Points         #######################
-- ####################### - 1 Coverage Point            #######################
-- #############################################################################

buildBoard :: String -> Board
buildBoard str = readBoard (split12 str)

readBoard :: [[String]] -> Board
readBoard = map (map readCell)

readCell :: String -> Cell
readCell [] = Empty
readCell (x:xs)
 | x == 'b' = Piece Black (read xs :: Int)
 | x == 'w' = Piece White (read xs :: Int)
 | otherwise = Empty


split1 :: String -> [String]
split1 = splitOn "/"

split2 :: [String] -> [[String]]
split2 = map (splitOn ",")

split12 :: String -> [[String]]
split12 str = split2 (split1 str)





-- #############################################################################
-- ####################### line :: Pos -> Pos -> [Pos]  ########################
-- ####################### - 3 Functional Points        ########################
-- ####################### - 1 Coverage Point           ########################
-- #############################################################################

line :: Pos -> Pos -> [Pos]
line p1@(Pos {col = c1, row = r1}) p2@(Pos {col = c2, row = r2})
 | p1 == p2 = [p2]
 | c1 == c2 && r2 > r1 = toUp p1 p2
 | c1 == c2 && r2 < r1 = toDown p1 p2
 | r1 == r2 && C.ord c2 > C.ord c1 = toRight p1 p2
 | r1 == r2 && C.ord c2 < C.ord c1 = toLeft p1 p2
 | r2 > r1 && C.ord c2 < C.ord c1 = toDiagLU p1 p2
 | r2 > r1 && C.ord c2 > C.ord c1 = toDiagRU p1 p2
 | r2 < r1 && C.ord c2 > C.ord c1 = toDiagRD p1 p2
 | r2 < r1 && C.ord c2 < C.ord c1 = toDiagLD p1 p2
 | otherwise = []



toUp :: Pos -> Pos -> [Pos]
toUp p1@(Pos {col = c1, row = r1}) p2
 | p1 == p2 = [p2]
 | otherwise = p1 : toUp p1 {col = c1, row = r1 + 1} p2

toDown :: Pos -> Pos -> [Pos]
toDown p1@(Pos {col = c1, row = r1}) p2
 | p1 == p2 = [p2]
 | otherwise = p1 : toDown p1 {col = c1, row = r1 - 1} p2

toRight :: Pos -> Pos -> [Pos]
toRight p1@(Pos {col = c1}) p2
 | p1 == p2 = [p2]
 |otherwise = p1 : toRight p1 {col = C.chr (C.ord c1 + 1)} p2

toLeft :: Pos -> Pos -> [Pos]
toLeft p1@(Pos {col = c1}) p2
 | p1 == p2 = [p2]
 |otherwise = p1 : toLeft p1 {col = C.chr (C.ord c1 - 1)} p2

toDiagLU :: Pos -> Pos -> [Pos]
toDiagLU p1@(Pos {col = c1, row = r1}) p2
 | p1 == p2 = [p2]
 |otherwise = p1 : toDiagLU p1 {col = C.chr (C.ord c1 - 1) , row = r1 + 1} p2

toDiagRU :: Pos -> Pos -> [Pos]
toDiagRU p1@(Pos {col = c1, row = r1}) p2
 | p1 == p2 = [p2]
 |otherwise = p1 : toDiagRU p1 {col = C.chr (C.ord c1 + 1) , row = r1 + 1} p2

toDiagRD :: Pos -> Pos -> [Pos]
toDiagRD p1@(Pos {col = c1, row = r1}) p2
 | p1 == p2 = [p2]
 |otherwise = p1 : toDiagRD p1 {col = C.chr (C.ord c1 + 1) , row = r1 - 1} p2

toDiagLD :: Pos -> Pos -> [Pos]
toDiagLD p1@(Pos {col = c1, row = r1}) p2
 | p1 == p2 = [p2]
 |otherwise = p1 : toDiagLD p1 {col = C.chr (C.ord c1 - 1) , row = r1 - 1} p2

cleansedEdge :: [Pos] -> [Pos]
cleansedEdge [] = []
cleansedEdge (x:xs)
 | checkPost x = x : cleansedEdge xs
 | otherwise = cleansedEdge xs
 

checkPost :: Pos -> Bool
checkPost (Pos{ col = c, row = r})
 | r <= 0 || r > 9 || C.ord c < 97 || C.ord c > 105 = False
 | otherwise = True


