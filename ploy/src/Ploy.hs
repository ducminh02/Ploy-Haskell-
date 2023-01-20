{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Ploy where  -- do NOT CHANGE export of module

import Board

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO CHANGE package.yaml, e.g.:
--       import Data.Char
import Data.Bits ( (.&.), (.|.), shift )
import qualified Data.Char as C


-- #############################################################################
-- ########################### GIVEN IMPLEMENTATION ############################
-- #############################################################################

data Move = Move {start :: Pos, target :: Pos, turn :: Int}

instance Show Move where
  show (Move (Pos startC startR) (Pos tarC tarR) tr) = [startC] ++ (show startR) ++ "-" ++ [tarC] ++ show tarR ++ "-" ++ show tr

instance Eq Move where
  (==) (Move (Pos sc1 sr1) (Pos tc1 tr1) r1) (Move (Pos sc2 sr2) (Pos tc2 tr2) r2) =
      sc1 == sc2 && sr1 == sr2 && tc1 == tc2 && tr1 == tr2 && r1 == r2

rotate :: Int -> Int -> Int
rotate o tr = (.&.) ((.|.) (shift o tr) (shift o (tr-8))) 255



-- #############################################################################
-- ####################### gameFinished :: Board -> Bool #######################
-- ####################### - 3 Functional Points         #######################
-- ####################### - 1 Coverage Point            #######################
-- #############################################################################



gameFinished :: Board -> Bool
gameFinished board
 | countCommando board < 2 = True
 | countWhiteBoard board < 2 = True
 | countBlackBoard board < 2 = True
 | otherwise = False

checkPieceCommando :: Cell -> Bool
checkPieceCommando Empty = False
checkPieceCommando (Piece _ i)
 | count1 (eightBitsCon(toBinary i)) == 4 = True
 | otherwise = False

countCommandoRow :: [Cell] -> Int
countCommandoRow [] = 0
countCommandoRow (x:xs)
 | checkPieceCommando x = 1 + countCommandoRow xs
 | otherwise = countCommandoRow xs

countCommando :: Board -> Int
countCommando = foldr ((+) . countCommandoRow) 0

checkPieceWhite :: Cell -> Bool
checkPieceWhite Empty = False
checkPieceWhite (Piece p _)
 | p == White = True
 |otherwise = False

checkPieceBlack :: Cell -> Bool
checkPieceBlack Empty = False
checkPieceBlack (Piece p _)
 | p == Black = True
 | otherwise = False

countWhiteRow :: [Cell] -> Int
countWhiteRow [] = 0
countWhiteRow (x:xs)
 | checkPieceWhite x = 1 + countWhiteRow xs
 | otherwise = countWhiteRow xs

countWhiteBoard :: Board -> Int
countWhiteBoard = foldr ((+). countWhiteRow) 0

countBlackRow :: [Cell] -> Int
countBlackRow [] = 0
countBlackRow (x:xs)
 | checkPieceBlack x = 1 + countBlackRow xs
 | otherwise = countBlackRow xs

countBlackBoard :: Board -> Int
countBlackBoard = foldr ((+). countBlackRow) 0



-- #############################################################################
-- ################### isValidMove :: Board -> Move -> Bool ####################
-- ################### - 5 Functional Points                ####################
-- ################### - 1 Coverage Point                   ####################
-- #############################################################################

isValidMove :: Board -> Move -> Bool
isValidMove board move@(Move{start = start, target = finish, turn = t})
 | findCell board start == Empty = False -- Empty Startpoint
 | checkAllyInWay board start (line start finish) = False -- Ally in the way
 | pieceMovCap (findCell board start) < length (line start finish) -1 = False -- Cant move that far
 | typeofMovement move `notElem` pieceMovDirection (findCell board start) = False -- go in the wrong direction
 | t < 0 || t > 7 = False -- wrong nums of turn
 | not (findCell board start == Piece White 16 || findCell board start == Piece Black 1) && t > 0 && length (line start finish) > 1= False -- turn and move when not shield
 | otherwise = True


checkAllyInWay :: Board -> Pos -> [Pos] -> Bool
checkAllyInWay board start (_:xs)
 | checkPieceWhite (findCell board start) && countWhiteRow (findCellRow board xs) > 0 = True
 | checkPieceBlack (findCell board start) && countBlackRow (findCellRow board xs) > 0 = True
 | otherwise = False

findCell :: Board -> Pos -> Cell
findCell board (Pos{col = c, row = r}) = (board!!a)!! (C.ord c - 97)
  where a = case r of 9 -> 0
                      8 -> 1
                      7 -> 2
                      6 -> 3
                      5 -> 4
                      4 -> 5
                      3 -> 6
                      2 -> 7
                      1 -> 8

findCellRow :: Board -> [Pos] -> [Cell]
findCellRow board = map (findCell board)

startingBoard :: Board
startingBoard = [[Empty,Piece White 84,Piece White 41,Piece White 56,Piece White 170,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]]

typeofMovement :: Move -> String
typeofMovement (Move{start = start@(Pos{col = c1, row = r1}), target = finish@(Pos{col = c2, row = r2})})
 | start == finish = "Stay"
 | c1 == c2 && r2 > r1 = "Up"
 | c1 == c2 && r2 < r1 = "Down"
 | r1 == r2 && C.ord c2 > C.ord c1 = "Right"
 | r1 == r2 && C.ord c2 < C.ord c1 = "Left"
 | r2 > r1 && C.ord c2 < C.ord c1 = "Left-Up"
 | r2 > r1 && C.ord c2 > C.ord c1 = "Right-Up"
 | r2 < r1 && C.ord c2 > C.ord c1 = "Right-Down"
 | r2 < r1 && C.ord c2 < C.ord c1 = "Left-Down"
 | otherwise = "Invalid"

toBinary :: Int -> [Int]
toBinary 0 = [0]
toBinary n = toBinary ( n `quot` 2 ) ++ [ n `rem` 2 ]

eightBitsCon :: [Int] -> [Int]
eightBitsCon a@(_:xs)
 | length a > 8 = xs
 | length a == 8 = a
 | otherwise = eightBitsCon (0 : a)

directionList :: [Int] -> [String]
directionList xs
 | (xs!!7) == 1 = "Up" : directionList (replace xs 7 0)
 | (xs!!6) == 1 = "Right-Up" : directionList (replace xs 6 0)
 | (xs!!5) == 1 = "Right" : directionList (replace xs 5 0)
 | (xs!!4) == 1 = "Right-Down" : directionList (replace xs 4 0)
 | (xs!!3) == 1 = "Down" : directionList (replace xs 3 0)
 | (xs!!2) == 1 = "Left-Down" : directionList (replace xs 2 0)
 | (xs!!1) == 1 = "Left" : directionList (replace xs 1 0)
 | head xs == 1 = "Left-Up" : directionList(replace xs 0 0)
 | head xs == 0 = ["Stay"]


replace :: [a] -> Int -> a -> [a]
replace xs i e = case splitAt i xs of
  (before, _:after) -> before ++ e: after
  _ -> xs

count1 :: [Int] -> Int
count1 [] = 0
count1 (x:xs)
 | x == 1 = 1 + count1 xs
 | otherwise = count1 xs

movingCap :: [Int] -> Int
movingCap xs
 | count1 xs == 1 = 1
 | count1 xs == 2 = 2
 | count1 xs == 3 = 3
 | count1 xs == 4 = 1
 | otherwise = 0

pieceMovCap :: Cell -> Int
pieceMovCap Empty = 0
pieceMovCap (Piece _ i) = movingCap(eightBitsCon (toBinary i))

pieceMovDirection :: Cell -> [String]
pieceMovDirection Empty = []
pieceMovDirection (Piece _ i) = directionList(eightBitsCon (toBinary i))


-- #############################################################################
-- ################### possibleMoves :: Pos -> Cell -> [Move] ##################
-- ################### - 6 Functional Points                  ##################
-- ################### - 1 Coverage Point                     ##################
-- #############################################################################


rotatingPieces :: Pos -> Int -> [Move]
rotatingPieces pos a
 | a > 7 = []
 | otherwise = Move {start = pos, target = pos, turn = a} : rotatingPieces pos (a + 1)

lineBetter :: Pos -> String -> Int -> [Pos]
lineBetter Pos{col = c, row = r} direction steps
 | direction == "Up" = cleansedEdge (line Pos{col = c, row = r} Pos{col = c, row = r + steps})
 | direction == "Down" = cleansedEdge (line Pos{col = c, row = r} Pos{col = c, row = r - steps})
 | direction == "Right" = cleansedEdge (line Pos{col = c, row = r} Pos{col = C.chr(C.ord c + steps), row = r})
 | direction == "Left" = cleansedEdge (line Pos{col = c, row = r} Pos{col = C.chr(C.ord c - steps), row = r})
 | direction == "Left-Up" = cleansedEdge (line Pos{col = c, row = r} Pos{col = C.chr(C.ord c - steps), row = r + steps})
 | direction == "Right-Up" = cleansedEdge (line Pos{col = c, row = r} Pos{col = C.chr(C.ord c + steps), row = r + steps})
 | direction == "Left-Down" = cleansedEdge (line Pos{col = c, row = r} Pos{col = C.chr(C.ord c - steps), row = r - steps})
 | direction == "Right-Down" = cleansedEdge (line Pos{col = c, row = r} Pos{col = C.chr(C.ord c + steps), row = r - steps})
 | direction == "Stay" = []
 | otherwise = []

moreThanOneDirection :: Pos -> [String] -> Int -> [Pos]
moreThanOneDirection pos xs i
  = foldr (\ x -> (++) (lineBetter pos x i)) [] xs

makingMoves :: Pos -> [Pos] -> [Move]
makingMoves _ [] = []
makingMoves pos (x:xs)
 | pos == x = makingMoves pos xs
 | otherwise = Move {start = pos, target = x, turn = 0} : makingMoves pos xs

makingMovesShield :: Pos -> [Pos] -> Int -> [Move]
makingMovesShield _ _ 8 = []
makingMovesShield pos (x:xs) i
 | pos == x = makingMovesShield pos xs i
 | otherwise = Move {start = pos, target = x, turn = i} : makingMovesShield pos [x] (i+1)

checkPieceShield :: Cell -> Bool
checkPieceShield Empty = False
checkPieceShield (Piece _ i)
 | count1 (eightBitsCon (toBinary i)) == 1 = True
 | otherwise = False

possibleMoves :: Pos -> Cell -> [Move]
possibleMoves pos piece = if not (checkPieceShield piece) then rotatingPieces pos 1 ++ makingMoves pos (moreThanOneDirection pos (pieceMovDirection piece) (pieceMovCap piece)) else rotatingPieces pos 1 ++ makingMovesShield pos (moreThanOneDirection pos (pieceMovDirection piece) (pieceMovCap piece)) 0




-- #############################################################################
-- ############# IMPLEMENT listMoves :: Board -> Player -> [Move] ##############
-- ############# - 2 Functional Points                            ##############
-- ############# - 1 Coverage Point                               ##############
-- #############################################################################

listMoves :: Board -> Player -> [Move]
listMoves board p
 | gameFinished board = []
 | p == White = whiteMovesBoard board
 | p == Black = blackMovesBoard board
 | otherwise = []


whitePosRow :: [Cell] -> Int -> [Char] -- find col of White Position
whitePosRow [] _ = []
whitePosRow (x:xs) c
 | checkPieceWhite x = C.chr(c + 97) : whitePosRow xs (c + 1)
 | otherwise = whitePosRow xs (c + 1)

blackPosRow :: [Cell] -> Int -> [Char] -- find col of Black Position
blackPosRow [] _ = []
blackPosRow (x:xs) c
 | checkPieceBlack x = C.chr(c + 97) : blackPosRow xs (c + 1)
 | otherwise = blackPosRow xs (c + 1)

toPos :: [Char] -> Int -> [Pos]
toPos [] _ = []
toPos (x:xs) row = Pos{col = x, row = row} : toPos xs row

whitePosBoard :: Board -> Int -> [Pos]
whitePosBoard [] _ = []
whitePosBoard (x:xs) i = toPos (whitePosRow x 0) i ++ whitePosBoard xs (i-1)

blackPosBoard :: Board -> Int -> [Pos]
blackPosBoard [] _ = []
blackPosBoard (x:xs) i = toPos (blackPosRow x 0) i ++ blackPosBoard xs (i-1)

whiteMoves :: [Pos] -> Board -> [Move]
whiteMoves [] _ = []
whiteMoves (x:xs) board = cleansedMoves board (possibleMoves x (findCell board x)) ++ whiteMoves xs board

blackMoves :: [Pos] -> Board -> [Move]
blackMoves [] _ = []
blackMoves (x:xs) board = cleansedMoves board (possibleMoves x (findCell board x)) ++ blackMoves xs board

cleansedMoves :: Board -> [Move] -> [Move]
cleansedMoves _ [] = []
cleansedMoves board (x:xs)
 | isValidMove board x = x : cleansedMoves board xs
 | otherwise = cleansedMoves board xs

whiteMovesBoard :: Board -> [Move]
whiteMovesBoard board = whiteMoves (whitePosBoard board 9) board

blackMovesBoard :: Board -> [Move]
blackMovesBoard board = blackMoves (blackPosBoard board 9) board