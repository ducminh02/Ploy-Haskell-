import Test.Hspec

import Board
    ( buildBoard,
      line,
      validateFEN,
      Board,
      Cell(Empty, Piece),
      Player(Black, White),
      Pos(Pos, row, col) )
import Ploy ( gameFinished, isValidMove, listMoves, Move(Move), possibleMoves )
import Data.List
-- #############################################################################
-- ########### YOUR UNIT TESTS                                    ##############
-- ########### Note: execute tests using "stack test ploy:units"  ##############
-- #############################################################################




main :: IO ()
main = hspec $ do
    testValidateFEN
    testBuildBoard
    testLine
    testGameFinished
    testIsValidMove
    testPossibleMoves
    testListMoves

testValidateFEN :: Spec
testValidateFEN = describe "Module Board: validateFEN ..." $ do
        it "fen has empty Board" $ do
            validateFEN ",,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,," `shouldBe` (True :: Bool)
        it "fen sample Board 1" $ do
            validateFEN ",,,,,,,,/,,,,w13,b56,,w19,b65/,b180,,w250,,,b170,,/,w43,,,b56,,w34,,/,,,,b54,,w57,,/,w177,,b74,,w54,,b77,/,,,b55,,w33,,,/,b13,,w67,,b67,,b98,/,w99,,w65,,b76,,b89," `shouldBe` (True :: Bool)
        it "fen sample Board 2" $ do
            validateFEN ",,w14,b53,b35,,w71,,/w63,,w177,,b52,,b62,,/,b52,,w63,,,b73,,/,w23,,w62,,w170,,,/,b179,,b170,,w42,,w63,/,b53,,b66,,b12,,w56,/,,,,,,,,/,,,,,,,,/,,w52,,,,,," `shouldBe` (True :: Bool)
        it "fen wrong number of commas" $ do
            validateFEN ",,,,,,,,/,,,,b89,,,,/,,b77,,,,/,,,,,,,,/,,,,,,,,/,w13,,,,,/,,,,,,,,/,,,,,,,,/,,,,,," `shouldBe` (False :: Bool)
        it "fen wrong number of slashes" $ do
            validateFEN ",,,,,,,,/,,,,,,,,/,,,,b54,,,,/,,,,,,,,/,,,,,,w13,,/,,,,,,,,/,,,,,,,," `shouldBe` (False :: Bool)
        it "fen invalide Figure simple" $ do
            validateFEN ",,,b12,,13,,,/,,,,,w55,,,/,,,,,w14,,,/,,,,,b54,,,/,,,a13,,b53,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,," `shouldBe` (False :: Bool)
        it "fen invalide Figure complex" $ do
            validateFEN ",,,,,w170,,,/,,,,b72,,,,/,,,b79,,,,,/,w13,,,,,,,/,,b32,,,,,,/,,,,,b05,,,/,,,,,,,,/,,,,,,,,/,,,,,,,," `shouldBe` (False :: Bool)

testBuildBoard :: Spec
testBuildBoard = describe "Module Board: buildBoard" $ do
    it "Build Sample Board 1" $ do
        buildBoard "w13,,,,,,,,b170/w170,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/b69,,,,,,,,w96" `shouldBe` [[Piece White 13,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Piece Black 170],[Piece White 170,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Piece Black 69,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Piece White 96]]
    it "Build default Board" $ do
        buildBoard ",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69," `shouldBe` [[Empty,Piece White 84,Piece White 41,Piece White 56,Piece White 170,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]] 

testLine :: Spec
testLine = describe "Module Board: line" $ do
    it "Movement: Up" $ do
         line Pos{col = 'e', row = 5} Pos{col = 'e', row = 9} `shouldBe` [Pos{col ='e', row =5}, Pos{col ='e', row =6}, Pos{col ='e', row =7},Pos{col ='e', row = 8}, Pos{col ='e', row =9}]
    it "Movement: Down" $ do
        line Pos{col = 'e', row = 5} Pos{col = 'e', row = 1} `shouldBe` [Pos {col = 'e', row = 5},Pos {col = 'e', row = 4},Pos {col = 'e', row = 3},Pos {col = 'e', row = 2},Pos {col = 'e', row = 1}]
    it "Movement: Right" $ do
        line Pos{col = 'e', row = 5} Pos{col = 'i', row = 5} `shouldBe` [Pos {col = 'e', row = 5},Pos {col = 'f', row = 5},Pos {col = 'g', row = 5},Pos {col = 'h', row = 5},Pos {col = 'i', row = 5}]
    it "Movement: Left" $ do
        line Pos{col = 'e', row = 5} Pos{col = 'a', row = 5} `shouldBe` [Pos {col = 'e', row = 5},Pos {col = 'd', row = 5},Pos {col = 'c', row = 5},Pos {col = 'b', row = 5},Pos {col = 'a', row = 5}]
    it "Movement: Diagonal Right Up" $ do
        line Pos{col = 'e', row = 5} Pos{col = 'i', row = 9} `shouldBe` [Pos {col = 'e', row = 5},Pos {col = 'f', row = 6},Pos {col = 'g', row = 7},Pos {col = 'h', row = 8},Pos {col = 'i', row = 9}]
    it "Movement: Diagonal Right Down" $ do
        line Pos{col = 'e', row = 5} Pos{col = 'i', row = 1} `shouldBe` [Pos {col = 'e', row = 5},Pos {col = 'f', row = 4},Pos {col = 'g', row = 3},Pos {col = 'h', row = 2},Pos {col = 'i', row = 1}]
    it "Movement: Diagonal Left Down" $ do
        line Pos{col = 'e', row = 5} Pos{col = 'a', row = 1} `shouldBe` [Pos {col = 'e', row = 5},Pos {col = 'd', row = 4},Pos {col = 'c', row = 3},Pos {col = 'b', row = 2},Pos {col = 'a', row = 1}]
    it "Movement: Diagonal Left Up" $ do
        line Pos{col = 'e', row = 5} Pos{col = 'a', row = 9} `shouldBe` [Pos {col = 'e', row = 5},Pos {col = 'd', row = 6},Pos {col = 'c', row = 7},Pos {col = 'b', row = 8},Pos {col = 'a', row = 9}]

testGameFinished :: Spec
testGameFinished = describe "Module Game: gameFinished" $ do
    it "Game Starting Board" $ do
        gameFinished [[Empty,Piece White 84,Piece White 41,Piece White 56,Piece White 170,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]] `shouldBe` (False :: Bool)
    it "Game Starting Board (2 Commander rotated)" $ do
        gameFinished [[Empty,Piece White 84,Piece White 41,Piece White 56,Piece White 85,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 85,Piece Black 131,Piece Black 146,Piece Black 69,Empty]] `shouldBe` (False :: Bool)
    it "White Commander fallen" $ do
        gameFinished [[Empty,Piece White 84,Piece White 41,Piece White 56,Empty,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]] `shouldBe` (True :: Bool)
    it "Black Commander fallen" $ do
        gameFinished [[Empty,Piece White 84,Piece White 41,Piece White 56,Piece White 170,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Empty,Piece Black 131,Piece Black 146,Piece Black 69,Empty]] `shouldBe` (True :: Bool)
    it "Sole Survivor: White Commander normal state (170)" $ do
        gameFinished [[Empty,Empty,Empty,Empty,Piece White 170,Empty,Empty,Empty,Empty],[Empty,Piece Black 13,Empty,Empty,Piece Black 65,Empty,Empty,Piece Black 24,Empty],[Empty,Empty,Empty,Piece Black 55,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Piece Black 170,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]] `shouldBe` (True :: Bool)
    it "Sole Survivor: Black Commander normal state (170)" $ do
        gameFinished [[Empty,Empty,Empty,Empty,Piece White 170,Empty,Empty,Empty,Empty],[Empty,Piece White 13,Empty,Empty,Piece White 65,Empty,Empty,Piece White 24,Empty],[Empty,Empty,Empty,Piece White 55,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Piece Black 170,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]] `shouldBe` (True :: Bool)
    it "Sole Survivor: White Commander rotated state (85)" $ do
        gameFinished [[Empty,Empty,Empty,Empty,Piece White 85,Empty,Empty,Empty,Empty],[Empty,Piece Black 13,Empty,Empty,Piece Black 65,Empty,Empty,Piece Black 24,Empty],[Empty,Empty,Empty,Piece Black 55,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Piece Black 85,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]] `shouldBe` (True :: Bool)
    it "Sole Survivor: Black Commander rotated (85)" $ do
        gameFinished [[Empty,Empty,Empty,Empty,Piece White 85,Empty,Empty,Empty,Empty],[Empty,Piece White 13,Empty,Empty,Piece White 65,Empty,Empty,Piece White 24,Empty],[Empty,Empty,Empty,Piece White 55,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Piece Black 85,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]] `shouldBe` (True :: Bool)

startingBoard :: Board
startingBoard = [[Empty,Piece White 84,Piece White 41,Piece White 56,Piece White 170,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]]

boardTestCommander :: Board
boardTestCommander = [[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Piece White 170,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]]

boardEmpty :: Board
boardEmpty = [[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]]


testIsValidMove :: Spec
testIsValidMove = describe "Modul Game: isValidMove" $ do
    it "Empty Piece as start point" $ do
        isValidMove startingBoard (Move (Pos 'a' 1) (Pos 'a' 2) 0) `shouldBe` (False :: Bool)
    it "Shield moves 1 step" $ do
        isValidMove startingBoard (Move (Pos 'e' 3) (Pos 'e' 4) 0) `shouldBe` (True :: Bool)
    it "Shield should not move 2 steps" $ do
        isValidMove startingBoard (Move (Pos 'e' 3) (Pos 'e' 5) 0) `shouldBe` (False :: Bool)
    it "Shield rotate 7 times" $ do
        isValidMove startingBoard (Move (Pos 'e' 3) (Pos 'e' 3) 7) `shouldBe` (True :: Bool)
    it "Shield should not rotate 8 times" $ do
        isValidMove startingBoard (Move (Pos 'e' 3) (Pos 'e' 3) 8) `shouldBe` (False :: Bool)
    it "Shield should not rotate -1 times" $ do
        isValidMove startingBoard (Move (Pos 'e' 3) (Pos 'e' 3) (-1)) `shouldBe` (False :: Bool)
    it "Probe moves 2 steps" $ do
        isValidMove startingBoard (Move (Pos 'c' 2) (Pos 'c' 4) 0) `shouldBe` (True :: Bool)
    it "Probe should not move 3 steps" $ do
        isValidMove startingBoard (Move (Pos 'g' 2) (Pos 'g' 5) 0) `shouldBe` (False :: Bool)
    it "Lancer moves 3 steps" $ do
        isValidMove startingBoard (Move (Pos 'b' 1) (Pos 'b' 4) 0) `shouldBe` (True :: Bool)
    it "Lancer should not move 4 steps" $ do
        isValidMove startingBoard (Move (Pos 'h' 1) (Pos 'h' 5) 0) `shouldBe` (False :: Bool)
    it "Lancer should not move and rotate" $ do
        isValidMove startingBoard (Move (Pos 'h' 1) (Pos 'h' 2) 1) `shouldBe` (False :: Bool)
    it "Shield moves in the wrong direction" $ do
        isValidMove startingBoard (Move (Pos 'd' 3) (Pos 'c' 3) 0) `shouldBe` (False :: Bool)
    it "Probe moves in the wrong direction" $ do
        isValidMove startingBoard (Move (Pos 'c' 2) (Pos 'b' 2) 0) `shouldBe` (False :: Bool)
    it "Lancer moves in the wrong direction" $ do
        isValidMove startingBoard (Move (Pos 'b' 1) (Pos 'a' 2) 0) `shouldBe` (False :: Bool)
    it "Probe tries to move over teammate" $ do
        isValidMove startingBoard (Move (Pos 'e' 2) (Pos 'e' 4) 0) `shouldBe` (False :: Bool)
    it "Shield moves 1 step then rotate" $ do
        isValidMove startingBoard (Move (Pos 'e' 3) (Pos 'e' 4) 7) `shouldBe` (True :: Bool)
    it "Commander moves 1 step" $ do
        isValidMove boardTestCommander (Move (Pos 'e' 5) (Pos 'f' 6) 0) `shouldBe` (True :: Bool)
    it "Commander should not move 2 steps" $ do
        isValidMove boardTestCommander (Move (Pos 'e' 5) (Pos 'g' 7) 0) `shouldBe` (False :: Bool)
    it "Commander moves in the wrong directions" $ do
        isValidMove boardTestCommander (Move (Pos 'e' 5) (Pos 'e' 6) 0) `shouldBe` (False :: Bool)

testPossibleMoves :: Spec
testPossibleMoves = describe "Module Game: possibleMoves" $ do
    
    it "Shield at e3 moves 1 step and rotates" $ do
        show (possibleMoves (Pos 'e' 3) (Piece Black 1)) `shouldBe` "[e3-e3-1,e3-e3-2,e3-e3-3,e3-e3-4,e3-e3-5,e3-e3-6,e3-e3-7,e3-e4-0,e3-e4-1,e3-e4-2,e3-e4-3,e3-e4-4,e3-e4-5,e3-e4-6,e3-e4-7]"
    it "Probe at c2 moves up to 2 steps or rotates" $ do
        show (possibleMoves (Pos 'c' 2) (Piece Black 3)) `shouldBe` "[c2-c2-1,c2-c2-2,c2-c2-3,c2-c2-4,c2-c2-5,c2-c2-6,c2-c2-7,c2-c3-0,c2-c4-0,c2-d3-0,c2-e4-0]"
    it "Lancer at h1 moves up to 3 steps or rotates (Edge case 1)" $ do
        show (possibleMoves (Pos 'h' 1) (Piece Black 69)) `shouldBe` "[h1-h1-1,h1-h1-2,h1-h1-3,h1-h1-4,h1-h1-5,h1-h1-6,h1-h1-7,h1-h2-0,h1-h3-0,h1-h4-0,h1-i1-0,h1-g1-0,h1-f1-0,h1-e1-0]"
    it "Lancer at b1 moves up to 3 steps or rotates (Edge case 2)" $ do
        show (possibleMoves (Pos 'b' 1) (Piece Black 69)) `shouldBe` "[b1-b1-1,b1-b1-2,b1-b1-3,b1-b1-4,b1-b1-5,b1-b1-6,b1-b1-7,b1-b2-0,b1-b3-0,b1-b4-0,b1-c1-0,b1-d1-0,b1-e1-0,b1-a1-0]"
    it "Lancer at b9 moves up to 3 steps or rotates (Edge case 3)" $ do
        show (possibleMoves (Pos 'b' 9) (Piece White 84)) `shouldBe` "[b9-b9-1,b9-b9-2,b9-b9-3,b9-b9-4,b9-b9-5,b9-b9-6,b9-b9-7,b9-c9-0,b9-d9-0,b9-e9-0,b9-b8-0,b9-b7-0,b9-b6-0,b9-a9-0]"
    it "Lancer at h9 moves up to 3 steps or rotates (Edge case 4)" $ do
        show (possibleMoves (Pos 'h' 9) (Piece White 84)) `shouldBe` "[h9-h9-1,h9-h9-2,h9-h9-3,h9-h9-4,h9-h9-5,h9-h9-6,h9-h9-7,h9-i9-0,h9-h8-0,h9-h7-0,h9-h6-0,h9-g9-0,h9-f9-0,h9-e9-0]"
    it "Commander at e1 moves 1 step or rotates (Edge case 5)" $ do
        show (possibleMoves (Pos 'e' 1) (Piece Black 170)) `shouldBe` "[e1-e1-1,e1-e1-2,e1-e1-3,e1-e1-4,e1-e1-5,e1-e1-6,e1-e1-7,e1-f2-0,e1-d2-0]"


hasDuplicates ::  [Move] -> Bool
hasDuplicates xs = length (nub xs) /= length xs


testListMoves :: Spec
testListMoves = describe "Module Game: ListMoves" $ do
    it "no Moves in empty Board" $ do
        listMoves boardEmpty White `shouldBe` []
    it "check for Duplicates in Moves" $ do
        hasDuplicates (listMoves startingBoard White) `shouldBe` (False :: Bool)
    it "List moves White" $ do
        show (listMoves startingBoard White) `shouldBe` "[b9-b9-1,b9-b9-2,b9-b9-3,b9-b9-4,b9-b9-5,b9-b9-6,b9-b9-7,b9-b8-0,b9-b7-0,b9-b6-0,b9-a9-0,c9-c9-1,c9-c9-2,c9-c9-3,c9-c9-4,c9-c9-5,c9-c9-6,c9-c9-7,c9-b8-0,c9-a7-0,d9-d9-1,d9-d9-2,d9-d9-3,d9-d9-4,d9-d9-5,d9-d9-6,d9-d9-7,e9-e9-1,e9-e9-2,e9-e9-3,e9-e9-4,e9-e9-5,e9-e9-6,e9-e9-7,f9-f9-1,f9-f9-2,f9-f9-3,f9-f9-4,f9-f9-5,f9-f9-6,f9-f9-7,g9-g9-1,g9-g9-2,g9-g9-3,g9-g9-4,g9-g9-5,g9-g9-6,g9-g9-7,g9-h8-0,g9-i7-0,h9-h9-1,h9-h9-2,h9-h9-3,h9-h9-4,h9-h9-5,h9-h9-6,h9-h9-7,h9-i9-0,h9-h8-0,h9-h7-0,h9-h6-0,c8-c8-1,c8-c8-2,c8-c8-3,c8-c8-4,c8-c8-5,c8-c8-6,c8-c8-7,c8-c7-0,c8-c6-0,d8-d8-1,d8-d8-2,d8-d8-3,d8-d8-4,d8-d8-5,d8-d8-6,d8-d8-7,d8-c7-0,d8-b6-0,e8-e8-1,e8-e8-2,e8-e8-3,e8-e8-4,e8-e8-5,e8-e8-6,e8-e8-7,f8-f8-1,f8-f8-2,f8-f8-3,f8-f8-4,f8-f8-5,f8-f8-6,f8-f8-7,f8-g7-0,f8-h6-0,g8-g8-1,g8-g8-2,g8-g8-3,g8-g8-4,g8-g8-5,g8-g8-6,g8-g8-7,g8-g7-0,g8-g6-0,d7-d7-1,d7-d7-2,d7-d7-3,d7-d7-4,d7-d7-5,d7-d7-6,d7-d7-7,d7-d6-0,d7-d6-1,d7-d6-2,d7-d6-3,d7-d6-4,d7-d6-5,d7-d6-6,d7-d6-7,e7-e7-1,e7-e7-2,e7-e7-3,e7-e7-4,e7-e7-5,e7-e7-6,e7-e7-7,e7-e6-0,e7-e6-1,e7-e6-2,e7-e6-3,e7-e6-4,e7-e6-5,e7-e6-6,e7-e6-7,f7-f7-1,f7-f7-2,f7-f7-3,f7-f7-4,f7-f7-5,f7-f7-6,f7-f7-7,f7-f6-0,f7-f6-1,f7-f6-2,f7-f6-3,f7-f6-4,f7-f6-5,f7-f6-6,f7-f6-7]"
    it "List moves Black" $ do
        show (listMoves startingBoard Black) `shouldBe` "[d3-d3-1,d3-d3-2,d3-d3-3,d3-d3-4,d3-d3-5,d3-d3-6,d3-d3-7,d3-d4-0,d3-d4-1,d3-d4-2,d3-d4-3,d3-d4-4,d3-d4-5,d3-d4-6,d3-d4-7,e3-e3-1,e3-e3-2,e3-e3-3,e3-e3-4,e3-e3-5,e3-e3-6,e3-e3-7,e3-e4-0,e3-e4-1,e3-e4-2,e3-e4-3,e3-e4-4,e3-e4-5,e3-e4-6,e3-e4-7,f3-f3-1,f3-f3-2,f3-f3-3,f3-f3-4,f3-f3-5,f3-f3-6,f3-f3-7,f3-f4-0,f3-f4-1,f3-f4-2,f3-f4-3,f3-f4-4,f3-f4-5,f3-f4-6,f3-f4-7,c2-c2-1,c2-c2-2,c2-c2-3,c2-c2-4,c2-c2-5,c2-c2-6,c2-c2-7,c2-c3-0,c2-c4-0,d2-d2-1,d2-d2-2,d2-d2-3,d2-d2-4,d2-d2-5,d2-d2-6,d2-d2-7,d2-c3-0,d2-b4-0,e2-e2-1,e2-e2-2,e2-e2-3,e2-e2-4,e2-e2-5,e2-e2-6,e2-e2-7,f2-f2-1,f2-f2-2,f2-f2-3,f2-f2-4,f2-f2-5,f2-f2-6,f2-f2-7,f2-g3-0,f2-h4-0,g2-g2-1,g2-g2-2,g2-g2-3,g2-g2-4,g2-g2-5,g2-g2-6,g2-g2-7,g2-g3-0,g2-g4-0,b1-b1-1,b1-b1-2,b1-b1-3,b1-b1-4,b1-b1-5,b1-b1-6,b1-b1-7,b1-b2-0,b1-b3-0,b1-b4-0,b1-a1-0,c1-c1-1,c1-c1-2,c1-c1-3,c1-c1-4,c1-c1-5,c1-c1-6,c1-c1-7,c1-b2-0,c1-a3-0,d1-d1-1,d1-d1-2,d1-d1-3,d1-d1-4,d1-d1-5,d1-d1-6,d1-d1-7,e1-e1-1,e1-e1-2,e1-e1-3,e1-e1-4,e1-e1-5,e1-e1-6,e1-e1-7,f1-f1-1,f1-f1-2,f1-f1-3,f1-f1-4,f1-f1-5,f1-f1-6,f1-f1-7,g1-g1-1,g1-g1-2,g1-g1-3,g1-g1-4,g1-g1-5,g1-g1-6,g1-g1-7,g1-h2-0,g1-i3-0,h1-h1-1,h1-h1-2,h1-h1-3,h1-h1-4,h1-h1-5,h1-h1-6,h1-h1-7,h1-h2-0,h1-h3-0,h1-h4-0,h1-i1-0]"
    it "List moves in a finished Game White" $ do
        listMoves boardTestCommander White `shouldBe` []
    it "List moves in a finished Game Black" $ do
        listMoves boardTestCommander Black `shouldBe` []