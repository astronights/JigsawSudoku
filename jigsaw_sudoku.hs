{- |
Module      :  jigsawSudoku
Description :  Interactive Jigsaw Sudoku game for Final Project for COMP3258 Functional Programming 2019-2020
Author      :  Shubhankar Agrawal
UID         :  3035345306

Maintainer  :  shubhankar.a31@gmail.com
Stability   :  stable
Portability :  portable
-}


module Main where

import           Data.Char
import           Data.List
import           System.Directory
import           System.IO

type Puzzle = [(Int, Char)]
type Moves = [(Int, Char)]

main :: IO ()
main = do putStrLn "\n**********************************"
          putStrLn "*     Jigsaw Sudoku Puzzle       *"
          putStrLn "**********************************"
          putStrLn "\nOptions available:"
          putStrLn "(1) Load file"
          putStrLn "(2) Quit game"
          putStrLn "\nPlease enter the number for the option you'd like to pick"
          optionPicker [] [] []


optionPicker :: Puzzle -> Moves -> Puzzle -> IO ()
optionPicker puzzle moves solved = do hSetBuffering stdout NoBuffering
                                      putStr "Option> "
                                      option_picked <- getLine
                                      if length option_picked == 1 then
                                        case head option_picked of '1' -> do puzzle_new <- loadFile
                                                                             if null puzzle_new then
                                                                              displayOptions puzzle moves solved
                                                                             else displayOptions puzzle_new moves []
                                                                   '2' -> quitGame puzzle moves
                                                                   '3' -> do saveFile puzzle
                                                                             displayOptions puzzle moves solved
                                                                   '4' -> do displayPuzzle puzzle
                                                                             displayOptions puzzle moves solved
                                                                   '5' -> do (puzzle_new, moves_new) <- makeMove puzzle moves
                                                                             displayOptions puzzle_new moves_new solved
                                                                   '6' -> do (puzzle_new, moves_new) <- undoMove puzzle moves
                                                                             displayOptions puzzle_new moves_new solved
                                                                   '7' -> do (puzzle_new, moves_new) <- redoMove puzzle moves
                                                                             displayOptions puzzle_new moves_new solved
                                                                   '8' -> do puzzle_new <- solveSudoku puzzle moves solved
                                                                             displayPuzzle puzzle_new
                                                                             displayOptions puzzle_new moves puzzle_new
                                                                   '9' -> if not (checkWin solved) then
                                                                           do solved_puzzle <- solveSudoku puzzle moves solved
                                                                              if checkWin solved then
                                                                                do putStrLn "\nPuzzle not solvable..."
                                                                                   displayOptions puzzle moves solved_puzzle
                                                                              else do getHint puzzle solved_puzzle
                                                                                      displayOptions puzzle moves solved_puzzle
                                                                          else do getHint puzzle solved
                                                                                  displayOptions puzzle moves solved
                                                                   _   -> invalidOption puzzle moves solved
                                      else putStrLn "\nPlease enter a valid number..."

displayOptions :: Puzzle -> Moves -> Puzzle -> IO ()
displayOptions puzzle moves solved = do putStrLn "\nOptions available:"
                                        putStrLn "(1) Load file"
                                        putStrLn "(2) Quit game"
                                        putStrLn "(3) Save file"
                                        putStrLn "(4) Show puzzle"
                                        putStrLn "(5) Make a move"
                                        putStrLn "(6) Undo move"
                                        putStrLn "(7) Redo move"
                                        putStrLn "(8) Solve"
                                        putStrLn "(9) Hint"
                                        putStrLn "\nPlease enter the number for the option you'd like to pick"
                                        optionPicker puzzle moves solved

invalidOption :: Puzzle -> Moves -> Puzzle -> IO ()
invalidOption puzzle moves solved = do putStrLn "Sorry you chose an invalid option.."
                                       putStrLn "Please choose a valid option from the list..."
                                       displayOptions puzzle moves solved


------------------------------------------------------------
-------------------- basic requirements --------------------

loadFile :: IO Puzzle
loadFile = do putStr "\nPlease enter the file name> "
              file_name <- getLine
              exists <- doesFileExist file_name
              if exists then
                do putStrLn "\nLoading file..."
                   contents <- readFile file_name
                   putStrLn "\nLoaded file..."
                   putStrLn "\nReading data..."
                   let puzzle = parseContents contents
                   putStrLn "\nFinished reading data..."
                   putStrLn "\nLoaded puzzle..."
                   displayPuzzle puzzle
                   return puzzle
               else do putStrLn "\nInvalid file name...\nDoesn't exist..."
                       return []

saveFile :: Puzzle -> IO ()
saveFile puzzle = do putStr "\nPlease enter the file name> "
                     file_name <- getLine
                     putStrLn "\nSaving to file..."
                     let size = puzzleSize puzzle
                     let contents = convertFileFormat size (map (intToDigit .fst) puzzle) ++ ['\n'] ++ convertFileFormat size (map snd puzzle)
                     writeFile file_name contents
                     putStrLn "\nSaved to file..."

makeMove :: Puzzle -> Moves -> IO (Puzzle, Moves)
makeMove puzzle moves = do putStr ("\nRow where you'd like to insert (1-"++ [intToDigit (puzzleSize puzzle)] ++ ")> ")
                           row <- getLine
                           putStr ("\nColumn where you'd like to insert (1-"++ [intToDigit (puzzleSize puzzle)] ++ ")> ")
                           col <- getLine
                           putStr ("\nNumber you'd like to enter (1-"++ [intToDigit (puzzleSize puzzle)] ++ ")> ")
                           num <- getLine
                           let temp = map intToDigit [1..(puzzleSize puzzle)]
                           if length row == 1 && head row `elem` temp &&
                              length col == 1 && head col `elem` temp &&
                              length num == 1 && head num `elem` temp then
                             do let puzzle_new = addMoves puzzle moves (puzzleSize puzzle * (digitToInt (head row) - 1) + (digitToInt (head col) - 1), head num)
                                if verifyAdd puzzle puzzle_new moves (digitToInt (head row) -1) (digitToInt (head col) - 1) then
                                  do putStrLn "\nThe move has been made..."
                                     displayPuzzle puzzle_new
                                     let moves_new = [(puzzleSize puzzle * (digitToInt (head row) - 1) + (digitToInt (head col) - 1), snd (puzzle !! (puzzleSize puzzle * (digitToInt (head row) - 1) + (digitToInt (head col) - 1))))] ++ moves
                                     if checkWin puzzle_new then
                                       do putStrLn "\nCongratulations! You have solved the puzzle!"
                                          return (puzzle_new, moves_new)
                                     else return (puzzle_new, moves_new)
                                else do putStrLn "\nYou have entered an invalid move...\nPlease try again..."
                                        return (puzzle, moves)
                           else do putStrLn "\nYou have entered numbers outside the range...\nPlease try again..."
                                   return (puzzle, moves)


quitGame :: Puzzle -> Moves -> IO ()
quitGame _ _ = putStrLn "Thank you for playing with us!"


displayPuzzle :: Puzzle -> IO ()
displayPuzzle puzzle = do putStrLn "\nPrinting jigsaw sudoku..."
                          let display = buildDisplay puzzle [] 0 (puzzleSize puzzle)
                          putStrLn display


undoMove :: Puzzle -> Moves -> IO (Puzzle, Moves)
undoMove puzzle moves = do putStrLn "\nUndoing move..."
                           if not (null moves) && head moves /= (-1, '#') then
                             do let new_puzzle = remMoves puzzle (head moves)
                                putStrLn "\nMove undone..."
                                displayMoves (updateMoves puzzle moves 0)
                                displayPuzzle new_puzzle
                                return (new_puzzle, updateMoves puzzle moves 0)
                           else do putStrLn "\nNo moves to undo..."
                                   return (puzzle, moves)

redoMove :: Puzzle -> Moves -> IO (Puzzle, Moves)
redoMove puzzle moves = do putStrLn "\nRedoing move..."
                           if not (null moves) && ((-1, '#') `elem` moves) && last moves /= (-1,'#') then
                             do let new_puzzle = addMoves puzzle moves (last moves)
                                putStrLn "\nMove redone..."
                                displayMoves (updateMoves puzzle moves 1)
                                displayPuzzle new_puzzle
                                return (new_puzzle, updateMoves puzzle moves 1)
                           else do putStrLn "\nNo moves to redo..."
                                   return (puzzle, moves)

solveSudoku :: Puzzle -> Moves -> Puzzle -> IO Puzzle
solveSudoku puzzle moves solved = do putStrLn "\nLooking for potential solutions..."
                                     puzzle_new <- resetPuzzle puzzle moves
                                     if not (checkWin solved) then
                                       do let result = solveHelper puzzle_new
                                          if checkWin result then
                                            do putStrLn "\nCongratulations, you have solved the puzzle"
                                               return result
                                          else do putStrLn "\nThe puzzle is unsolvable..."
                                                  return puzzle
                                     else do putStrLn "\nCongratulations, you have solved the puzzle"
                                             return solved

getHint :: Puzzle -> Puzzle -> IO ()
getHint puzzle solved = do putStrLn "\nObtaining hint values..."
                           putStr ("\nRow where you'd like hint (1-"++ [intToDigit (puzzleSize puzzle)] ++ ")> ")
                           row <- getLine
                           putStr ("\nColumn where you'd like hint (1-"++ [intToDigit (puzzleSize puzzle)] ++ ")> ")
                           col <- getLine
                           let temp = map intToDigit [1..(puzzleSize puzzle)]
                           if length row == 1 && head row `elem` temp &&
                              length col == 1 && head col `elem` temp then
                            do let valueAt = snd(puzzle !! ((digitToInt (head row) - 1)* puzzleSize puzzle + digitToInt (head col) - 1))
                               if valueAt /= '.' then
                                 if valueAt == snd(solved !! ((digitToInt (head row) - 1)* puzzleSize solved + digitToInt (head col) - 1)) then
                                   putStrLn "\nThe value you have is correct!"
                                 else do putStrLn "\nThe value you have entered is incorrect!"
                                         putStrLn ("\nHint for number at row: "++[head row]++" and column: "++[head col]++
                                                   " is "++[snd(solved !! ((digitToInt (head row) - 1)* puzzleSize solved + digitToInt (head col) - 1))])
                               else putStrLn ("\nHint for number at row: "++[head row]++" and column: "++[head col]++
                                         " is "++[snd(solved !! ((digitToInt (head row) - 1)* puzzleSize solved + digitToInt (head col) - 1))])
                           else putStrLn "\nYou have entered numbers outside the range...\nPlease try again..."


------------------------------------------------------------
-------------------- helper functions  --------------------


convertFileFormat :: Int -> String -> String
convertFileFormat _ [] = []
convertFileFormat n xs
  | length xs > n = take n xs ++ ['\n'] ++ convertFileFormat n (drop n xs)
  | otherwise = xs

parseContents :: String -> Puzzle
parseContents xs
  | size xs == length (split xs) = buildBoard (concat (take (size xs) (split xs)))
  | 2 * size xs == length (split xs) = placeNumbers (buildBoard (concat (take (size xs) (split xs)))) (concat (drop (size xs) (split xs)))
  | otherwise = []
  where
    size :: String -> Int
    size zs = length (takeWhile (/= '\n') zs)

placeNumbers :: Puzzle -> String -> Puzzle
placeNumbers puzzle = zip [fst x | x <- puzzle]

buildBoard :: String -> Puzzle
buildBoard = map (\x -> (digitToInt x, '.'))

splithelp :: String -> [String]
splithelp [] = [""]
splithelp (x:xs)
  | x == '\n' = "": splithelp xs
  | otherwise = (x:head (splithelp xs)):tail (splithelp xs)

split :: String -> [String]
split [] = []
split xs = filter (not . null) (splithelp xs)


buildDisplay :: Puzzle -> String -> Int -> Int -> String
buildDisplay puzzle xs i n
  | i == 0 = ['.'] ++ concatList puzzle n (i-1) (replicate n (replicate 3 '-')) ++ ['.'] ++ ['\n'] ++ buildDisplay puzzle xs (i+1) n
  | i == n*n = ' ' : snd (puzzle !! (i-1)) : ' ' : '|' : ['\n'] ++ ['\''] ++ concatList puzzle n (n+1) (replicate n (replicate 3 '-')) ++ ['\'']
  | i `mod` n == 0 = ' ' : snd (puzzle !! (i-1)) : ' ' : '|' : ['\n'] ++ pickSep n (n*n) (n*n) (fst $ puzzle !! (i-n)) (fst $ puzzle !! i) ++
   insertLine puzzle n ((i-1) `div` n) ++ pickSep n (n*n) (n*n) (fst $ puzzle !! (i-1)) (fst $ puzzle !! (i+(n-1))) ++ ['\n'] ++ buildDisplay puzzle xs (i+1) n
  | i `mod` n == 1 = '|' : ' ' : snd (puzzle !! (i-1)) : ' ' : insertWall puzzle i : buildDisplay puzzle xs (i+1) n
  | otherwise = ' ' : snd (puzzle !! (i-1)) : ' ': insertWall puzzle i : buildDisplay puzzle xs (i+1) n

insertLine :: Puzzle -> Int -> Int -> String
insertLine puzzle n i = concatList puzzle n i (map (uncurry sep) (zip (map fst (slice (n*i) ((n*(i+1))-1) puzzle)) (map fst (slice (n*(i+1)) ((n*(i+2))-1) puzzle))))
  where
    sep :: Int -> Int -> String
    sep x y
      | x == y = "   "
      | otherwise = "---"

insertWall :: Puzzle -> Int -> Char
insertWall puzzle n
  | fst (puzzle !! (n-1)) /= fst (puzzle !! n) = '|'
  | otherwise = ' '

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

concatList :: Puzzle -> Int -> Int -> [String] -> String
concatList puzzle n i xss@(x:xs)
  | null x = ""
  | null xs = x
  | i < 0 = x ++ pickSep n i i (fst $ puzzle !! (n - length xss)) (fst $ puzzle !! ((n - length xss)+1)) ++ concatList puzzle n i xs
  | i > n = x ++ pickSep n i i (fst $ puzzle !! (n*(n-1) + (n - length xss))) (fst $ puzzle !! ((n*(n-1)+(n - length xss))+1)) ++ concatList puzzle n i xs
  | otherwise = x ++ pickSep n (fst $ puzzle !! (n*i + (n - length xss))) (fst $ puzzle !! (n*i + (n - length xss)+1))
    (fst $ puzzle !! (n*(i+1) + (n - length xss))) (fst $ puzzle !! (n*(i+1) + (n - length xss)+1))
   ++ concatList puzzle n i xs


pickSep :: Int -> Int -> Int -> Int -> Int -> String
pickSep n a b c d
  | a == n*n && b == n*n && c == d = ['|']
  | a == n*n && b == n*n = [':']
  | a == -1 && b == -1 && c == d = ['-']
  | a == -1 && b == -1 = ['.']
  | a == n+1 && b == n+1 && c == d = ['-']
  | a == n+1 && b == n+1 = ['\'']
  | a == b && c == -1 && d == -1 = [':']
  | c == -1 && d == -1 = ['|']
  | a == b && b == c && c == d = [' ']
  | a == b && c == d = ['-']
  | a == b = ['.']
  | c == d = ['\'']
  | a == c && b == d = ['|']
  | a == c || b == d = [':']
  | otherwise = [' ']

addMoves :: Puzzle -> Moves -> (Int,Char) -> Puzzle
addMoves puzzle moves (i,n)
  | i `notElem` [fst x | x <- takeWhile (\x -> x /= (-1, '#')) moves] = slice 0 (i-1) puzzle ++
                                                                        [(fst (puzzle !! i), n)] ++
                                                                        slice (i+1) (length puzzle) puzzle
  | otherwise = [(-1, '#')]

verifyAdd :: Puzzle -> Puzzle -> Moves -> Int -> Int -> Bool
verifyAdd og_puzzle puzzle moves r c
  | puzzle == [(-1, '#')] = False
  | (r* puzzleSize puzzle +c) `notElem` map fst moves && snd(og_puzzle !! (puzzleSize puzzle *r + c)) /= '.' = False
  | otherwise = checkRow puzzle (r* puzzleSize puzzle +c) &&
                checkCol puzzle (r* puzzleSize puzzle +c) &&
                checkBox puzzle (r* puzzleSize puzzle +c)

checkRow :: Puzzle -> Int -> Bool
checkRow puzzle i = allUnique [ y | ((_,y),z) <- zip puzzle [0..(length puzzle)], z `div` 9 == i `div` puzzleSize puzzle, y /= '.']

checkCol :: Puzzle -> Int -> Bool
checkCol puzzle i = allUnique [ y | ((_,y),z) <- zip puzzle [0..(length puzzle)], z `mod` 9 == i `mod` puzzleSize puzzle, y /= '.']

checkBox :: Puzzle -> Int -> Bool
checkBox puzzle i = allUnique [ y | ((x,y),_) <- zip puzzle [0..(length puzzle)], x == fst (puzzle !! i), y /= '.']

allUnique :: Ord a => [a] -> Bool
allUnique = all ( (==) 1 . length) . sg

sg :: Ord a => [a] -> [[a]]
sg = group . sort

puzzleSize :: Puzzle -> Int
puzzleSize puzzle = round (sqrt (fromIntegral (length puzzle)))

displayMoves :: Moves -> IO()
displayMoves = print

remMoves :: Puzzle -> (Int, Char) -> Puzzle
remMoves puzzle (i, n) = slice 0 (i-1) puzzle ++ [(fst (puzzle !! i), n)] ++ slice (i+1) (length puzzle) puzzle

updateMoves :: Puzzle -> Moves -> Int -> Moves
updateMoves puzzle moves n
  | n == 0 = if (-1, '#') `elem` moves then
                drop 1 moves++[(fst (head moves), snd (puzzle !! fst(head moves)))]
             else drop 1 moves++[(-1, '#')]++[(fst (head moves), snd (puzzle !! fst(head moves)))]
  | otherwise = (fst(last moves), snd (puzzle !! fst(last moves))) : init moves

checkWin :: Puzzle -> Bool
checkWin [] = False
checkWin puzzle = and [checkElem puzzle x | x <- puzzle]
  where
    checkElem :: Puzzle -> (Int, Char) -> Bool
    checkElem p (i, _) = [1..(puzzleSize p)] == sort [ digitToInt y | ((_,y),z) <- zip p [0..(length p)], z `div` puzzleSize puzzle == i `div` puzzleSize puzzle, y /= '.'] &&
                         [1..(puzzleSize p)] == sort [ digitToInt y | ((_,y),z) <- zip p [0..(length p)], z `mod` puzzleSize puzzle == i `mod` puzzleSize puzzle, y /= '.'] &&
                         [1..(puzzleSize p)] == sort [ digitToInt y | ((x,y),_) <- zip puzzle [0..(length puzzle)], x == fst (puzzle !! i), y /= '.']

resetPuzzle :: Puzzle -> Moves -> IO Puzzle
resetPuzzle puzzle moves = do (puzzle_new, moves_new) <- undoMove puzzle moves
                              if puzzle_new == puzzle && moves_new == moves then
                                return puzzle
                              else resetPuzzle puzzle_new moves_new

getPossibilities :: Puzzle -> Int -> [Int]
getPossibilities puzzle i =  listDifference [1..(puzzleSize puzzle)]
                            ([ digitToInt y | ((_,y),z) <- zip puzzle [0..(length puzzle)], z `div` puzzleSize puzzle == i `div` puzzleSize puzzle, y /= '.'] ++
                             [ digitToInt y | ((_,y),z) <- zip puzzle [0..(length puzzle)], z `mod` puzzleSize puzzle == i `mod` puzzleSize puzzle, y /= '.'] ++
                             [ digitToInt y | ((x,y),_) <- zip puzzle [0..(length puzzle)], x == fst (puzzle !! i), y /= '.'])

listDifference :: [Int] -> [Int] -> [Int]
listDifference a b = [x | x <- a, x `notElem` b]

solveHelper :: Puzzle -> Puzzle
solveHelper puzzle = solve puzzle 0 (getPossibilities puzzle 0)

solve :: Puzzle -> Int -> [Int] -> Puzzle
solve _ _ [] = []
solve puzzle i (x:xs)
  | i == length puzzle - 1 =  slice 0 (length puzzle - 2) puzzle ++
                                      [(fst (puzzle !! (length puzzle - 1)), intToDigit x)] ++
                                      slice (length puzzle) (length puzzle) puzzle
  | null solvedNext = solve puzzle i xs
  | otherwise = solvedNext
  where solveNext sudoku index = solve sudoku (nextBlank sudoku index) (getPossibilities sudoku (nextBlank sudoku index))
        solvedNext = solveNext (slice 0 (i-1) puzzle ++
                     [(fst (puzzle !! i), intToDigit x)] ++
                     slice (i+1) (length puzzle) puzzle) i

nextBlank:: Puzzle -> Int ->Int
nextBlank puzzle i
  | i == length puzzle - 1 = i
  | snd(puzzle!!(i+1)) =='.' = i +1
  | otherwise = nextBlank puzzle (i+1)

------------------------------------------------------------
------------------------------------------------------------
