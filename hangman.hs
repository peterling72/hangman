import Data.List
import System.Random
import Data.Char

main :: IO()
main = do
        file <- readFile  "words.txt"
        let arrayOfWords = (lines file)
        x <- randomRIO (0, (length arrayOfWords))
        let word = arrayOfWords !! x
        let displayWord = (createWord word (length word) "")
        startHangMan word displayWord
        --let loop = do 
               -- input <- getLine
               -- if (completeWord displayWord) == True
               --         then return()
               --         else loop
        --loop

createWord :: String -> Int -> String -> String
createWord _ 0 retWord = retWord
createWord word len retWord = createWord word (len-1) (retWord ++ "_")

lowercase :: String -> String
lowercase word = map toLower word

completeWord :: String-> Bool
completeWord "" = True
completeWord (x:xs)
        | x /= '_' = completeWord xs
        | otherwise = False

startHangMan :: String -> String-> IO() -- word -> wordWithUnderScores
startHangMan word displayWord
        | completeWord displayWord == True = putStrLn displayWord
        | otherwise = do
                putStrLn displayWord
                x <- getLine
                let input = (lowercase x)
                startHangMan word (applyLetter word displayWord input "" 0)

applyLetter :: String-> String ->String -> String -> Int -> String
applyLetter word displayWord input retWord incre
        | incre == (length word) = retWord
        | (displayWord !! incre) /= '_' = applyLetter word displayWord input (retWord ++ [(displayWord !! incre)]) (incre+1)
        | otherwise = if (word !! incre) == (input!!0) then (applyLetter word displayWord input (retWord ++ input) (incre+1)) else (applyLetter word displayWord input (retWord ++ "_") (incre +1))
       -- | (input !! 0) == (word !! incre) = applyLetter word input (retWord ++ input) (incre+1)
       -- | otherwise = applyLetter word input (retWord ++ "_") (incre+1)
