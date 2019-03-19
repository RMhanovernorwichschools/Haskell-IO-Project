{-
    Write and submit a Haskell program (distribution.hs) that computes and displays 
    the distribution of characters in a given sample of text.
    
    Output of your program should look like this:
    
    Please enter a string of text (the bigger the better): 
    The rain in Spain stays mainly in the plain.
    The distribution of characters in "The rain in Spain stays mainly in the plain." is:
    iiiiii
    nnnnnn
    aaaaa
    sss
    ttt
    ee
    hh
    ll
    pp
    yy
    m
    r
    
    Notice about this example:
    * The text: 'The rain ... plain' is provided by the user as input to your program.
    * Uppercase characters are converted to lowercase
    * Spaces and punctuation marks are ignored completely.
    * Characters that are more common appear first in the list.
    * Where the same number of characters occur, the lines are ordered alphabetically. 
      For example, in the printout above, the letters e, h, l, p and y both occur twice 
      in the text and they are listed in the output in alphabetical order.
    * Letters that do not occur in the text are not listed in the output at all.
-}
import Data.Char

count :: Eq a => a -> [a] -> Int
count a l = length [x| x<-l, x==a]

distrib :: [Char] -> [[Char]]
distrib l = [take (count x l) (repeat x) | x<- ['a'..'z'], count x l >0]

sectionByQuan :: [[a]] -> Int -> [[a]]
sectionByQuan xs s = [x | x<-xs, length x == s]

main = do
    putStrLn "Please enter a string of text (the bigger the better):"
    typed <- getLine
    let st = map toLower typed
    putStrLn "The distribution of characters in \"The rain in Spain stays mainly in the plain.\" is:"
    let m = maximum [length x | x<- (distrib st)] in
      mapM_ putStrLn (foldl1 (++) [sectionByQuan (distrib st) a | a<- [m,(m-1)..1]])