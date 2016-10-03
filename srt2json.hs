--{-# LANGUAGE AllowAmbiguousTypes #-}

import System.Environment
import Text.Parsec
import Text.Parsec.ByteString
import Control.Monad
import qualified Data.ByteString as B
import Data.Text.Encoding
import qualified Data.Text as T
import Data.Text.Internal
import Text.JSON
import Data.List
import Data.ByteString.UTF8
import qualified Data.ByteString.Char8 as C

-- type UString = Data.Text.Internal.Text
type UString = String

main :: IO ()
main = do
    a <- getArgs
    print a
    case a of
      [str1, str2] -> do
                    res1 <- parseFromFile myParser str1
                    res2 <- parseFromFile myParser str2
		    printErr res1
		    printErr res2
                    matchSrtList (tuples2tuple(deMonad res1)) (tuples2tuple(deMonad res2)) 10
      _ -> error "please pass two arguments with the files containing the text to parse"



printErr result = case result of
                    Left val -> print (show val)
                    Right val  ->  putStr ""
third (_, _, x) = x
second (_, x, _) = x
first (x, _, _) = x

--tuples2tuple :: [(t, (([a], b), b1), t1)] -> (t, c, t1)
tuples2tuple :: [(UString, (([UString], UString), ([UString], UString)), [UString])] -> [(UString, Int, [UString])]
tuples2tuple [] = []
tuples2tuple (x:xs) = (first x, time2int (fst (fst (second x))), third (x)) : tuples2tuple xs

myTail [] = []
myTail xs = tail xs

readInt :: String -> Int
readInt = read


time2int strs = let     h = readInt (strs !! 0)
                        m = readInt (strs !! 1)
                        s = readInt (strs !! 2)
                in
                    3600*h + 60*m + s
--matchSrtList :: (Eq a, Ord a, Eq a0, Ord a0) => [(UString, Int, [UString])] -> [(UString, Int, [UString])] -> Int -> IO ()
matchSrtList [] _ _ = putStr ""
matchSrtList _ [] _ = putStr ""
matchSrtList (x:xs) (x1:xs1) slideMax =
                    let t1 = (second x) :: Int
                        t2 = (second x1) :: Int
                    in
                      do
                        if  (t1 :: Int) == (t2 :: Int) then
                            do print "Match"
                               print x
                               print x1
                               matchSrtList xs xs1 slideMax
                        else
                         do print "No match"
                            if t1 > t2 then
                                do
                                    print x1
                                    matchSrtList (x:xs) xs1 slideMax
                            else
                                do
                                    print x
                                    matchSrtList xs (x1:xs1) slideMax



extractTime :: (t, (([a], b), b1), t1) -> [a]
extractTime (id1,times1,text1) =  fst (fst times1)

matchSrt :: (Eq a, Ord a) => (t, (([a], b), b1), t1) -> (t2, (([a], b2), b3), t3) -> Bool
matchSrt times1 times2 =
                        if   (cmpTimes (extractTime times1) (extractTime  times2)) > 2 then
                            True
                        else
                            False

cmpTimes :: Eq a => [a] -> [a] -> Int
cmpTimes times1 times2 = Data.List.length (cmpPairs (zip  times1 times2))

cmpPairs :: Eq a => [(a, a)] -> [(a, a)]
cmpPairs [] = []
cmpPairs (x:xs) = if (fst x) == (snd x) then
                            x:cmpPairs xs
                          else
                            []

parseSTDIN :: IO ()
parseSTDIN = do
            c <- B.getContents
            putStr  (formatOutput (parse myParser "(stdin)" c))

formatOutput :: Either ParseError [(UString, (([UString], UString), ([UString], UString)), [UString])] -> UString
formatOutput result = case result of
                    Left val -> show val
                    Right val  ->  concatMap (\x ->encodeStrict x ++ "\n") val

deMonad result = case result of
                    Left val -> [ ]
                    Right val  ->  val

myParser :: Parser [(UString, (([UString], UString), ([UString], UString)), [UString])]
myParser = do
             optional bom
             blocks

bom = do
        oneOf [ '\xff', '\254', '\239' ]
        oneOf [ '\xff', '\254', '\187' ]
        oneOf [ '\xff', '\254', '\187', '\191' ]
        return ()

blocks = manyTill block eof

block = do
            id <- num
            myCrlf
            time <- timeLine
            t <- myText
            optional myCrlf
            return (id,time,t)

num = many1 digit

hms = do
        h <- num
        string ":"
        m <- num
        string ":"
        s <- num
        return [h,m,s]
--sepBy num (char ':')

time = do
        startHMS <- hms
        char ','
        startMilli <- many1 digit
        return (startHMS, startMilli)

timeLine = do
            start <- time
            string " --> "
            finish <- time
            myCrlf
            optional myCrlf
            return (start,finish)

myText = many1 textLine

textLine = do
            text <- many1 (noneOf "\r\n")
            optional myCrlf
            return (toString (C.pack text))

myCrlf = do
        optional cr
        newline

cr :: Parser ()
cr = do
        char '\r'
        return ()

