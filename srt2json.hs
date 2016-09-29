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
                    matchSrtList (deMonad res1) (deMonad res2) 3
      _ -> error "please pass one argument with the file containing the text to parse"

myTail [] = []
myTail xs = tail xs

readInt :: String -> Int
readInt = read


time2int strs = let     h = readInt (strs !! 0)
                        m = readInt (strs !! 1)
                        s = readInt (strs !! 2)
                in
                    3600*h + 60*m + s
matchSrtList ::
     (Eq a, Ord a) => (Show a) => (Show b) => (Show b1) =>(Show b2) =>(Show b3) =>(Show t) => (Show t1) => (Show t2) => (Show t3)  =>
    [(t, (([a], b), b1), [t1])]->[(t2, (([a], b2), b3), [t3])] -> Int ->IO ()
matchSrtList [] _ _ = putStr ""
matchSrtList _ [] _ = putStr ""
matchSrtList (x:xs) (x1:xs1) slideMax = do
                        -- print "---"
                        -- print (Data.List.length xs)
                        -- print (Data.List.length xs1)
                        print x
                        if matchSrt x  x1 then
                            do print "Match"
                               print x
                               print x1
                            else
                                if slideMax > 0 then
                                    do
                                        putStrLn ("Failed, splitting at " ++ (show x))
                                        matchSrtList (x:xs) xs1 (slideMax-1)
                                        matchSrtList xs (x1:xs1) (slideMax-1)
                                        putStr ""
                                else
                                        putStr ""
                        matchSrtList (myTail xs) (myTail xs1) ( slideMax - 1)

extractTime :: (t, (([a], b), b1), t1) -> [a]
extractTime (id1,times1,text1) =  fst (fst times1)

matchSrt :: (Eq a, Ord a) => (t, (([a], b), b1), t1) -> (t2, (([a], b2), b3), t3) -> Bool
matchSrt times1 times2 =
                                -- print (show ((fst (fst times1)) !! 1))
                                -- print (show ((fst (fst times2)) !! 1))
                                -- print (show (zip (fst (fst times1)) (fst (fst times2)) ))
                                -- print (cmpPairs (zip (fst (fst times1)) (fst (fst times2))))
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
                    Left val -> []
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

