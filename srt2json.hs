{-# LANGUAGE OverloadedStrings #-}

import Text.Regex
import Database.HDBC 
import Database.HDBC.Sqlite3
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
type MyTime = Float

data SubTitle = SubTitle {  start :: Float,
                            end   :: Float,
                            talky  :: [UString]
                        }


main :: IO ()
main = do
    a <- getArgs
    print a
    case a of
      [str1, str2] -> do
                    conn <- connectSqlite3 "srt.sqlite"
                    void $ createDB conn
                    res1 <- tryParsers str1
                    res2 <- tryParsers str2
                    printErr res1
                    printErr res2
                    count <- mapM (\(x1, x2) -> do
                                        return(x1)
                        ) (matchSrtList (deMonad res1) (deMonad res2) 10)
                    if calcPercent count res1 res2 > 0.74 then
                      do
                        count <- mapM (\(x1, x2) -> do
                                            s1 <- addString conn $ unwords $ talky x1
                                            s2 <- addString conn $ unwords $ talky x2
                                            addLink conn s1 s2
                                            return(x1)
                            ) (matchSrtList (deMonad res1) (deMonad res2) 10)
                        putStr "Matched "
                        print $ Data.List.length (count)
                        putStr " of "
                        print $ Data.List.length (deMonad (res1))
                        print $ calcPercent count res1 res2
                    else
                      do
                        print $ calcPercent count res1 res2
                        print "Files did not match - perhaps they are from different recordings?"
                    -- total <- fromIntegral (Data.List.length (tuples2tuple (deMonad (res1)))) :: float
                    -- print $ counted / total

                    commit conn
                    disconnect conn
                    print "Done"
      _ -> error "please pass two arguments with the files containing the text to parse"

calcPercent a b c =  foo (Data.List.length a) (min (Data.List.length (deMonad (c))) (Data.List.length (deMonad (b))))
foo :: Int -> Int -> Float
foo a b = (fromIntegral a) / (fromIntegral b)

-- foldCount = fold (1 +) 0 

-- To translate:
-- select * from test where id=(select b from links where a=(select id from test where str='ll ne fallait pas.'));

createDB :: Connection -> IO ()
createDB conn =
  do
    run conn "CREATE TABLE IF NOT EXISTS links (id INTEGER PRIMARY KEY,  a INTEGER, b INTEGER)" []
    run conn "CREATE UNIQUE INDEX IF NOT EXISTS 'links_idx' ON links (a,b);" []
    run conn "CREATE TABLE IF NOT EXISTS strs (id INTEGER PRIMARY KEY, str TEXT UNIQUE)" []
    run conn "CREATE UNIQUE INDEX IF NOT EXISTS 'strings_idx' ON strs (str);" []
    -- "Cannot change into WAL mode from within a transaction"
    -- run conn "PRAGMA journal_mode = WAL;" []
    commit conn
    return ()

addLink :: Connection -> Integer -> Integer -> IO ()
addLink conn id1 id2 = do
  run conn "INSERT OR IGNORE INTO links (a,b) VALUES (?,?)" [toSql id1, toSql id2]
  run conn "INSERT OR IGNORE INTO links (b,a) VALUES (?,?)" [toSql id1, toSql id2]
  commit conn
  return ()

addString :: Connection -> UString -> IO Integer
addString conn aStr = do
  r <- quickQuery conn "SELECT id from strs where str = ?" [toSql aStr]
  ret <- if Data.List.length r  < 1 then
            do
              run conn "INSERT OR IGNORE INTO strs (str) VALUES (?)" [toSql aStr]
              commit conn
              r <- quickQuery conn "SELECT id from strs where str = ?" [toSql aStr]
              if Data.List.length r  < 1 then
                do
                  print r
                  return (-1)
              else
                  return (fromSql ((r !! 0)!!0))
          else
            return (fromSql ((r !! 0)!!0))
  return ret

printErr result = case result of
                    Left val -> print (show val)
                    Right val  ->  putStr ""
myTail [] = []
myTail xs = tail xs

readInt :: String -> Int
readInt = read
readFloat :: String -> MyTime
readFloat = read

time2float (strs, millis) = let h = readFloat (strs !! 0) :: Float
                                m = readFloat (strs !! 1) :: Float
                                s = readFloat (strs !! 2) :: Float
                                mm= readFloat millis :: Float
                            in
                                3600*h + 60*m + s + (mm/1000)::Float


time2int strs = let     h = readInt (strs !! 0)
                        m = readInt (strs !! 1)
                        s = readInt (strs !! 2)
                in
                    3600*h + 60*m + s
--matchSrtList :: (Eq a, Ord a, Eq a0, Ord a0) => [(UString, Int, [UString])] -> [(UString, Int, [UString])] -> Int -> IO ()
matchSrtList :: [SubTitle] -> [SubTitle] -> Int -> [(SubTitle, SubTitle)]
matchSrtList [] _ _ = []
matchSrtList _ [] _ = []
matchSrtList (x:xs) (x1:xs1) slideMax =
                    let t1 = (start x) :: MyTime
                        t2 = (start x1) :: MyTime
                    in
                      do
                        --print ((t1 :: MyTime)- (t2 :: MyTime))
                        if  abs((t1 :: MyTime)- (t2 :: MyTime)) < 0.9 then
                            do
                               -- print "Match"
                               (x, x1) : matchSrtList xs xs1 slideMax
                        else
                         do
                            -- print "No match"
                            if t1 > t2 then
                                do
                                    -- Advance list 2
                                    matchSrtList (x:xs) xs1 slideMax
                            else
                                do
                                    -- Advance list 1
                                    matchSrtList xs (x1:xs1) slideMax



extractTime :: (t, (([a], b), b1), t1) -> [a]
extractTime (id1,times1,text1) =  fst (fst times1)

cmpPairs :: Eq a => [(a, a)] -> [(a, a)]
cmpPairs [] = []
cmpPairs (x:xs) = if (fst x) == (snd x) then
                            x:cmpPairs xs
                          else
                            []

parseSTDIN :: IO ()
parseSTDIN = do
            c <- B.getContents
            putStr  (formatOutput (parse srtParser "(stdin)" c))

--formatOutput :: Either ParseError [(UString, (([UString], UString), ([UString], UString)), [UString])] -> UString
formatOutput :: Either ParseError [SubTitle] -> UString
formatOutput result = case result of
                    Left val -> show val
                    Right val  ->  concatMap (\x ->encodeStrict (start x, end x , talky x) ++ "\n") val

deMonad result = case result of
                    Left val -> [ ]
                    Right val  ->  val

tryParsers fileName = do
                        result <- parseFromFile srtParser fileName
                        case result of
                            Right val  ->  return(result)
                            Left val -> do
				    -- print result
                                    res <- parseFromFile ssaParser fileName
                                    return res

ssaComment = do
    string ";"
    manyTill anyChar myCrlf

keyValLine = do
    many1 (noneOf ":\r\n") 
    char ':'
    many1 (noneOf "\r\n")
    myCrlf

crap = do
    -- (char '[')
    -- many1 ( noneOf "]")
    -- (char ']')
    myCrlf
    optional $ many ssaComment
    many1 keyValLine
    many1 myCrlf

ssaParser :: Parser [SubTitle]
ssaParser =
            do
                optional bom
                string "[Script Info]"
                crap
                string "[V4+ Styles]"
                crap
                string "[Events]"
                crap
                ssbs <- ssaBlocks
                return (ssbs)


-- srtParser :: Parser [(UString, (([UString], UString), ([UString], UString)), [UString])]
srtParser :: Parser [SubTitle]
srtParser = do
             optional bom
             blocks

bom = do
        oneOf [ '\xff', '\254', '\239' ]
        oneOf [ '\xff', '\254', '\187' ]
        oneOf [ '\xff', '\254', '\187', '\191' ]
        return ()


hms2float :: String -> Float
hms2float time_str
  = hours * 3600 + minutes * 60 + seconds /60
    where [hours, minutes, seconds] = map read $ splitRegex (mkRegex ":") time_str


ssaBlocks = do
            bs <- many1 ssaBlock 
            optional eof
            return bs

blocks = manyTill block eof

ssaField = do
            value <- many $ noneOf ","
            char ','
            return value

ssaBlock = do
            string "Dialogue: "
            fields <- count 9 ssaField
            tex <- manyTill anyChar myCrlf
            return $ SubTitle ( hms2float $ fields!!1) ( hms2float $ fields!!2) [ tex ]

block = do
            id <- num
            myCrlf
            time <- timeLine
            t <- myText
            optional myCrlf
            optional myCrlf
            return $ SubTitle (time2float (fst time)) (time2float (snd time)) t

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

myText = many textLine

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

