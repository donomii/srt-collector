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

-- type UString = Data.Text.Internal.Text
type UString = String

main :: IO ()
--main = do
    --a <- getArgs
    --case a of
      --[str] -> parseFromFile myParser str >>= either print print
      --_ -> error "please pass one argument with the file containing the text to parse"

main = do
    a <- getArgs
    case a of
      [str] -> do
            result <- parseFromFile myParser str
            putStr (case result of
                    Left val -> show val
                    Right val  ->  (concat(map (\x ->(encode x) ++ "\n") val)))
      _ -> error "please pass one argument with the file containing the text to parse"

--myParser :: Parser [(UString, (([UString], UString), ([UString], UString)), [UString])]
myParser = do
             b <- blocks
             return (b)

blocks = do
            bs <- manyTill block eof
            return bs

block = do
            id <- num
            void $ myCrlf
            time <- timeLine
            t <- myText
            void $ myCrlf
            return (id,time,t)


num :: Parser UString
num = do
    n <- many1 digit
    return (n)

hms = sepBy num (char ':')

time = do
        startHMS <- hms
        void $ char ','
        startMilli <- many1 digit
        return (startHMS, startMilli)
        
timeLine = do
            start <- time
            void $ string " --> "
            finish <- time
            void $ myCrlf
            return (start,finish)
myText = do
            t <- many1 textLine
            return (t)

textLine = do
            text <- many1 (noneOf "\r\n")
            void $ myCrlf
            return (text)

myCrlf = do
        optional cr
        newline

cr :: Parser ()
cr = do
        (char '\r')
        return ()
            
