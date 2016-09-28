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
main =
        do
            c <- B.getContents
            putStr  (formatOutput (parse myParser "(stdin)" c))

formatOutput :: Either ParseError [(UString, (([UString], UString), ([UString], UString)), [UString])] -> UString
formatOutput result = case result of
                    Left val -> show val
                    Right val  ->  concat (map (\x ->(encodeStrict x) ++ "\n") val)

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

hms = sepBy num (char ':')

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
        (char '\r')
        return ()

