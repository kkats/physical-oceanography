--
-- | parse SAM index data
-- from
-- http://www.nerc-bas.ac.uk/icd/gjma/sam.html
--
module Oceanogr.SAM (readSAM) where

import Text.ParserCombinators.Parsec
import Data.Array

--
-- this is the file downloaded from the above website
--
dataSAM :: FilePath
dataSAM = "/data/pub/SAMI.dat"

--
-- parsers
--
samFile       :: GenParser Char st [[String]]
line          :: GenParser Char st [String]
nums, comment :: GenParser Char st String
eol           :: GenParser Char st Char
parseSAM      :: String -> Either ParseError [[String]]

samFile = endBy line eol
line    = (:[]) <$> comment <|> sepBy nums (many1 $ char ' ') -- (:[]) put in a list
nums    = many (oneOf "-.0123456789")
comment = char '%' >> skipMany (noneOf "\n") >> return ""
eol     = char '\n'

parseSAM = parse samFile "(unknown)"

---
--- Array
---
convSAM :: [[String]] -> Array (Int, Int) Double -- (year, month) SAM
convSAM p = array ((y1,1), (yN,12))
                  $ zip [(y, m) | y <- [y1 .. yN], m <- [1 .. 12]] sams
    where
        p' = filter ((> 1) . length) p
        years = map (fromIntegral . (read :: String -> Int) . head) p' :: [Int]
        y1 = foldl min 9999 years
        yN = foldl max (-9999) years
        sams = map read . concat $ map tail p' :: [Double]


readSAM :: IO (Array (Int, Int) Double)
readSAM = do
    c <- readFile dataSAM
    case parseSAM c of
        Left err -> error  $ show err
        Right p  -> return $ convSAM p
