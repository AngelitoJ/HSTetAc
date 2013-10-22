-- ------------------------------------------------------------------------------------
--  Gaussian 2003 checkpointing file parser
--  Authors: Angel Alvarez, Felipe Zapata
--
-- data file consists on two lined header and one or more data blocks
-- every item in a line is separated by one or more spaces
-- There are single line datablocks 'S' or a multi line datablocks 'M'
-- All data block start with a one line header 
-- 'S' blocks contain a text label, a type field and a single value
-- 'M' blocks contain a text label, a type field and cardinality field
-- A data type filed consist one of 'I' , 'R' for for Integer data , real data etc...
-- A cardinality field consists of 'N='  and a integer representing amount of data
-- Integer values appear in lines, up to six values per line
-- Real values appear in lines up to five values per line

-- Some header types...
-- "Number of atoms                            I               26"
-- "Info1-9                                    I   N=           9"
-- "Nuclear charges                            R   N=          26"
-- "Int Atom Types                             I   N=          26"
-- ------------------------------------------------------------------------------------

module Gaussian where

import Data.Char
import Text.ParserCombinators.Parsec
-- import Text.Parsec.ByteString.Lazy



type Label = String

data Block = 
      IBlock Label Int [Integer]     -- A Block containing one or more integer values
    | RBlock Label Int [Double]      -- A block containing one or more real values
    | TBlock String                  -- A Block containing just unformatted text

showRblock :: String -> Int -> [Double] -> String
showRblock l i v 
    | i == 1 = "Real block : " ++ (show l) ++ " Element : " ++ (show v)
    | i <= 5 = "Real block : " ++ (show l) ++ " Elements : " ++ (show v)
    | otherwise = "Real block : " ++ (show l) ++ "\n\tElements(" ++ (show i) ++ "): " ++ (show v)

showIblock :: String -> Int -> [Integer] -> String
showIblock  l i v 
    | i == 1 = "Integer block : " ++ (show l) ++ " Element : " ++ (show v)
    | i <= 6 = "Integer block : " ++ (show l) ++ " Elements : " ++ (show v)
    | otherwise = "Integer block : " ++ (show l) ++ "\n\tElements(" ++ (show i) ++ "): " ++ (show v)

showTblock :: String -> String
showTblock l = "Text    block : " ++ (show l)

instance Show Block where
    show (IBlock l i v) = showIblock l i v
    show (RBlock l i v) = showRblock l i v
    show (TBlock l)     = showTblock l 



-- Note :
-- "Return" refers to lifting results in the proper monad
-- as most consumers are monadic, most producers just "return" 
-- data in the general sense


-- Parse a file and returns either an error string or a list of data blocks
parseGaussianCheckpoint :: String -> IO (Either ParseError [Block])
parseGaussianCheckpoint = parseFromFile gaussianChkParser

-- Parse Gaussian Checkpoint format
gaussianChkParser :: GenParser Char st [Block]
gaussianChkParser = do
    fileheader <- count 2 anyline      -- discard first two lines of header
    datablocks <- many1 goodDataBlock  -- parse at least one datablock
    eof
    return datablocks              -- 

-- Parse a text line as a whole into a string
anyline :: GenParser Char st String
anyline = manyTill anyChar newline     -- whatever we found till we hit a newline

--sanitize datablocks
goodDataBlock :: GenParser Char st Block
goodDataBlock = do
    candidate <- datablock
    isGood <- blockcheck candidate   -- check for sane blocks
    if isGood
       then return candidate
       else fail "Block cardinality is not good"  -- TODO: Just tell us what was the offeding block

-- Try to parse one of the known datablock types or give up
datablock :: GenParser Char st Block
datablock = do
    try singleblock    -- Is it a single block?
    <|> multiblock     -- can it be multi block?
    <|> whatever       -- Well, sure we can put it in a text block
    <?> "Cachis!"      -- Something gone wrong here we cannot not parse anything at all!

   -- Parse while text lines into TBlocks 
whatever :: GenParser Char st Block
whatever = do
    iFound <- anyline
    return $ TBlock iFound

-- Check block have proper number of elements
blockcheck :: Block -> GenParser Char st Bool
blockcheck (IBlock t n v) = return $ n == length v
blockcheck (RBlock t n v) = return $ n == length v
blockcheck (TBlock t) = return True


-- Try to parse single data block that contains a header and one datavalue somewhat spaced
singleblock :: GenParser Char st Block
singleblock = try $ do
    (blockLabel,valueType) <- simpleheader
    case valueType of
        'I' -> do
                skipMany1 space
                valueData <- integerNumber
                newline
                return $ IBlock blockLabel 1 [valueData]
        'R' -> do
                skipMany1 space
                valueData <- realNumber
                newline
                return $ RBlock blockLabel 1 [valueData]

-- Try to parse a multi block containing a header and two or more values across several lines (up to five or six elements every line)
multiblock :: GenParser Char st Block
multiblock = try $ do
    (blockLabel,valueType,valueNumber) <- multiheader
    case valueType of
        'I' -> do
                values              <- ivalues valueNumber
                return $ IBlock blockLabel valueNumber values           -- IBlocks store integer data
        'R' -> do
                values              <- rvalues valueNumber
                return $ RBlock blockLabel valueNumber values           -- RBlocks store real data

-- Parse simple block header consisting of a label, and a type
simpleheader :: GenParser Char st (String,Char)
simpleheader = do
    dlabel <- labelfield
    dtype  <- oneOf "IR"         -- data types are 'I'nteger or 'R'eal
    return $ (dlabel,dtype)

-- Parse a multi block header line consisting of a label, a type, and a cardinality
multiheader :: GenParser Char st (String,Char,Int)
multiheader = do
    dlabel <- labelfield
    dtype  <- oneOf "IR"         -- data types are 'I'nteger or 'R'eal
    spaces
    dcar   <- cardinality        -- How many values will have to parse?
    newline
    return $ (dlabel,dtype,dcar)

-- Parse the label (always 43 chars)
labelfield :: GenParser Char st String
labelfield = do
    result <- count 43 anyChar
    return $ result

-- Parse cardinality field as "N=    Integer"
cardinality :: GenParser Char st Int
cardinality = do
    string "N="
    spaces
    n <- integerNumber 
    return $ fromIntegral n -- we need just an Int

-- Parse an multi block consisting of n integer values up to six per line
ivalues :: Int -> GenParser Char st [Integer]
ivalues n = 
    if (n `rem` 6 ) == 0 
       then do
           result <- count (n `div` 6) ivaluesline    -- we need just an integral number of lines
           return $ concat result
       else do
           result1 <- count (n `div` 6) ivaluesline   -- we need an integral number of lines 
           result2 <- ivaluesline                     -- plus one more to get the rest of values
           return $ concat [concat result1, result2 ] -- we need to flatten the lists

-- Parse a line of integer values up to six
ivaluesline :: GenParser Char st [Integer]
ivaluesline = do
    idata <- manyTill ivalue newline
    return idata

-- A ivalue consists of one integer after one or more blanks
ivalue :: GenParser Char st Integer
ivalue = do
    simpleSpace
    result <- integerNumber
    return result

-- A rvalue datablock consists of n rvalues up to six per line
rvalues :: Int -> GenParser Char st [Double]
rvalues n = 
    if (n `rem` 5 ) == 0 
       then do
           result <- count (n `div` 5) rvaluesline
           return $ concat result
       else do
           result1 <- count (n `div` 5) rvaluesline
           result2 <- rvaluesline
           return $ concat [concat result1, result2 ]

-- Parse a line of rvalues up to six
rvaluesline :: GenParser Char st [Double]
rvaluesline = do
    rdata <- manyTill rvalue newline
    return rdata

-- A rvalue consists of one integer after one or more blanks
rvalue :: GenParser Char st Double
rvalue = do
    simpleSpace
    result <- realNumber
    return result

-- Parse oner or more blanks discarding them
simpleSpace :: GenParser Char st ()
simpleSpace = skipMany (satisfy isSpace)

-- Parse an integer as "-/+"{*,[0-9]}
integerNumber :: GenParser Char st Integer
integerNumber = int

-- Parse a real as "-"[0-9]"."{8,[0-9]}"E"["-","+"][0-9][0-9]
realNumber :: GenParser Char st Double
realNumber = do
    intPart  <- int             -- First a integer number
    fracPart <- option 0.0 fraction
    expoPart <- option 1.0 exponent'
    let basePart = case intPart >= 0 of
         True  -> fromInteger intPart + fracPart
         False -> fromInteger intPart - fracPart
    return (basePart * expoPart)

-- Parse a fractional part
fraction :: GenParser Char st Double
fraction = do
    { char '.' 
    ; digits <- many1 digit <?> "fraction"
    ; return (foldr op 0.0 digits)
    }
    <?> "fraction"
    where
        op d f    = (f + fromIntegral (digitToInt d))/10.0

exponent' :: GenParser Char st Double
exponent' = do
    { oneOf "eE"
    ; f <- sign
    ; e <- decimal <?> "exponent"
    ; return (power (f e))
    }
    <?> "exponent"
    where
        power e  | e < 0      = 1.0/power(-e)
                 | otherwise  = fromInteger (10^e)

-- Parse an integer number (a.k.a positive naturals on stack exchange forums...)
int :: GenParser Char st Integer
int = do
    f <- sign
    n <- nat
    return (f n)

-- Parse sign
sign :: GenParser Char st (Integer -> Integer)
sign =
    (char '-' >> return negate)
    <|> (char '+' >> return id)
    <|> return id

-- Parse a natural number
nat :: GenParser Char st Integer
nat = 
    zero              -- Just '0'  
    <|> zeroNumber    -- A number starting with zero
    <|> decimal       -- Just a number

zero :: GenParser Char st Integer
zero = do
    char '0'
    return 0

-- Parse a zero number 
zeroNumber :: GenParser Char st Integer
zeroNumber = do
    char '0'
    hexadecimal  -- Silly but someday we will need this
    <|> decimal  
    <?> "number"

-- Parse a number in base 16 computer geeks like this
hexadecimal :: GenParser Char st Integer
hexadecimal = do
    oneOf "xX"
    number 16 hexDigit

-- Parse a number in base 10
decimal :: GenParser Char st Integer
decimal = number 10 digit

-- Parse one or more digits into a suitable natural number (as a integer in base BaseDigit)
number :: Integer -> GenParser Char st Char -> GenParser Char st Integer
number base baseDigit = do
    digits <- many1 baseDigit
    let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
    seq n (return n)
