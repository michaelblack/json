{-# LANGUAGE OverloadedStrings #-}
module JSON (Value, Number, json) where
import           Control.Applicative hiding ((<|>))
import           Data.List           (intercalate)
import           Data.Maybe
import           Data.Text           hiding (concat, count, intercalate, length,
                                      map)
import qualified Data.Text           as T
import           Prelude             hiding (null)
import           Text.Parsec
import           Text.Parsec.Text

-- A simple library for parsing a JSON string as specified by <http://www.json.org json.org>

-- | A JSON Value
data Value  = String Text  -- ^ A JSON String (note that it uses text to support utf-8) e.g \"some text\"
             |Number Number -- ^ A JSON Number (see below) e.g 1, 3.1415, 1e6
             |Object [(Text, Value)] -- ^ A JSON Object consisting of string keys and JSON Value values e.g. {\"postId\" : 1234, \"message\" : \"lol\"}
             |Array [Value] -- ^ A JSON Array e.g. [1,2,3]
             |Boolean Bool -- ^ A JSON boolean i.e. true or false
             |Null  -- ^ The JSON null vlaue

-- | A JSON Number, can either be an integer or a float
data Number = Integer Int
             |Float   Float


------- Show Instances -------
-- parsing a shown value should return the same value

instance Show Value where
  show (String text)   = show text
  show (Number number) = show number
  show (Object maps)   = showObject maps
  show (Array values)  = showArray values
  show (Boolean True)  = "true"
  show (Boolean False) = "false"
  show Null            = "null"

instance Show Number where
  show (Integer n) = show n
  show (Float   n) = show n


showObject :: [(Text, Value)] -> String
showObject vals = concat ["{", (intercalate "," . map pairify) vals, "}"]
  where pairify (k, v) = concat [show (unpack k), ":", show v]

showArray :: [Value] -> String
showArray vals = concat ["[", (intercalate "," . map show) vals, "]"]



------- Parsers -------

stringValue :: Parser Value
stringValue = String <$> stringParse

stringParse :: Parser Text
stringParse = between (char '"') (char '"') (T.concat <$> (many1 $ (textify <$> noneOf "\"\\") <|> escape))
  where escape :: Parser Text
        escape   = do char '\\'
                      code <- oneOf "\"\\/burnft"
                      rest <- if code == 'u' then hexCode else return ""
                      return $ cons '\\' (cons code rest)
        hexCode = pack <$> (count 4 $ digit <|> oneOf "AaBbCcDdEeFf")
        textify = flip cons ""

numberValue :: Parser Value
numberValue = Number <$> do minus  <- maybe False (const True) <$> optionMaybe (char '-')
                            first  <- many1 digit
                            second <- optionMaybe (char '.' *> many1 digit)
                            exp    <- optionMaybe (oneOf "eE" *> liftA2 (,) (optionMaybe $ oneOf "-+") (many1 digit))
                            if isNothing second && isNothing exp
                              then return $ makeInteger minus first
                              else return $ makeFloat   minus first second exp
  where makeInteger minus first            = Integer ((if minus then negate else id) (read first))
        makeFloat   minus first second exp = Float   ((if minus then negate else id)
                                                      (((read first) +
                                                       (maybe 0 decimalize  second)) *
                                                       (maybe 1 exponentize exp   )))
        decimalize  str         = (read str) * (recip  $ 10**(fromIntegral $ length str))
        exponentize (sign, str) = 10 ** ((maybe id sign2func sign) (read str))
        sign2func '+'           = id
        sign2func '-'           = negate


booleanValue :: Parser Value
booleanValue = Boolean <$> do b <- choice [string "true", string "false"]
                              return $ case b of
                                "true"  -> True
                                "false" -> False

objectValue :: Parser Value
objectValue = Object  <$>
              between (char '{') (char '}')
              ((liftA2 (,) ((spaces *> stringParse <* spaces) <* char ':') (spaces *> value <* spaces)) `sepBy` (char ','))

arrayValue :: Parser Value
arrayValue = Array <$>
             between (char '[') (char ']')
             ((spaces *> value <* spaces) `sepBy` (char ','))

nullValue :: Parser Value
nullValue = string "null" *> return Null

value :: Parser Value
value = choice [objectValue, arrayValue, stringValue, numberValue, booleanValue, nullValue]

------- The Main Parser -------
-- | 'json' parses a JSON string (though it must be a piece of Text)
--   e.g.
-- @
-- parse json "Whoops." (pack "[null, true, false]")
-- @
json :: Parser Value
json = spaces *> value <* spaces
