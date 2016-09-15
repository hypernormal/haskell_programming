{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Logger where

import Text.RawString.QQ
import Control.Applicative
import Text.Trifecta
import Text.Parser.LookAhead
import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as M

type Year = Integer
type Month = Integer
type Day = Integer
data Date = Date Year Month Day deriving (Eq, Ord, Show)

type Hour = Integer
type Minute = Integer
data Time = Time Hour Minute deriving (Eq, Ord, Show)
newtype Description = Description String deriving Show
data Entry = Entry Time Description deriving Show

data DailyLog = DailyLog Date [Entry] deriving Show
data Log = Map Date [Entry]

type LogFileString = String

parseDate :: Parser Date
parseDate =
  char '#' *> space *> liftA3 Date (integer <* char '-') (integer <* char '-') (integer <* skipToNextLine)

parseTime :: Parser Time
parseTime = liftA2 Time (integer <* char ':') integer

parseDescription :: Parser Description
parseDescription = do
  description <- manyTill anyChar (try (char '-' >> char '-') <|> char '\n')
  return $ Description description

parseEntry :: Parser Entry
parseEntry = liftA2 Entry parseTime parseDescription

parseToNextLog :: Parser Char
parseToNextLog = manyTill anyChar (lookAhead $ string "\n#") *> newline

parseDailyLog :: Parser DailyLog
parseDailyLog = do
  try parseToNextLog
  date <- parseDate
  entries <- many parseEntry
  return $ DailyLog date entries

parseLog :: Parser [DailyLog]
parseLog = many parseDailyLog

skipToNextLine :: Parser String
skipToNextLine = manyTill anyChar newline

logSample :: LogFileString
logSample = [r|
-- wheee a comment

# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]
