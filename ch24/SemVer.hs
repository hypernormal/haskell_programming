module SemVer where

import Control.Applicative
import Text.Trifecta
import Data.Monoid

data NumberOrString =
    NOSS String
  | NOSI Integer
  deriving (Eq, Ord, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release Metadata deriving (Show)

instance Eq SemVer where
  (SemVer ma mi pa _ _) == (SemVer ma' mi' pa' _ _) =
    (ma == ma') && (mi == mi') && (pa == pa')

instance Ord SemVer where
  (SemVer ma mi pa _ _) `compare` (SemVer ma' mi' pa' _ _) =
    (ma `compare` ma') <> (mi `compare` mi') <> (pa `compare` pa')

parseNumOrString :: Parser NumberOrString
parseNumOrString = (NOSI <$> integer) <|> (NOSS <$> some letter)

parseReleaseOrMeta :: Parser [NumberOrString]
parseReleaseOrMeta = parseNumOrString `sepEndBy` char '.'

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- integer
  char '.'
  minor <- integer
  char '.'
  patch <- integer
  release <- option [] (char '-' >> parseReleaseOrMeta)
  meta <- option [] (char '+' >> parseReleaseOrMeta)
  return $ SemVer major minor patch release meta
