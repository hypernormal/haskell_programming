module LearnParsers where

import Text.Trifecta
import Control.Applicative

stop :: Parser a
stop = unexpected "stop"

one = char '1'

two =  char '2'

three = char '3'

testParse :: Parser () -> IO ()
testParse p =
  print $ parseString p mempty "123"

pNL s =
  putStrLn ('\n' : s)

main = do
  -- pNL "one:"
  -- testParse one
  -- pNL "oneTwo:"
  -- testParse oneTwo
  -- pNL "oneTwoThreeStop:"
  -- testParse (one >> two >> three)
  pNL "oneTwoThree EOF:"
  testParse (one >> two >> three >> eof)
