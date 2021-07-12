
import Solver.SMT (solveFor, Session (..))

import Text.Read (readMaybe)
import System.Environment (getArgs)

main :: IO ()
main = do
  as <- getArgs
  n <- case as of
    []   -> fail "PROBLEM_NUMBER required."
    n:_  -> maybe (fail $ "fail to read number from argument" ++ n) return $ readMaybe n
  solveFor Main n
