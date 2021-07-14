
import Solver.SMT (solveFor, Session (..))

import Data.Default.Class
import Text.Read (readMaybe)
import System.IO
 (BufferMode(LineBuffering), hSetBuffering, stderr, stdout)
import System.Environment (getArgs)

main :: IO ()
main = do
  as <- getArgs
  n <- case as of
    []   -> fail "PROBLEM_NUMBER required."
    n:_  -> maybe (fail $ "fail to read number from argument" ++ n) return $ readMaybe n

  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  solveFor def Main n
