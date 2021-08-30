
import qualified Solver.SMT as Solver

import Options.Applicative
import System.IO
 (BufferMode(LineBuffering), hSetBuffering, stderr, stdout)
import System.Environment (lookupEnv)


data Options = Options
  { optSolver :: Solver.Options
  , optProblemNumber :: Int
  }
  deriving Show

optionsParser :: Parser Options
optionsParser = Options <$> solver <*> problemNumber
  where
    solver :: Parser Solver.Options
    solver = Solver.Options <$> threads <*> zeroDislikes <*> getBonus
    
    threads :: Parser (Maybe Word)
    threads = optional $ option auto $ mconcat
      [long "threads", metavar "NUM", help "number of Z3 threads"]

    zeroDislikes :: Parser Bool
    zeroDislikes = switch $ mconcat
      [long "zero-dislikes", help "find a solution with dislikes=0"]

    getBonus :: Parser [Int]
    getBonus = many $ option auto $ mconcat
      [long "get-bonus", metavar "NUM", help "get i-th bonuses (can be used multiple times)"]

    problemNumber :: Parser Int
    problemNumber = argument auto $ mconcat
      [metavar "NUM", help "problem number"]

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> optionsParser) fullDesc

main :: IO ()
main = do
  opt <- execParser parserInfo

  let f s = 
        case Solver.optThreads s of
          Just _ -> return s
          Nothing -> do
            m <- traverse readIO =<< lookupEnv "CUSTOM_Z3_THREADS"
            return $ s{ Solver.optThreads = m }
  solverOpt <- f (optSolver opt)

  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  Solver.solveFor solverOpt Solver.Main (optProblemNumber opt)
