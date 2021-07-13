import Z3.Monad (evalZ3, solverGetHelp)

main :: IO ()
main = putStr =<< evalZ3 solverGetHelp
