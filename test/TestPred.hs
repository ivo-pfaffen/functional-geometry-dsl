module Main (main) where
import System.Exit (exitFailure)
import Control.Monad (when)
import Pred (falla)

main :: IO ()
main = do
    putStrLn "Test suite not yet implemented."
    when falla exitFailure