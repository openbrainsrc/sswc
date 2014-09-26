import System.Environment
import Text.SSWC

main :: IO ()
main = getArgs >>= dispatch

dispatch :: [String] -> IO ()
dispatch [] =   putStrLn "usage: sswc filename"
dispatch (fnm:_) = do
  loadFile fnm
  return ()
  
