import System.Environment
import Text.SSWC
import qualified Data.Text.IO as T

main :: IO ()
main = getArgs >>= dispatch

dispatch :: [String] -> IO ()
dispatch [] =   putStrLn "usage: sswc filename"
dispatch (fnm:_) = do
  edoc <- loadDocument fnm
  case edoc of 
    Right doc -> do Right ts <- loadTemplates doc
                    let newDoc = runTemplates ts doc
                    T.putStrLn $ renderDocument newDoc
    Left err -> putStrLn $ "error: "++err
