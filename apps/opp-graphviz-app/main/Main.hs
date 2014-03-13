import Prelude
-- import qualified Data.Yaml as Y
import Persist.Mongo.Settings
import Permissions.Onping.GraphVizWriter 

main :: IO () 
main = do
  eConf <- readDBConf "config.yml"
  case eConf of 
    (Left s) -> errorMessage s
    (Right conf) -> do 
               main' conf 

main' :: MongoDBConf -> IO () 
main' mdbc = do 
  putStrLn "generatig dot file..."
  writeGraphViz mdbc 
  putStrLn "done generating dot file."
  putStrLn "run with $> dot autograph.gv -T svg -o <outputname>.svg" 
 


errorMessage :: String -> IO ()
errorMessage s  = do
	putStrLn "Error reading a config file."
	putStrLn $ "Error: " ++ s





