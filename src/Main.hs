import           App.Factory                   as Factory
                                                ( Factory
                                                , drawFullRecipeGraph
                                                )
import           Data.Yaml                      ( ParseException
                                                , decodeFileEither
                                                )
import           Prelude                        ( ($)
                                                , Either(Left, Right)
                                                , IO
                                                , print
                                                , putStrLn
                                                )

main :: IO ()
main = do
  file <- decodeFileEither "factorio.yml" :: IO (Either ParseException Factory)
  case file of
    Left  parse_exception -> print parse_exception
    Right factory         -> putStrLn $ drawFullRecipeGraph factory
