
import App.Factory as Factory (Factory)
import Data.Maybe (Maybe, maybe)
import Data.Yaml (decode, decodeFileEither, parseJSON, parseEither, ParseException)
import Data.Aeson.Types(Value)
import Prelude (Either(Left, Right), Float, IO, String, putStrLn, print, show, ($), (>>=))

main :: IO ()
main = do
  file <- decodeFileEither "factorio.yml" :: IO(Either ParseException Factory)
  case file of
    Left parse_exception -> print parse_exception
    Right factory -> print factory
