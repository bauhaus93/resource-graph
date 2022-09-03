import App.Factory as Factory (Factory)
import Data.Aeson.Types (Value)
import Data.Maybe (Maybe, maybe)
import Data.Yaml
  ( ParseException,
    decode,
    decodeFileEither,
    parseEither,
    parseJSON,
  )
import Prelude
  ( Either (Left, Right),
    Float,
    IO,
    String,
    print,
    putStrLn,
    show,
    ($),
    (>>=),
  )

main :: IO ()
main = do
  file <- decodeFileEither "factorio.yml" :: IO (Either ParseException Factory)
  case file of
    Left parse_exception -> print parse_exception
    Right factory -> print factory
