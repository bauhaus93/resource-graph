import Factory

target_factory :: String
target_factory = "factorio"

main :: IO ()
main = case maybe_factory of
  Just factory -> (putStrLn . Factory.to_string) factory
  Nothing -> putStrLn ("Error: Factory of type '" ++ target_factory ++ "' is not known!")
  where
    maybe_factory = Factory.create_factory target_factory
