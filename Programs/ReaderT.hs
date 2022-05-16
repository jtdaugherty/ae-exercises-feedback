module ReaderT where
import Control.Monad.Reader
import Data.AST
import Reader ( wc
              , WeatherConditions(..)
              )

printConditionString :: ReaderT WeatherConditions IO ()
printConditionString = do
    weather <- ask

    -- If WeatherConditions had optional fields using Maybe:
    let maybeResult :: Maybe String
        maybeResult = do
            temp <- temperature weather
            scale <- temperatureScale weather
            status <- weatherStatus weather
            return $ "The current weather conditions are "++ show status++
                ", " ++ show temp ++ " degrees " ++ show scale)

        eitherResult :: Either Int String
        eitherResult = do
            ...

    case maybeResult of
        Nothing -> return ()
        Just msg -> liftIO $ putStrLn msg

main :: IO ()
main = runReaderT printConditionString wc
