{-# LANGUAGE RecordWildCards #-}

module Maybe ( WeatherConditions(..)
             , wc
             , main
             )
where
import Data.AST

data WeatherConditions =
    WeatherConditions { temperature :: Maybe Int
                      , temperatureScale :: Maybe TemperatureScale
                      , weatherStatus :: Maybe String
                      }

-- temperature :: WeatherConditions -> Maybe Int

weather :: WeatherConditions -> Maybe String
weather wc =
    case wc of
        WeatherConditions
            { temperature = Just temp
            , temperatureScale = Just scale
            , weatherStatus = Just status
            }   -> Just $ "The current weather conditions are "++ status++ ", " ++ show temp ++ " degrees " ++ show scale
        WeatherConditions {..} -> Nothing

someOtherFunction :: Maybe String
someOtherFunction = Nothing

weather :: WeatherConditions -> Maybe String
weather wc = do
    temp <- temperature wc
    scale <- temperatureScale wc
    status <- weatherStatus wc
    return $ "The current weather conditions are "++ status++ ", " ++ show temp ++ " degrees " ++ show scale

wc = WeatherConditions
        { temperature = Just 53
        , temperatureScale = Just Fahrenheit
        , weatherStatus = Just $ show Clouds
        }

main :: IO()
main = do
    let Just v = weather wc
    putStrLn v
