{-# LANGUAGE ScopedTypeVariables #-}
module Either where
import Control.Monad.Fail
import Data.Either
import Text.Read (readMaybe)
import System.Environment (getArgs)
import Data.Maybe ( fromMaybe )

data Either e a
    -- "e" is the error type
    -- "a" is the success type
    -- Maybe a

-- failIfOdd :: Int -> Either Int ()
-- failIfOdd n = if n `mod` 2 == 0 then Right () else Left n

failIfOdd :: Int -> Either Int ()
failIfOdd n = do
    -- if n `mod` 2 == 0 then Right () else Left n
    when (n `mod` 2 /= 0) $ Left n

instance Monad (Either e) where
    return = Right
    (Right a) >>= f = f a
    (Left e) >>= _  = Left e

when :: (Monad m) => Bool -> m () -> m ()
when True act = act
when False _  = return ()

failIfAnyOdd :: [Int] -> Either Int ()
failIfAnyOdd = mapM_ failIfOdd

maybeInt :: Maybe Int -> Int
maybeInt = fromMaybe 0

main :: IO()
main = do
    --line <- getArgs
    let ls = [3, 4, 6]
    let res0 = failIfAnyOdd ls
    -- let ml::[Maybe Int] = map readMaybe line
    -- let res = failIfAnyOdd $ map (maybeInt.readMaybe) line
    putStrLn $ show res0
