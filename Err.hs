module Failure where

-- Err Kleisli arrow example

-- "Why Do Monads Matter?"
-- London Haskell user group 24-Oct-2012
-- talk given by Derek Wright
-- based on blog post by Chris Smith
-- http://cdsmith.wordpress.com/2012/04/18/why-do-monads-matter/


-- Use division (by zero) as a function that
-- could produce an error.
-- (divBy 2) applied to 6 equals 3. 
divBy :: Integer -> Integer -> Err Integer
divBy 0 y = Error
divBy x y = OK (div y x)

-- An example including function composition
main = do
            print ((divBy 2 `composeErr` divBy 5) 2310) -- (2310 / 5) / 2
            print ((divBy 0 `composeErr` divBy 7) 2310) -- (2310 / 7) / 0 => Divide By Zero Error!!!
            print ((divBy 5 `composeErr` divBy 11) 2310) -- (2310 / 11) / 5

-- Add an Err data-type that can record Success / Failure
data Err a = OK a
           | Error -- only 1 type of error but there could be more
        deriving Show

-- Kleisli composition
-- Use a type like (f . g) x :: (b -> c) -> (a -> b) -> (a -> c)
-- but it is for Kleisli arrows (a -> Err b)
composeErr :: (b -> Err c) -> (a -> Err b) -> (a -> Err c)
composeErr f g x = case g x of          -- Apply g
                        OK y -> f y     -- and check for success
                        Error -> Error  -- or error

-- Kleisli identity
idErr :: a -> Err a
idErr x = OK x  -- Always success

-- Define join using composeErr
joinErr :: Err (Err a) -> Err a
joinErr = composeErr id id

-- Define bind using composeErr
bindErr :: Err a -> (a -> Err b) -> Err b
bindErr e f = (composeErr f id) e

-- Prove we have implemented a monad by defining an instance of the type-class
instance Monad Err where
    return = idErr
    (>>=) = bindErr
