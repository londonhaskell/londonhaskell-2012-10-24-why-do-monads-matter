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
-- Comment out because of error with composition since types don't line up now
-- main = do
--             print ((divBy 2 . divBy 5) 100) -- (2310 / 5) / 2
--             print ((divBy 0 . divBy 7) 2310) -- (2310 / 7) / 0 => Divide By Zero Error!!!
--             print ((divBy 5 . divBy 11) 2310) -- (2310 / 11) / 5

-- Add an Err data-type that can record Success / Failure
data Err a = OK a
           | Error -- only 1 type of error but there could be more
        deriving Show
