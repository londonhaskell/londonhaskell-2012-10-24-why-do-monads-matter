module Dependence where

-- Pref Kleisli arrow example

-- "Why Do Monads Matter?"
-- London Haskell user group 24-Oct-2012
-- talk given by Derek Wright
-- based on blog post by Chris Smith
-- http://cdsmith.wordpress.com/2012/04/18/why-do-monads-matter/

-- Example using composition of 
-- (very simplistic) pretty printing functions.
-- Each function takes a string and returns an
-- extended string.

-- running main will display
-- < < < foo > > >
main = putStrLn (f "foo" cfg) -- pass in configuration at top level
         where f = left `composePref` right
               cfg = 3
       
-- left adds brackets on left
left :: String -> Pref String
left s = \i -> (repeatString i "< ") ++ s

-- right adds brackets on right
right :: String -> Pref String
right s = \i -> s ++ (repeatString i " >")

-- a version of repeat for Strings
repeatString :: Integer -> String -> String
repeatString i s = if (i <= 0)
                    then ""
                    else s ++ repeatString (i - 1) s

-- Very simple configuration type
type Config = Integer

-- Functions that can take preferences are returning a function
-- that takes the configuration and returns a value
type Pref a = (Config -> a)

-- Kleisli composition
composePref :: (b -> Pref c) -> (a -> Pref b) -> (a -> Pref c)
composePref f g x = \c -> let  y = (g x) c  -- Use the the same configuration
                          in       (f y) c  -- in for both function

-- Kleisli identity
idPref :: a -> Pref a
idPref x = \_ -> x  -- Ignore the configuration

-- Define join using composePref
joinPref :: Pref (Pref a) -> Pref a
joinPref = composePref id id

-- Define bind using composePref
bindPref :: Pref a -> (a -> Pref b) -> Pref b
bindPref e f = (composePref f id) e

