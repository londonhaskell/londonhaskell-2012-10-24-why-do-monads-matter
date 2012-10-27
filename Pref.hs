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
main = putStrLn (f "foo")
        where f = left . right
       
-- left adds brackets on left
left :: String -> String
left s = (repeatString 3 "< ") ++ s

-- right adds brackets on right
right :: String -> String
right s = s ++ (repeatString 3 " >")

-- a version of repeat for Strings
repeatString :: Integer -> String -> String
repeatString i s = if (i <= 0)
                    then ""
                    else s ++ repeatString (i - 1) s
