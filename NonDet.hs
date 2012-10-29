module Uncertainty where

-- P (PowerSet) Kleisli arrow example

-- "Why Do Monads Matter?"
-- London Haskell user group 24-Oct-2012
-- talk given by Derek Wright
-- based on blog post by Chris Smith
-- http://cdsmith.wordpress.com/2012/04/18/why-do-monads-matter/

-- TODO
-- Blackjack problem taken from Haskell
-- Hoodlums meetup on 17-Oct-2012.
-- http://www.meetup.com/

-- Non-Determinism comes from rule in Blackjack
-- that allows an Ace to be valued either as 1 
-- or 11.

data Card = Ace
          | Number Integer
          | Picture

valueCard :: Card -> P Integer
valueCard Ace = P [1,11] -- Aces can be either 1 or 11
valueCard (Number x) = P [x]
valueCard Picture = P [10]

-- Hands are just lists of cards
valueHand :: [Card] -> Integer
valueHand cs = let (P hvs) = hv cs
                   nonBust = filter (<= 21) hvs
               in if not (null nonBust)
                  then maximum nonBust
                  else minimum hvs
        where 
            hv [] = P [0]
            hv (c:cs) = (valueCard c) `addP` (hv cs)

-- Print the value of some test hands
main = do
    print (valueHand []) -- 0
    print (valueHand [Ace, Number 2, Number 3]) -- 6 or 16
    print (valueHand [Ace, Ace, Number 2]) -- 4, 14 or 24
    print (valueHand [Ace, Ace, Ace]) -- 3, 13, 23 or 33
    print (valueHand [Picture, Picture]) -- 20
    print (valueHand [Ace, Picture]) -- 11 or 21
    print (valueHand [Ace, Picture, Picture]) -- 21 or 31
    print (valueHand [Ace, Picture, Picture, Picture]) -- 31 or 41

-- Type synonym for Power-Sets
-- A non-deterministic choice will be represented by a list
data P a = P [a]
        deriving Show

-- Define some helper functions that un-wrap and re-wrap power-sets
appendP :: P a -> P a -> P a
appendP (P xs) (P ys) = P (xs ++ ys)

mapP :: (a -> b) -> P a -> P b
mapP f (P xs) = P (map f xs)

-- Kleisli composition
-- Apply f to every possible result from applying g to x and merge the results
composeP :: (b -> P c) -> (a -> P b) -> (a -> P c)
composeP f g x = h ( g x ) -- Redefine so not using list comprehension
              where
                     h (P []) = P []
                     h (P (y:ys)) = (f y) `appendP` (h (P ys))

-- Kleisli identity
idP :: a -> P a
idP x = P [x] -- A choice of one

-- Add two non-deterministic integers and return every possible combination
addP :: P Integer -> P Integer -> P Integer
addP xs ys = (f xs `composeP` f ys) 0
    where f n i = mapP (+i) n

-- Define join using composeP
joinP :: P (P a) -> P a
joinP = composeP id id

-- Define bind using composeP
bindP :: P a -> (a -> P b) -> P b
bindP e f = (composeP f id) e

-- Prove we have implemented a monad by defining an instance of the type-class
instance Monad P where
    return = idP
    (>>=) = bindP
