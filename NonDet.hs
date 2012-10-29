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

-- Start with a version where Ace is only worth 1
valueCard :: Card -> Integer
valueCard Ace = 1
valueCard (Number x) = x
valueCard Picture = 10

-- Hands are just lists of cards
valueHand :: [Card] -> Integer
valueHand [] = 0
valueHand (c:cs) = (valueCard c) + (valueHand cs)

-- Print the value of some test hands
main = do
    print (valueHand []) -- 0
    print (valueHand [Ace, Number 2, Number 3]) -- 6
    print (valueHand [Ace, Ace, Number 2]) -- 4
    print (valueHand [Ace, Ace, Ace]) -- 3
    print (valueHand [Picture, Picture]) -- 20
    print (valueHand [Ace, Picture]) -- 11
    print (valueHand [Ace, Picture, Picture]) -- 21
    print (valueHand [Ace, Picture, Picture, Picture]) -- 31

-- Type synonym for Power-Sets
-- A non-deterministic choice will be represented by a list
type P a = [a]
