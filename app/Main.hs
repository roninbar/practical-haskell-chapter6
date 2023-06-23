{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Lens.Micro             (Lens, Lens', lens, (^.))
import           Lens.Micro.TH          (makeLenses)
import           System.Posix.Internals (c_link)

data Client i
  = GovOrg
      { _identifier :: i
      , _name       :: String
      } -- i String
  | Company
      { _identifier :: i
      , _name       :: String
      , _person     :: Person
      , _duty       :: String
      } -- i String Person String
  | Individual
      { _identifier :: i
      , _person     :: Person
      } -- i Person
  deriving (Show)

data Person =
  Person
    { _firstName :: String
    , _lastName  :: String
    } -- String String
  deriving (Show)

-- firstName :: Lens' Person String
-- firstName = lens (\(Person f _) -> f) (\(Person _ l) newF -> Person newF l)
-- lastName :: Lens' Person String
-- lastName = lens (\(Person _ l) -> l) (\(Person f _) newL -> Person f newL)
-- person :: Lens' (Client i) Person
-- person =
--   lens
--     (\case
--        (Company _ _ p _) -> p
--        (Individual _ p) -> p
--        c -> error "Attempting to read `person` from ...")
--     (\client p ->
--        case client of
--          Company i n _ r -> Company i n p r
--          Individual i _  -> Individual i p
--          _ -> error "Attempting to write `person` to ...")
-- identifier :: Lens (Client i) (Client j) i j
-- identifier =
--   lens
--     (\case
--        (GovOrg i _) -> i
--        (Company i _ _ _) -> i
--        (Individual i _) -> i)
--     (\client newId ->
--        case client of
--          GovOrg _ n      -> GovOrg newId n
--          Company _ n p r -> Company newId n p r
--          Individual _ p  -> Individual newId p)
fullName :: Lens' Person String
fullName =
  lens
    (\(Person f l) -> f ++ " " ++ l)
    (\_ newFullName ->
       case words newFullName of
         f:l:_ -> Person f l
         _     -> error "Incorrect name")

makeLenses ''Client

makeLenses ''Person

main :: IO ()
main =
  let p = Person "John" "Smith"
      client = Individual 3 p
   in print $ client ^. person . fullName
