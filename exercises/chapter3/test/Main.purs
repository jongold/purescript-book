module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.AddressBook (AddressBook, Entry, containsName, emptyBook, findEntry, findEntryByAddress, insertEntry, removeDuplicates, showEntry)
import Data.List (length)
import Data.Maybe (Maybe)

example :: Entry
example =
  { firstName: "John"
  , lastName: "Smith"
  , address: { street: "123 Fake St."
             , city: "Faketown"
             , state: "CA"
             }
  }

book0 :: AddressBook
book0 = emptyBook

printEntry :: String -> String -> AddressBook -> Maybe String
printEntry firstName lastName book = showEntry <$> findEntry firstName lastName book

main :: Eff (console :: CONSOLE) Unit
main = do
  let book1 = insertEntry example emptyBook

  logShow $ printEntry "John" "Smith" book0
  logShow $ printEntry "John" "Smith" book1
  logShow $ map showEntry $ findEntryByAddress "123 Fake St." book1

  logShow $ containsName "James" "Smith" book1

  let book2 = insertEntry example $ insertEntry example book1
  logShow $ length $ removeDuplicates book2
