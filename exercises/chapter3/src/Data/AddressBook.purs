module Data.AddressBook where

import Prelude
import Control.Plus (empty)
import Data.List (List(..), filter, head, nubBy, null)
import Data.Maybe (Maybe)

type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type AddressBook = List Entry

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <> addr.city <> ", " <> addr.state

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <> entry.firstName <> ": " <> showAddress entry.address

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons
-- insertEntry entry book = Cons entry book

findEntry :: String -> String -> AddressBook -> Maybe Entry
-- findEntry firstName lastName book = head $ filter filterEntry book
findEntry firstName lastName = head <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

-- Write a function which looks up an Entry given a street address,
-- by reusing the existing code in findEntry
findEntryByAddress :: String -> AddressBook -> Maybe Entry
-- findEntryByAddress address book = head $ filter hasAddress book
findEntryByAddress address = head <<< filter hasAddress
  where
    hasAddress :: Entry -> Boolean
    hasAddress entry = entry.address.street == address

-- Write a function which tests whether a name appears in a AddressBook,
-- returning a Boolean value
containsName :: String -> String -> AddressBook -> Boolean
-- containsName firstName lastName book = not $ null $ filter hasName book
containsName firstName lastName = not <<< null <<< filter hasName
  where
    hasName :: Entry -> Boolean
    hasName entry = entry.firstName == firstName && entry.lastName == lastName

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubBy sameName
  where
    sameName :: Entry -> Entry -> Boolean
    sameName a b = a.firstName == b.firstName &&
                   a.lastName == b.lastName
