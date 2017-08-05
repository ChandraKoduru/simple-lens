{-# LANGUAGE RankNTypes #-}
module Main where

import Data.Functor.Const
import Data.Functor.Identity

import Lib

type SimpleLens s a = forall f. Functor f => (a -> f a) -> s -> f s

data Person =
  Person
    { name :: String
    , age  :: Int
    } deriving (Eq, Read, Show)

-- expand type signature
-- _name :: forall f. Functor f => (String -> f String) -> Person -> f Person
_name :: SimpleLens Person String
_name a_to_f_a (Person pName pAge) = (\ppName -> Person ppName pAge) <$> a_to_f_a pName

_age :: SimpleLens Person Int
_age a_to_f_a (Person pName pAge) = (\ppAge -> Person pName ppAge) <$> a_to_f_a pAge

-- view :: ((a -> f a) -> s -> f s) -> s -> a
view :: SimpleLens s a -> s -> a
view l = getConst . l Const

-- view_name
view_name :: Person -> String
view_name (Person pName pAge) = getConst 
  $ (\ppName -> Person ppName pAge) 
  <$> Const pName

view_age :: Person -> Int
view_age (Person pName pAge) = getConst
  $ (\ppAge -> Person pName ppAge)
  <$> Const pAge

-- set :: ((a -> f a) -> s -> f s) -> s -> s -> s
set :: SimpleLens s a -> a -> s -> s
set l b = runIdentity . l (\_ -> Identity b)

-- set _name
set_name :: String -> Person -> Person
set_name b (Person pName pAge) = runIdentity 
  $ (\ppName -> Person ppName pAge)
  <$> Identity b

-- over :: ((a -> f a) -> s -> f s) -> (a -> a) -> s -> s
over :: SimpleLens s a -> (a -> a) -> s -> s
over l f  = runIdentity . l (Identity . f)

-- over _age 
over_age :: (Int -> Int) -> Person -> Person
over_age a_to_a (Person pName pAge) = runIdentity 
  $ (\ppAge -> Person pName ppAge) 
  <$> Identity (a_to_a pAge)

over_name :: (String -> String) -> Person -> Person
over_name n_to_n (Person pName pAge) = runIdentity
  $ (\ppName -> Person ppName pAge)
  <$> Identity (n_to_n pName)

data Phone = 
  Phone
    { phoneNumber :: String
    } deriving (Show)

data Employee =
  Employee 
    { employeeName :: String
    , employeePhone :: Phone
    } deriving (Show)

_phoneNumber :: SimpleLens Phone String
_phoneNumber a_to_f_a (Phone phoneNum) = (\pPhoneNum -> Phone pPhoneNum) 
  <$> a_to_f_a phoneNum

_employeePhone :: SimpleLens Employee Phone
_employeePhone a_to_f_a (Employee eName ePhone) = (\eEPhone -> Employee eName eEPhone)
  <$> a_to_f_a ePhone

_employeeName :: SimpleLens Employee String
_employeeName a_to_f_a (Employee eName ePhone) = (\eEName -> Employee eEName ePhone)
  <$> a_to_f_a eName

main :: IO ()
main = do
  let matthias = Employee "Matthias" $ Phone "123-345-8888"
      matthiasNewPhone = set (_employeePhone . _phoneNumber) "222-333-1212" matthias
      matthiasJr = set (_employeePhone . _phoneNumber ) "432-234-1177" 
                   $ over _employeeName (++ " Jr.") $ matthias
  print matthias
  print matthiasNewPhone
  print matthiasJr
  return ()

