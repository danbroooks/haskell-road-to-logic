{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Logic where

import Prelude hiding (all)

(¬) :: Bool -> Bool
(¬) True = False
(¬) False = True

infixr 3 /\

(/\) :: Bool -> Bool -> Bool
False /\ _ = False
True /\ a = a

all :: [Bool] -> Bool
all = foldr (/\) True

infixr 2 \/

(\/) :: Bool -> Bool -> Bool
False \/ a = a
True \/ _ = True

any :: [Bool] -> Bool
any = foldr (\/) False

infixr 2 <+>

(<+>) :: Bool -> Bool -> Bool
(<+>) = (/=)

infixr 1 ==>

(==>) :: Bool -> Bool -> Bool
a ==> b = (¬) a \/ b

infixr 1 <=>

(<=>) :: Bool -> Bool -> Bool
(<=>) = (==)

bools :: [Bool]
bools = [True, False]

class Verifiable a where
  check :: a -> Bool

deriveVerifiable :: Verifiable a => (Bool -> a) -> Bool
deriveVerifiable f = all (check . f <$> bools)

instance Verifiable Bool where
  check = id

instance Verifiable (Bool -> Bool) where
  check = deriveVerifiable

instance Verifiable (Bool -> Bool -> Bool) where
  check = deriveVerifiable

instance Verifiable (Bool -> Bool -> Bool -> Bool) where
  check = deriveVerifiable

instance Verifiable (Bool -> Bool -> Bool -> Bool -> Bool) where
  check = deriveVerifiable

infixr 1 ≡

class Equiv a where
  (≡) :: a -> a -> Bool

deriveEquivalence :: Equiv eq => (Bool -> eq) -> (Bool -> eq) -> Bool
deriveEquivalence f g = all (equiv <$> bools)
  where
    equiv b = f b ≡ g b

instance Equiv (Bool -> Bool) where
  f ≡ g = (f True <=> g True) /\ (f False <=> g False)

instance Equiv (Bool -> Bool -> Bool) where
  (≡) = deriveEquivalence

instance Equiv (Bool -> Bool -> Bool -> Bool) where
  (≡) = deriveEquivalence

instance Equiv (Bool -> Bool -> Bool -> Bool -> Bool) where
  (≡) = deriveEquivalence

instance Equiv (Bool -> Bool -> Bool -> Bool -> Bool -> Bool) where
  (≡) = deriveEquivalence

logEquiv :: Equiv eq => eq -> eq -> Bool
logEquiv = (≡)
