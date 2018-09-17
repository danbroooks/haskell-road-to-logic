module Chapter2 where

import Logic

formula1 :: Bool
formula1 = formula2 p q
  where
    p = True
    q = False

formula2 :: Bool -> Bool -> Bool
formula2 p q = (¬) p /\ ((p ==> q) <=> (¬) (q /\ (¬) p))

excludedMiddle :: Bool -> Bool
excludedMiddle p = p \/ (¬) p

deMorgans :: Bool -> Bool -> Bool
deMorgans p q = (¬) (p /\ q) <=> ((¬) p \/ (¬) q)

logEquiv :: (Bool -> Bool) -> (Bool -> Bool) -> Bool
logEquiv bf1 bf2 = (bf1 True <=> bf2 True) /\ (bf1 False <=> bf2 False)

logEquiv2 :: (Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool) -> Bool
logEquiv2 bf1 bf2 = and (equiv <$> bools <*> bools)
  where
    equiv p q = bf1 p q <=> bf2 p q

logEquiv3 :: (Bool -> Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool -> Bool) -> Bool
logEquiv3 bf1 bf2 = and (equiv <$> bools <*> bools <*> bools)
  where
    equiv p q r = bf1 p q r <=> bf2 p q r

formula3 :: Bool -> Bool -> Bool
formula3 p _ = p

formula4 :: Bool -> Bool -> Bool
formula4 p q = (p <+> q) <+> q

formula5 :: Bool -> Bool -> Bool
formula5 p q = p <=> ((p <+> q) <+> q)
