{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}



module TypeFamilies where 
import Data.Kind (Type)

appe :: [a] -> [a] -> [a]
appe [] ys = ys
appe (x:xs) ys = x : appe xs ys

type Appe :: [a] -> [a] -> [a]
type family Appe xs ys where
    Appe '[] ys = ys
    Appe (x:xs) ys = x : Appe xs ys

not :: Bool -> Bool
not True = False
not False = True

type Not :: Bool -> Bool
type family Not a where
    Not True = False
    Not False = True

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe _ (Just x) = x

type FromMaybe :: a -> Maybe a -> a
type family FromMaybe d x where 
    FromMaybe a Nothing  = a
    FromMaybe _ (Just x) = x

type Fst :: (a,b) -> a
type family Fst t where 
    Fst '(x, _) = x

type S :: (Type -> Type) -> Type
data S k = MkS (k Bool) (k Integer)

data Nat = Zero | Succ Nat


--GADTs
data IntOrBool a where
    Int :: Int -> IntOrBool Int
    Bool :: Bool -> IntOrBool Bool

data IntRBool = I Int | B Bool   

-- using the type pattern matching
extractIntOrBool :: IntOrBool a -> a
extractIntOrBool (Int a)  = a -- IntOrBool a ~ IntOrBool Int
extractIntOrBool (Bool b) = b -- IntOrBool a ~ IntOrBool Bool


----------------------------
-- length indexed vectors --
data Vector(n :: Nat) (a :: *) where -- data Vector n a where
    VNil  :: Vector 'Zero a
    VCons :: a -> Vector n a -> Vector ('Succ n) a

type family Add n m where
    Add 'Zero m     = m
    Add ('Succ n) m = 'Succ (Add n m) --Add ('Succ n) m = Add n ('Succ m)

append :: Vector n a -> Vector m a -> Vector (Add n m) a
append VNil xs = xs
append (VCons a rest) xs = undefined --VCons a (append rest xs) != Vector (Add n m) a
--(VCons a rest) ~ Vector ('Succ n) a
--xs             ~ Vector m a
--result         ~ Vector (Add n m) a


instance Show a => Show(Vector n a) where
    show VNil = "VNil"
    show (VCons a as) = "VCons " ++ show a ++ " (" ++ show as ++ ")"

add :: Nat -> Nat -> Nat
add Zero n     = n
add (Succ a) n = add a (Succ n)

-- Heterogeneous Lists
data HList xs where
    HNil :: HList '[]
    (:::) :: a -> HList as -> HList (a ': as)

infixr 6 :::

-- instance Show (HList xs) where
--     show HNil = "Nil"
--     show (x ::: rest) = "_ ::: " ++ show rest

instance Show (HList '[]) where
    show HNil       = "HNil"

instance (Show (HList as), Show a) 
        => Show (HList (a ': as)) where
    show (a ::: rest) = show a ++ " ::: " ++ show rest