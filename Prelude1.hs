
id x = x

data Bool = True | False

not b = case b of
    True -> False
    False -> True

data Maybe a = Just a | Nothing

class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

instance Eq Int where
    (==) = primIntEq
    (/=) x y = not (primIntEq x y)

class Num a where
    (+) :: a -> a -> a
    (-) :: a -> a -> a
    (*) :: a -> a -> a
    fromInteger :: Int -> a

instance Num Int where
    (+) x y = primIntAdd x y
    (-) x y = primIntSubtract x y
    (*) x y = primIntMultiply x y
    fromInteger = id

instance Num Double where
    (+) x y = primDoubleAdd x y
    (-) x y = primDoubleSubtract x y
    (*) x y = primDoubleMultiply x y
    fromInteger x = primIntToDouble x

(||) :: Bool -> Bool -> Bool
(||) x y = case x of
    True -> True
    False -> y

(&&) :: Bool -> Bool -> Bool
(&&) x y = case x of
    True -> y
    False -> False

otherwise :: Bool
otherwise = True

(.) :: (b -> c) -> (a -> b) -> (a -> c)
(.) f g x = f (g x)

($) :: (a -> b) -> a -> b
($) f x = f x

map :: (a -> b) -> [a] -> [b]
map f xs = case xs of
    : y ys -> f y : map f ys
    [] -> []

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f x xs = case xs of
    : y ys -> foldl f (f x y) ys
    [] -> x

add x y = x + y

sum :: Num a => [a] -> a
sum = foldl add 0
