
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

map :: (b -> a -> b) -> [a] -> [b]
foldl f x xs = case xs of
    : y ys -> foldl f (f x y) ys
    [] -> x

sum :: Num a => [a] -> a
sum = foldl (+)
