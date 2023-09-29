instance Enum ( Float , Char ) where
    enumFrom (x , c ) = p : enumFrom p
        where
            p = ( x *1.1 , succ c )
        
f :: Float -> ( any , ( Float , Char )) -> Bool
f x (_ ,( y , _ )) = x < y

myMap f ( x : xs ) = f x : myMap f xs
myMap _ [] = []

myFilt _ [] = []
myFilt p ( x : xs ) = if p x then x : ys else ys
    where
        ys = myFilt p xs
        
myZip [] _ = []
myZip _ [] = []
myZip ( x : xs ) ( y : ys ) = (x , y ) : myZip xs ys
myZip _ _ = error "ouch !!"

myMap snd (myFilt (f 0) (myZip (error "ERROR":"do") [(2,’a’)..]))
