{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Lab1 where

import Prelude (Show)

data Bool where {False::Bool ; True::Bool}
    deriving Show

-- Ejercicio 1 - NOT
not :: Bool -> Bool
not = \b -> case b of
    False -> True;
    True -> False


--testMyNot :: (Bool, Bool)
--testMyNot = (myNot True, myNot False)

-- Ejercicio 2
-- OR
(||) :: Bool -> Bool -> Bool
(||) = \b1 b2 -> case b1 of
    False -> b2;
    True -> b1

-- AND
(&&) :: Bool -> Bool -> Bool
(&&) = \b1 b2 -> case b1 of
    False -> False;
    True -> b2

-- XOR
myXor :: Bool -> Bool -> Bool
myXor = \b1 b2 -> case b1 of
    False -> b2;
    True -> (not) b2

-- >>
myImplicacionLogica :: Bool -> Bool -> Bool
myImplicacionLogica = \b1 b2 -> case b1 of
    False -> case b2 of { False -> True; True -> True }
    True -> b2

-- Ejercicio 3
-- parte a
equals :: Bool -> Bool -> Bool
equals = \b1 b2 -> case b1 of
    False -> case b2 of { False -> True; True -> False };
    True -> case b2 of { False -> False; True -> True }

-- parte b
equalsUsingMy :: Bool -> Bool -> Bool
equalsUsingMy = \b1 b2 -> not (myXor b1 b2)

-- parte c
notEquals :: Bool -> Bool -> Bool
notEquals = \b1 b2 -> case b1 of
    False -> case b2 of { False -> False; True -> True };
    True -> case b2 of { False -> True; True -> False }

-- CONCLUSIÓN: 
-- en estas últimas dos funciones vemos que la desigualdad puede expresarse como XOR y la igualdad como su negación 

-- parte d
sorted :: Bool -> Bool -> Bool
sorted = \b1 b2 -> case b1 of
    False -> True
    True -> b2


-- Ejercicio 4
unanimidad :: Bool -> Bool -> Bool -> Bool
unanimidad = \b1 b2 b3 -> case b1 of 
    False -> False; 
    True -> case b2 of { False -> False; True -> b3 } 
         
unanimidadUsingMy :: Bool -> Bool -> Bool -> Bool       
unanimidadUsingMy = \b1 b2 b3 -> (&&) ( (&&) b1 b2 ) b3

mayoria :: Bool -> Bool -> Bool -> Bool
mayoria = \b1 b2 b3 -> case b1 of 
    False -> case b2 of { False -> False; True ->  b3 };-- b1 || (b2 && b3)
    --False -> b2 && b3
    True -> case b2 of { False -> b3; True -> True -- b1 && (b2 || b3)
    }-- True -> b2 || b3

mayoriaUsingMy :: Bool -> Bool -> Bool -> Bool       
--mayoriaUsingMy = \b1 b2 b3 -> (b1 || (b2 && b3)) || (b1 && (b2 || b3)) 
mayoriaUsingMy = \b1 b2 b3 -> ((b1 || b2) && (b1 || b3)) || ((b1 && b2) || (b1 && b3)) 


impar :: Bool -> Bool -> Bool -> Bool
impar = \b1 b2 b3 -> case b1 of 
    False -> case b2 of { False -> b3; True -> case b3 of { False -> True; True -> False } }; 
    True -> case b2 of { False -> case b3 of { False -> True; True -> False }; True -> b3 } 

imparUsingMy :: Bool -> Bool -> Bool -> Bool
imparUsingMy = \b1 b2 b3 ->  case b1 of 
    False -> case b2 of { False -> b3; True -> (not b3) }; 
    True -> case b2 of { False -> (not b3); True -> b3 }
