  {-# HLINT ignore "Use camelCase" #-}
  {-# LANGUAGE InstanceSigs #-}
  module Angabe3 where











  type Nat1 = Int
  type Typ  = (Nat1,Nat1)






  data Zeile = LE Int                       
               | E Int Zeile deriving Show



  data Matrix = LZ Zeile                       
                | Z Zeile Matrix deriving Show  








  fehler = error "Argument(e) typfehlerhaft"



  data Matrixtyp = Matrix_vom_Typ Typ | KeineMatrix deriving (Eq,Show)




  matrixtyp :: Matrix -> Matrixtyp
  matrixtyp (LZ z) = Matrix_vom_Typ (1, getWidth z)
  matrixtyp (Z z m)
    | not (equalWidths m (getWidth z)) = KeineMatrix
    | otherwise = Matrix_vom_Typ (1 + getHeight m, getWidth z)





    where
        equalWidths :: Matrix -> Int -> Bool
        equalWidths (LZ z) x = getWidth z == x
        equalWidths (Z z m) x = getWidth z == x && equalWidths m x

  getWidth :: Zeile -> Int
  getWidth (E _ x) = 1 + getWidth x
  getWidth (LE _) = 1

  getHeight :: Matrix -> Int
  getHeight (LZ z) = 1
  getHeight (Z z m) = 1 + getHeight m




  instance Eq Matrix where
   (==) :: Matrix -> Matrix -> Bool
   m1 == m2 = cMat m1 m2
    where
        cMat :: Matrix -> Matrix -> Bool
        cMat (LZ x)(LZ y)       = cRow x y
        cMat (Z x xs)(Z y ys)   = cRow x y && cMat xs ys
        cMat _ _ = fehler




        cRow :: Zeile -> Zeile -> Bool
        cRow (LE x)(LE y)       = x == y
        cRow (E x xs)(E y ys)   = x == y && cRow xs ys
        cRow _ _ = fehler








 
   (/=) :: Matrix -> Matrix -> Bool
   m1 /= m2 = not (m1==m2)















  instance Num Zeile where
   (+) :: Zeile -> Zeile -> Zeile
   (LE x) + (LE y)      = LE (x + y)
   (E x xs) + (E y ys)  = E (x + y)(xs+ys)
   (LE _) + (E _ _) = fehler
   (E _ _) + (LE _) = fehler

   (-) :: Zeile -> Zeile -> Zeile
   (LE x) - (LE y)      = LE (x - y)
   (E x xs) - (E y ys)  = E (x - y)(xs-ys)
   (LE _) - (E _ _) = fehler
   (E _ _) - (LE _) = fehler

   abs :: Zeile -> Zeile
   abs (LE x) = LE(abs x)
   abs (E x xs) = E (abs x) (abs xs)

  instance Num Matrix where

   (+) :: Matrix -> Matrix -> Matrix
   (LZ x)+(LZ y)        = LZ (x + y)
   (Z x xs)+(Z y ys)    = Z (x + y)(xs+ys)
   (LZ _) + (Z _ _) = fehler
   (Z _ _) + (LZ _) = fehler

   (-) :: Matrix -> Matrix -> Matrix
   (LZ x)-(LZ y)        = LZ (x - y)
   (Z x xs)-(Z y ys)    = Z (x - y)(xs-ys)
   (LZ _) - (Z _ _) = fehler
   (Z _ _) - (LZ _) = fehler

   abs :: Matrix -> Matrix
   abs x
    | matrixtyp x == KeineMatrix = fehler
    | otherwise = abs' x
    where
        abs' (LZ x)   = LZ(abs x)
        abs' (Z x xs) = Z (abs x) (abs xs)





   fromInteger :: Integer -> Matrix
   fromInteger z = LZ (LE (fromIntegral z))


   (*) :: Matrix -> Matrix -> Matrix
   m1 * m2 = error "(*) bleibt unimplementiert!"

   negate :: Matrix -> Matrix
   negate m = error "negate bleibt unimplementiert!"

   signum :: Matrix -> Matrix
   signum m = error "signum bleibt unimplementiert!"







