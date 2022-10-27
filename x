  {-# LANGUAGE InstanceSigs #-}
  module Angabe3 where











  type Nat1 = Int
  type Typ  = (Nat1,Nat1)






  data Zeile = LE Int                       
               | E Int Zeile deriving Show



  data Matrix = LZ Zeile                       
                | Z Zeile Matrix deriving Show  








  fehler = error "Argument(e) typfehlerhaft"



  data Matrixtyp = Matrix_vom_Typ Typ 
                    | KeineMatrix deriving (Eq,Show)




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




  m32:: Matrix
  m32 = Z(E 1 (LE 1)) (Z (E 1 (LE 1)) (LZ (E 1 (LE 1))))
  m32':: Matrix
  m32' = Z(E 1 (LE 1)) (Z (E 1 (LE 1)) (LZ (E 1 (LE 1))))
  m11:: Matrix
  m11 = LZ (LE 1)
  m11':: Matrix
  m11' = LZ (LE 1)
  m14:: Matrix
  m14 = LZ (E 1(E 1(E 1 (LE 1))))
  m14':: Matrix
  m14' = LZ (E 1(E 1(E 1 (LE 1))))
  m24:: Matrix
  m24 = Z (E 1(E 1(E 1 (LE 1)))) (LZ (E 1(E 1(E 1 (LE 1)))))
  m24':: Matrix
  m24' = Z (E 1(E 1(E 1 (LE 1)))) (LZ (E 1(E 1(E 1 (LE 1)))))

  n32:: Matrix
  n32 = Z(E 2 (LE 1)) (Z (E 1 (LE 1)) (LZ (E 1 (LE 1))))
  n32':: Matrix
  n32' = Z(E 1 (LE 1)) (Z (E 1 (LE 3)) (LZ (E 1 (LE 1))))
  n11:: Matrix
  n11 = LZ (LE 5)
  n11':: Matrix
  n11' = LZ (LE 10)
  n14:: Matrix
  n14 = LZ (E 1(E 1(E 1 (LE 21))))
  n14':: Matrix
  n14' = LZ (E 1(E 1(E 2 (LE 1))))
  n24:: Matrix
  n24 = Z (E 1(E 1(E 1 (LE 1)))) (LZ (E 3(E 1(E 1 (LE 1)))))
  n24':: Matrix
  n24' = Z (E 1(E 1(E 1 (LE 1)))) (LZ (E 1(E 1(E 3 (LE 1)))))

  mk :: Matrix
  mk = Z (E 1(E 1(LE 1))) (LZ (E 1(E 1(E 1 (LE 1)))))







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

   (-) :: Zeile -> Zeile -> Zeile
   (LE x) - (LE y)      = LE (x - y)
   (E x xs) - (E y ys)  = E (x - y)(xs-ys)

   abs :: Zeile -> Zeile
   LE x   = LE(abs x)
   E x xs = E (abs x) xs

  instance Num Matrix where
   (+) :: Matrix -> Matrix -> Matrix
   (LZ x)+(LZ y)        = LZ (x + y)
   (Z x xs)+(Z y ys)    = Z (x + y)(xs+ys)

   (-) :: Matrix -> Matrix -> Matrix
   (LZ x)-(LZ y)        = LZ (x - y)
   (Z x xs)-(Z y ys)    = Z (x - y)(xs-ys)

   abs :: Matrix -> Matrix
   LZ x   = LZ(abs x)
   Z x xs = Z (abs x) xs

 
 
   fromInteger :: Integer -> Matrix
   fromInteger z = error "noch nicht implementiert!"
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
   (*) :: Matrix -> Matrix -> Matrix
   m1 * m2 = error "(*) bleibt unimplementiert!"
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
   negate :: Matrix -> Matrix
   negate m = error "negate bleibt unimplementiert!"
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
   signum :: Matrix -> Matrix
   signum m = error "signum bleibt unimplementiert!"
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 









