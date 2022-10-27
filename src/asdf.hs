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
  matrixtyp m
    | not equalWidths m = KeineMatrix
    | otherwise = (height m, width m)
    where
        equalWidths :: Matrix -> Bool
        equalWidths (LZ x) = True
        equalWidths (Z x y) = getWidth x == getWidth y && equalWidths y
        getWidth :: Zeile -> Int
        getWidth (LE _) = 1
        getWidth (E _ x) = 1 + getWidth x

  m24 = Matrix (Zeile 1 (Zeile 1)) Matrix (Zeile 1 (Zeile 1)) Matrix (Zeile 1 (Zeile 1))







  instance Eq Matrix where
   m1 == m2 = error "noch nicht implementiert!"
   m1 /= m2 = error "noch nicht implementiert!"







  instance Num Matrix where
   m1 + m2 = error "noch nicht implementiert!"
   m1 - m2 = error "noch nicht implementiert!"
   abs m = error "noch nicht implementiert!"
   fromInteger z = error "noch nicht implementiert!"
   m1 * m2 = error "(*) bleibt unimplementiert!"
   negate m = error "negate bleibt unimplementiert!"
   signum m = error "signum bleibt unimplementiert!"









