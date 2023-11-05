{-|
Module      : 1JC3-Assign2.Assign_2.hs
Copyright   :  (c) Curtis D'Alves 2021
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Assignment 2 - McMaster CS 1JC3 2021
-}
module Assign_2 where

-----------------------------------------------------------------------------------------------------------
-- INSTRUCTIONS              README!!!
-----------------------------------------------------------------------------------------------------------
-- 1) DO NOT DELETE/ALTER ANY CODE ABOVE THESE INSTRUCTIONS
-- 2) DO NOT REMOVE / ALTER TYPE DECLERATIONS (I.E THE LINE WITH THE :: ABOUT THE FUNCTION DECLERATION)
--    IF YOU ARE UNABLE TO COMPLETE A FUNCTION, LEAVE IT'S ORIGINAL IMPLEMENTATION (I.E. THROW AN ERROR)
-- 3) MAKE SURE THE PROJECT COMPILES (I.E. RUN STACK BUILD AND MAKE SURE THERE ARE NO ERRORS) BEFORE
--    SUBMITTING, FAILURE TO DO SO WILL RESULT IN A MARK OF 0
-- 4) REPLACE macid = "TODO" WITH YOUR ACTUAL MACID (EX. IF YOUR MACID IS jim THEN macid = "jim")
-----------------------------------------------------------------------------------------------------------

-- Name: UJJWAL RAJ 
-- Date:31 /October/ 2021
macid :: String
macid = "raju1"

type Vector3D = (Double,Double,Double)

{- -----------------------------------------------------------------
 - getX
 - -----------------------------------------------------------------
 - Description:
 -   In this particular question we are implementing a function , where in we are fetching the X Co-Ordinate/Vector . When we have  (x,y,z).
  # Stack Tests- 
  1)Input - getX (2,3,4) 
  Output - 2.0
  2)Input - getX (178,239,430)
Output - 178.0
  3)Input - getX (100,230,650)
  Output - 100.0
 -}
getX :: Vector3D -> Double
getX (x,y,z) = x

{- -----------------------------------------------------------------
 - getY
 - -----------------------------------------------------------------
 - Description:
 -  This question is similar to Question 1 , Where in we are fetching the Y Co-ordinate/Vector , Instead of X Co-ordinate/Vector  . When we have (x,y,z).
  # Stack Tests- 
  1) Input - getY (190,227,440)
  Output - 227.0
  2) Input - getY (444,677,346)
  Output - 677.0
  3) Input - getY (567,876,900)
  Output - 876.0
 -}
getY :: Vector3D -> Double
getY (x,y,z) = y

{- -----------------------------------------------------------------
 - getZ
 - -----------------------------------------------------------------
 - Description:
 -Again this question is also similar to Question 1 and Question 2 . In this question, We are fetching the Z Co-ordinate/Vector, Instead of X and Y Co-ordinate/Vectors .
  # Stack Tests-
  1) Input - getZ (200,340,870)
  Output - 870.0
  2) Input - getZ (567,874,367)
  Output - 367.0
  3) Input - getZ (543,765,123)
  Output - 123.0
 -}
getZ :: Vector3D -> Double
getZ (x,y,z) = z

{- -----------------------------------------------------------------
 - scalarMult
 - -----------------------------------------------------------------
 - Description:
 - scalarMult does nothing but it multiplies an integer with the (x,y,z) vectors . 
  # Stack Tests -
  1) Input - scalarMult 2 (2,3,4)
  Output -  (4.0,6.0,8.0)
  2) Input - scalarMult 3 (4,9,7)
  Output - (12.0,27.0,21.0)
  3) Input - scalarMult 4 (6,4,9)
  Output - (24.0,16.0,36.0)


 -}
scalarMult :: Double -> Vector3D -> Vector3D
scalarMult s (x,y,z) = ( s * x , s * y, s * z )

{- -----------------------------------------------------------------
 - add
 - -----------------------------------------------------------------
 - Description:
 -  add does nothing but it adds the two x vectors (eg: x1 and x2) , two y vectors (eg:y1 and y2) and two z vectors (eg: z1 and z2)
   # Stack Tests-
   1) Input -  add (5,4,3) (7,3,2)
   Output - (12.0,7.0,5.0)
   2) Input - add (7,8,2) (4,1,9)
   Output - (11.0,9.0,11.0)
   3) Input - add (89,76,56) (56,78,45)
   Output - (145.0,154.0,101.0)
 -}
add :: Vector3D -> Vector3D -> Vector3D
add (x1,y1,z1) (x2,y2,z2) = ( x1 + x2 , y1 + y2 , z1 + z2 )


{- -----------------------------------------------------------------
 - innerProduct
 - -----------------------------------------------------------------
 - Description:
 - innerProduct is multiplying the two x vectors (x1 , x2) , two  y vectors (y1,y2) and two z vectors (z1,z2) ,
 and then add all the values up.
  # Stack Tests -
  1) Input - innerProduct (3,45,67) (65,23,22)
  Output - 2704.0
  2) Input - innerProduct (2,3,4) (4,5,6)
  Output - 47.0
  3) Input - innerproduct (32,34,56) (12,15,23)
  Output - 2182.0


 -}
innerProduct :: Vector3D -> Vector3D -> Double
innerProduct (x1,y1,z1) (x2,y2,z2) = ( x1 * x2 + y1 * y2 + z1 * z2 )

{- -----------------------------------------------------------------
 - distance
 - -----------------------------------------------------------------
 - Description:
 -  It returns the distance between two vectors . By using the functions described above , i.e - innerProduct , add ,and scalarMult .
  # Stack Tests-
  1) Input - distance (78,90,68) (62,54,50)
  Output - 43.31281565541543
  2) Input - distance (65,45,50) (23,14,96)
  Output - 69.57729514719583
  3) Input - distance (32,19,83) (22,100,750)
  Output - 671.9747019047667
 -}
distance :: Vector3D -> Vector3D -> Double
distance p q= ( innerProduct ( add p ( scalarMult(-1) q ) ) (add p (scalarMult(-1) q ) ) )**(1/2)

{- ------------------------------------------------------------------------
 - maxDistance
 - ------------------------------------------------------------------------
 - Description:
 -  I have done this using guarded expressions and implementing list.
  This function returns the vector that has maxDistance from the origin , that is , (0,0,0). 
 # Stack Tests - 
 1) Input - maxDistance [(33,44,55),(8,9,10)]
 Output - (33.0,44.0,55.0)
 2) Input - maxDistance [(12,48,18),(43,99,100)]
 Output - (43.0,99.0,100.0)
 3) Input - maxDistance [(45,34,56),(15,75,234)]
Output - (15.0,75.0,234.0)
 
 -}
maxDistance :: [Vector3D] -> Vector3D
maxDistance [] = (0,0,0)
maxDistance (g:gs)
  | distance (0,0,0) (g) == distance(0,0,0) (maxDistance (gs)) = (g)
  | distance (0,0,0) (g) > distance (0,0,0) (maxDistance (gs)) = (g)
  | otherwise = maxDistance (gs) 