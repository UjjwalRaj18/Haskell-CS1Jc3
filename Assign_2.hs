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

-- Name: TODO add name
-- Date: TODO add date
macid :: String
macid = "TODO"

type Vector3D = (Double,Double,Double)

{- -----------------------------------------------------------------
 - getX
 - -----------------------------------------------------------------
 - Description:
 -   TODO add comments
 -}
getX :: Vector3D -> Double
getX v = error "TODO implement getX"

{- -----------------------------------------------------------------
 - getY
 - -----------------------------------------------------------------
 - Description:
 -   TODO add comments
 -}
getY :: Vector3D -> Double
getY v = error "TODO implement getY"

{- -----------------------------------------------------------------
 - getZ
 - -----------------------------------------------------------------
 - Description:
 -   TODO add comments
 -}
getZ :: Vector3D -> Double
getZ v = error "TODO implement getY"

{- -----------------------------------------------------------------
 - scalarMult
 - -----------------------------------------------------------------
 - Description:
 -   TODO add comments
 -}
scalarMult :: Double -> Vector3D -> Vector3D
scalarMult s v = error "TODO implement scalarMult"

{- -----------------------------------------------------------------
 - add
 - -----------------------------------------------------------------
 - Description:
 -   TODO add comments
 -}
add :: Vector3D -> Vector3D -> Vector3D
add v0 v1 = error "TODO implement add"


{- -----------------------------------------------------------------
 - innerProduct
 - -----------------------------------------------------------------
 - Description:
 -   TODO add comments
 -}
innerProduct :: Vector3D -> Vector3D -> Double
innerProduct v0 v1 = error "TODO implement innerProduct"

{- -----------------------------------------------------------------
 - distance
 - -----------------------------------------------------------------
 - Description:
 -   TODO add comments
 -}
distance :: Vector3D -> Vector3D -> Double
distance v1 v2 = error "TODO implement distance"

{- ------------------------------------------------------------------------
 - maxDistance
 - ------------------------------------------------------------------------
 - Description:
 -   TODO add comments
 -}
maxDistance :: [Vector3D] -> Vector3D
maxDistance vs = error "TODO implement maxDistance"
