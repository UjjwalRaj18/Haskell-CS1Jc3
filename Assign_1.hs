{-|
Module      : 1JC3-Assign1.Assign_1.hs
Copyright   :  (c) Curtis D'Alves 2021
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Assignment 1 - McMaster CS 1JC3 2021
  Name: Ujjwal Raj 
  Date: 3rd october,2021
-}
module Assign_1 where

import Prelude hiding (sin,cos,tan)

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
macid :: String
macid = "raju1"



{- -----------------------------------------------------------------
 - factorial
 - -----------------------------------------------------------------
 - Description:
 -    Computes the factorial of any Integer n
 - -----------------------------------------------------------------
 - |   Input     |                                                 |
 - |      n      | Integer input                                   |
 - -----------------------------------------------------------------
 - |   Output    |                                                 |
 - |      n <= 1 | 1                                               |
 - |      n >  1 | n * (n-1) ...  * 1   while (n-k) > 0            |
 - -----------------------------------------------------------------
 -}
factorial :: Integer -> Integer
factorial n = if n > 0
              then n * factorial (n-1)
              else 1

{- ------------------------------------------------------------------
 - sinTaylor
 - ------------------------------------------------------------------
 - Description:
 -  I have coded the equation 4 in the assignment 1 template given to us , I have used "*" for multiplication, and '**' for exponential power 
 apart from them , i have used '/' for division,fromIntegral to convert the answer into integer form and factorial function which was given to us predifined above .
 Last but not the least , I used parenthesis where ever needed . 
 -}
sinTaylor :: Double -> Double -> Double -> Double -> Double
sinTaylor a cos_a sin_a x =  (sin_a * (x-a)**0  / fromIntegral (factorial 0)) + (cos_a* (x-a)**1 / fromIntegral(factorial 1)) + ((-1) * sin_a* (x-a)**2  / fromIntegral(factorial 2)) + ((-1)  * cos_a * (x-a)**3 / fromIntegral(factorial(3))) + (sin_a* (x-a)**4  / fromIntegral(factorial(4))) 
{- -----------------------------------------------------------------
 - fmod
 - -----------------------------------------------------------------
 - Description:
 - In this i used fromintegral(floor( x / y)) because floor functions rounds off the answer and fromIntegral coverts it to integer . fmod returns the remainder.
 -}
fmod :: Double -> Double -> Double
fmod x y =  
  let
    -- z is the largest integer s.t. z*y <= x
    -- HINT use floating point division, then round down
    z =  fromIntegral(floor( x / y ))  
  in x - z*y

{- ----------------------------------------------------------------------
 - sinApprox
 - ----------------------------------------------------------------------
 - Description:
 -   I coded down the values given to us in the table , in the left hand side we have specified. In this , the function takes a value x,
 for which it returns the value of sinx . In this, I have used guarded expressions as it is the best alternative for if and else statement.    

 -}
sinApprox :: Double -> Double
sinApprox x 
  |(0 <= (fmod x (2*pi)) &&  (fmod x (2*pi) < (pi/4))) = sinTaylor 0 1 0 (fmod x (2*pi))
  |((pi/4) <= (fmod x (2*pi)) &&  (fmod x (2*pi) < (3*pi/4))) = sinTaylor (pi/2) 0 1 (fmod x (2*pi))
  |((3*pi/4) <= (fmod x (2*pi)) && (fmod x (2*pi) < (5*pi/4))) = sinTaylor pi (-1) 0 (fmod x (2*pi)) 
  |((5*pi/4) <= (fmod x (2*pi)) && (fmod x (2*pi) < (7*pi/4))) = sinTaylor (3*pi/2) 0 (-1) (fmod x (2*pi))
  |otherwise = sinTaylor (2*pi) 1 0 (fmod x (2*pi))
{- ---------------------------------------------------------------------
 - sinApprox
 - ---------------------------------------------------------------------
 - Description:
 -   The code in this is very starightforward as we have only used the trigonometric identity of cosApprox in terms of sinApprox .
 -}
cosApprox :: Double -> Double
cosApprox x =  -1 * sinApprox (x - (pi/2))

{- ---------------------------------------------------------------------
 - tanApprox
 - ---------------------------------------------------------------------
 - Description:
 -  In this i have used nothing but the simple trigonometric identity where in tanApprox = sinAprrox x / cosApprox x 
 -}
tanApprox :: Double -> Double
tanApprox x = sinApprox x / cosApprox x 
