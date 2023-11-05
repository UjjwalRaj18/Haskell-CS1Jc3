{-|
Module      : 1JC3-Assign3.Assign_3.hs
Copyright   :  (c) Curtis D'Alves 2021
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Assignment 3 - McMaster CS 1JC3 2021
-}
module Assign_3 where

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

-- Name: Ujjwal Raj 
-- Date: 21 November , 2021
macid :: String
macid = "raju1"

{- -----------------------------------------------------------------
 - Datatypes
 - -----------------------------------------------------------------
 -}
data Graph a = Graph { getNodes :: [Node a]
                     , getEdges :: [Edge] }
  deriving (Show,Eq)

type Edge = (NodeID,NodeID)
type NodeID = Int

data Node a = Node { getNodeID  :: NodeID,
                     getNodeVal :: a }
  deriving (Show,Eq,Ord)


{- -----------------------------------------------------------------
 - Example Graph
 - -----------------------------------------------------------------
 -              -----        ----------
 -              | A |------->| C |    |
 -              -----        ----- <---
 -                |           |
 -                |     ------|
 -                v     |
 -              ----- <-|
 -              | B |
 -              -----
 -}
nodeA,nodeB,nodeC :: Node Char
nodeA = Node 0 'A'
nodeB = Node 1 'B'
nodeC = Node 2 'C'

exGraph :: Graph Char
exGraph =
  let
    nodes = [nodeA,nodeB,nodeC]
    edges = [(0,1),(0,2),(2,2),(2,1)]
  in Graph nodes edges

{- -----------------------------------------------------------------
 - maxNodeID
 - -----------------------------------------------------------------
 - Description:
 - I have used list comprehension for this question , so getNodeId helps me get the list of nodes and
maximum gives the max Node and 'Just' is the data type as in the function definition it says 'Maybe NodeID'.

 -}
maxNodeID :: Graph a -> Maybe NodeID
maxNodeID (Graph node edge) = Just (maximum [getNodeID x | x <- node])
{- -----------------------------------------------------------------
 - insertNode
 - -----------------------------------------------------------------
 - Description:
 - My code  inserts a new Node with the given value into a Graph exactly what was needed by the question .
 
 -}
insertNode :: a -> Graph a -> Graph a
insertNode g (Graph [] [n]) = Graph [Node 0 g] [n] 
insertNode g (Graph [] [] ) = Graph [Node 0 g] []
insertNode v (Graph n g) = Graph (n++[Node c v]) g
  where c = maximum[getNodeID x | x <-n ] + 1



{- -----------------------------------------------------------------
 - removeNode
 - -----------------------------------------------------------------
 - Description:
 -   My code removes any Node with the given NodeID from the given graph . For this as well ,
 I have used List comprehension and functions like 'fst' which takes first element of a tuple 
 and 'snd' which takes the second element of the given tuple .
 -}
removeNode :: NodeID -> Graph a -> Graph a
removeNode nID (Graph n e)= Graph bettern bettere
  where 
    bettern = [ x | x <-  n , getNodeID x /= nID ]
    bettere = [y | y <- e,  (fst y)  /= nID && (snd y) /= nID]


{- -----------------------------------------------------------------
 - lookupNode
 - -----------------------------------------------------------------
 - Description:
 - my code returns the Node corresponding to the given NodeID in the given Graph, If nothing is given it returns nothing. I have used Guarded Functions for this .
 -}
lookupNode :: NodeID -> Graph a -> Maybe (Node a)
lookupNode nID (Graph [] _) = Nothing
lookupNode nID (Graph (g:gs) f) 
  | getNodeID g == nID = Just g
  | otherwise = lookupNode nID (Graph gs f)
{- -----------------------------------------------------------------
 - insertEdge
 - -----------------------------------------------------------------
 - Description:
  my code inserts an
edge from the Node with the given NodeID in the first part of the tuple
to the Node with the given NodeID in the second part of the tuple.
If the edge already exists, it should NOT introduce a duplicate. If
the nodes corresponding to either of the given NodeIDs do not already
exist, the function should return Nothing.

3

 -  
 -}
insertEdge :: Eq a => (NodeID,NodeID) -> Graph a -> Maybe (Graph a)
insertEdge _ (Graph [] _)  = Nothing
insertEdge (n1,n2) (Graph ns es)
  | not containsBothNodes = Nothing
  | containsEdge          = Just(Graph ns es)
  | otherwise             = Just(Graph ns(es ++[(n1,n2)]))
  where
    containsBothNodes :: Bool
    containsBothNodes = elem n1 [getNodeID d | d<- ns] && elem n2 [getNodeID d | d<- ns]
    containsEdge :: Bool
    containsEdge = (n1,n2) `elem` es
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
{- Function Name : maxNodeID
Test case Number 1:
  Input: Graph {getNodes = [Node {getNodeID = 1, getNodeVal = 'M'},Node {getNodeID = 2, getNodeVal = 'N'},Node {getNodeID = 3, getNodeVal = 'P'}], getEdges = [(2,7),(7,8),(9,6),(9878,78899)]}
  Expected Output : Just 3
  Actual Output   : Just 3
Test Case Number 2:
  Input: Graph {getNodes = [Node {getNodeID = 8, getNodeVal = 'J'},Node {getNodeID = 7, getNodeVal = 'K'},Node {getNodeID = 9, getNodeVal = 'S'}], getEdges = [(3,5),(12,18),(19,16),(187,678)]}
  Expected Output : Just 9
  Actual Output : Just 9
Test Case Number 3:
  Input: Graph {getNodes = [Node {getNodeID = 23, getNodeVal = 'I'},Node {getNodeID = 45, getNodeVal = 'Q'},Node {getNodeID = 34, getNodeVal = 'O'}], getEdges = [(43,65),(17,98),(69,56),(237,455)]}
  Expected Output : Just 45
  Actual output : Just 45
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------}
{- Function Name : insertNode
  -Test Case Number 1:
  Input : insertNode 'Z' Graph {getNodes = [Node {getNodeID = 15, getNodeVal = 'P'},Node {getNodeID = 67, getNodeVal = 'O'},Node {getNodeID = 45, getNodeVal = 'F'}], getEdges = [(13,347),(212,3454),(1452,2318)]}
  Expected Output : Graph {getNodes = [Node {getNodeID = 15, getNodeVal = 'P'},Node {getNodeID = 67, getNodeVal = 'O'},Node {getNodeID = 45, getNodeVal = 'F'},Node {getNodeID = 68, getNodeVal = 'Z'}], getEdges = [(13,347),(212,3454),(1452,2318)]}
  Actual Output : Graph {getNodes = [Node {getNodeID = 15, getNodeVal = 'P'},Node {getNodeID = 67, getNodeVal = 'O'},Node {getNodeID = 45, getNodeVal = 'F'},Node {getNodeID = 68, getNodeVal = 'Z'}], getEdges = [(13,347),(212,3454),(1452,2318)]}
  -Test Case Number 2:
  Input :insertNode 'F' Graph {getNodes = [Node {getNodeID = 34, getNodeVal = 'C'},Node {getNodeID = 6437, getNodeVal = 'I'},Node {getNodeID = 2345, getNodeVal = 'Q'}], getEdges = [(1343,3147),(21232,3456),(14212,24568)]}
  Expected Output :Graph {getNodes = [Node {getNodeID = 34, getNodeVal = 'C'},Node {getNodeID = 6437, getNodeVal = 'I'},Node {getNodeID = 2345, getNodeVal = 'Q'},Node {getNodeID = 6438, getNodeVal = 'F'}], getEdges = [(1343,3147),(21232,3456),(14212,24568)]}
  Actual Output :Graph {getNodes = [Node {getNodeID = 34, getNodeVal = 'C'},Node {getNodeID = 6437, getNodeVal = 'I'},Node {getNodeID = 2345, getNodeVal = 'Q'},Node {getNodeID = 6438, getNodeVal = 'F'}], getEdges = [(1343,3147),(21232,3456),(14212,24568)]}


  --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------}
{-Function Name : 
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------}
{-Function Name : removeNode 
- Test case Number 1:
 Input - removeNode 5 Graph {getNodes = [Node {getNodeID = 6799744, getNodeVal = 'Y'},Node {getNodeID = 423, getNodeVal = 'X'},Node {getNodeID = 67, getNodeVal = 'I'}], getEdges = [(17,45),(323,15),(231,2347)]}
 Expected Output - Graph {getNodes = [Node {getNodeID = 6799744, getNodeVal = 'Y'},Node {getNodeID = 423, getNodeVal = 'X'},Node {getNodeID = 67, getNodeVal = 'I'}], getEdges = [(17,45),(323,15),(231,2347)]}
 Actual Output - Graph {getNodes = [Node {getNodeID = 6799744, getNodeVal = 'Y'},Node {getNodeID = 423, getNodeVal = 'X'},Node {getNodeID = 67, getNodeVal = 'I'}], getEdges = [(17,45),(323,15),(231,2347)]}




 -}
