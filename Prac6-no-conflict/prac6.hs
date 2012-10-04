module Prac6 where

import Prelude
import FPPrac.Graphics
import FPPrac.Events
import Graphics
import Data.List

import System.FilePath (splitPath, dropExtension)

import CreateGraph
import Debug.Trace

data MyStore = MyStore
  { 
    myGraph :: Graph,
	isBpressed :: Bool,
    isEpressed :: Bool
  }

initPrac6 graph = MyStore {myGraph = graph, isEpressed = False}

main = doGraph doPrac6 initPrac6 myGraph drawMypracBottomLine

doPrac6 :: MyStore -> Input -> (MyStore,[Output])
-- =======================================
-- = Voeg hier extra doPrac6 clauses toe =
-- =======================================
-- doPrac6 myStore (KeyIn 'r') = (myStore', o)
--   where
--     myStore' = ...
--     o        = ...
--

doPrac6 myStore (KeyIn 'e') = (myStore', [])
    where
        myStore' = myStore{isEpressed = True}

doPrac6 myStore (KeyIn 'b') = (myStore', [])
    where
        myStore' = myStore{isBpressed = True}

doPrac6 myStore@MyStore{myGraph = graph} (KeyIn 'c') = (myStore', o)
    where
        graph' = trace "hey" graph{nodes = (clearColor (nodes graph))}
        myStore' = myStore{myGraph = graph'}
        o = [DrawPicture $ drawGraph graph']
  
doPrac6 myStore@MyStore{myGraph = graph, isEpressed = True} (MouseDown (x,y))
	| n == Nothing = (myStore{isEpressed = False}, [])
	| otherwise = (myStore', o)
	where
		graph' = (redColorNode graph i)
		myStore' = myStore{myGraph=graph' , isEpressed = False}
		o = [DrawPicture $ drawGraph graph']
		n = onNode (nodes graph) (x,y)
		Just i = n

doPrac6 myStore@MyStore{myGraph = graph, isBpressed = True} (MouseDown (x,y))
	| n == Nothing = (myStore{isBpressed = False}, [])
	| otherwise = (myStore', o)
	where
		graph' = (makeNeighboursBlue graph i)
		myStore' = myStore{myGraph=graph' , isBpressed = False}
		o = [DrawPicture $ drawGraph graph']
		n = onNode (nodes graph)
        Just i = n
       }
{-
makeNeighboursBlue:: Graph-> (Char, Color, Point) -> Graph
makeNeighboursBlue myGraph@Graph{nodes = nodes, directed = directed, edges = edges} nod = myGraph{nodes = (makeNeighboursBlueList directed edges nodes nod), directed = directed, edges = edges}

makeNeighboursBlueList :: Bool -> [Edge] -> [(Char, Color, Point)] -> (Char, Color, Point) -> [(Char, Color, Point)]
makeNeighboursBlueList _ [] nodes _ = nodes
makeNeighboursBlueList directed ((ch1, ch2, col, n) :xs) nodes (ch, co, po) = --Find correct edges and call colorNeighbours
	| ch1 == ch = makeNeighboursBlueList directed xs (blueColorNodeList nodes ch1) (ch, co, po)
	| ch2 == ch && not(directed) = makeNeighboursBlueList directed xs (blueColorNodeList nodes ch2) (ch, co, po)
	| otherwise = makeNeighboursBlueList directed xs nodes (ch, co, po)
	
blueColorNodeList :: [(Char,Color,Point)] -> Char -> [(Char,Color,Point)]
blueColorNodeList ((ch, co, po):xs) ch1
	| ch1 == ch = (ch, blue, po) : xs
	| otherwise = (ch, co, po) : blueColorNodeList xs ch1
-}
doPrac6 myStore i = (myStore,[])

redColorNode :: Graph -> (Char,Color,Point) -> Graph
redColorNode myGraph@Graph{nodes = nodes} nod = myGraph{nodes = (redColorNodeList nodes nod)}

redColorNodeList :: [(Char,Color,Point)] -> (Char,Color,Point) -> [(Char,Color,Point)]
redColorNodeList (x:xs) (ch, co, po) 
	| equal (ch, co, po) x = (ch, red, po) : xs
	| otherwise = x : redColorNodeList xs (ch, co, po)
    	
equal :: (Char, Color, Point) -> (Char, Color, Point) -> Bool
equal (ch1, co1, po1) (ch2, co2, po2) = (ch1 == ch2) && (co1 == co2) && (po1 == po2)

clearColor :: [(Char, Color, Point)] -> [(Char, Color, Point)]
clearColor [] = []
clearColor ((c, cl, p):xs) = (c, white, p):clearColor xs

isCompleteGraph :: Graph -> Bool
isCompleteGraph g = foldr (&&) True [hasEdge x y edgs | x <- nods, y <- (nods \\ (x:[]))]
    where
        nods = (nodes g)
        edgs = (edges g)
        
hasEdge :: (Char, Color, Point) -> (Char, Color, Point) -> [(Char,Char,Color,Int)] -> Bool
hasEdge _ _ [] = False
hasEdge (ch1, c1, p1) (ch2, c2, p2) ((che1, che2, _, _):xs)
    | ch1 == che1 && ch2 == che2 = True
    | ch1 == che2 && ch2 == che1 = True
    | otherwise = hasEdge (ch1, c1, p1) (ch2, c2, p2) xs

testTrace :: String -> Bool
testTrace s = trace s True

drawMypracBottomLine :: Graph -> Picture
drawMypracBottomLine graph =
  Pictures
    [ Translate 0 (-300 + bottomLineHeight / 2) $ Color white $ rectangleSolid 800 bottomLineHeight
    , Color black $ Line [(-400,height1),(400,height1)]
    , Color black $ Line [(-240,height1),(-240,-300)]
    , Translate (-392) height2 $ Color black $ Scale 0.11 0.11 $ Text "myprac:"
    , Translate (-332) height2 $ Color red   $ Scale 0.11 0.11 $ Text $ (case (name graph) of "" -> "" ; xs -> dropExtension $ last $ splitPath xs)
    -- Vervang onderstaande tekst, indien nodig, door extra informatie
    , Translate (-235) height2 $ Color black $ Scale 0.11 0.11 $ Text "Press 'q' to return to node-drawing"
    ]
    where
      height1 = -300 + bottomLineHeight
      height2 = -300 + bottomTextHeight
