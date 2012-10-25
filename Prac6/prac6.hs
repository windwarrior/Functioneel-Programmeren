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
    isEpressed :: Bool,
    isBpressed :: Bool,
    isDpressed :: Bool
  }

initPrac6 graph = MyStore {myGraph = graph, isEpressed = False, isBpressed = False, isDpressed = False}

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

doPrac6 myStore (KeyIn 'd') = (myStore', [])
    where
        myStore' = myStore{isDpressed = True}

doPrac6 myStore@MyStore{myGraph = graph, isDpressed = True} (MouseDown (x,y))
	| n == Nothing = (myStore{isDpressed = False}, [])
	| otherwise = (myStore', o)
	where
		graph' = (makeBlackAndWhite graph i)
		myStore' = myStore{myGraph=graph' , isDpressed = False}
		o = [DrawPicture $ drawGraph graph']
		n = onNode (nodes graph) (x,y)
		Just i = n

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
		n = onNode (nodes graph) (x,y)
		Just i = n
        
doPrac6 myStore i = (myStore,[])

        
------------------------COLOR RED---------------------------
        
redColorNode :: Graph -> (Char,Color,Point) -> Graph
redColorNode myGraph@Graph{nodes = nodes} nod = myGraph{nodes = (redColorNodeList nodes nod)}

redColorNodeList :: [(Char,Color,Point)] -> (Char,Color,Point) -> [(Char,Color,Point)]
redColorNodeList (x:xs) (ch, co, po) 
	| equal (ch, co, po) x = (ch, red, po) : xs
	| otherwise = x : redColorNodeList xs (ch, co, po)
    
-----------------------COLOR BLUE----------------------------------

makeNeighboursBlue:: Graph-> (Char, Color, Point) -> Graph
makeNeighboursBlue myGraph@Graph{nodes = nodes, directed = directed, edges = edges} nod = myGraph{nodes = (makeNeighboursBlueList directed edges nodes nod), directed = directed, edges = edges}

makeNeighboursBlueList :: Bool -> [Edge] -> [(Char, Color, Point)] -> (Char, Color, Point) -> [(Char, Color, Point)]
makeNeighboursBlueList _ [] nodes _ = nodes
makeNeighboursBlueList directed ((ch1, ch2, col, n) :xs) nodes (ch, co, po)  --Find correct edges and call colorNeighbours
	| ch1 == ch = makeNeighboursBlueList directed xs (blueColorNodeList nodes ch2) (ch, co, po)
	| ch2 == ch && not(directed) = makeNeighboursBlueList directed xs (blueColorNodeList nodes ch1) (ch, co, po)
	| otherwise = makeNeighboursBlueList directed xs nodes (ch, co, po)
	
blueColorNodeList :: [(Char,Color,Point)] -> Char -> [(Char,Color,Point)]
blueColorNodeList ((ch, co, po):xs) ch1
	| ch1 == ch = (ch, blue, po) : xs
	| otherwise = (ch, co, po) : blueColorNodeList xs ch1

-----------------COLOR WHITE-------------------------------------

makeBlackAndWhite:: Graph-> (Char, Color, Point) -> Graph
makeBlackAndWhite myGraph@Graph{nodes = nodes, directed = directed, edges = edges} nod = myGraph{nodes = makeNodesWhite nodes, directed = directed, edges = makeEdgesBlack edges}

makeNodesWhite :: [(Char, Color, Point)] -> [(Char, Color, Point)]
makeNodesWhite [] = []
makeNodesWhite ((ch, co, po):xs) = (ch, white, po) : (makeNodesWhite xs)

makeEdgesBlack :: [Edge] -> [Edge]
makeEdgesBlack [] =[]
makeEdgesBlack ((ch1, ch2, col, n):xs) = (ch1, ch2, black, n) : (makeEdgesBlack xs)

------------------------------------------------------------------

testVolledigeGraaf edges nodes = volledigeGraaf edges' nodes'
    where
        edges' = getCharsEdges edges 
        nodes' = listComprehension (getCharsNodes nodes)

volledigeGraaf :: [(Char, Char)] -> [(Char, Char)] -> Bool
-- volledigeGraaf edges nodes
volledigeGraaf [] [] = True
volledigeGraaf [] _ = False
volledigeGraaf _ [] = True
volledigeGraaf edges ((ch1, ch2):xs)
 | (ch1, ch2) `elem` edges = volledigeGraaf edges xs
 | (ch2, ch1) `elem` edges = (volledigeGraaf edges xs)
 | otherwise = False

getCharsNodes :: [(Char,Color, Point)] -> [Char]
getCharsNodes [] = []
getCharsNodes ((ch, co, po):xs) = ch : (getCharsNodes xs)

getCharsEdges :: [Edge] -> [(Char, Char)]
getCharsEdges [] = []
getCharsEdges ((ch1, ch2, co, n):xs) = (ch1, ch2) : (getCharsEdges xs)

listComprehension (x:[]) = []
listComprehension (x:xs) = [(i,j) | i <- [x], j <- xs] ++ listComprehension xs

-------------------------------------------------------------------------------------------

isSamenhangend :: [Edge] -> [(Char, Color, Point)] -> Bool
isSamenhangend edges ((ch, co, po):xs) = bereikbaar == nodes'
    where
        nodes' = sort (getCharsNodes ((ch, co, po):xs))
        bereikbaar = sort (nub (getBereikbareNodes ch edges))

getBereikbareNodes :: Char -> [Edge] -> [Char]
getBereikbareNodes ch [] = []
getBereikbareNodes ch edges = edge ++ (concat (map (\x -> getBereikbareNodes x unvisited) edge))
    where 
       buren = getNeighbours ch edges
       edge = concat (map (\(x,y) -> [x,y]) (getCharsEdges buren))
       unvisited = edges \\ buren
       
getNeighbours :: Char -> [Edge] -> [Edge]
getNeighbours ch [] = []
getNeighbours ch ((ch1, ch2, co, n):xs)
    | ch == ch1 || ch == ch2 = ((ch1, ch2, co, n) : (getNeighbours ch xs))
    | otherwise = getNeighbours ch xs

------------------------------------------------------------------------------------------
equal :: (Char, Color, Point) -> (Char, Color, Point) -> Bool
equal (ch1, co1, po1) (ch2, co2, po2) = (ch1 == ch2) && (co1 == co2) && (po1 == po2)

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
      
------------------------------ TESTGRAPHS


testNodes2a = [('a', red, (1,2)),('d', red, (1,2)),('c', red, (1,2)),('b', red, (1,2))] 
testEdges2a = [('b', 'a', red,1),('a', 'd', red,1),('a', 'c', red,1),('d', 'b', red,1),('d', 'c', red,1),('c', 'b', red,1)]