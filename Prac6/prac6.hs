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

doPrac6 myStore@MyStore{myGraph = graph} (KeyIn 'f') = (myStore', o)
	where
		graph' = (kleur graph [red, green, blue])
		myStore' = myStore{myGraph=graph'}
		o = [DrawPicture $ drawGraph graph']

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

------------------------------------------------------------------2a

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

-------------------------------------------------------------------------------------------2b

isSamenhangend :: [Edge] -> [(Char, Color, Point)] -> Bool
isSamenhangend edges ((ch, co, po):xs) = bereikbaar == nodes'
    where
        nodes' = sort (getCharsNodes ((ch, co, po):xs))
        bereikbaar = sort (nub (getBereikbareNodes ch edges)) -- onnodig veel berekeningen om dat bij elke iteratie van getBereikbareNodes te doen

getBereikbareNodes :: Char -> [Edge] -> [Char]
getBereikbareNodes ch [] = [ch  ]
getBereikbareNodes ch edges = ch : (edge ++ (concat (map (\x -> getBereikbareNodes x unvisited) edge)))
    where 
       buren = getNeighbours ch edges
       edge = concat (map (\(x,y) -> [x,y]) (getCharsEdges buren))
       unvisited = edges \\ buren
       
getNeighbours :: Char -> [Edge] -> [Edge]
getNeighbours ch [] = []
getNeighbours ch ((ch1, ch2, co, n):xs)
    | ch == ch1 || ch == ch2 = ((ch1, ch2, co, n) : (getNeighbours ch xs))
    | otherwise = getNeighbours ch xs

------------------------------------------------------------------------------------------2c

kleur :: Graph -> [Color] -> Graph
kleur myGraph@Graph{nodes = nodes, directed = directed, edges = edges} col 
    = myGraph{nodes = nodes', directed = directed, edges = edges}
    where
        nodes' = (colorSubgraphs edges nodes (getSubgrafen edges nodes) col)
    
colorSubgraphs :: [Edge] -> [(Char, Color, Point)] -> [[Char]] -> [Color] -> [(Char, Color, Point)] 
colorSubgraphs _ [] _ _ = []
colorSubgraphs edges ((ch, co, po):xs) subgrafen colors = (ch, (colors !! i ), po): (colorSubgraphs edges xs subgrafen colors)
    where
        i = getSubgraphNumber ch subgrafen

getSubgraphNumber :: Char -> [[Char]] -> Int
getSubgraphNumber ch (x:xs)
    | ch `elem` x = 0
    | otherwise = 1 + getSubgraphNumber ch xs

getSubgrafen :: [Edge] -> [(Char, Color,Point)] -> [[Char]]
getSubgrafen edges nodes = grafen
    where
        nodes' = getCharsNodes nodes
        grafen = nub (map (\x -> sort $ nub x) (map (\x -> getBereikbareNodes x edges) nodes'))

------------------------------------------------------------------------------------------3a

kanNodeBereiken :: Char -> Char -> [Edge] -> Bool
kanNodeBereiken a b edges = b `elem` bereikbaar
    where 
        bereikbaar = sort (nub (getBereikbareNodesDirected a edges))

getBereikbareNodesDirected :: Char -> [Edge] -> [Char]
getBereikbareNodesDirected ch [] = [ch  ]
getBereikbareNodesDirected ch edges = ch : (edge ++ (concat (map (\x -> getBereikbareNodesDirected x unvisited) edge)))
    where 
       buren = getNeighboursDirected ch edges
       edge = concat (map (\(x,y) -> [x,y]) (getCharsEdges buren))
       unvisited = edges \\ buren
       
getNeighboursDirected :: Char -> [Edge] -> [Edge]
getNeighboursDirected ch [] = []
getNeighboursDirected ch ((ch1, ch2, co, n):xs)
    | ch == ch1 = ((ch1, ch2, co, n) : (getNeighbours ch xs))
    | otherwise = getNeighboursDirected ch xs

------------------------------------------------------------------------------------------3b

vindPadenVan :: Char -> Char -> [Edge] -> [Char] -> [[Char]]
vindPadenVan a b edges visited
    | not(b `elem`visited) = map (a:) (concat (map (\x -> (vindPadenVan x b edges (visited ++ [x]))) onPath))
    | otherwise = [b:""]
    where
        buren = (map (\(_, x, _, _) -> x) (getNeighboursDirected a edges)) 
        unvisited = buren \\ visited
        onPath = filter (\x -> kanNodeBereiken x b edges) unvisited

charsToPath :: [Char] -> [Edge] -> [Edge]
charsToPath [] _ = []
charsToPath (ch:[]) _ = []
charsToPath (ch:chars) edges = (getEdge ch1 ch2 edges) ++ (charsToPath chars edges)
    where
        ch1 = ch
        ch2 = head chars

getEdge :: Char -> Char -> [Edge] -> [Edge]
getEdge _ _ [] = []
getEdge ch1 ch2 ((ch3, ch4, co, n):xs)
    | ch1 == ch3 && ch2 == ch4 = [(ch3,ch4,co,n)]
    | otherwise = getEdge ch1 ch2 xs
    
    
colorEdgesRed :: [Edge] -> [Edge]
colorEdgesRed ((ch1, ch2, co, n):xs) = (ch1, ch2, red, n):(colorEdgesRed xs)
------------------------------------------------------------------------------------------3c

getWeight :: [Edge] -> Int
getWeight [] = 0
getWeight ((ch1, ch2, co, n):xs) = n + getWeight xs

getSmallestPath :: Char -> Char -> [Edge] -> ([Char], Int, [Edge])
getSmallestPath c1 c2 edges = ((paths !! i), smallestPath, (edgePaths !! i))
    where
        paths = vindPadenVan c1 c2 edges (c1:"")
        edgePaths = map  (\x -> charsToPath x edges) paths
        weights = map getWeight (edgePaths)
        smallestPath = head (sort weights)
        indexSmallestPath = elemIndex smallestPath weights
        Just i = indexSmallestPath
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
testEdges2b = [('b', 'a', red,1),('c', 'd', red,1)]

testNodes2c = [('a', red, (3,4)),('b', red, (3,4)),('c', red, (3,4))]
testEdges3a = [('a', 'b', red, 1),('a', 'c', red, 1), ('d', 'a', red, 1), ('c', 'd', red, 1)]
testEdges3b = [('z', 'a', red,1),('a', 'c', red, 1),('a', 'd', red, 1), ('a', 'e', red, 1), ('c', 'g', red, 1), ('c', 'b', red, 1), ('d', 'b', red, 1), ('f', 'b', red, 1),('e', 'f', red, 1), ('g', 'h', red, 1), ('a', 'i', red, 1), ('j', 'b', red, 1),('z', 'c', red, 1)]