module Prac6 where

import Prelude
import FPPrac.Graphics
import FPPrac.Events
import Graphics

import System.FilePath (splitPath, dropExtension)

import CreateGraph
import Debug.Trace

data MyStore = MyStore
  { 
    myGraph :: Graph,
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
        
doPrac6 myStore (MouseUp (x,y)) | n == Nothing = (myStore, [])
								| otherwise = (myStore', o)
	where
		myStore' = myStore{myGraph= (redColorNode graph i), isEpressed = False}
		MyStore{myGraph = graph} = myStore
		o = []
		n = onNode (x,y)
		Just i = n
doPrac6 myStore i = (myStore,[])

redColorNode :: Graph -> (Char,Color,Point) -> (Char,Color,Point)
redColorNode (x:nodeList) node 
	| curNode == node =
	| 
	where
		nodeList = Graph{nodes = nodes}
		curNode = take nodeList

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
