import FPPrac
import FPPrac.Graphics

main :: IO ()
main = graphicsout $ drawSudoku

drawCircleAt :: Float -> Float -> Picture
drawCircleAt x y = Pictures[ translate x y $ Color black $ circleSolid 10]

drawLineFromTo :: (Float, Float) -> (Float, Float) -> Picture
drawLineFromTo  (x1, y1) (x2, y2) = Color black $ Line [(x1, y1), (x2, y2)]


drawSudoku :: Picture
drawSudoku =  [(map (Line) [[(-300 + x * (300/4.5), 300), (-300 + x * (300/4.5), -300)]| x <- [0..8]])]