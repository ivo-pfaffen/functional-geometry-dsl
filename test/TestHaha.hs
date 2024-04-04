module Main (main) where
import Graphics.Ascii.Haha.Terminal
import Graphics.Ascii.Haha.Geometry
import Graphics.Ascii.Haha.Bitmap
import Graphics.Ascii.Haha.Plot
import System.Exit (exitFailure)

-- Este test verifica si pueden usar haha.
-- The viewport.

screen :: Rect Integer
screen = Rect (Point 1 1) (Point 40 40)

view :: Rect Double
view = Rect (Point 1 1) (Point 40 40)

main :: IO ()
main = do 
    putStr $ plot (drawPoly rectangulo (Pixel '*' magentaBold) empty)
    exitFailure

plot :: Bitmap Double Pixel -> String
plot = string True screen (Point 1 1) " " "" . list 1 view

rectangulo :: Poly Double
rectangulo = Poly $ map (\(Point x y) -> Point (x+10) (y+10))
    [Point 0 0, Point 0 20 , Point 20 20, Point 20 0, Point 0 0]
     
    
