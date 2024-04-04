module Main (main) where
import Graphics.Gloss(Display(InWindow), white, display, rectangleSolid)

-- Este test verifica si pueden usar gloss.
main :: IO ()
main = do
    display (InWindow "Test" (200, 200) (0,0)) white (rectangleSolid 30 40)
