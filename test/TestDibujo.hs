module Main (main) where

import Test.HUnit
import Dibujo (figura, rotar, espejar, rot45, apilar, juntar, encimar, r90, r180, r270, encimar4, cuarteto, ciclar, mapDib, change, foldDib, figuras, Dibujo(..))
import Dibujos.Feo (BasicaSinColor(Triangulo, Rectangulo))

-- Define some basic Dibujo instances for testing
dib1, dib2 :: Dibujo BasicaSinColor
dib1 = figura Triangulo
dib2 = figura Rectangulo

-- Define a transformation function for testing
toRectangulo :: BasicaSinColor -> Dibujo BasicaSinColor
toRectangulo _ = dib2

-- Define the test cases
testFigura :: Test
testFigura = "figura creates a Dibujo instance with a single figure" ~: dib1 ~?= figura Triangulo

testRotar :: Test
testRotar = "rotar creates a Dibujo instance that is a rotation of another Dibujo" ~: rotar dib1 ~?= Rotar dib1

testEspejar :: Test
testEspejar = "espejar creates a Dibujo instance that is a mirror image of another Dibujo" ~: espejar dib1 ~?= Espejar dib1

testRot45 :: Test
testRot45 = "rot45 creates a Dibujo instance that is a 45-degree rotation of another Dibujo" ~: rot45 dib1 ~?= Rot45 dib1

testApilar :: Test
testApilar = "apilar creates a Dibujo instance that is a stack of two other Dibujo instances" ~: apilar 0.5 0.5 dib1 dib2 ~?= Apilar 0.5 0.5 dib1 dib2

testJuntar :: Test
testJuntar = "juntar creates a Dibujo instance that is a combination of two other Dibujo instances" ~: juntar 0.5 0.5 dib1 dib2 ~?= Juntar 0.5 0.5 dib1 dib2

testEncimar :: Test
testEncimar = "encimar creates a Dibujo instance that is a superposition of two other Dibujo instances" ~: encimar dib1 dib2 ~?= Encimar dib1 dib2

testR90 :: Test
testR90 = "r90 creates a Dibujo instance that is a 90-degree rotation of another Dibujo" ~: r90 dib1 ~?= Rotar dib1

testR180 :: Test
testR180 = "r180 creates a Dibujo instance that is a 180-degree rotation of another Dibujo" ~: r180 dib1 ~?= Rotar (Rotar dib1)

testR270 :: Test
testR270 = "r270 creates a Dibujo instance that is a 270-degree rotation of another Dibujo" ~: r270 dib1 ~?= Rotar (Rotar (Rotar dib1))

testEncimar4 :: Test
testEncimar4 = "encimar4 creates a Dibujo instance that is a superposition of four rotations of another Dibujo" ~: encimar4 dib1 ~?= Encimar dib1 (Encimar (r90 dib1) (Encimar (r180 dib1) (r270 dib1)))

testCuarteto :: Test
testCuarteto = "cuarteto creates a Dibujo instance that is a combination of four other Dibujo instances" ~: cuarteto dib1 dib2 dib1 dib2 ~?= Apilar 1.0 1.0 (Juntar 1.0 1.0 dib1 dib2) (Juntar 1.0 1.0 dib1 dib2)

testCiclar :: Test
testCiclar = "ciclar creates a Dibujo instance that is a combination of four rotations of another Dibujo" ~: ciclar dib1 ~?= Apilar 1.0 1.0 (Juntar 1.0 1.0 dib1 (r90 dib1)) (Juntar 1.0 1.0 (r180 dib1) (r270 dib1))

testMapDib :: Test
testMapDib = "mapDib applies a function to all figures in a Dibujo" ~: mapDib toRectangulo dib1 ~?= figura dib2

testChange :: Test
testChange = "change applies a function to all figures in a Dibujo" ~: change toRectangulo dib1 ~?= dib2

testFoldDib :: Test
testFoldDib = "foldDib applies a function to all figures in a Dibujo" ~: foldDib (\_ -> (1 :: Integer)) (+1) (+1) (+1) (\_ _ x y -> x + y) (\_ _ x y -> x + y) (+) dib1 ~?= 1

testFiguras :: Test
testFiguras = "figuras returns a list of all figures in a Dibujo" ~: figuras dib1 ~?= [Triangulo]

main :: IO Counts
main = runTestTT $ TestList
    [testFigura, testRotar, testEspejar, testRot45, testApilar, testJuntar, testEncimar, testR90, testR180, testR270, testEncimar4, testCuarteto, testCiclar, testMapDib, testChange, testFoldDib, testFiguras]