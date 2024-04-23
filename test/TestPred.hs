module Main (main) where

import Test.HUnit
import Pred (Pred, cambiar, anyFig, allFig, orP, andP, falla)
import Dibujo (Dibujo, figura)
import Dibujos.Feo (BasicaSinColor(Triangulo, Rectangulo))

-- Define some basic predicates for testing
isTriangulo, isRectangulo :: Pred BasicaSinColor
isTriangulo = (== Triangulo)
isRectangulo = (== Rectangulo)

-- Define a transformation function for testing
toRectangulo :: BasicaSinColor -> Dibujo BasicaSinColor
toRectangulo _ = figura Rectangulo

-- Define some basic Dibujo instances for testing
dib1, dib2 :: Dibujo BasicaSinColor
dib1 = figura Triangulo
dib2 = figura Rectangulo

-- Define the test cases
testFalla :: Test
testFalla = "falla always returns True" ~: falla ~?= True

testAndP1 :: Test
testAndP1 = "andP returns True when both predicates are satisfied" ~: andP isTriangulo isTriangulo Triangulo ~?= True

testAndP2 :: Test
testAndP2 = "andP returns False when one of the predicates is not satisfied" ~: andP isTriangulo isRectangulo Triangulo ~?= False

testOrP1 :: Test
testOrP1 = "orP returns True when one of the predicates is satisfied" ~: orP isTriangulo isRectangulo Triangulo ~?= True

testOrP2 :: Test
testOrP2 = "orP returns False when none of the predicates is satisfied" ~: orP isRectangulo isRectangulo Triangulo ~?= False

testCambiar :: Test
testCambiar = "cambiar changes the figures that satisfy the predicate" ~: cambiar isTriangulo toRectangulo dib1 ~?= dib2

testAnyFig1 :: Test
testAnyFig1 = "anyFig returns True when any figure satisfies the predicate" ~: anyFig isTriangulo dib1 ~?= True

testAnyFig2 :: Test
testAnyFig2 = "anyFig returns False when no figure satisfies the predicate" ~: anyFig isRectangulo dib1 ~?= False

testAllFig1 :: Test
testAllFig1 = "allFig returns True when all figures satisfy the predicate" ~: allFig isTriangulo dib1 ~?= True

testAllFig2 :: Test
testAllFig2 = "allFig returns False when not all figures satisfy the predicate" ~: allFig isRectangulo dib1 ~?= False

main :: IO Counts
main = runTestTT $ TestList
    [testFalla, testAndP1, testAndP2, testOrP1, testOrP2, testCambiar, testAnyFig1, testAnyFig2, testAllFig1, testAllFig2]