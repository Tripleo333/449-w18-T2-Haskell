module Main
 where

import Constraints
import Test.Hspec
import Test.Hspec.Expectations
import Test.HUnit

main = hspec $ do
  describe "meetsFpa" $ do
    it "Empty list" $ do
      Constraints.meetsFpa [] (1,'B') `shouldBe` True
    it "False series" $ do
      Constraints.meetsFpa [(0,'A')] (1,'A') `shouldBe` False
      Constraints.meetsFpa [(0,'B')] (0,'A') `shouldBe` False
    it "True series" $ do
      Constraints.meetsFpa [(0,'A')] (0,'A') `shouldBe` True
      Constraints.meetsFpa [(0,'A')] (1,'B') `shouldBe` True
      Constraints.meetsFpa [(0,'B')] (1,'A') `shouldBe` True
      Constraints.meetsFpa [(1,'A')] (0,'B') `shouldBe` True
    it "False List series" $ do
      Constraints.meetsFpa [(0,'A'),(1,'B')] (1,'C') `shouldBe` False
      Constraints.meetsFpa [(0,'A'),(1,'C')] (2,'C') `shouldBe` False
      Constraints.meetsFpa [(0,'A'),(2,'C'),(3,'B')] (1,'B') `shouldBe` False
    it "True List" $ do
      Constraints.meetsFpa [(0,'B'),(3,'D'),(2,'E')] (2,'E') `shouldBe` True

  describe "meetsFm" $ do
    it "Empty list" $ do
      Constraints.meetsFm [] (0,'A') `shouldBe` True
    it "False series" $ do
      Constraints.meetsFm [(1,'A')] (1,'A') `shouldBe` False
    it "True series" $ do
      Constraints.meetsFm [(0,'A')] (1,'B') `shouldBe` True
      Constraints.meetsFm [(0,'A')] (1,'A') `shouldBe` True
      Constraints.meetsFm [(0,'A')] (0,'B') `shouldBe` True
    it "False List series" $ do
      Constraints.meetsFm [(0,'A'),(1,'B')] (0,'A') `shouldBe` False
      Constraints.meetsFm [(0,'A'),(1,'B')] (1,'B') `shouldBe` False
      Constraints.meetsFm [(0,'A'),(1,'C'),(1,'B')] (1,'B') `shouldBe` False
    it "True List" $ do
      Constraints.meetsFm [(0,'A'),(1,'B')] (2,'B') `shouldBe` True
      Constraints.meetsFm [(0,'A'),(1,'B')] (1,'E') `shouldBe` True
      Constraints.meetsFm [(0,'B'),(3,'D'),(2,'E')] (1,'A') `shouldBe` True
      Constraints.meetsFm [(0,'B'),(3,'D'),(2,'E')] (2,'A') `shouldBe` True
      Constraints.meetsFm [(0,'B'),(3,'D'),(2,'E')] (1,'E') `shouldBe` True
      Constraints.meetsFm [(0,'B'),(3,'D'),(2,'E')] (0,'E') `shouldBe` True

  describe "meetsTnt" $ do
    it "Empty List" $ do
--      Constraints.meetsTnt [] [] `shouldBe` True
      Constraints.meetsTnt [] ['A'] `shouldBe` True
      Constraints.meetsTnt [] ['A'..'H'] `shouldBe` True
    it "False series" $ do
      Constraints.meetsTnt [('A','B')] ['A'..'H'] `shouldBe` False
      Constraints.meetsTnt [('G','H')] ['A'..'H'] `shouldBe` False
      Constraints.meetsTnt [('H','A')] ['A'..'H'] `shouldBe` False
    it "True series" $ do
      Constraints.meetsTnt [('A','C')] ['A'..'H'] `shouldBe` True
      Constraints.meetsTnt [('B','A')] ['A'..'H'] `shouldBe` True
      Constraints.meetsTnt [('A','H')] ['A'..'H'] `shouldBe` True
    it "False List series" $ do
      Constraints.meetsTnt [('A','B'),('B','C')] ['A'..'H'] `shouldBe` False
      Constraints.meetsTnt [('A','C'),('E','G'),('B','C')] ['A'..'H'] `shouldBe` False
      Constraints.meetsTnt [('A','C'),('E','G'),('D','C'),('H','A')] ['A'..'H'] `shouldBe` False
    it "True List series" $ do
      Constraints.meetsTnt [('A','C'),('B','D')] ['A'..'H'] `shouldBe` True
      Constraints.meetsTnt [('A','C'),('A','H')] ['A'..'H'] `shouldBe` True
      Constraints.meetsTnt [('A','C'),('C','B')] ['A'..'H'] `shouldBe` True
      Constraints.meetsTnt [('A','C'),('B','D'),('D','C')] ['A'..'H'] `shouldBe` True
      Constraints.meetsTnt [('A','C'),('B','D'),('D','F')] ['A'..'H'] `shouldBe` True
      Constraints.meetsTnt [('A','C'),('B','D'),('A','H')] ['A'..'H'] `shouldBe` True

  describe "calcMp" $ do
    it "Empty List" $ do
      Constraints.calcMp (replicate 8 [1..8]) ['X','X','X','X','X','X','X','X'] `shouldBe` 0
    it "Short List series" $ do
      Constraints.calcMp (replicate 8 [1..8]) ['A','B','X','X','X','X','X','X'] `shouldBe` 3
      Constraints.calcMp (replicate 8 [1..8]) ['X','X','X','X','A','X','X','X'] `shouldBe` 1
      Constraints.calcMp (replicate 8 [1..8]) ['A','X','X','B','X','X','X','X'] `shouldBe` 3
      Constraints.calcMp (replicate 8 [1..8]) ['A','X','X','X','X','X','X','H'] `shouldBe` 9
      Constraints.calcMp (replicate 8 [1..8]) ['A','X','X','X','X','X','X','G'] `shouldBe` 8
    it "Full List series" $ do
      Constraints.calcMp (replicate 8 [1..8]) ['A'..'H'] `shouldBe` 36
      Constraints.calcMp [[1..8],[8,1,2,3,4,5,6,7],[7,8,1,2,3,4,5,6],[6,7,8,1,2,3,4,5],[5,6,7,8,1,2,3,4],[4,5,6,7,8,1,2,3],[3,4,5,6,7,8,1,2],[2,3,4,5,6,7,8,1]] ['A'..'H'] `shouldBe` 8

  describe "calcTnp" $ do
    it "Empty List" $ do
      Constraints.calcTnp [] ['A'..'H'] `shouldBe` 0
    it "Singleton List series" $ do
      Constraints.calcTnp [('A','B',10)] ['A'..'H'] `shouldBe` 10
      Constraints.calcTnp [('H','A',10)] ['A'..'H'] `shouldBe` 10
    it ">1 List series" $ do
      Constraints.calcTnp [('A','F',10),('H','A',10)] ['A'..'H'] `shouldBe` 10
      Constraints.calcTnp [('A','F',10),('B','F',10),('H','A',10)] ['A'..'H'] `shouldBe` 10
      Constraints.calcTnp [('A','F',10),('B','A',10),('H','A',10)] ['A'..'H'] `shouldBe` 10
      Constraints.calcTnp [('A','F',10),('G','H',10),('H','A',10)] ['A'..'H'] `shouldBe` 20
      Constraints.calcTnp [('A','B',10),('B','A',10),('H','A',10)] ['A'..'H'] `shouldBe` 20

  describe "calcPenalty" $ do
    it "Empty List" $ do
      Constraints.calcPenalty (replicate 8 [1..8]) [] ['X','X','X','X','X','X','X','X'] `shouldBe` 0
      Constraints.calcPenalty (replicate 8 [1..8]) [] ['A'..'H'] `shouldBe` 36
    it "Single TNP and No State series" $ do
      Constraints.calcPenalty (replicate 8 [1..8]) [('A','B',10)] ['X','X','X','X','X','X','X','X'] `shouldBe` 0
      Constraints.calcPenalty (replicate 8 [1..8]) [('H','A',10)] ['X','X','X','X','X','X','X','X'] `shouldBe` 0
    it "Single TNP and Short State series" $ do
      Constraints.calcPenalty (replicate 8 [1..8]) [('A','B',10)] ['A','B','X','X','X','X','X','X'] `shouldBe` 13
      Constraints.calcPenalty (replicate 8 [1..8]) [('H','A',10)] ['X','X','X','X','A','X','X','X'] `shouldBe` 1
    it "Single TNP and Full State series" $ do
      Constraints.calcPenalty (replicate 8 [1..8]) [('A','B',10)] ['A'..'H'] `shouldBe` 46
      Constraints.calcPenalty (replicate 8 [1..8]) [('H','A',10)] ['A'..'H'] `shouldBe` 46
      Constraints.calcPenalty (replicate 8 [1..8]) [('A','C',10)] ['A'..'H'] `shouldBe` 36
    it ">1 TNP and No State series" $ do
      Constraints.calcPenalty (replicate 8 [1..8]) [('A','B',10),('H','A',10)] ['X','X','X','X','X','X','X','X'] `shouldBe` 0
      Constraints.calcPenalty (replicate 8 [1..8]) [('H','A',10),('B','A',10),('C','A',10)] ['X','X','X','X','X','X','X','X'] `shouldBe` 0
    it ">1 TNP and Short State series" $ do
      Constraints.calcPenalty (replicate 8 [1..8]) [('A','B',10),('H','A',10)] ['A','B','X','X','X','X','X','X'] `shouldBe` 13
      Constraints.calcPenalty (replicate 8 [1..8]) [('H','A',10),('B','A',10),('C','A',10)] ['X','X','X','X','A','X','X','X'] `shouldBe` 1

--caught error expected 14 - got 4
      Constraints.calcPenalty (replicate 8 [1..8]) [('H','A',10),('B','A',10),('C','A',10)] ['X','X','X','C','A','X','X','X'] `shouldBe` 14


    it ">1 TNP and Full State series" $ do
      Constraints.calcPenalty (replicate 8 [1..8]) [('A','B',10),('H','A',10)] ['A'..'H'] `shouldBe` 56
      Constraints.calcPenalty (replicate 8 [1..8]) [('H','A',10),('B','A',10),('C','A',10)] ['A'..'H'] `shouldBe` 46
      Constraints.calcPenalty (replicate 8 [1..8]) [('H','A',10),('C','A',10),('A','B',10)] ['A'..'H'] `shouldBe` 56
      Constraints.calcPenalty (replicate 8 [1..8]) [('G','A',10),('A','C',10),('B','F',10)] ['A'..'H'] `shouldBe` 36
      Constraints.calcPenalty (replicate 8 [1..8]) [('G','A',10),('A','C',10),('H','G',10)] ['A'..'H'] `shouldBe` 36




















