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