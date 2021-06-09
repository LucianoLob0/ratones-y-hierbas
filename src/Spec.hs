module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "lograEstabilizar" $ do
    it "Comunidad con infinitos ratones, con uno que no queda estabilizado" $ do
      lograEstabilizar (reduceFatFast 2) comunidadInfinita `shouldBe` False
