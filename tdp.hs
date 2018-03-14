#!/usr/bin/env stack
-- stack runghc --resolver lts-11.0 --install-ghc --package text

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Data.Text.Read (decimal, rational)
import Data.Monoid

data TaskId = Task1 | Task2 | Task3a | Task3b | Task4 | Task5
data ChildId = ChildId Int
data AgeInMonths = Age12 | Age24 | Age36
data Observer = Observer String
data ParentRole = Mother | Father
data ChildGender = Boy | Girl
-- data Parent = Mother | Father
data Subject = Parent | Child deriving (Show)
-- data Child = Boy | Girl
data Header = Header ChildId AgeInMonths Observer ParentRole ChildGender
data Record = Record Header [Task]
data Task = Task TaskId [ObservationUnit]


data Initiator = Initiator Subject deriving (Show)
data ContentCode = ContentCode T.Text deriving (Show)
data Reciprocator = Reciprocator Subject | ReciprocatorObject deriving (Show)
data Valence = Valence T.Text deriving (Show) -- a number in the range [1,8]
data ObservationTime = ObservationTime Rational
data ObservationCode = ObservationCode Initiator ContentCode Reciprocator Valence deriving (Show)
data ObservationUnit = ObservationUnit ObservationCode ObservationTime

data Error = Error T.Text deriving (Show)


validContentCodes = [
    "00", "01", "03", "11", "12", "13", "21", "22", "23",
    "31", "32", "33", "41", "42", "43", "51", "53", "62",
    "71", "72", "73", "81", "82", "83", "91", "92", "93"
    ] :: [T.Text]

validValences = ["1", "2", "3", "4", "5", "6", "7", "8"] :: [T.Text]

parseContentCode :: T.Text -> Either Error ContentCode
parseContentCode val =    
    if (elem val validContentCodes)
        then Right(ContentCode val)
        else Left $ Error ("Invalid ObservationCode: " <> val <> ".")


parseInitiator :: T.Text -> Either Error Initiator
parseInitiator val =
    case val of
        "2" -> Right (Initiator Parent)
        "3" -> Right (Initiator Parent)
        "1" -> Right (Initiator Child)
        "8" -> Right (Initiator Child)
        _ -> Left $ Error ("Invalid Initiator: " <> val <> ".")

parseReciprocator val =
    case val of
        "0" -> Right (ReciprocatorObject)
        "2" -> Right (Reciprocator Parent)
        "3" -> Right (Reciprocator Parent)
        "1" -> Right (Reciprocator Child)
        "8" -> Right (Reciprocator Child)
        _ -> Left $ Error ("Invalid Reciprocator: " <> val <> ".")

parseValence val = 
    if (elem val validValences)
        then Right(Valence val)
        else Left $ Error ("Invalid Valence: " <> val <> ".")

parseObservationCode :: T.Text -> Either Error ObservationCode
parseObservationCode val =
    let
        proc :: String -> Either Error ObservationCode
        proc (c1:c2:c3:c4:c5:[]) =
            do
                initiator <- parseInitiator (T.singleton c1)
                content <- parseContentCode (T.singleton(c2) <> T.singleton(c3))
                reciprocator <- parseReciprocator (T.singleton c4)
                valence <- parseValence (T.singleton c5)
                return $ ObservationCode initiator content reciprocator valence
        proc other = 
            Left $ Error ("Invalid observation code: " <> (T.pack other) <> ".")
    in
        proc (T.unpack val)


main = do
    putStrLn("Hello world!")
