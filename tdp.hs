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
data Subject = Parent | Child
data Parent = Mother | Father
data Child = Boy | Girl
data Header = Header ChildId AgeInMonths Observer Parent Child
data Record = Record Header [Task]
data Task = Task TaskId [ObservationUnit]


data Initiator = Initiator Subject
data ContentCode = ContentCode T.Text deriving (Show)
data Reciprocator = Reciprocator Subject
data Valence = Valence Int -- a number in the range [1,8]
data ObservationTime = ObservationTime Rational
data ObservationCode = ObservationCode Initiator ContentCode Reciprocator Valence
data ObservationUnit = ObservationUnit ObservationCode ObservationTime

data Error = Error T.Text deriving (Show)


validContentCodes = [
    "00", "01", "03", "11", "12", "13", "21", "22", "23",
    "31", "32", "33", "41", "42", "43", "51", "53", "62",
    "71", "72", "73", "81", "82", "83", "91", "92", "93"
    ] :: [T.Text]

parseObservationCode :: T.Text -> Either Error ContentCode
parseObservationCode oc =    
    let 
        invalidObservationCode val = "Invalid ObservationCode: " <> val <> "."
        --toDecimal val =
        --    case (decimal val) of
        --        Right(v, rest) -> Right(v)
        --        Left(_) -> Left $ Error (invalidObservationCode val)

        validate val = 
            if (elem val validContentCodes)
                then Right(val)
                else Left $ Error (invalidObservationCode val)



            --if (val > 0 && val < 100)
            --    then Right(val)
            --    else Left $ Error (invalidObservationCode val)
    in
        fmap ContentCode (validate oc) 
        --do
        --    s0 <- toDecimal oc
        --    s1 <- validate(s0)
        --    return $ ContentCode s1

--parseObservationUnit (a:b:c:d:e:[]) =




test = "zupu" :: T.Text

main = do
    let a = "Hei" :: T.Text
    putStrLn("Hello world!")
