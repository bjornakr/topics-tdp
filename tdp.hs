#!/usr/bin/env stack
-- stack runghc --resolver lts-11.0 --install-ghc --package text --package safe

-- Topics observational data processor
-- TOPDAP

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Data.Text.Read (decimal, rational)
import Data.Monoid
import Safe
import Data.Either

data Task = Task1 | Task2 | Task3a | Task3b | Task4 | Task5 deriving (Show)
data ChildId = ChildId Int deriving (Show)
data AgeInMonths = Age12 | Age24 | Age36 deriving (Show)
data Observer = Observer T.Text deriving (Show)
data Parent = Mother | Father deriving (Show)
data Child = Boy | Girl deriving (Show)
data Family = Family Parent Child deriving (Show)
data Header = Header Task ChildId AgeInMonths Observer Family deriving (Show)

data Subject = Parent | Child deriving (Show, Eq)
data Initiator = Initiator Subject deriving (Show, Eq)
data ContentCode = ContentCode T.Text deriving (Show, Eq)
data Reciprocator = Reciprocator Subject | ReciprocatorObject deriving (Show)
data Valence = Valence T.Text deriving (Show, Eq) -- a number in the range [1,8]
data ObservationTime = ObservationTime Double deriving (Show)
data ObservationCode = ObservationCode Initiator ContentCode Reciprocator Valence deriving (Show)
data ObservationUnit = ObservationUnit ObservationCode ObservationTime deriving (Show)



data Error = Error T.Text deriving (Show)


-- Header
--Task ChildID Age      Observer    FMembers        Date     Time   dbl_int is_cut                                 
-- 1   1366    12       OV          13              08/13/10 15:18  1       0

--Data
  --0  -00100    40.0
  --1   17232    40.03
  --1   38112    42.2
  --1   17232    42.6
  --1   34112    43.56
  --1   17232    44.13
  --1   34112    46.46
  --1   17232    47.03
  --1   31112    49.76


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

parseObservationTime :: T.Text -> Either Error ObservationTime
parseObservationTime val =
    case (rational val) of
        Left(_) -> Left $ Error("Invalid ObservationTime: " <> val <> ".")
        Right((v,_)) -> Right $ ObservationTime v



parseObservationUnit :: T.Text -> Either Error ObservationUnit
-- input e.g:   1   34112    46.46
parseObservationUnit val =
    let 
        parts = T.words val
    in
    do
        oc <- case atMay parts 1 of
                Nothing -> Left $ Error("Missing ObservationCode in: " <> val <> ".")
                Just v -> parseObservationCode v

        ot <- case atMay parts 2 of
                Nothing -> Left $ Error("Missing ObservationTime in: " <> val <> ".")
                Just v -> parseObservationTime v

        return $ ObservationUnit oc ot










parseTask :: T.Text -> Either Error Task
parseTask val =
    case val of
        "1" -> Right Task1
        "2" -> Right Task2
        "3a" -> Right Task3a
        "3b" -> Right Task3b
        "4" -> Right Task4
        "5" -> Right Task5
        _ -> Left $ Error("Invalid task: " <> val <> ".")

parseChildId :: T.Text -> Either Error ChildId
parseChildId val =
    case decimal val of
        Right((i, "")) -> Right $ ChildId i        
        _ -> Left $ Error("Invalid ChildId: " <> val <> ".")

parseAge :: T.Text -> Either Error AgeInMonths
parseAge val =
    case val of
        "12" -> Right Age12
        "24" -> Right Age24
        "36" -> Right Age36
        "_" -> Left $ Error("Invalid age: " <> val <> ".")

parseObserver :: T.Text -> Either Error Observer
parseObserver val =
    if (T.length val) == 2
        then Right (Observer val)
        else Left $ Error("Invalid observer: " <> val <> ".")


parseFamily :: T.Text -> Either Error Family
parseFamily val =
    case val of 
        "12" -> Right $ Family Father Boy
        "13" -> Right $ Family Mother Boy
        "28" -> Right $ Family Father Girl
        "38" -> Right $ Family Mother Girl
        _ -> Left $ Error("Invalid Family: " <> val <> ".")


parseHeader :: T.Text -> Either Error Header
parseHeader val =
    let 
        parts = T.words val
    in do
        task <- case atMay parts 0 of
                Nothing -> Left $ Error("Missing Task in: " <> val <> ".")
                Just v -> parseTask v
        childId <- case atMay parts 1 of
                Nothing -> Left $ Error("Missing ChildId in: " <> val <> ".")
                Just v -> parseChildId v
        age <- case atMay parts 2 of
                Nothing -> Left $ Error("Missing Age in: " <> val <> ".")
                Just v -> parseAge v
        observer <- case atMay parts 3 of
                Nothing -> Left $ Error("Missing Observer in: " <> val <> ".")
                Just v -> parseObserver v
        family <- case atMay parts 4 of
                Nothing -> Left $ Error("Missing Family in: " <> val <> ".")
                Just v -> parseFamily v
        
        return $ Header task childId age observer family




---data RecodeSequence = RecodeSequence WindowSizeInSecs [RecodeSpec]
--data SequenceMatch = SequenceMatch [ObservationUnit]




--rseq = RecodeSequence (WindowSizeInSecs 6) [rSpec1, rSpec2]

--data State = State [RecodeSpec] [ObservationUnit] -- SequenceMatch
--data FindResult = FindResult [ObservationUnit]


-- data FindSpec = RecodeSpec WindowSizeInSecs [ObservationUnit]

-- data ObservationUnit = ObservationUnit ObservationCode ObservationTime deriving (Show)

-- data ObservationCode = ObservationCode Initiator ContentCode Reciprocator Valence deriving (Show)


isMatch :: RecodeSpec -> ObservationUnit -> Bool
isMatch 
    (RecodeSpec initiatorSpec contentCodesSpec valencesSpec)
    obsUnit@(ObservationUnit (ObservationCode initiator contentCode _ valence) _) =
        initiator == initiatorSpec
            && (elem contentCode contentCodesSpec)
            && (elem valence valencesSpec)


--isMatch :: [RecodeSpec] -> ObservationUnit -> Bool
--isMatch [] _ = False
--isMatch (r:rs) ou = 
--    if isMatch r ou
--        then True
--        else isMatch rs obsUnit

data RecodeSpec = RecodeSpec Initiator [ContentCode] [Valence]
data WindowSizeInSecs = WindowSizeInSecs Double
type StartRecodeSpec = RecodeSpec
type StopRecodeSpec = RecodeSpec
--data OpenSequence = OpenSequence [ObservationUnit]
data RecodeSequence = RecodeSequence StartRecodeSpec StopRecodeSpec WindowSizeInSecs
data FoundSequence = EmptySequence | OpenSequence ObservationUnit | ClosedSequence ObservationUnit ObservationUnit deriving (Show)


units = fromRight [] (mapM parseObservationUnit [
    "  1   24281    0.00",
    "  1   86221    1.00",
    "  1   85121    2.00",
    "  1   24281    3.00",
    "  1   24381    4.00",
    "  1   85121    10.00"
    ])
rSpec1 = RecodeSpec (Initiator Parent) [ContentCode "42"] [Valence "1", Valence "2", Valence "3"]
rSpec2 = RecodeSpec (Initiator Child) [ContentCode "01", ContentCode "51"] (map (Valence . T.pack . show) [1..8])
rseq = RecodeSequence rSpec1 rSpec2 (WindowSizeInSecs 6)


boom :: RecodeSequence -> [ObservationUnit] -> FoundSequence -> (FoundSequence, [ObservationUnit])
boom _ [] s = (EmptySequence, [])
boom recSeq@(RecodeSequence rStart rStop (WindowSizeInSecs winSize)) (o@(ObservationUnit _ (ObservationTime curTime)):os) found =
    -- if time out, close sequence
    -- if start recode appears, begin again
    case found of
        EmptySequence ->
            if isMatch rStart o
            then boom recSeq os (OpenSequence o)
            else boom recSeq os EmptySequence
        OpenSequence firstUnit@(ObservationUnit _ (ObservationTime firstTime)) ->
            if isMatch rStart o 
            then boom recSeq os (OpenSequence o)  -- if start recode appears, we start over.
            else 
                if (curTime - firstTime > winSize)
                then boom recSeq (o:os) EmptySequence  -- time out -> sequence lost
                else 
                    if isMatch rStop o
                    then (ClosedSequence firstUnit o, os)  -- found the stop code
                    else boom recSeq os found




sproing :: RecodeSequence -> [ObservationUnit] -> [FoundSequence] -> [FoundSequence]
sproing _ [] fs = fs
sproing reqSeq os fs =
    case boom reqSeq os EmptySequence of 
        (EmptySequence, rest) -> sproing reqSeq rest fs
        (closed@(ClosedSequence _ _), rest) -> sproing reqSeq rest (closed:fs)



-- if new first recode appears, what to do?







--lookForNext :: [RecodeSpec] -> [ObservationUnit] -> Maybe ObservationUnit


--nextSequence :: [RecodeSpec] -> [ObservationUnit] -> FindResult -> (FindResult, [ObservationUnit])
--nextSequence [] os result = (result, os)
--nextSequence _ [] result = (result, [])
--nextSequence (r:rs) os (FindResult ros) =


--nextSequence :: State -> Maybe FindResult -> State
--nextSequence (State rSpecs oUnits)


--timePassed :: Double -> ObservationUnit -> Double


--findRecodeSpec :: Double RecodeSpec WindowSizeInSecs [ObservationUnit] -> FindResult
--findRecodeSpec _ _ [] = FindResult Nothing []
--findRecodeSpec startTime rs win ous@(a:as) =
--    in

--        if win < 0
--            then FindResult Nothing ous
--            else
--                if (isMatch rs a) then
--                    FindResult Just(a) as
--                else
--                    findRecodeSpec rs ()



--nextMatch :: State -> State
--nextMatch (State rs os ss@(SequenceMatch sso)) =
--    case lookFor (head rs) os of
--        FindResult Nothing os' -> nextMatch (State rs os' ss)
--        FindResult (Just ou) os' -> nextMatch (State (tail rs) os' (SequenceMatch (ou : sso)))


---- 


--8lookFor :: RecodeSpec [ObservationUnit]
--lookFor (head recodeSpec)


--isRecodeSpecMatch :: [RecodeSpec] ObservationUnit -> Boolean
--isRecodeSpecMatch 





main = do
    putStrLn("Hello world!")



-- if (obsCode = startCode) {
--   curSequence.start = obsCode
--   next obsCode

--   while (obsCode <= allCodes) {
--     if (obsCode = endCode) {
--       curSequence.end = obsCode
--       validSeqs.add(curSequence)
--     }
--     else {
--       next obsCode    
--     }
--   }
-- }
-- 


