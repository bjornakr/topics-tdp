-- #!/usr/bin/env stack
{- stack
    --resolver lts-11.0 
    --install-ghc
    exec ghci 
    --package text
    --package safe
-}

 --runghc

-- Topics Sequence Finder
-- TOPSEQ

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import System.IO
import Data.Text.IO as IO (readFile, writeFile)
import Data.Text.Read (decimal, rational)
import Data.Monoid
import Safe
import Data.Either



-- CONFIGURABLE PART: You are expected to change these values.

-- 1) Write the name of the report.
reportName = ReportName("TEST_REPORT")

-- 2) Define the sequence. See README.md for details.
seqStart = RecodeSpec (Initiator Parent) [ContentCode "42"] [Valence "1", Valence "2", Valence "3"]
seqStop = RecodeSpec (Initiator Child) [ContentCode "01", ContentCode "51"] []
rseq = RecodeSequence seqStart seqStop (WindowSizeInSecs 6)

--- END CONFIG



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
data ObservationCode = ObservationCode Initiator ContentCode Reciprocator Valence | ControlCode deriving (Show)
data ObservationUnit = ObservationUnit ObservationCode ObservationTime deriving (Show)

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
parseObservationCode "-00100" = Right ControlCode
parseObservationCode "-00500" = Right ControlCode
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




isMatch :: RecodeSpec -> ObservationUnit -> Bool
isMatch _ (ObservationUnit ControlCode _) = False
isMatch 
    (RecodeSpec initiatorSpec contentCodesSpec valencesSpec)
    obsUnit@(ObservationUnit (ObservationCode initiator contentCode _ valence) _) =
        initiator == initiatorSpec
            && (null contentCodesSpec || elem contentCode contentCodesSpec)
            && (null valencesSpec || elem valence valencesSpec)



data RecodeSpec = RecodeSpec Initiator [ContentCode] [Valence]
data WindowSizeInSecs = WindowSizeInSecs Double
type StartRecodeSpec = RecodeSpec
type StopRecodeSpec = RecodeSpec
data RecodeSequence = RecodeSequence StartRecodeSpec StopRecodeSpec WindowSizeInSecs
data FoundSequence = 
    EmptySequence | 
    OpenSequence ObservationUnit | 
    FinishedSequence ObservationUnit ObservationUnit |
    TimedOutSequence ObservationUnit deriving (Show)



isTimedOut :: RecodeSequence -> ObservationUnit -> ObservationUnit -> Bool
isTimedOut (RecodeSequence _ _ (WindowSizeInSecs winSize))
            (ObservationUnit _ (ObservationTime startTime))
            (ObservationUnit _ (ObservationTime curTime)) = curTime - startTime > winSize



findSequence :: RecodeSequence -> [ObservationUnit] -> FoundSequence -> (FoundSequence, [ObservationUnit])
findSequence _ [] s = (EmptySequence, [])
findSequence recSeq@(RecodeSequence rStart rStop _) (o:os) found =
    let 
        next = findSequence recSeq
    in

    case found of
        EmptySequence ->
            if isMatch rStart o
            then next os (OpenSequence o)
            else next os EmptySequence
        OpenSequence firstUnit ->
            if isMatch rStart o 
            then next os (OpenSequence o)  -- if start recode appears, we start over.
            else 

            if (isTimedOut recSeq firstUnit o)
            then (TimedOutSequence firstUnit, (o:os))    -- time out
            else 

            if isMatch rStop o
            then (FinishedSequence firstUnit o, os)  -- found the stop code
            else next os found




findAllSequences :: RecodeSequence -> [ObservationUnit] -> [FoundSequence]
findAllSequences reqSeq obsUnits =
    let
        proc :: [ObservationUnit] -> [FoundSequence] -> [FoundSequence]
        proc [] fs = fs
        proc os fs =
            case findSequence reqSeq os EmptySequence of 
                (EmptySequence, rest) -> proc rest fs
                (timedOut@(TimedOutSequence _), rest) -> proc rest (timedOut:fs)
                (finished@(FinishedSequence _ _), rest) -> proc rest (finished:fs)
    in
    proc obsUnits []




--rSpecStart = RecodeSpec (Initiator Parent) [ContentCode "42"] [Valence "1", Valence "2", Valence "3"]
--rSpecStop = RecodeSpec (Initiator Child) [ContentCode "01", ContentCode "51"] []

--rstart = RecodeSpec (Initiator Parent) [ContentCode "42"] []
--rstop = RecodeSpec (Initiator Child) [ContentCode "01"] []

--rseq = RecodeSequence rstart rstop (WindowSizeInSecs 6)

data FinishedSequenceCount = FinishedSequenceCount Int deriving (Show)
data TimedOutSequenceCount = TimedOutSequenceCount Int deriving (Show)
data ReportName = ReportName T.Text deriving (Show)
data ReportId = ReportId ReportName ChildId Task deriving (Show)
data Report = Report ReportId FinishedSequenceCount TimedOutSequenceCount deriving (Show)


-- data Header = Header Task ChildId AgeInMonths Observer Family deriving (Show)
createReportId :: ReportName -> Header -> ReportId
createReportId name (Header task childId age _ _) =
    ReportId name childId task


makeReport :: [FoundSequence] -> ReportId -> Report
makeReport found reportId =
    let
        proc [] r = r
        proc (f:fs) r@(Report rId fc@(FinishedSequenceCount i) tc@(TimedOutSequenceCount j)) =
            case f of
                (FinishedSequence _ _) -> proc fs (Report rId (FinishedSequenceCount (i+1)) tc)
                (TimedOutSequence _) -> proc fs (Report rId fc (TimedOutSequenceCount (j+1)))
                _ -> proc fs r
    in
        proc found (Report reportId (FinishedSequenceCount 0) (TimedOutSequenceCount 0))


mapTaskToText :: Task -> T.Text
mapTaskToText t =
    let proc Task1 = "1"
        proc Task2 = "2"
        proc Task3a = "3a"
        proc Task3b = "3b"
        proc Task4 = "4"
        proc Task5 = "5" in
    T.pack $ (proc t)


mapReportToLine :: Report -> T.Text
mapReportToLine (Report 
                    (ReportId (ReportName name) (ChildId childId) task)
                    (FinishedSequenceCount fsCount)
                    (TimedOutSequenceCount toCount)) =
    let ps = T.pack . show in 
        T.intercalate ";" [name, ps childId, mapTaskToText task, ps fsCount, ps toCount]




bop :: [Either Error ObservationUnit] -> [ObservationUnit]
bop [] = []
bop (e:es) =
    case e of
        Left _ -> bop(es)
        Right o -> o:(bop(es))



processFile :: FilePath -> IO Report
processFile filePath = do
    content <- IO.readFile filePath
    let lines0 = T.splitOn "\n" content
    let hdrStr = (head . tail) lines0
    let hdr = parseHeader hdrStr
    let data0 = (tail . tail) lines0
    let data1 = filter (\t -> t /= T.empty) data0

    let parse = do
        hdr <- parseHeader hdrStr
        let units = bop $ map parseObservationUnit data1
        -- units <- mapM parseObservationUnit data1
        return (hdr, units)

    let report = case parse of
                Left (Error msg) -> error (T.unpack msg)
                Right (hdr, units) -> do
                    let a = findAllSequences rseq units
                    makeReport a (createReportId reportName hdr)
    
    return report


readFileList :: FilePath -> IO [T.Text]
readFileList filePath = do 
    content <- IO.readFile filePath
    let filePaths = T.lines content
    return filePaths


main = do
    fileListHandle <- openFile "filelist.txt" ReadMode
    fileList <- hGetContents fileListHandle
    --fileList <- readFileList "filelist.txt"
    reports <- mapM processFile (lines fileList)
    let reportStrings = fmap mapReportToLine reports
    let fullString = T.unlines reportStrings
    IO.writeFile "output.csv" fullString
    putStrLn $ show reportStrings
    hClose fileListHandle
    putStrLn("Done!")
