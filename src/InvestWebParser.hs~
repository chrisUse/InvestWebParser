module InvestWebParser where
--
--
import Text.HTML.TagSoup
--
-- cabal install HTTP-Types HTTP-Enumerator
--
-- cabal haddock --executebal
--
import Network.HTTP.Enumerator
import Network.HTTP.Types
import qualified Data.ByteString.Lazy.UTF8 as BSLU
--
--
csvFile = "test.csv"
-- 
-- | Read the main file bye uri from file and analyse it.
--  
getFirstFile uri = do
     kDListToCSV
     print "Lade Haupseite..."
     ret <- readL uri
     print "Analysiere Stamdaten..."
     requestHTML $ tableSpan $ parseTags ret
-- 
readL :: FilePath -> IO String
readL file = readFile file
-- 
-- | The link order from the Page is damage.
--  Thats the reason why the path must be insert.
insertPath [] = []
insertPath ('?' : rest) = "service/firmendatenbank/?" ++ (insertPath rest)
insertPath (c : rest) = c : (insertPath rest)
--
aTagwithText (tag : tags) =
      case tag of
       TagText text -> text
       TagClose "a" -> []
       _ -> aTagwithText tags
--
aTag (tag : tags) =
      case tag of
       TagOpen "a" content -> ((aTagwithText tags), (insertPath $ snd $ head content))
       _ -> aTag tags
--
tableSpan [] = []
tableSpan (tag : tags) = 
      case tag of
       TagOpen "span" [("class","RecordTitle")] -> aTag tags : (tableSpan tags)
       _ -> tableSpan tags
--
analyseContent (tag : tags) =
      case tag of
       TagText text -> text
       TagClose "span" -> []
       _ -> analyseContent tags
--
analyseSpan (tag : tags) =
      case tag of
       TagOpen "span" content -> analyseContent tags : (analyseSpan tags)
       TagClose "tr" -> []
       _ -> analyseSpan tags
--
analyseTR (tag : tags) =
      case tag of
       TagOpen "tr" content -> analyseSpan tags : (analyseTR tags)
       TagClose "table" -> []
       _ -> analyseTR tags
--
analyseFD (tag : tags) =
      case tag of 
       TagOpen "table" content -> analyseTR tags 
       TagClose "td" -> []
       _ -> analyseFD tags
--
readFirmenDb [] = []
readFirmenDb (tag : tags) = 
      case tag of
       TagOpen "td" [("class","result"),_] -> analyseFD tags
       _ -> readFirmenDb tags
--
readFDaten = do 
--        daten <- readL "quelle/Invest in Thuringia_daten.html"
        daten <- readL "tmp.txt"
        print $ readFirmenDb $ parseTags daten
--
requestHTML [] = return ()
requestHTML ((firma, addr) : rest) = do
     print firma
--
     req0 <- parseUrl addr
     let req = req0 { method = methodGet }
     res <- withManager $ httpLbs req
--
     appendCSV csvFile $ strukToString firma $ readFirmenDb $ parseTags $ BSLU.toString $ responseBody res
     requestHTML rest
--
-- ===========================================================================
--
kDListToCSV = writeFile csvFile "Firma;Straße;PLZ;Ort;Kreis;Telefon;Telefax;Internet;E-Mail\n"
--
appendCSV :: FilePath -> String -> IO ( )
appendCSV file content = appendFile file content
--
removeSpace [] = []
removeSpace (x : xss) = 
     case x of 
      ' ' -> removeSpace xss
      _   -> x : (removeSpace xss)
--
-- | 
strukToString firma ( [_, strasse] : [_, plz, ort] : [_, kreis] : _ : [_, tele] : [_, telefax] : [_, uri] : [_, email] : _ ) =
    firma ++ ";" ++ 
    strasse ++ ";" ++ (removeSpace plz) ++ ";" ++ ort ++ ";" ++ kreis ++ ";" ++ tele ++ ";" ++ telefax ++ ";" ++ uri ++ ";" ++ email ++ "\n"
--
strukToString firma ( [_, strasse] : [_, plz, ort] : _ : [_, tele] : [_, telefax] : [_, uri] : [_, email] : _ ) =
    firma ++ ";" ++ 
    strasse ++ ";" ++ (removeSpace plz) ++ ";" ++ ort ++ ";" ++ ";" ++ tele ++ ";" ++ telefax ++ ";" ++ uri ++ ";" ++ email ++ "\n"
--
strukToString firma ( [_, strasse] : [_, plz, ort] : [_, kreis] : _ : [_, tele] : [_, telefax] : [_, uri] : [] : _ ) =
    firma ++ ";" ++ 
    strasse ++ ";" ++ (removeSpace plz) ++ ";" ++ ort ++ ";" ++ kreis ++ ";" ++ tele ++ ";" ++ telefax ++ ";" ++ uri ++ ";" ++ "\n"
--
strukToString firma ( [_, strasse] : [_, plz, ort] : [_, kreis] : _ : [_, tele] : [_, telefax] : [] : _ ) =
    firma ++ ";" ++ 
    strasse ++ ";" ++ (removeSpace plz) ++ ";" ++ ort ++ ";" ++ kreis ++ ";" ++ tele ++ ";" ++ telefax ++ ";" ++ ";" ++ "\n"
--
strukToString firma ( [_, strasse] : [_, plz, ort] : [_, kreis] : _ ) =
    firma ++ ";" ++ 
    strasse ++ ";" ++ (removeSpace plz) ++ ";" ++ ort ++ ";" ++ kreis ++ ";" ++ ";" ++ ";" ++ "\n"
--
strukToString firma ( [_, strasse] : [_, plz, ort] : _ ) =
    firma ++ ";" ++ 
    strasse ++ ";" ++ (removeSpace plz) ++ ";" ++ ort ++ ";" ++ ";" ++ ";" ++ ";" ++ "\n"
--
strukToString firma ( [_, strasse] : [_, ort] : _ ) =
    firma ++ ";" ++
    strasse ++ ";" ++ ";"  ++ ort ++ ";" ++ ";" ++ ";" ++ ";" ++ "\n"
--
strukToString firma ( [_, strasse] : _ ) =
    firma ++ ";" ++
    strasse ++ ";" ++ ";"  ++ ";" ++ ";" ++ ";" ++ ";" ++ "\n"
--
strukToString firma  _ =
    firma ++ ";" ++
    ";" ++ ";"  ++ ";" ++ ";" ++ ";" ++ ";" ++ "\n"
--
