module Pandoc where

import Data.List (intercalate)
import Text.Pandoc (readers, writers)

import Zepto.Types

multiDoc :: [String] -> String
multiDoc = intercalate "\n"

exports :: [(String, [LispVal] -> IOThrowsError LispVal, String)]
exports = [("readers", noIOArg getReaders, multiDoc readersDoc),
           ("writers", noIOArg getWriters, multiDoc writersDoc)]

readersDoc :: [String]
readersDoc = [ "get a list of all of the available pandoc readers."
             , ""
             , "  complexity: O(n)"
             , "  returns: a list of strings"
             ]

getReaders :: IOThrowsError LispVal
getReaders = return $ List $ map (fromSimple . String . fst) readers

writersDoc :: [String]
writersDoc = [ "get a list of all of the available pandoc writers."
             , ""
             , "  complexity: O(n)"
             , " returns: a list of strings"
             ]

getWriters :: IOThrowsError LispVal
getWriters = return $ List $ map (fromSimple . String . fst) writers
