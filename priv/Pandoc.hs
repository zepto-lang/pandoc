module Pandoc where

import Data.List (intercalate)
import Data.ByteString.Lazy (toStrict, fromStrict)
import Text.Pandoc (ReaderOptions(..),
                    WriterOptions(..),
                    Reader(..),
                    Writer(..),
                    readers,
                    writers,
                    getReader,
                    getWriter,
                    def)

import Zepto.Types

multiDoc :: [String] -> String
multiDoc = intercalate "\n"

exports :: [(String, [LispVal] -> IOThrowsError LispVal, String)]
exports = [("readers", noIOArg getReaders, multiDoc readersDoc),
           ("writers", noIOArg getWriters, multiDoc writersDoc),
           ("convert", convert, multiDoc convertDoc)]

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

write writer f contents = do
    processed <- liftLisp $ f (def ReaderOptions) contents
    write' writer contents processed

writeB writer f contents = do
    processed <- liftLisp $ f (def ReaderOptions) (fromStrict contents)
    write' writer contents
           (case processed of
             Right val -> Right $ fst val
             Left err -> Left err)

write' writer contents processed =
    case processed of
      Right read ->
        case writer of
          PureStringWriter w ->
            return $ fromSimple $ String $ w (def WriterOptions) $ read
          IOStringWriter w -> do
            str <- liftLisp $ w (def WriterOptions) $ read
            return $ fromSimple $ String str
          IOByteStringWriter w -> do
            str <- liftLisp $ w (def WriterOptions) $ read
            return $ ByteVector $ toStrict str
      Left err -> lispErr $ Default $ show err

convertDoc :: [String]
convertDoc = [ "convert <par>inpt</par> from format <par>from</par> to "
             , "<par>to</par>. If the input format is binary, the input "
             , "must be a bytevector."
             , ""
             , "  params:"
             , "    - from: the input format"
             , "    - to: the output format"
             , "    - inpt: the string/bytevector to convert"
             , "  complexity: varies between formats"
             , "  returns: a string/bytevector"
             ]

convert :: [LispVal] -> IOThrowsError LispVal
convert [SimpleVal (String from), SimpleVal (String to),
         SimpleVal (String contents)] =
    case getReader from of
      Right reader ->
        case getWriter to of
          Right writer ->
            case reader of
              StringReader f -> write writer f contents
              ByteStringReader _ ->
                lispErr $ Default $
                  "reader " ++ from ++
                  " only accepts binary input, was given string."
          Left err ->
            lispErr $ Default err
      Left err ->
        lispErr $ Default err
convert [SimpleVal (String from), SimpleVal (String to),
         ByteVector contents] =
    case getReader from of
      Right reader ->
        case getWriter to of
          Right writer ->
            case reader of
              StringReader _ ->
                lispErr $ Default $
                  "reader " ++ from ++
                  " only accepts string input, was given bytevector."
              ByteStringReader f ->
                writeB writer f contents
          Left err ->
            lispErr $ Default err
      Left err ->
        lispErr $ Default err
convert [SimpleVal (String _), SimpleVal (String _), x] =
    lispErr $ TypeMismatch "string" x
convert [SimpleVal (String _), x, _] = lispErr $ TypeMismatch "string" x
convert [x, _, _] = lispErr $ TypeMismatch "string" x
