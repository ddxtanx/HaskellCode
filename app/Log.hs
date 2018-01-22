-- CIS 194 Homework 2

module Log where

import Control.Applicative

data MessageType = Info
                 | Warning
                 | Error Int
  deriving (Show, Eq)

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String
                | Unknown String
  deriving (Show, Eq)

data MessageTree = Leaf
                 | Node MessageTree LogMessage MessageTree
  deriving (Show, Eq)


getLeftChild :: MessageTree -> MessageTree
getLeftChild (Node l _ _) = l
getLeftChild x = x

getRightChild :: MessageTree -> MessageTree
getRightChild (Node _ _ r) = r
getRightChild x = x

getMessageFromNode :: MessageTree -> LogMessage
getMessageFromNode (Node _ m _) = m
getMessageFromNode x = Unknown "Nothing"

setMessage :: MessageTree -> LogMessage -> MessageTree
setMessage (Node lChild _ rChild) message = Node lChild message rChild
setMessage Leaf _ = Leaf

getTimestampFromLogMessage :: LogMessage -> Int
getTimestampFromLogMessage (LogMessage _ timestamp _) = timestamp
getTimestampFromLogMessage (Unknown _) = 0

getMessageFromLogMessage :: LogMessage -> String
getMessageFromLogMessage (LogMessage _ _ message) = message
getMessageFromLogMessage (Unknown message) = message

getErrorLevelFromLogMessage :: LogMessage -> Int
getErrorLevelFromLogMessage (LogMessage (Error num) _ _) = num
getErrorLevelFromLogMessage (LogMessage Info _ _) = 0
getErrorLevelFromLogMessage (LogMessage Warning _ _) = 0
getErrorLevelFromLogMessage (Unknown _) = 0

-- | @testParse p n f@ tests the log file parser @p@ by running it
--   on the first @n@ lines of file @f@.
testParse :: (String -> [LogMessage])
          -> Int
          -> FilePath
          -> IO [LogMessage]
testParse parse n file = take n . parse <$> readFile file


-- | @testWhatWentWrong p w f@ tests the log file parser @p@ and
--   warning message extractor @w@ by running them on the log file
--   @f@.
testWhatWentWrong :: (String -> [LogMessage])
                  -> ([LogMessage] -> [String])
                  -> FilePath
                  -> IO [String]
testWhatWentWrong parse whatWentWrong file
  = whatWentWrong . parse <$> readFile file