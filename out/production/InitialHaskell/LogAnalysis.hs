{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
import StringFunctions

parseMessage :: String -> LogMessage
parseMessage x = case x of
                  ('E':' ':rest) -> parseError rest
                  ('I':' ':rest) -> parseInfo rest
                  ('W':' ':rest) -> parseWarning rest
                  str -> Unknown str



parseInfo :: String -> LogMessage
parseInfo infoStr = LogMessage Info (getTimestamp infoStr) (getMessage infoStr)

parseWarning :: String -> LogMessage
parseWarning warnStr = LogMessage Info (getTimestamp warnStr) (getMessage warnStr)

getTimestampStr :: String -> String
getTimestampStr x = head (splitBySpace x)

getTimestamp :: String -> Int
getTimestamp x = read (getTimestampStr x) :: Int

getMessage :: String -> String
getMessage x = drop (length timestamp + 1) x
  where timestamp = getTimestampStr x

parseError :: String -> LogMessage
parseError errorStr = LogMessage (Error (getErrorLevel errorStr)) (getTimestamp errorStrNoLevel) (getMessage errorStrNoLevel)
  where errorStrNoLevel = drop (length (show (getErrorLevel errorStr)) + 1) errorStr --Drops error level from string

getErrorLevel :: String -> Int
getErrorLevel x = read(head (splitBySpace x)) :: Int


parse :: String ->[LogMessage]
parse x = [parseMessage message | message<-split x '\n']

insert :: LogMessage -> MessageTree -> MessageTree
insert message (Node Leaf nodeMessage Leaf) = Node (Node Leaf message Leaf) nodeMessage Leaf
insert message (Node leftChild nodeMessage rightChild) = sortTree(Node (insert message leftChild) nodeMessage rightChild)
insert message tree = Node tree message tree

sortTree :: MessageTree -> MessageTree
sortTree Leaf = Leaf
sortTree node@(Node Leaf _ Leaf) = node
sortTree (Node lNode message rNode)
                            | rightTimestamp > leftTimestamp = sortTree (Node rNode message lNode)
                            | leftTimestamp > messageTimestamp = Node (sortTree (setMessage lNode message)) leftMessage (sortTree rNode)
                            | otherwise = Node (sortTree lNode) message (sortTree rNode)
                            where rightTimestamp = getTimestampFromLogMessage (getMessageFromNode rNode)
                                  leftTimestamp =  getTimestampFromLogMessage (getMessageFromNode lNode)
                                  messageTimestamp = getTimestampFromLogMessage message
                                  leftMessage = getMessageFromNode lNode

treeToMessageArray :: MessageTree -> [LogMessage]
treeToMessageArray (Node Leaf message Leaf) = [message]
treeToMessageArray (Node lNode message rNode) = message:(treeToMessageArray lNode ++ treeToMessageArray rNode)
treeToMessageArray _ = []

messageArrayToTimestampArray :: [LogMessage] -> [Int]
messageArrayToTimestampArray [] = []
messageArrayToTimestampArray [message] = [getTimestampFromLogMessage message]
messageArrayToTimestampArray (mess:rest) = getTimestampFromLogMessage mess:messageArrayToTimestampArray rest

build :: [LogMessage] -> MessageTree
build [] = Leaf
build [message] = Node Leaf message Leaf
build (message:rest) = insert message (build rest)

inOrder :: MessageTree -> [LogMessage]
inOrder tree = treeToMessageArray (sortTree tree)

sortMessageArray :: [LogMessage] -> [LogMessage]
sortMessageArray messageArr = inOrder(build messageArr)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong unsortedArray = reverse [getMessageFromLogMessage x | x<-sortedArray, getErrorLevelFromLogMessage x > 50]
                              where sortedArray = sortMessageArray unsortedArray



