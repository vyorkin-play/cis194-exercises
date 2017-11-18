{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
import Text.Read (readMaybe)

parse :: String -> [LogMessage]
parse = map parseMessage . lines

parseMessage :: String -> LogMessage
parseMessage msg = case maybeParseMessage msg of
                     Just r -> r
                     Nothing -> Unknown msg

maybeParseMessage :: String -> Maybe LogMessage
maybeParseMessage raw = do
  (messageType, maybeTimestamp:rest) <- parseMessageType . words $ raw
  timestamp <- readMaybe maybeTimestamp
  return $ LogMessage messageType timestamp $ unwords rest

parseMessageType :: [String] -> Maybe (MessageType, [String])
parseMessageType ("I":rest) = Just (Info, rest)
parseMessageType ("W":rest) = Just (Warning, rest)
parseMessageType ("E":maybeSeverity:rest) = do
    severity <- readMaybe maybeSeverity
    return $ (Error severity, rest)
parseMessageType _ = Nothing

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ timestamp' _) (Node left msg'@(LogMessage _ timestamp _) right) =
  case compare timestamp' timestamp of
    GT -> Node left msg' (insert msg right)
    LT -> Node (insert msg left) msg' right
    EQ -> Node left msg right
insert _ (Node _ (Unknown _) _) = error "Unknown log message format"

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = (inOrder left) ++ [msg] ++ (inOrder right)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMessage . filter isCritical . inOrder . build
  where
    isCritical (LogMessage (Error severity) _ _) = severity >= 50
    isCritical _ = False
    getMessage (LogMessage _ _ message) = message
    getMessage (Unknown _) = error "Unknown log message format"
