module Main (main) where

import Types
import Control.Concurrent
import System.Random

names = ["Macbeth","Macduff","Duncan","LMacbeth","Banquo","LMacduff","Malcolm","Witch1","Witch2","Witch3"]

goodWords = ["happy","nice","friendly","fun","cool","swag"]
badWords = ["evil","mean","ugly","dishonorable","stupid","stinky"]
subjectsObjects = ["I", "you", "they", "everyone", "me"]
verbs = ["think","say","know","yell","believe","whisper"]

initInbox :: Counter -> User -> IO InMsg
initInbox count user = do
    x <- newEmptyMVar
    let i = InMsg x
    forkIO (inbox count i user)
    return i

initOutbox :: Counter -> User -> IO InMsg
initOutbox count user = forkIO (outbox count i user)

--builds a random message
constructMessage :: User -> IO Message
constructMessage user = do
    let objectSubjectChooser = randomRIO (0, (length subjectsObjects)-1)
    objIndex <- objectSubjectChooser
    subjIndex <- objectSubjectChooser
    verbIndex <- randomRIO (0, (length verbs)-1)
    goodBadIndex <- randomRIO (0, (length goodWords)-1)
    goodBad <- randomIO :: IO Bool
    let sentimentWord = if goodBad then (goodWords !! goodBadIndex) else (badWords !! goodBadIndex)
    let sentiment = if goodBad then Good else Bad
    let msgString = (subjectsObjects !! objIndex) <> " " <> (verbs !! verbIndex) <> " " <> (subjectsObjects !! subjIndex) <> " " <> sentimentWord
    return (Message msgString sentiment (name user)) 

--outbox function, sends message from one user to other users
outbox :: Counter -> User -> [IO InMsg] -> IO()
outbox (Counter y) user receivers = loop
    where
        loop = do
            let msg = constructMessage
            ctr <- takeMVar y
            receiverIndex <- randomRIO (0, (length receivers)-1)
            receiver <- receivers !! receiverIndex
            threadDelay ((2 - (mood user))*10000) :: Int
            sendMessage receiver msg


sendMessage :: InMsg -> Message -> IO()
sendMessage (InMsg x) msg = putMVar x msg

--io function for the forkIO thread
inbox :: Counter -> InMsg -> User -> IO()
inbox (Counter y) (InMsg x) user = loop
    where
        loop = do
            msg <- takeMVar x
            ctr <- takeMVar y
            case (ctr < 100) of
                True -> do
                    case (sentiment msg) of
                        Good -> do 
                            --0.01 chosen cos there's maximum 100 messages so mood can never go outside -1 to 1 range
                            let user = User {name = (name user), messages = (messages user) ++ [msg], mood = (mood user) + 0.01}
                            loop
                        Bad -> do
                            let user = User {name = (name user), messages = (messages user) ++ [msg], mood = (mood user) - 0.01}
                            loop
                False -> do
                    putStrLn "inbox closed"

main :: IO ()
main = do
    let users = [User{name = x, messages = [], mood = 0.0} | x <- names]
    ctr <- newMVar 0
    let count = Counter ctr
    let inboxes = [initInbox count user | user <- users]
    putStrLn "simulation finished"
