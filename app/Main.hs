module Main (main) where

import Types
import Control.Concurrent
import System.Random

names = ["Macbeth","Macduff","Duncan","LMacbeth","Banquo","LMacduff","Malcolm","Witch1","Witch2","Witch3"]

goodWords = ["happy","nice","friendly","fun","cool","swag"]
badWords = ["evil","mean","ugly","dishonorable","stupid","stinky"]
subjectsObjects = ["I", "you", "they", "everyone", "me"]
verbs = ["think","say","know","yell","believe","whisper"]

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

initUser :: String -> IO User
initUser x = do
    m <- newMVar 0
    im <- newEmptyMVar
    let u = User{name = x, messages = [], mood = m, inMsg = im}
    return u

simulateUserInbox :: User -> Counter -> IO()
simulateUserInbox user (Counter counter) = loop
    where
        loop = do
            msg <- takeMVar (inMsg user)
            m <- takeMVar (mood user)
            count <- takeMVar counter
            case (count < 100) of
                True -> do
                    case (sentiment msg) of
                        Good -> putMVar (mood user) (m + 10000)
                        Bad -> putMVar (mood user) (m - 10000)
                    putMVar counter count
                False -> do
                    putStrLn ("Closing " <> (name user) <>" Inbox")

simulateUserOutbox :: User -> [IO User] -> Counter -> IO()
simulateUserOutbox user recipients (Counter counter) = loop
    where
        loop = do
            count <- takeMVar counter
            case (count < 100) of
                True -> do
                    msg <- constructMessage user
                    recipientIndex <- randomRIO (0, (length recipients) -1)
                    recipient <- recipients !! recipientIndex
                    --not sending messages to themselves
                    case (recipient == user) of
                        True -> loop
                        False -> do
                            m <- takeMVar (mood user)
                            threadDelay (2000000-m) 
                            putMVar (inMsg recipient) msg
                            putMVar (mood user) m
                            putMVar counter (count + 1)
                            loop
                False -> do
                    putStrLn ("Closing " <> (name user) <>" Outbox")

main :: IO ()
main = do
    let users = [initUser x | x <- names]
    ctr <- newMVar 0
    let count = Counter ctr
    
