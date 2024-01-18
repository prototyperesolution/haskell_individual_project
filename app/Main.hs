module Main (main) where

import Types
import Control.Concurrent
import System.Random

names = ["Macbeth","Macduff","Duncan","LMacbeth","Banquo","LMacduff","Malcolm","Witch1","Witch2","Witch3"]
limit = 100
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

{--initUser :: String -> IO User
initUser x = do
    m <- newMVar 0
    im <- newEmptyMVar
    let u = User{name = x, messages = [], mood = m, inMsg = im}
    return u
--}
simulateUserInbox :: User -> Counter -> IO()
simulateUserInbox user (Counter counter) = loop
    where
        loop = do
            inm <- takeMVar (inMsg user)
            putStrLn "stuck"
            m <- takeMVar (mood user)
            count <- takeMVar counter
            case inm of
                Empty -> do
                    putStrLn (" p ")
                _ -> do
                    putStrLn "hello"
                    case (sentiment inm) of
                                Good -> putMVar (mood user) (m + 1000)
                                Bad -> putMVar (mood user) (m - 1000)
                    loop

simulateUserOutbox :: User -> [User] -> Counter -> IO()
simulateUserOutbox user recipients (Counter counter) = loop
    where
        loop = do
            count <- takeMVar counter
            case (count <= limit) of
                True -> do
                    ---putStrLn ("here "<> name user <>" outbox")
                    msg <- constructMessage user
                    recipientIndex <- randomRIO (0, (length recipients) -1)
                    let recipient = recipients !! recipientIndex
                    --not sending messages to themselves
                    case (recipient == user) of
                        True -> do
                            putStrLn "same guy"
                            putMVar counter count
                            loop
                        False -> do
                            m <- takeMVar (mood user)
                            let moodDelay = (20000-m)
                            randomDelay <- randomRIO (0, 100000)
                            threadDelay (randomDelay + moodDelay) 
                            putMVar (inMsg recipient) msg
                            putStrLn "sent"
                            putMVar (mood user) m
                            putMVar counter (count + 1)
                            loop
                False -> do
                    putMVar counter count
                    closeInboxes recipients
                    --putStrLn ("closing "<>(name user)<>" outbox")

closeInboxes :: [User] -> IO()
closeInboxes [] = putStrLn "inboxes closed"
closeInboxes (x:xs) = do
        putMVar (inMsg x) Empty
        closeInboxes xs

main :: IO ()
main = do
    let users = [User {name = x, messages = [], mood <- newMVar 0, inMsg <- newEmptyMVar} | x <- names]
    ctr <- newMVar 0
    let count = Counter ctr
    let firstUser = users !! 0
    let sndUser = users !! 1
    let thdUser = users !! 2
    let frthUser = users !! 3
    let ffthUser = users !! 4
    let sxthUser = users !! 5
    let svnthUser = users !! 6
    let eightthUser = users !! 7
    let nnthUser = users !! 8
    let tenthUser = users !! 9
    _ <- forkIO (simulateUserOutbox firstUser users count)
    _ <- forkIO (simulateUserInbox firstUser count)
    _ <- forkIO (simulateUserOutbox sndUser users count)
    _ <- forkIO (simulateUserInbox sndUser count)
    _ <- forkIO (simulateUserOutbox thdUser users count)
    _ <- forkIO (simulateUserInbox thdUser count)
    _ <- forkIO (simulateUserOutbox frthUser users count)
    _ <- forkIO (simulateUserInbox frthUser count)
    _ <- forkIO (simulateUserOutbox ffthUser users count)
    _ <- forkIO (simulateUserInbox ffthUser count)
    _ <- forkIO (simulateUserOutbox sxthUser users count)
    _ <- forkIO (simulateUserInbox sxthUser count)
    _ <- forkIO (simulateUserOutbox svnthUser users count)
    _ <- forkIO (simulateUserInbox svnthUser count)
    _ <- forkIO (simulateUserOutbox eightthUser users count)
    _ <- forkIO (simulateUserInbox eightthUser count)
    _ <- forkIO (simulateUserOutbox nnthUser users count)
    _ <- forkIO (simulateUserInbox nnthUser count)
    _ <- forkIO (simulateUserOutbox tenthUser users count)
    _ <- forkIO (simulateUserInbox tenthUser count)

    putStrLn "x"