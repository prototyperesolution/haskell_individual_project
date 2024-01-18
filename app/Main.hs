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

sendMessages :: MVar User -> [MVar User] -> MVar Int -> IO()
sendMessages sender recipients count = loop
    where
        loop = do
            --taking and putting back, we just want to check mood value
            ctr <- readMVar count
            sndr <- readMVar sender
            --putMVar sender sndr
            recipientIndex <- randomRIO (0, (length recipients)-1)
            receiver <- takeMVar (recipients !! recipientIndex)
            case (sndr == receiver) of
                True -> do
                    --putStrLn "same guy"
                    putMVar (recipients !! recipientIndex) receiver
                    threadDelay 10
                    loop
                False -> do
                    case (ctr < limit) of
                        True -> do
                            msg <- constructMessage sndr
                            let m = mood sndr
                            let moodDelay = (200000 - m)
                            randomDelay <- randomRIO (0, moodDelay)
                            threadDelay (moodDelay + randomDelay) 
                            case sentiment msg of
                                Good -> do
                                    let newUser = User{name = (name receiver), mood = (mood receiver + 10000),
                                                        messages = (messages receiver) ++ [msg]}
                                    putMVar (recipients !! recipientIndex) newUser
                                    modifyMVar_ count (\counter -> return (counter + 1))
                                    --putStrLn "sent good msg"
                                Bad -> do
                                    let newUser = User{name = (name receiver), mood = (mood receiver - 10000),
                                                        messages = (messages receiver) ++ [msg]}
                                    putMVar (recipients !! recipientIndex) newUser
                                    modifyMVar_ count (\counter -> return (counter + 1))
                                    --putStrLn "sent bad msg"
                            --putStrLn ((show ctr) <>" "<>(show (index sndr)))
                            threadDelay 10
                            loop
                        False -> do
                            putMVar (recipients !! recipientIndex) receiver
                            return ()



prettyPrintMessage :: Message -> IO()
prettyPrintMessage msg = do
    putStr ("From: "<>(from msg))
    putStrLn(" Message body: "<>(content msg))

prettyPrintUser :: MVar User -> IO()
prettyPrintUser mperson = do
    person <- readMVar mperson
    putStrLn ("\nName: "<>(name person))
    putStrLn ("Received: "<>show(length (messages person))<>" messages")
    case ((mood person) < 0) of
        True -> do
            putStrLn ("They were mostly mean messages. Their current mood is: "<>show(mood person))
        False -> do
            putStrLn ("They were mostly nice messages. Their current mood is: "<>show(mood person))
    putStrLn "Inbox \n"
    mapM_ prettyPrintMessage (messages person)

main :: IO ()
main = do
    let users = [User{name = x, mood = 0, messages = []} | x <- names]
    ctr <- newMVar 0
    userList <- mapM newMVar users
    mapM_ (\user -> forkIO $ sendMessages user userList ctr) userList
    mapM_ prettyPrintUser userList

