module Main (main) where

import Types
import Control.Concurrent
import System.Random
import Data.List

names = ["Macbeth","Macduff","Duncan","LMacbeth","Banquo","LMacduff","Malcolm","Witch1","Witch2","Witch3"]
limit = 100
goodWords = ["happy","nice","friendly","fun","cool","swag"]
badWords = ["evil","mean","ugly","dishonorable","stupid","stinky"]
subjectsObjects = ["I", "you", "they", "everyone", "me"]
verbs = ["think","say","know","yell","believe","whisper"]

-- | Constructs an instance of the Message type. This will contain a real sentence of the structure Object-Subject-Verb-Adjective
-- with each word being chosen fron a list. The message will also have a sentiment of Good or Bad which will impact which adjective is chosen
-- and will impact the mood of the recipient. A lower mood in the recipient means they send messages slower
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

-- | The main simulation function. Takes as input a user (sender) and all other users (potential recipients).
-- Ensures that a user does not send a message to themselves. The handling of the recipient's mood changes is done
-- in this function. Function signature is IO as it changes MVar values referring to the users themselves
sendMessages :: MVar User -> [MVar User] -> MVar Int -> IO()
sendMessages sender recipients count = loop
    where
        loop = do
            --taking and putting back, we just want to check mood value
            sndr <- readMVar sender
            --putMVar sender sndr
            recipientIndex <- randomRIO (0, ((length recipients)-1))
            receiver <- takeMVar (recipients !! recipientIndex)
            case (sndr == receiver) of
                True -> do
                    putMVar (recipients !! recipientIndex) receiver
                    loop
                False -> do
                    ctr <- takeMVar count
                    if ctr < limit
                        then do
                            putMVar count (ctr + 1)
                            msg <- constructMessage sndr
                            let m = mood sndr
                            let moodDelay = (2000 - m)
                            randomDelay <- randomRIO (0,moodDelay)
                            threadDelay randomDelay
                            if (sentiment msg) == Good
                                then do
                                    let newUser = User{name = (name receiver), mood = (mood receiver + 100),
                                                        messages = (messages receiver) ++ [msg]}
                                    putMVar (recipients !! recipientIndex) newUser
                                else do
                                    let newUser = User{name = (name receiver), mood = (mood receiver - 100),
                                                        messages = (messages receiver) ++ [msg]}
                                    putMVar (recipients !! recipientIndex) newUser
                            loop
                        else do
                            putMVar count ctr
                            putMVar (recipients !! recipientIndex) receiver
                            return ()


-- | Prints messages in a more readable way
prettyPrintMessage :: Message -> IO()
prettyPrintMessage msg = do
    putStr ("From: "<>(from msg))
    putStrLn(" Message body: "<>(content msg))

-- | Prints user information, including contents of their inboxes
prettyPrintUser :: User -> IO()
prettyPrintUser person = do
    putStrLn ("\nName: "<>(name person))
    putStrLn ("Received: "<>show(length (messages person))<>" messages")
    case ((mood person) < 0) of
        True -> do
            putStrLn ("They were mostly mean messages. Their current mood is: "<>show(mood person))
        False -> do
            putStrLn ("They were mostly nice messages. Their current mood is: "<>show(mood person))
    putStrLn "Inbox \n"
    mapM_ prettyPrintMessage (messages person)

-- | Main function from which the threads are forked. There is one thread per user.
-- To ensure the main thread does not terminate before its child threads, it only terminates when 100 messages
-- have been sent. After the simulation, sorts the list by who received the most messages.
main :: IO ()
main = do
    let users = [User{name = x, mood = 0, messages = []} | x <- names]
    ctr <- newMVar 0
    userList <- mapM newMVar users
    mapM_ (\user -> forkIO $ sendMessages user userList ctr) userList
    --dont want main thread to terminate before everything is finished
    let waitForThreads = do
            count <- readMVar ctr
            if (count < limit) 
                then do
                    threadDelay 100000 -- Wait a bit before checking again
                    waitForThreads
                else return ()
    waitForThreads
    finalList <- mapM readMVar userList
    let sortedList = sortBy (flip compare) finalList
    mapM_ prettyPrintUser sortedList
    putStrLn ("The person with the most messages at the end of the simulation was "<>(name (head sortedList)))

