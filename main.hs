import Text.Printf (printf)
import System.IO (hFlush, stdout)
import Control.Concurrent (threadDelay)
import System.Process (callCommand)

printTime :: Int -> IO ()
printTime totalSeconds = do
    let minutes = totalSeconds `div` 60
        seconds = totalSeconds `mod` 60
    printf "\r%02d:%02d" minutes seconds
    hFlush stdout

countDown :: Int -> IO ()
countDown 0 = do
    putStrLn "\nTime's up!"
    playSound
countDown time = do
    printTime time
    threadDelay 1000000
    countDown (time - 1)

pomodoro :: Int -> Int -> IO ()
pomodoro workMinutes breakMinutes = do
    putStrLn "Time to get to work!"
    countDown (workMinutes * 60)
    putStrLn "Take a break!"
    countDown (breakMinutes * 60)
    pomodoro workMinutes breakMinutes

playSound :: IO ()
playSound = do
    let soundFile = "audio/timer-sound.wav"
    callCommand ("paplay " ++ soundFile)

main :: IO ()
main = do
    putStrLn "How many minutes would you like for work?"
    workMinutes <- getLine
    putStrLn "How many minutes would you like for break?"
    breakMinutes <- getLine
    let convertedWorkMinutes = read workMinutes :: Int
    let convertedBreakMinutes = read breakMinutes :: Int
    pomodoro convertedWorkMinutes convertedBreakMinutes
