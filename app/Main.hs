module Main where

import qualified Graphics.UI.GLFW as GLFW
import Control.Monad (unless)
import System.Exit (exitFailure)

main :: IO ()
main = do
    success <- GLFW.init
    unless success $ do
        putStrLn "[!] Failed to initialize GLFW"
        exitFailure
    
    GLFW.windowHint $ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI
    GLFW.windowHint $ GLFW.WindowHint'Resizable False
    
    maybeWindow <- GLFW.createWindow 800 600 "xrd" Nothing Nothing
    case maybeWindow of
        Nothing -> do
            putStrLn "[!] Failed to create GLFW window"
            GLFW.terminate
            exitFailure
        Just window -> do
            putStrLn "[+] GLFW window created"
            
            -- Set up key callback
            GLFW.setKeyCallback window (Just keyCallback)
            
            -- Main loop
            mainLoop window
            
            -- Cleanup
            GLFW.destroyWindow window
            GLFW.terminate

keyCallback :: GLFW.KeyCallback
keyCallback window key scancode action mods
    | key == GLFW.Key'Escape && action == GLFW.KeyState'Pressed = 
        GLFW.setWindowShouldClose window True
    | otherwise = return ()

mainLoop :: GLFW.Window -> IO ()
mainLoop window = do
    shouldClose <- GLFW.windowShouldClose window
    unless shouldClose $ do
        GLFW.pollEvents
        mainLoop window
