import Data.Word
import Network hiding(accept)
import Network.Socket
import System.Environment
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Fix (fix)
 
type Msg = (Int, String)
 
main :: IO ()
main = do
    [portStr] <- getArgs
    let port = fromIntegral (read portStr :: Int)
    chan <- newChan
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet port iNADDR_ANY)
    listen sock 2
    forkIO $ fix $ \loop -> do
        (_, msg) <- readChan chan
        loop
    mainLoop sock chan 0
 
mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan nr = do
    conn <- accept sock
    forkIO (runConn conn chan nr)
    mainLoop sock chan $! nr+1
 
runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) chan nr = do
    let broadcast msg = writeChan chan (nr, msg)
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    hPutStrLn hdl "Welcome to Cratos2"
    chan' <- dupChan chan
    reader <- forkIO $ fix $ \loop -> do
        (nr', line) <- readChan chan'
        when (nr /= nr') $ hPutStrLn hdl line
        loop
    let alwaysError :: SomeException -> IO ()
    	alwaysError = \_ -> return ()
    handle alwaysError $ fix $ \loop -> do
        line <- liftM init (hGetLine hdl)
        case line of
         "!quit" -> hPutStrLn hdl "Bye!"
         _      -> do
	    appendFile "log.txt" (line ++ "\n")
            broadcast line
            loop
    killThread reader
    hClose hdl
