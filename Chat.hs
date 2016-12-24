-- A simple TCP Chat server

import Network (listenOn, PortID(PortNumber))
import Network.Socket (send, close, Socket, SockAddr, accept, socketToHandle)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle, IOMode(..))
import Control.Concurrent (forkIO) 
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan, dupChan)
import Control.Monad.Fix (fix)
import Control.Monad (when)
import Data.Char (ord)

data Msg = Msg String Int deriving (Show)

main :: IO ()
main = do
  sock <- listenOn $ PortNumber 3000
  chan <- newChan
  mainLoop sock chan 0

mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan userId = do
  conn <- accept sock
  forkIO $ runConn conn chan userId
  novaChan <- dupChan chan
  mainLoop sock novaChan (userId + 1)

runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) chan userId = do
  handle <- socketToHandle sock ReadWriteMode
  hSetBuffering handle NoBuffering
  forkIO $ fix $ \loop -> do 
    (Msg str senderId) <- readChan chan
    when (senderId /= userId) $ hPutStrLn handle str
    loop

  fix $ \loop -> do
    str <- init <$> (hGetLine handle)
    writeChan chan (Msg str userId)
    loop
