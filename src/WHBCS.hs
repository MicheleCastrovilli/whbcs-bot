module WHBCS where

import Network

import System.IO
import System.Timeout

import Text.Printf

import Control.Exception
import Control.Concurrent
import Control.Monad.State
import Control.Monad.Reader

import Data.Char
import Data.Maybe

type Conn = ReaderT BotConnection IO
type Net = StateT BotState Conn

type NickName = String

type DomainName = String
type Port = Int

data BotState = BotState { currentNick :: String
                         , inRoom :: Bool }

newtype BotConnection = BotConnection { socket :: Handle }

data TerminalType = Dumb | Ansi | VTE

instance Show TerminalType where
    show Dumb = "dumb"
    show Ansi = "ansi"
    show VTE  = "vte"

data Response = Reply NickName String
              | System String
              | Evt String

instance Read Response where
    readsPrec _ input =
        let (nick,rn2) = span (/='>') $ drop 1 input
            (text,rn3) = span (/='\n') $ drop 2 rn2
            (sys,rs2)  = span (/='\n') $ drop 1 input
            (evt,re2)  = span (=='*') input
            in case input of
                 '<':_ -> [(Reply nick text, drop 1 rn3)]
                 '#':_ -> [(System sys, drop 1 rs2)]
                 '*':_ -> [(Evt (drop 1 evt), drop 1 re2)]
                 _     -> []

instance Show Response where
    show (Reply n t) = "<" ++ n ++ "> " ++ t
    show (System t) = "#" ++ t

emptyState = BotState "" False

withConnectionTo :: DomainName -> Port -> Net a -> IO a
withConnectionTo d p loop = bracket (connect d p) disconnect loop'
    where loop' = runReaderT (evalStateT loop emptyState)
          disconnect = hClose . socket

connect :: DomainName -> Port -> IO BotConnection
connect d p = notify $ do
             h <- connectTo d (PortNumber (fromIntegral p))
             hSetBuffering h NoBuffering
             hSetNewlineMode h (NewlineMode LF CRLF)
             hSetBuffering stdout NoBuffering
             return $ BotConnection h
    where notify a = bracket_
                (printf "Connection to %s:%d... " d p >> hFlush stdout)
                (putStrLn "Done.")
                a

start :: NickName -> IO ()
start n = withConnectionTo "leet.nu" 4321 $ do
            setTerminal Dumb
            setName n
            enterRoom
            (lift $ asks socket ) >>= listen
            quit

listen :: Handle -> Net ()
listen h = forever $ do
    s <- liftIO $ timeout (10000000) (xGetLine h)
    case s of
        Nothing -> write ""
        Just "" -> return ()
        Just str -> do maybe (return ()) eval $ maybeRead $ filter (/='\NUL') str
                       liftIO $ putStrLn $ show s

xGetLine :: Handle -> IO String
xGetLine h = xGet h ""
    where xGet h s = do c <- hGetChar h
                        case c of
                            '\NUL' -> return s
                            '\n' -> return s
                            _ -> xGet h (s++[c])

eval :: Response -> Net ()
eval (Reply n "!test") = write $ "Hello " ++ n
eval (Reply n "!ping") = write $ "Pong!"
eval (Reply n s) = do
                   nick <- gets currentNick
                   when (s == "!help @" ++ nick)
                     (write "Hi, I am a bot prototype made by @viviff.")
eval _ = return ()

write :: String -> Net ()
write s = do h <- lift $ asks socket
             liftIO $ hPrintf h "%s\n" s
             liftIO $ printf "%s\n" s

setTerminal :: TerminalType -> Net ()
setTerminal = write . ("/term " ++) . show

setName :: NickName -> Net ()
setName s = do
            state <- get
            write ("/nick " ++ s)
            put (state {currentNick = s})

enterRoom :: Net ()
enterRoom = do
            state <- get
            h <- lift $ asks socket
            when (not $ inRoom $ state)
                (write "/join")
            put (state {inRoom = True})

leaveRoom :: Net ()
leaveRoom = do
            state <- get
            when (inRoom $ state)
                (write "/leave")
            put (state {inRoom = False})

quit :: Net ()
quit = write "/quit"

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . filter (null . dropWhile isSpace . snd) . reads
