module Main (main) where

import Control.Exception.Base(bracket, handle, SomeException)
import Control.Monad(liftM, liftM2, forM, forM_, (>>))
import Control.Monad.ST
import Data.Array.IArray(Array, array, assocs)
import Data.Array.MArray(newArray, getElems, readArray, writeArray)
import Data.Array.ST(STUArray)
import Data.Maybe(fromMaybe, isJust, fromJust)
import Data.STRef
import Data.Text(pack, unpack, toLower)
import System.Cmd(system)
import System.Console.GetOpt
import System.Environment(getArgs)
import System.Directory(getDirectoryContents, doesDirectoryExist, copyFile)
import System.DiskSpace(getAvailSpace)
import System.Exit(exitFailure, ExitCode(ExitSuccess))
import System.FilePath((</>), takeExtension)
import System.IO(putStrLn, hPutStrLn, stdout, stderr, hFileSize, hClose, openFile, IOMode(ReadMode))
import System.IO(hSetBuffering, BufferMode(LineBuffering))
import System.Random(RandomGen, getStdGen, randomR)
import Text.Printf(printf)
import Text.Regex.Posix((=~))


-- Parsing command line part.
data Options = Options
     {
        outDir  :: FilePath,
        szLimit :: Integer,
        bitRate :: Int
     } deriving Show

defOpts = Options
    {
        outDir  = [],
        szLimit = 0,
        bitRate = 256
    }

options :: [OptDescr (Options -> Options)]
options =
    [
        Option ['o'] ["outDir"] (ReqArg (\s opts -> opts {outDir = s}) "Dir") "Output directory",
        Option ['s'] ["sizeLimit"] (ReqArg (\s opts -> opts {szLimit = size2int s}) "Limit") "Total transfer size (in K, M or G)",
        Option ['b'] ["bitRate"] (ReqArg (\s opts -> opts {bitRate = read s}) "BitRate") "Mp3 encode bit rate, default 256"
    ]

validateOptions :: Options -> [String] -> Maybe String
validateOptions Options {outDir = []} _ = Just "No output directory"
validateOptions _ [] = Just "No input directory"
validateOptions _ _ = Nothing

compileOpts :: [String] -> IO (Options, [String])
compileOpts argv = do
    let (o, dirs, errs) = getOpt RequireOrder options argv
    if not(null errs)
       then showUsage errs
       else do
            let opts = foldl (flip id) defOpts o
            case validateOptions opts dirs of
                 Just(s) -> showUsage [s]
                 Nothing -> return (opts, dirs)
    where header = "Usage: mshflr [OPTION...] search_directories..."
          showUsage errs = die (concat errs ++ "\n" ++ usageInfo header options)

size2int :: String -> Integer
size2int s = floor(f m)
    where (_, m, _) = s =~ "([0-9]+(.[0-9]+)?[KkMG])" :: (String, String, String)
          f [] = 0.0
          f m = multiplier * value
          multiplier = case (head . reverse $ m) of
                'G' -> 1024^3
                'M' -> 1024^2
                'K' -> 1024
                'k' -> 1024
                _   -> 0.0
          value = (read . init $ m) :: Double

       
-- Shuffling part.
shuffleOneToN :: RandomGen g => g -> Int -> [Int]
shuffleOneToN g n = runST $ do
    arr <- newArray (1, n) 1 :: ST s (STUArray s Int Int)
    gst <- newSTRef g
    forM_ [2 .. n] $ \i -> do
        gg <- readSTRef gst
        let (k, ggg) = randomR (1, i) gg
        writeSTRef gst ggg
        if k == i
           then do
                writeArray arr i i
           else do
                v <- readArray arr k
                writeArray arr k i
                writeArray arr i v
    getElems arr
    

   -- flac -d /path/to/file.flac
   -- lame --perset insane /path/to/decoded/file.wav
   -- flac -cd 1.flac | lame --preset extreme - 1.mp3
   -- flac -cd 1.flac | lame -b 256 - 1.mp3
   -- ffmpeg -loglevel quiet -i "2.mp4" -vn -ar 44100 -ac 2 -ab 256k -f mp3 "2.mp3"

-- Type alias for copy/convert function.
type CCFunc = FilePath -> FilePath -> IO(Maybe Integer)

-- Maps file name into pair (file name, copy function).
mapAudioFile :: CCFunc -> CCFunc -> FilePath -> Maybe(FilePath, CCFunc)
mapAudioFile ccFlac ccMp4 f =
    let ext = unpack . toLower . pack . takeExtension $ f
    in case ext of
        ".mp3"  -> Just(f, cpMp3)
        ".flac" -> Just(f, ccFlac)
        ".mp4"  -> Just(f, ccMp4)
        _       -> Nothing

          
scanFs :: FilePath -> IO [FilePath]
scanFs root = do
     let notDots p = p /= "." && p /= ".."
     items <- liftM (filter notDots) $ getDirectoryContents root
     paths <- forM items $ \item -> do
           let path = root </> item
           isDir <- doesDirectoryExist path
           if isDir
              then scanFs path
              else return [path]
     return (concat paths)

     
die :: String -> IO a
die msg = do
    hPutStrLn stderr ("Error: " ++ msg)
    exitFailure


cpFile  ::  (FilePath -> FilePath -> IO())  -- Copy/Convert function
    -> FilePath                             -- From
    -> FilePath                             -- To
    -> IO(Maybe Integer)                    -- Copied size
cpFile cc from to = handle handler $ do
    cc from to
    bracket (openFile to ReadMode) hClose (\h -> liftM Just (hFileSize h))
    where
        handler :: SomeException -> IO (Maybe Integer)
        handler ex = putStrLn (show ex) >> return Nothing

cpFlac :: Int               -- Bit rate
    -> FilePath             -- From
    -> FilePath             -- To
    -> IO(Maybe Integer)    -- Copied size
cpFlac br = cpFile (convertFlac br)
    where convertFlac br from to = do
            let cmd = "flac -cds \"" ++ from ++ "\" | lame -h --quiet -b " ++ show(br) ++ " - \"" ++ to ++ "\""
            system cmd
            return ()

cpMp4 :: Int                -- Bit rate
    -> FilePath             -- From
    -> FilePath             -- To
    -> IO(Maybe Integer)    -- Copied size
cpMp4 br = cpFile (convertMp4 br)
    where convertMp4 br from to = do
            let cmd = "ffmpeg -loglevel quiet -i \"" ++ from ++ "\" -vn -ar 44100 -ac 2 -ab "
                        ++ show(br) ++ "k -f mp3 \"" ++ to ++ "\""
            system cmd
            return ()

cpMp3 :: FilePath           -- From
    -> FilePath             -- To
    -> IO(Maybe Integer)    -- Copied size
cpMp3 = cpFile copyFile

cpNope :: FilePath -> FilePath -> IO(Maybe Integer)     -- No copy function.
cpNope _ _ = return(Just(0))


copyFiles :: Integer                        -- Left space
          -> FilePath                       -- Output directory
          -> [(Int, (FilePath, CCFunc))]    -- Indexed, shuffled files with copiyng functions
          -> IO()
copyFiles _ _ [] = return ()
copyFiles space _ _ | space < 1 = putStrLn "Done, enjoy!"
copyFiles space od ((i, (f, ccf)):fx) = do
    let outPath = od </> (printf "%05d.mp3" i)
    cpr <- ccf f outPath
    case cpr of
        Nothing -> return ()
        Just(s) -> do
            printf "%05d - %s\n" i f
            copyFiles (space - s) od fx

-- Checks if external command available.
checkCmd :: String -> IO(Bool)
checkCmd name = do
    putStr $ "Checking " ++ name ++ ": "
    hasCmd <- liftM (==ExitSuccess) (system $ "which " ++ name)
    putStrLn $ if hasCmd then "yes" else "no"
    return hasCmd

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    (opts, dirs) <- getArgs >>= compileOpts
    let od = outDir opts
    sz <- if (szLimit opts > 0)
       then return (szLimit opts)
       else do
           putStrLn "WARNING: No size limit. Filling out all available space."
           getAvailSpace od
    hasLame <- checkCmd "lame"
    hasFlac <- checkCmd "flac"
    hasFfmpeg <- checkCmd "ffmpeg"
    let br = bitRate opts
    files <- liftM concat $ forM dirs scanFs
    let mapFileCC = mapAudioFile
                        (if hasLame && hasFlac then (cpFlac br) else cpNope)
                        (if hasFfmpeg then (cpMp4 br) else cpNope)
    let ccFiles = map fromJust . filter isJust . map mapFileCC $ files
    rg <- getStdGen
    let total = length(ccFiles)
    let indexes = shuffleOneToN rg total
    let shuffled = array (1, total) $ zip indexes ccFiles :: Array Int (FilePath, CCFunc)
    copyFiles sz od $ assocs shuffled
     
     
     

