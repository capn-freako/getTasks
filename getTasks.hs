-- |
-- = /getTasks/
-- Find all incomplete tasks in my /Gitit/ notes.
--
-- Original author: David Banas <capn.freako@gmail.com>
-- Original date: January 31, 2018
--
-- Copyright (c) 2018 David Banas; all rights reserved World wide.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Options.Applicative as O
import Options.Applicative  ( strOption, long, short, metavar, showDefault, value, help
                            , argument, str, execParser, info, helper, fullDesc, progDesc
                            , header, (<**>))

import Control.Arrow        ((&&&), (***))
import Control.Exception    (catch, SomeException (..))
import Control.Monad        (forM, forM_, unless)
import Data.Char            (isSpace)
import Data.Either          (isRight)
import Data.List            (groupBy, sortOn)
import Data.Maybe           (mapMaybe)
import Data.Ord             (Down(..))
import Data.Semigroup       ((<>))
import Data.Text            (strip, pack, unpack, Text)
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Calendar
import System.Directory     (doesFileExist, doesDirectoryExist, getDirectoryContents)
import System.FilePath.Posix ((</>), takeExtension, dropExtensions, takeBaseName)
import Text.ParserCombinators.Parsec

jrnlFileExts = [ ".page"
               , ".md"
               ]

-- Base names of any files that should be excluded from processing.
excludedFiles = [ "currentTasks"
                ]

data Opts = Opts
  { ext        :: String
  , srcd       :: String
  }

opts :: O.Parser Opts
opts = Opts
      <$> strOption
          ( long "ext"
         <> short 'e'
         <> metavar "EXT"
         <> showDefault
         <> value "page"
         <> help "File extension" )
      <*> argument str
          ( metavar "FILE"
         <> help "Source file/directory in which to search" )

-- |
-- == Custom data types.

-- | A /paragraph/ consists of:
--
-- * a group of lines,
-- * an indentation level, and
-- * a header level (0 for plain text).
newtype Par = Par ([String], Int, Int)
  deriving (Eq)

instance Show Par where
  show (Par (ss, ind, hdr)) =
    if hdr /= 0
      then replicate hdr '#' ++ (' ' : unlines ss)
      else unlines $ map (replicate (4 * ind) ' ' ++) ss

instance Ord Par where  -- Used to establish subordinance.
  compare (Par (_, ind1, hdr1)) (Par (_, ind2, hdr2)) =
    case hdr1 of
      0 -> if hdr2 /= 0
             then LT
             else compare ind2 ind1
      _ -> if hdr2 == 0
             then GT
             else compare hdr2 hdr1
      
-- | A /section/ consists of:

-- * an opening paragraph, and
-- * an optional list of subsections.
newtype Sec = Sec (Par, [Sec])

instance Show Sec where
  show = unlines . map show . secToPars

-- | Convert a /section/ to a list of /paragraph/s.
secToPars :: Sec -> [Par]
secToPars (Sec (p, subs)) = p : concatMap secToPars subs

-- | Filter a list of /section/s, keeping only those that are,
-- or contain, incomplete actions items.
getToDos :: Unop [Sec]
getToDos [] = []
getToDos (s@(Sec (p, subs)) : ss) =
  if hasToDo s
    then let s' = if isToDo p
                    then s
                    else Sec (p, getToDos subs)
         in  s' : getToDos ss
    else getToDos ss
  
-- |
-- == Main program control flow.

-- | Process command line options and dispatch to @'findTasks'@ helper function.
main :: IO ()
main = findTasks =<< execParser opts'
  where
    opts' = info (opts <**> helper)
      ( fullDesc
     <> progDesc "Extract incomplete tasks from personal notes."
     <> header "get_tasks - a task fetcher/consolidator" )

-- | Make file vs. directory choice, after verifying existence.
--
-- Call @'scanFile'@ or @'scanDir'@ helper function, as appropriate.
findTasks :: Opts -> IO ()
findTasks Opts{..} = do
  t  <- getCurrentTime
  tz <- getCurrentTimeZone
  let t' = utcToZonedTime tz t
  putStrLn "---"
  putStrLn $ "title: Incomplete Action Items as of: " ++ show t'
  putStrLn "format: markdown"
  putStrLn "toc: no"
  putStrLn "...\n\n"
  putStrLn "[Back to *Home*](home)\n"
  isFile <-doesFileExist      srcd
  isDir  <-doesDirectoryExist srcd
  if isFile then scanFile srcd
            else if isDir then scanDir srcd
                          else error $ "Sorry, but " ++ show srcd ++ " does not exist."
  putStrLn "\n[Back to *Home*](home)\n"

-- | Scan a file for incomplete action items.
--
-- The lion's share of the work is done here,
-- relying heavily on various helper functions.
scanFile :: FilePath -> IO ()
scanFile fp = catch
  ( do ft <- readFile fp
       let toDos = getToDos $ sections ft
       unless (null toDos) $ do
         putStrLn "<hr>"
         putStrLn "<hr>"
         let fn = dropExtensions fp
         putStrLn $ "[" ++ fn ++ "](" ++ fn ++ ")\n"
         putStrLn $ unlines $ map show toDos
  )
  ( \(SomeException e) -> do
      print e
      return ()
  )

-- | Recursively scan a directory, making use of both:
--
-- * @'scanFile'@, and
-- * @'getRecursiveContents'@.
scanDir :: FilePath -> IO ()
scanDir fp = do
  fps <- getRecursiveContents fp
  let fps' = sortOn Down
           . filter ( uncurry (&&)
                    . ( ((`elem` jrnlFileExts) . takeExtension)
                        &&& (not .  (`elem` excludedFiles) . takeBaseName)
                      )
                    ) $ fps
  forM_ fps' scanFile

-- | Recursively scan a directory, yielding a list of all encompassed files.
--
-- From Ch. 9 in Real World Haskell
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)


-- |
-- == Text processing machinery.

-- |
-- === Parsing primitives.

-- | End of line.
eol :: Parser String
eol = try (string "\n\r")
  <|> try (string "\r\n")
  <|> string "\n"
  <|> string "\r"
  <?> "end of line"

-- | Space delimited symbol.
symbol :: Parser a -> Parser a
symbol p = skipMany space >> p

-- | Incomplete action item.
todo :: Parser String
todo = symbol (string "- [ ]")

-- | Paragraph.
--
-- Assume:
--
-- * Headers are prefaced by some number of /#/ characters, indicating their depth.
-- * Non-header paragraphs are, optionally, prefaced by either /tabs/ or /spaces/, where:
--
--     * levels of indentation correspond to 4 spaces, and
--     * a tab is equivalent to 4 spaces.
paragraph :: Parser Par
paragraph = try ( do hdr <- many1 (char '#')
                     rst <- many anyChar
                     return $ Par (toLines rst, 0, length hdr)
                )
            <|> ( do ind <- many1 (char '\t')
                     rst <- many anyChar
                     return $ Par (toLines rst, length ind, 0)
                )
            <|> ( do ind <- many space
                     rst <- many anyChar
                     return $ Par (toLines rst, length ind `div` 4, 0)
                )
  where toLines = map strip' . lines
        
-- |
-- === Misc. functions.

-- | Unary operations.
type Unop a = a -> a

-- | Detect a blank line.
blank :: String -> Bool
blank = all isSpace

-- | Apply a function on Text to a String.
onString :: Unop Text -> Unop String
onString f = unpack . f . pack

-- | Strip space from beginning and end of line.
--
-- @strip@ comes from the @Text@ module and has type: @Text -> Text@.
-- There is no equally convenient function on @String@.
-- So, we use our handy '@onString@' utility.
strip' :: Unop String
strip' = onString strip

-- | Convert a multi-line string to paragraphs (i.e. consecutive non-blank lines).
toParagraphs :: String-> [Par]
toParagraphs = map ( either (error "Bad paragraph parse!")
                            id
                   . parse paragraph "paragraph" . unlines)
               . filter (not . all blank)
               . groupBy (\l1 l2 -> not (blank l1 || blank l2))
               . lines

-- | Check if a /paragraph/ is a header.
isHeader :: Par -> Bool
isHeader (Par (ss, _, _)) = case ss of
  []    -> False
  s:ss' -> case s of []   -> False
                     c:cs -> c == '#'
                     
-- | Checks for action item in a /paragraph/.
isToDo :: Par -> Bool
isToDo (Par (ss, _, _)) = any (isRight . parse todo "todo") ss

-- | Checks for action item in a /section/.
hasToDo :: Sec -> Bool
hasToDo (Sec (Par (ss, _, _), subs)) =
  any (isRight . parse todo "todo") ss
    || any hasToDo subs

-- | Split file contents into /section/s.
sections :: String -> [Sec]
sections = parsToSecs . toParagraphs

-- | Convert a list of /paragraph/s into a list of /section/s.
parsToSecs :: [Par] -> [Sec]
parsToSecs []     = []
parsToSecs (p:ps) = Sec (p, subs) : parsToSecs rem
  where (subs, rem) = getSubs p ps

-- | Consume all contiguous subordinate /paragraph/s, converting them into /section/s,
-- and return unconsumed /paragraph/s.
getSubs :: Par -> [Par] -> ([Sec], [Par])
getSubs p []          = ([], [])
getSubs p ps@(p':ps') =
  if p' < p then let (secs,  pars)  = getSubs p' ps'
                     (secs', pars') = getSubs p pars
                 in  (Sec (p', secs) : secs', pars')
            else ([], ps)
    
-- |
-- == Misc.

-- | for
for = flip map

-- | Get the current date.
date :: IO (Integer, Int, Int) -- :: (year, month, day)
date = getCurrentTime >>= return . toGregorian . utctDay
