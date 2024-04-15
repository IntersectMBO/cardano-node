{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

--------------------------------------------------------------------------------

module Data.Log (lineFoldl') where

--------------------------------------------------------------------------------

-- base.
import           System.IO
  (
    Handle
  , withFile, IOMode (ReadMode)
  , hSetBuffering, BufferMode (NoBuffering)
  )

-- package: bytestring.
import qualified Data.ByteString as BS
-- package: text.
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextE
-- Using strict ByteString and Text
-- Text.Lazy has no streaming decoding support.

--------------------------------------------------------------------------------

-- TODO: Make it a parameter!
hGetBufferSizeMB :: Int
hGetBufferSizeMB = 25

--------------------------------------------------------------------------------

-- `foldl'` through a file's `Text` UTF-8 lines in constant space.
-- Folds through lines of `Text` with strict application of the operator.
-- Does not treat '\r' (carriage return) as a newline character, only `\n`.
-- An empty file is considered to have just an empty line, an empty line is "".
-- The operator's application is strict but it's up to the user of this function
-- to be sure lines are consumed/discarded/garbage-collected so if you want to,
-- for example, only count the number of lines in a file the decoding and Text
-- creation can be a "little lazy" (isn't that customizability desirable ?????).
lineFoldl' :: (a -> Text.Text -> a) -> a -> FilePath -> IO a
lineFoldl' f initialAcc filePath = do
  -- Creates a `Handle` and closes it when finished or on exception.
  withFile filePath ReadMode $ \handle -> do
    -- Force the no use of buffers (TODO: Check the internals).
    hSetBuffering handle NoBuffering
    -- TODO: This one does not need to be strict right?
    !acc <- lineFoldl'' handle initialDecoder f initialAcc
    return acc

lineFoldl'' :: Handle -> Decoder -> (a -> Text.Text -> a) -> a -> IO a
lineFoldl'' handle decoder f initialAcc = do
  (textLine, maybeDecoder) <- {-# SCC "lineFoldl''_nextLine" #-}
                              nextLine handle decoder
  -- CRITICAL: MUST BE "STRICT" by function contract.
  --           I repeat, the accumulator function has to be strict!
  let !nextAcc = {-# SCC "lineFoldl''_f" #-} f initialAcc textLine
  case maybeDecoder of
    Nothing -> return initialAcc
    (Just decoder') -> lineFoldl'' handle decoder' f nextAcc

--------------------------------------------------------------------------------

-- The cycle is: Handle -> ByteString -> UTF-8 decode -> Text.Text
data Decoder = Decoder
  -- TODO: Strict or not? Let the function caller decide?
  -- Function `nextLine` is all about pattern matching these things.
  { _unfinishedLine :: !Text.Text
  , _textLeft       :: !Text.Text
  , _byteStringLeft :: !BS.ByteString
  , _textDecoding   :: !(BS.ByteString -> TextE.Decoding)
  }

-- Use empty `Text`s and create an empty/initial `Data.Text.Encoding.Some`
initialDecoder :: Decoder
initialDecoder = Decoder "" "" "" TextE.streamDecodeUtf8

-- There's always at least one line present because I say so.
nextLine :: Handle -> Decoder -> IO (Text.Text, Maybe Decoder)
--------------------------------------------------------------------------------
-- IO reads: Maybe a partial line but no decoded Text and no fetched ByteString.
--------------------------------------------------------------------------------
nextLine handle (Decoder unfinishedLine "" "" continue) = {-# SCC "nextLine_1" #-} do
  --print ((1::Int, unfinishedLine, "", "")::(Int,Text.Text,Text.Text,BS.ByteString))
  -- Use `Data.Text.IO.hGetChunk` ? It uses an unknown buffer size!
  bs <- {-# SCC "nextLine_1_hGet" #-}
        BS.hGetNonBlocking handle (hGetBufferSizeMB * 1024 * 1024)
  -- Also use BS.length ? To end if lower than requested ? But it's O(n)!
  if bs == BS.empty
  -- Last (or maybe first of an empty file) line and no more input available!
  then return (unfinishedLine, Nothing)
  -- Call `newLine` again to handle the newly fetched ByteString.
  else nextLine handle $ Decoder unfinishedLine "" bs continue
--------------------------------------------------------------------------------
-- UTF-8 decode: Maybe a partial line, no decoded Text and only some ByteString.
--------------------------------------------------------------------------------
nextLine handle (Decoder unfinishedLine "" bs continue) = {-# SCC "nextLine_2" #-} do
  --print ((2::Int, "", "", bs)::(Int,Text.Text,Text.Text,BS.ByteString))
  -- We only have a fetched ByteString and we only need to decode more Text.
  let (TextE.Some text' bs' continue') = {-# SCC "nextLine_2_decode" #-}
                                         continue bs
  -- Call `nextLine` again to handle lines and partial lines.
  nextLine handle $ Decoder unfinishedLine text' bs' continue'
------------------------------------------------------------------------------
-- Newline split: everything to find newline characters or ask for more input.
------------------------------------------------------------------------------
nextLine handle (Decoder unfinishedLine text bs !continue) = {-# SCC "nextLine_3" #-} do
  --print ((5::Int, unfinishedLine, text, bs)::(Int,Text.Text,Text.Text,BS.ByteString))
  let (consumed, remainder) = {-# SCC "nextLine_3_break" #-}
                              Text.break (== '\n') text
  case remainder of
    -- No newline character found!
    -- break (== 1) []      -> ( [],      []      )
    -- break (== 1) [0,0,0] -> ( [0,0,0], []      )
    "" -> {-# SCC "nextLine_3_newline_no" #-} do
      nextLine handle $ Decoder (unfinishedLine <> text) "" bs continue
    -- One newline character was found!
    -- break (== 1) [1]     -> ( []     , [1]     )
    -- break (== 1) [1,0,0] -> ( []     , [1,0,0] )
    -- break (== 1) [0,0,1] -> ( [0,0]  , [1]     )
    -- break (== 1) [0,1,0] -> ( [0]    , [1,0]   )
    _ -> {-# SCC "nextLine_3_newline_yes" #-} do
      -- Remove the `\n`.
      -- If `reminder` is not `empty`, a `\n` was found and it's the first char.
      let text' = {-# SCC "nextLine_3_drop" #-} Text.drop 1 remainder
      return $ case consumed of
        -- Next character was a newline. Return the line buffer with no append.
        -- break (== 1) [1]     -> ( []     , [1]     )
        -- break (== 1) [1,0,0] -> ( []     , [1,0,0] )
        "" -> {-# SCC "nextLine_3_append_no" #-}
            ( unfinishedLine
            , Just $ Decoder "" text' bs continue
            )
        -- break (== 1) [0,0,1] -> ( [0,0]  , [1]     )
        -- break (== 1) [0,1,0] -> ( [0]    , [1,0]   )
        _  -> {-# SCC "nextLine_3_append_yes" #-}
            ( unfinishedLine <> consumed
            , Just $ Decoder "" text' bs continue
            )
