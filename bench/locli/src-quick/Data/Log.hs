{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

--------------------------------------------------------------------------------

module Data.Log ( LineFoldBufferSize (..)
                , lineFoldl'
                ) where

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

data LineFoldBufferSize
  = BufferSizeDefault
  | BufferSizeMiB Int

getBufferSize :: LineFoldBufferSize -> Int
getBufferSize = \case
  BufferSizeDefault -> 25     -- default value: 25MiB, because @fmaste says so.
  BufferSizeMiB i   -> i

--------------------------------------------------------------------------------

-- `foldl'` through a file's `Text` UTF-8 lines in constant space.
-- Folds through lines of `Text` with strict application of the operator.
-- Does not treat '\r' (carriage return) as a newline character, only `\n`.
-- An empty file is considered to have just an empty line, an empty line is "".
-- The operator's application is strict but it's up to the user of this function
-- to be sure lines are consumed/discarded/garbage-collected so if you want to,
-- for example, only count the number of lines in a file the decoding and Text
-- creation can be a "little lazy" (isn't that customizability desirable ?????).
lineFoldl' :: (a -> Text.Text -> a) -> a -> FilePath -> LineFoldBufferSize -> IO a
lineFoldl' f initialAcc filePath bufSize =
  -- Creates a `Handle` and closes it when finished or on exception.
  withFile filePath ReadMode $ \handle -> do
    -- Force the no use of buffers (TODO: Check the internals).
    hSetBuffering handle NoBuffering
    -- TODO: This one does not need to be strict right?
    !acc <- lineFoldl'' handle hGetBufferSize initialDecoder f initialAcc
    return acc
  where
    hGetBufferSize = getBufferSize bufSize * 1024 * 1024    

lineFoldl'' :: Handle -> Int -> Decoder -> (a -> Text.Text -> a) -> a -> IO a
lineFoldl'' handle hGetBufferSize decoder f initialAcc = do
  (textLine, maybeDecoder) <- nextLine handle hGetBufferSize decoder
  -- CRITICAL: MUST BE "STRICT" by function contract.
  --           I repeat, the accumulator function has to be strict!
  let !nextAcc = f initialAcc textLine
  case maybeDecoder of
    Nothing -> return initialAcc
    Just decoder' -> lineFoldl'' handle hGetBufferSize decoder' f nextAcc
    
--------------------------------------------------------------------------------

-- The cycle is: Handle -> ByteString -> UTF-8 decode -> Text.Text
data Decoder = Decoder
  -- TODO: Strict or not? Let the function caller decide?
  -- Function `nextLine` is all about pattern matching these things.
  { _unfinishedLine :: Text.Text
  , _textLeft       :: Text.Text
  , _byteStringLeft :: BS.ByteString
  , _textDecoding   :: BS.ByteString -> TextE.Decoding
  }

-- Use empty `Text`s and create an empty/initial `Data.Text.Encoding.Some`
initialDecoder :: Decoder
initialDecoder = Decoder "" "" "" TextE.streamDecodeUtf8

-- There's always at least one line present because I say so.
nextLine :: Handle -> Int -> Decoder -> IO (Text.Text, Maybe Decoder)
--------------------------------------------------------------------------------
-- IO reads: Maybe a partial line but no decoded Text and no fetched ByteString.
--------------------------------------------------------------------------------
nextLine handle hGetBufferSize = go
  where
    go = \case
      Decoder unfinishedLine "" "" continue -> do
        -- Use `Data.Text.IO.hGetChunk` ? It uses an unknown buffer size!
        bs <- BS.hGetNonBlocking handle hGetBufferSize
        if BS.null bs
        -- Last (or maybe first of an empty file) line and no more input available!
        then return (unfinishedLine, Nothing)
        -- Call `newLine` again to handle the newly fetched ByteString.
        else go $ Decoder unfinishedLine "" bs continue

      --------------------------------------------------------------------------------
      -- UTF-8 decode: Maybe a partial line, no decoded Text and only some ByteString.
      --------------------------------------------------------------------------------
      Decoder unfinishedLine "" bs continue ->
        -- We only have a fetched ByteString and we only need to decode more Text.
        let (TextE.Some text' bs' continue') = continue bs
        -- Call `go` again to handle lines and partial lines.
        in go $ Decoder unfinishedLine text' bs' continue'

      ------------------------------------------------------------------------------
      -- Newline split: everything to find newline characters or ask for more input.
      ------------------------------------------------------------------------------
      Decoder unfinishedLine text bs !continue -> do
        let (consumed, remainder) = Text.break (== '\n') text
        if Text.null remainder
        -- No newline character found!
        -- break (== 1) []      -> ( [],      []      )
        -- break (== 1) [0,0,0] -> ( [0,0,0], []      )
        then
          go $ Decoder (unfinishedLine <> text) "" bs continue
        -- One newline character was found!
        -- break (== 1) [1]     -> ( []     , [1]     )
        -- break (== 1) [1,0,0] -> ( []     , [1,0,0] )
        -- break (== 1) [0,0,1] -> ( [0,0]  , [1]     )
        -- break (== 1) [0,1,0] -> ( [0]    , [1,0]   )
        else
          -- Remove the `\n`.
          -- If `reminder` is not `empty`, a `\n` was found.
          let text' = Text.drop 1 remainder
          -- Put the most common case first.
          -- Use `Text.null` as much as possible because it's O(1).
          in if not $ Text.null consumed
            -- break (== 1) [0,0,1] -> ( [0,0]  , [1]     )
            -- break (== 1) [0,1,0] -> ( [0]    , [1,0]   )
            then return
              ( unfinishedLine <> consumed
              , Just $ Decoder "" text' bs continue
              )
            -- Next character was a newline. Return the line buffer with no append.
            -- break (== 1) [1]     -> ( []     , [1]     )
            -- break (== 1) [1,0,0] -> ( []     , [1,0,0] )
            else return
              ( unfinishedLine
              , Just $ Decoder "" text' bs continue
              )
