{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OpenTelemetry.LogRecordProcessor.Simple (
  simpleProcessor,
) where

import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.Chan.Unagi
import Control.Exception
import Control.Monad (forever)
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import OpenTelemetry.Internal.Common.Types
import OpenTelemetry.Internal.Logs.Types


{- | This is an implementation of LogRecordProcessor which passes finished logs and passes the export-friendly ReadableLogRecord
representation to the configured LogRecordExporter, as soon as they are finished.
-}
simpleProcessor :: LogRecordExporter body -> IO (LogRecordProcessor body)
simpleProcessor LogRecordExporter {..} = do
  (inChan :: InChan (LogRecord body), outChan :: OutChan (LogRecord body)) <- newChan
  exportWorker <- async $ forever $ do
    bracket
      (readChan outChan)
      (writeChan inChan)
      exportSingleLogRecord

  let logRecordProcessorForceFlush =
        ( do
            chanFlushRes <-
              takeWorstFlushResult
                . fmap exportResultToFlushResult
                <$> forceFlushOutChan outChan []

            exporterFlushRes <- logRecordExporterForceFlush

            pure $ takeWorseFlushResult exporterFlushRes chanFlushRes
        )
          `catch` \(SomeException _) -> pure FlushError

  pure $
    LogRecordProcessor
      { logRecordProcessorOnEmit = \lr _ -> writeChan inChan lr
      , logRecordProcessorShutdown = mask $ \restore -> do
          cancel exportWorker
          flushResult <- restore logRecordProcessorForceFlush

          shutdownResult <- logRecordExporterShutdown

          pure $ takeWorseShutdownResult shutdownResult $ flushResultToShutdownResult flushResult
      , logRecordProcessorForceFlush
      }
  where
    forceFlushOutChan outChan acc = do
      (Element m, _) <- tryReadChan outChan
      mlr <- m
      case mlr of
        Nothing -> pure acc
        Just lr -> do
          res <- exportSingleLogRecord lr
          forceFlushOutChan outChan (res : acc)

    exportSingleLogRecord = logRecordExporterExport . (H.singleton <$> readLogRecordInstrumentationScope <*> V.singleton)
