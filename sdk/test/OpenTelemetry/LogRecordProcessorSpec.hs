{-# LANGUAGE NamedFieldPuns #-}

module OpenTelemetry.LogRecordProcessorSpec where

import Data.IORef
import qualified Data.Vector as V
import OpenTelemetry.Internal.Common.Types
import OpenTelemetry.Logs.Core
import Test.Hspec


getTestExporter :: IO (IORef Int, LogRecordExporter body)
getTestExporter = do
  numExportsRef <- newIORef 0
  shutdownRef <- newIORef False

  let logRecordExporterExport logRecordsByLibrary = do
        shutdown <- readIORef shutdownRef
        if shutdown
          then pure (Failure Nothing)
          else do
            let numLogRecords = foldr (\lrs n -> n + V.length lrs) 0 logRecordsByLibrary
            modifyIORef numExportsRef (+ numLogRecords)

            pure Success

      logRecordExporterForceFlush = pure FlushSuccess

      logRecordExporterShutdown = pure ShutdownSuccess

  pure
    ( numExportsRef
    , LogRecordExporter
        { logRecordExporterExport
        , logRecordExporterForceFlush
        , logRecordExporterShutdown
        }
    )


spec :: Spec
spec = describe "LogRecordProcessor" $ do
  describe "Simple Processor" $ do
    it "Sends LogRecords to the Exporter" $ do
      (numExportsRef, testExporter) <- getTestExporter

      let lp = createLoggerProvider [testExporter] emptyLoggerProviderOptions
          l = makeLogger lp

      pending

    it "Force flushes correctly" $ do
      pending
    it "Shuts down correctly" $ do
      pending
