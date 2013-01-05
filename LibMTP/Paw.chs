{-# LANGUAGE ForeignFunctionInterface #-}

#include <libmtp.h>

module LibMTP.Paw where

import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Control.Monad
import Control.Applicative
import Data.Functor

{#enum LIBMTP_error_number_t as LibmtpErrorNumber {underscoreToCase} deriving (Show, Eq) #}

data DeviceEntry = DeviceEntry { deVendor :: String
                               , deVendorId :: Word16
                               , deProduct :: String
                               , deProductId :: Word16
                               , deDeviceFlags :: Word32
                               , deBusLocation :: Word32
                               , deDevnum :: Word8
                               , deHandle :: DeviceEntryHandle
                               } deriving (Show)

data DeviceStorage = DeviceStorage { dsId :: Word32
                                   , dsStorageType :: Word16
                                   , dsFilesystemType :: Word16
                                   , dsAccessCapability :: Word16
                                   , dsMaxCapacity :: Int
                                   , dsFreeSpaceInBytes :: Int
                                   , dsFreeSpaceInObjects :: Int
                                   , dsDescription :: String
                                   , dsVolumeIdentifier :: String
                                   , dsHandle :: DeviceStorageHandle
                                   } deriving (Show)

type LibmtpError = (LibmtpErrorNumber, String)
newtype DeviceEntryHandle   = DeviceEntryHandle { unDEH :: (Ptr (DeviceEntryHandle)) }
                              deriving (Show)
newtype DeviceStorageHandle = DeviceStorageHandle (Ptr (DeviceStorageHandle))
                              deriving (Show)

{#pointer *LIBMTP_raw_device_t    as DeviceEntryHandlePtr -> DeviceEntryHandle  #}
{#pointer *LIBMTP_mtpdevice_t     as DeviceHandle newtype #}
{#pointer *LIBMTP_devicestorage_t as DeviceStorageHandlePtr -> DeviceStorageHandle  #}

libmtpInit :: IO ()
libmtpInit = {#call LIBMTP_Init as ^ #}

peekCString_ :: CString -> IO String
peekCString_ ptr = if ptr == nullPtr then return "" else peekCString ptr

cFromRawDevice :: DeviceEntryHandlePtr -> IO DeviceEntry
cFromRawDevice ptr = DeviceEntry
      <$> (peekCString_ =<< {#get LIBMTP_raw_device_t->device_entry.vendor#} ptr)
      <*> (fromIntegral <$> {#get LIBMTP_raw_device_t->device_entry.vendor_id#} ptr)
      <*> (peekCString_ =<< {#get LIBMTP_raw_device_t->device_entry.product#} ptr)
      <*> (fromIntegral <$> {#get LIBMTP_raw_device_t->device_entry.product_id#} ptr)
      <*> (fromIntegral <$> {#get LIBMTP_raw_device_t->device_entry.device_flags#} ptr)
      <*> (fromIntegral <$> {#get LIBMTP_raw_device_t->bus_location#} ptr)
      <*> (fromIntegral <$> {#get LIBMTP_raw_device_t->devnum#} ptr)
      <*> return (DeviceEntryHandle ptr)

detectRawDevices :: IO (Either LibmtpErrorNumber [DeviceEntry])
detectRawDevices = do
    p <- mallocForeignPtr
    r <- mallocForeignPtr
    withForeignPtr p $ \p' -> do
      withForeignPtr r $ \r' -> do
        err <- toEnum . fromIntegral <$> {#call LIBMTP_Detect_Raw_Devices as ^ #} p' r'
        case err of
          LibmtpErrorNone -> do
            cnt <- fromIntegral <$> peek r'
            p'' <- peek p'
            l <- forM [0..cnt-1] $ \i -> cFromRawDevice $ p'' `plusPtr` (i*{#sizeof LIBMTP_raw_device_t#})
            return (Right l)
          _ -> return (Left err)

openRawDevice :: DeviceEntry -> IO DeviceHandle
openRawDevice = {#call LIBMTP_Open_Raw_Device as ^ #} . unDEH . deHandle

releaseRawDevice :: DeviceHandle -> IO ()
releaseRawDevice = {#call LIBMTP_Release_Device as ^ #}

withDevice :: DeviceEntry -> (DeviceHandle -> IO a) -> IO a
withDevice d f = do
  h <- openRawDevice d
  r <- f h
  releaseRawDevice h
  return r

lastErrors :: DeviceHandle -> IO [LibmtpError]
lastErrors h = do
    start <- {#call LIBMTP_Get_Errorstack as ^ #} h
    res <- toErrList start
    {#call LIBMTP_Clear_Errorstack as ^ #} h
    return res
  where
    toErrList lp
      | lp == nullPtr = return []
      | otherwise     = do next <- toErrList =<< {#get LIBMTP_error_t->next#} lp
                           eid  <- toEnum . fromIntegral <$> {#get LIBMTP_error_t->errornumber#} lp
                           txt  <- peekCString_ =<< {#get LIBMTP_error_t->error_text#} lp
                           return $ (eid, txt) : next

getStorage :: DeviceHandle -> IO (Either [LibmtpError] [DeviceStorage])
getStorage h@(DeviceHandle hptr) = do
    err <- {#call LIBMTP_Get_Storage as ^ #} h 0
    if err /= 0
      then Left <$> lastErrors h
      else do
        start <- {#get LIBMTP_mtpdevice_t->storage#} hptr
        Right <$> toStorageList start
  where
    toStorageList lp
      | lp == nullPtr = return []
      | otherwise = do
          next <- toStorageList =<< {#get LIBMTP_devicestorage_t->next#} lp
          item <- DeviceStorage
                      <$> (fromIntegral <$> {#get LIBMTP_devicestorage_t->id#} lp)
                      <*> (fromIntegral <$> {#get LIBMTP_devicestorage_t->StorageType#} lp)
                      <*> (fromIntegral <$> {#get LIBMTP_devicestorage_t->FilesystemType#} lp)
                      <*> (fromIntegral <$> {#get LIBMTP_devicestorage_t->AccessCapability#} lp)
                      <*> (fromIntegral <$> {#get LIBMTP_devicestorage_t->MaxCapacity#} lp)
                      <*> (fromIntegral <$> {#get LIBMTP_devicestorage_t->FreeSpaceInBytes#} lp)
                      <*> (fromIntegral <$> {#get LIBMTP_devicestorage_t->FreeSpaceInObjects#} lp)
                      <*> (peekCString_ =<< {#get LIBMTP_devicestorage_t->StorageDescription#} lp)
                      <*> (peekCString_ =<< {#get LIBMTP_devicestorage_t->VolumeIdentifier#} lp)
                      <*> return (DeviceStorageHandle lp)
          return (item : next)
