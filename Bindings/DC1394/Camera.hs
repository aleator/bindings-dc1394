{-#LANGUAGE ScopedTypeVariables#-}

module Bindings.DC1394.Camera where

import Bindings.DC1394
import Bindings.DC1394.Types
import Control.Exception
import Control.Monad
import Data.Bits
import Foreign.C.Types
import Foreign.Concurrent
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import GHC.Word
import System.IO.Unsafe

-- | wrapper around dc1394 bindings that sets up a firewire
--   camera, performs the user action with the camera, then
--   stops the transmission and stops the camera. This wrapper
--   calls 'setVideMode' 'setFrameRate' 'setupCamera' and 'startVideoTransmission'
--   before executing the IO action, then the firewire resource
--   is closed with 'stopVideoTransmission' and 'stopCapture'.
withFirewireCamera :: ISOSpeed         -- ^ ISO rating for light sensitivityo
           -> VideoMode        -- ^ video mode
           -> Framerate        -- ^ capture frame rate
           -> CInt             -- ^ DMA buffer size
           -> CaptureFlag      -- ^ capture flags
           -> (Camera -> IO a) -- ^ IO action using a camera
           -> IO ()
withFirewireCamera speed mode rate dmaBufferSize captureFlags action = do
    -- use dc1394 bindings to set up camera
    dc <- getDC1394 --c'dc1394_new 
    xs <- getCameras dc
    when (null xs) (error "withFirewireCamera: unable to find camera")
    let e = head xs
    cam <- cameraFromID dc e -- c'dc1394_camera_new dc guid
    setISOSpeed  cam speed
    setVideoMode cam mode
    setFrameRate cam rate
    setupCamera
      cam           -- camera
      dmaBufferSize -- DMA buffers
      captureFlags  -- capture flag
    startVideoTransmission cam

    -- perform user action with the camera
    void (action cam)

    -- use dc1394 bindings to stop the camera
    stopVideoTransmission cam
    stopCapture cam

getMode :: Ptr C'dc1394camera_t -> IO CInt
getMode camera = alloca $ \(mode :: Ptr CInt) -> do 
                    void (c'dc1394_video_get_mode camera mode)
                    peek mode

-- | Join flags
(&+) :: CaptureFlag -> CaptureFlag -> CaptureFlag
CF a &+ CF b = CF (a .&. b)

-- | Set ISO speed
setISOSpeed :: Camera -> ISOSpeed -> IO ()
setISOSpeed c iso = withCameraPtr c $ \camera -> 
    checking $ c'dc1394_video_set_iso_speed camera (fromISO iso)

-- | Set the video mode
setVideoMode :: Camera -> VideoMode -> IO ()
setVideoMode c mode = withCameraPtr c $ \camera -> 
    checking $ c'dc1394_video_set_mode camera (toVideoMode mode)

-- | Set the frame rate
setFrameRate :: Camera -> Framerate -> IO ()
setFrameRate c rate = withCameraPtr c $ \camera -> 
    checking $ c'dc1394_video_set_framerate camera  (toFramerate rate)

-- | Setup the camera for capturing
setupCamera :: Camera -> CInt -> CaptureFlag -> IO ()
setupCamera c dmaBuffers cf = withCameraPtr c $ \camera -> 
                               checking $ c'dc1394_capture_setup camera 
                                                                 (fromIntegral dmaBuffers) (fromCF cf)

-- | Execute a libdc1394 function and check for the error code. Currently raises the error as 
--   UserError, but in future might provide a more reasonable error hierarchy.
checking :: IO CInt -> IO ()
checking op = do 
    r <- toResult <$>op
    case r of
        SUCCESS -> return ()
        e   -> error (show e)

itob :: forall a. (Eq a, Num a) => a -> Bool
itob 0 = False
itob _ = True

getIds :: C'dc1394camera_list_t -> IO [C'dc1394camera_id_t]
getIds camList = peekArray (fromIntegral $ c'dc1394camera_list_t'num camList)
                           (c'dc1394camera_list_t'ids camList)

-- | Get list of available cameras
getCameras :: DC1394 -> IO [CameraId]
getCameras dc = 
            withDC1394 dc $ \c_dc ->
            alloca $ \list        -> bracket 
               (checking $ c'dc1394_camera_enumerate c_dc list)
               (\_ -> peek list >>= c'dc1394_camera_free_list)
               (\_ -> peek list >>= peek >>= getIds) 

sizeFromMode :: Ptr C'dc1394camera_t -> CInt -> IO (Word32, Word32)
sizeFromMode camera mode = alloca $ \w -> alloca $ \h -> do
    void (c'dc1394_get_image_size_from_video_mode camera mode w h)
    (,) <$> peek w <*> peek h 

-- | Create Camera from ID. Although the camera type is memory managed, the user is required
-- to stop data transfer and reset appropriate settings. Finalizers are not quaranteed to run.
cameraFromID :: DC1394 -> C'dc1394camera_id_t -> IO Camera
cameraFromID dc e = do
    let guid = c'dc1394camera_id_t'guid e
    camera <- withDC1394 dc $ \c_dc -> c'dc1394_camera_new c_dc guid
    when (camera==nullPtr) $ error "Could not create camera"
    Camera <$> newForeignPtr camera (c'dc1394_camera_free camera)
    -- #TODO What else should be cleaned up?

withCameraPtr :: Camera -> (Ptr C'dc1394camera_t -> IO b) -> IO b
withCameraPtr     (Camera fptr)    = withForeignPtr fptr

withCameraPtrPeek :: Camera -> (C'dc1394camera_t -> IO b) -> IO b
withCameraPtrPeek (Camera fptr) op = withForeignPtr fptr (peek >=> op)

-- | Create a new DC1394 context
getDC1394 :: IO DC1394
getDC1394 = do
    dc <- c'dc1394_new 
    when (dc==nullPtr) $ error "Could not get dc1394 context"
    DC1394 <$> newForeignPtr dc (c'dc1394_free dc)

withDC1394 :: DC1394 -> (Ptr C'dc1394_t -> IO b) -> IO b
withDC1394 (DC1394 fptr) = withForeignPtr fptr

-- | Set the video transmission on
startVideoTransmission :: Camera -> IO ()
startVideoTransmission c = withCameraPtr c $ \camera -> checking $ c'dc1394_video_set_transmission camera c'DC1394_ON

-- | Set the video transmission off
stopVideoTransmission :: Camera -> IO ()
stopVideoTransmission c = withCameraPtr c $ \camera -> checking $ c'dc1394_video_set_transmission camera c'DC1394_OFF

-- | Stop capturing
stopCapture :: Camera -> IO ()
stopCapture c = withCameraPtr c $ \camera -> checking $ c'dc1394_capture_stop camera

-- | Does the camera have one-shot functionality?
oneShotCapable :: Camera -> Bool
oneShotCapable camera = unsafePerformIO $ withCameraPtrPeek camera (return . itob . c'dc1394camera_t'one_shot_capable)
