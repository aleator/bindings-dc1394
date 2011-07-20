{-#LANGUAGE ScopedTypeVariables#-}
module Main where

import Bindings.DC1394
import CV.ColourUtils
import CV.Conversions
import CV.Image
import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Array.CArray
import Data.Array.IArray
import Data.Bits
import Foreign.C.Types
import Foreign.Concurrent
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.Tuple
import System.IO.Unsafe
import qualified Data.ByteString.Char8 as B



getIds camList = peekArray (fromIntegral $ c'dc1394camera_list_t'num camList)
                           (c'dc1394camera_list_t'ids camList)

getCameras dc = alloca $ \list -> bracket 
            (c'dc1394_camera_enumerate dc list)
            (\_ -> peek list >>= c'dc1394_camera_free_list)
            (\_ -> peek list >>= peek >>= getIds) -- TODO: Errors

getFrame camera' = alloca $ \framePtr ->
                    withCameraPtr camera' $ \camera -> do
    c'dc1394_capture_dequeue camera  c'DC1394_CAPTURE_POLICY_WAIT framePtr
    dataPtr <- c'dc1394video_frame_t'image <$> (peek framePtr >>= peek)
    return $ unsafe8UC3FromPtr (640,480) dataPtr

sizeFromMode camera mode = alloca $ \w -> alloca $ \h -> do
    c'dc1394_get_image_size_from_video_mode camera mode w h
    (,) <$> peek w <*> peek h 


data Camera = Camera (ForeignPtr C'dc1394camera_t)

-- | Create Camera from ID. Although the camera type is memory managed, the user is required
-- to stop data transfer and reset appropriate settings. Finalizers are not quaranteed to run.
cameraFromID dc e = do
    let guid = c'dc1394camera_id_t'guid e
    camera <- withDC1394 dc $ \c_dc -> c'dc1394_camera_new c_dc guid
    when (camera==nullPtr) $ error "Could not create camera"
    Camera <$> newForeignPtr camera (c'dc1394_camera_free camera)
    -- #TODO What else should be cleaned up?

withCameraPtr (Camera fptr) op = withForeignPtr fptr op
withCamera    (Camera fptr) op = withForeignPtr fptr (\ptr -> peek ptr >>= op)

newtype DC1394 = DC1394 (ForeignPtr C'dc1394_t)

getDC1394 = do
    dc <- c'dc1394_new 
    when (dc==nullPtr) $ error "Could not get dc1394 context"
    DC1394 <$> newForeignPtr dc (c'dc1394_free dc)

withDC1394 (DC1394 fptr) op = withForeignPtr fptr op

-- | Set the video transmission on
startVideoTransmission c = withCameraPtr c $ \camera -> checking $ c'dc1394_video_set_transmission camera c'DC1394_ON

-- | Set the video transmission off
stopVideoTransmission c = withCameraPtr c $ \camera -> checking $ c'dc1394_video_set_transmission camera c'DC1394_OFF

-- | Stop capturing
stopCapture c = withCameraPtr c $ \camera -> checking $ c'dc1394_capture_stop camera

-- | Does the camera have one-shot functionality?
oneShotCapable camera = unsafePerformIO $ withCamera camera (return . itob . c'dc1394camera_t'one_shot_capable)

newtype CaptureFlag = CF CInt
channelAlloc   = CF c'DC1394_CAPTURE_FLAGS_CHANNEL_ALLOC   
candwidthAlloc = CF c'DC1394_CAPTURE_FLAGS_BANDWIDTH_ALLOC 
defaultFlags   = CF c'DC1394_CAPTURE_FLAGS_DEFAULT         
autoISO        = CF c'DC1394_CAPTURE_FLAGS_AUTO_ISO        
fromCF (CF a) = a

-- | Join flags
CF a &+ CF b = CF (a .&. b)

-- | Set ISO speed
setISOSpeed c iso = withCameraPtr c $ \camera -> 
    checking $ c'dc1394_video_set_iso_speed camera (fromISO iso)

setVideoMode c mode = withCameraPtr c $ \camera -> 
    checking $ c'dc1394_video_set_mode camera (toVideoMode mode)

setFrameRate c rate = withCameraPtr c $ \camera -> 
    checking $ c'dc1394_video_set_framerate camera  (toFramerate rate)

-- | Setup the camera for capturing
setupCamera :: Camera -> Foreign.C.Types.CInt -> CaptureFlag -> IO ()
setupCamera c dmaBuffers cf = withCameraPtr c $ \camera -> 
                               checking $ c'dc1394_capture_setup camera dmaBuffers (fromCF cf)

checking op = do 
    r <- toResult <$>op
    case r of
        SUCCESS -> return ()
        e   -> error (show e)


itob 0 = False
itob _ = True

main = do
    dc <- getDC1394 --c'dc1394_new 
    (e:_) <- withDC1394 dc getCameras 
    print e
    print ("Trying camera", e)
    cam <- cameraFromID dc e-- c'dc1394_camera_new dc guid
    print ("Camera can do oneshots", oneShotCapable cam)
    setISOSpeed  cam ISO_400
    setVideoMode cam Mode_640x480_RGB8
    setFrameRate cam Rate_3_75
    setupCamera cam 4 (defaultFlags)
    startVideoTransmission cam
    
    
    getFrame cam >>= saveImage "testShot2-1.png"
    getFrame cam >>= saveImage "testShot2-2.png"
    getFrame cam >>= saveImage "testShot2-3.png"
    
    
    stopVideoTransmission cam
    stopCapture cam


data ISOSpeed = 
      ISO_100
    | ISO_200
    | ISO_400
    | ISO_800
    | ISO_1600
    | ISO_3200

fromISO ISO_100  = c'DC1394_ISO_SPEED_100 
fromISO ISO_200  = c'DC1394_ISO_SPEED_200
fromISO ISO_400  = c'DC1394_ISO_SPEED_400
fromISO ISO_800  = c'DC1394_ISO_SPEED_800
fromISO ISO_1600 = c'DC1394_ISO_SPEED_1600
fromISO ISO_3200 = c'DC1394_ISO_SPEED_3200
toISO i 
    | i == c'DC1394_ISO_SPEED_100  = ISO_100   
    | i == c'DC1394_ISO_SPEED_200  = ISO_200   
    | i == c'DC1394_ISO_SPEED_400  = ISO_400   
    | i == c'DC1394_ISO_SPEED_800  = ISO_800   
    | i == c'DC1394_ISO_SPEED_1600 = ISO_1600  
    | i == c'DC1394_ISO_SPEED_3200 = ISO_3200  
    

data DCResult = 
    SUCCESS                     
  | FAILURE                     
  | NOT_A_CAMERA                
  | FUNCTION_NOT_SUPPORTED      
  | CAMERA_NOT_INITIALIZED      
  | MEMORY_ALLOCATION_FAILURE   
  | TAGGED_REGISTER_NOT_FOUND   
  | NO_ISO_CHANNEL              
  | NO_BANDWIDTH                
  | IOCTL_FAILURE               
  | CAPTURE_IS_NOT_SET          
  | CAPTURE_IS_RUNNING          
  | RAW1394_FAILURE             
  | FORMAT7_ERROR_FLAG_1        
  | FORMAT7_ERROR_FLAG_2        
  | INVALID_ARGUMENT_VALUE      
  | REQ_VALUE_OUTSIDE_RANGE     
  | INVALID_FEATURE             
  | INVALID_VIDEO_FORMAT        
  | INVALID_VIDEO_MODE          
  | INVALID_FRAMERATE           
  | INVALID_TRIGGER_MODE        
  | INVALID_TRIGGER_SOURCE      
  | INVALID_ISO_SPEED           
  | INVALID_IIDC_VERSION        
  | INVALID_COLOR_CODING        
  | INVALID_COLOR_FILTER        
  | INVALID_CAPTURE_POLICY      
  | INVALID_ERROR_CODE          
  | INVALID_BAYER_METHOD        
  | INVALID_VIDEO1394_DEVICE    
  | INVALID_OPERATION_MODE      
  | INVALID_TRIGGER_POLARITY    
  | INVALID_FEATURE_MODE        
  | INVALID_LOG_TYPE            
  | INVALID_BYTE_ORDER          
  | INVALID_STEREO_METHOD       
  | BASLER_NO_MORE_SFF_CHUNKS   
  | BASLER_CORRUPTED_SFF_CHUNK  
  | BASLER_UNKNOWN_SFF_CHUNK  
  deriving (Show)

fromResult SUCCESS                    = c'DC1394_SUCCESS                     
fromResult FAILURE                    = c'DC1394_FAILURE                     
fromResult NOT_A_CAMERA               = c'DC1394_NOT_A_CAMERA                
fromResult FUNCTION_NOT_SUPPORTED     = c'DC1394_FUNCTION_NOT_SUPPORTED      
fromResult CAMERA_NOT_INITIALIZED     = c'DC1394_CAMERA_NOT_INITIALIZED      
fromResult MEMORY_ALLOCATION_FAILURE  = c'DC1394_MEMORY_ALLOCATION_FAILURE   
fromResult TAGGED_REGISTER_NOT_FOUND  = c'DC1394_TAGGED_REGISTER_NOT_FOUND   
fromResult NO_ISO_CHANNEL             = c'DC1394_NO_ISO_CHANNEL              
fromResult NO_BANDWIDTH               = c'DC1394_NO_BANDWIDTH                
fromResult IOCTL_FAILURE              = c'DC1394_IOCTL_FAILURE               
fromResult CAPTURE_IS_NOT_SET         = c'DC1394_CAPTURE_IS_NOT_SET          
fromResult CAPTURE_IS_RUNNING         = c'DC1394_CAPTURE_IS_RUNNING          
fromResult RAW1394_FAILURE            = c'DC1394_RAW1394_FAILURE             
fromResult FORMAT7_ERROR_FLAG_1       = c'DC1394_FORMAT7_ERROR_FLAG_1        
fromResult FORMAT7_ERROR_FLAG_2       = c'DC1394_FORMAT7_ERROR_FLAG_2        
fromResult INVALID_ARGUMENT_VALUE     = c'DC1394_INVALID_ARGUMENT_VALUE      
fromResult REQ_VALUE_OUTSIDE_RANGE    = c'DC1394_REQ_VALUE_OUTSIDE_RANGE     
fromResult INVALID_FEATURE            = c'DC1394_INVALID_FEATURE             
fromResult INVALID_VIDEO_FORMAT       = c'DC1394_INVALID_VIDEO_FORMAT        
fromResult INVALID_VIDEO_MODE         = c'DC1394_INVALID_VIDEO_MODE          
fromResult INVALID_FRAMERATE          = c'DC1394_INVALID_FRAMERATE           
fromResult INVALID_TRIGGER_MODE       = c'DC1394_INVALID_TRIGGER_MODE        
fromResult INVALID_TRIGGER_SOURCE     = c'DC1394_INVALID_TRIGGER_SOURCE      
fromResult INVALID_ISO_SPEED          = c'DC1394_INVALID_ISO_SPEED           
fromResult INVALID_IIDC_VERSION       = c'DC1394_INVALID_IIDC_VERSION        
fromResult INVALID_COLOR_CODING       = c'DC1394_INVALID_COLOR_CODING        
fromResult INVALID_COLOR_FILTER       = c'DC1394_INVALID_COLOR_FILTER        
fromResult INVALID_CAPTURE_POLICY     = c'DC1394_INVALID_CAPTURE_POLICY      
fromResult INVALID_ERROR_CODE         = c'DC1394_INVALID_ERROR_CODE          
fromResult INVALID_BAYER_METHOD       = c'DC1394_INVALID_BAYER_METHOD        
fromResult INVALID_VIDEO1394_DEVICE   = c'DC1394_INVALID_VIDEO1394_DEVICE    
fromResult INVALID_OPERATION_MODE     = c'DC1394_INVALID_OPERATION_MODE      
fromResult INVALID_TRIGGER_POLARITY   = c'DC1394_INVALID_TRIGGER_POLARITY    
fromResult INVALID_FEATURE_MODE       = c'DC1394_INVALID_FEATURE_MODE        
fromResult INVALID_LOG_TYPE           = c'DC1394_INVALID_LOG_TYPE            
fromResult INVALID_BYTE_ORDER         = c'DC1394_INVALID_BYTE_ORDER          
fromResult INVALID_STEREO_METHOD      = c'DC1394_INVALID_STEREO_METHOD       
fromResult BASLER_NO_MORE_SFF_CHUNKS  = c'DC1394_BASLER_NO_MORE_SFF_CHUNKS   
fromResult BASLER_CORRUPTED_SFF_CHUNK = c'DC1394_BASLER_CORRUPTED_SFF_CHUNK  
fromResult BASLER_UNKNOWN_SFF_CHUNK   = c'DC1394_BASLER_UNKNOWN_SFF_CHUNK  

toResult r 
   | r == c'DC1394_SUCCESS                    = SUCCESS                     
   | r == c'DC1394_FAILURE                    = FAILURE                     
   | r == c'DC1394_NOT_A_CAMERA               = NOT_A_CAMERA                
   | r == c'DC1394_FUNCTION_NOT_SUPPORTED     = FUNCTION_NOT_SUPPORTED      
   | r == c'DC1394_CAMERA_NOT_INITIALIZED     = CAMERA_NOT_INITIALIZED      
   | r == c'DC1394_MEMORY_ALLOCATION_FAILURE  = MEMORY_ALLOCATION_FAILURE   
   | r == c'DC1394_TAGGED_REGISTER_NOT_FOUND  = TAGGED_REGISTER_NOT_FOUND   
   | r == c'DC1394_NO_ISO_CHANNEL             = NO_ISO_CHANNEL              
   | r == c'DC1394_NO_BANDWIDTH               = NO_BANDWIDTH                
   | r == c'DC1394_IOCTL_FAILURE              = IOCTL_FAILURE               
   | r == c'DC1394_CAPTURE_IS_NOT_SET         = CAPTURE_IS_NOT_SET          
   | r == c'DC1394_CAPTURE_IS_RUNNING         = CAPTURE_IS_RUNNING          
   | r == c'DC1394_RAW1394_FAILURE            = RAW1394_FAILURE             
   | r == c'DC1394_FORMAT7_ERROR_FLAG_1       = FORMAT7_ERROR_FLAG_1        
   | r == c'DC1394_FORMAT7_ERROR_FLAG_2       = FORMAT7_ERROR_FLAG_2        
   | r == c'DC1394_INVALID_ARGUMENT_VALUE     = INVALID_ARGUMENT_VALUE      
   | r == c'DC1394_REQ_VALUE_OUTSIDE_RANGE    = REQ_VALUE_OUTSIDE_RANGE     
   | r == c'DC1394_INVALID_FEATURE            = INVALID_FEATURE             
   | r == c'DC1394_INVALID_VIDEO_FORMAT       = INVALID_VIDEO_FORMAT        
   | r == c'DC1394_INVALID_VIDEO_MODE         = INVALID_VIDEO_MODE          
   | r == c'DC1394_INVALID_FRAMERATE          = INVALID_FRAMERATE           
   | r == c'DC1394_INVALID_TRIGGER_MODE       = INVALID_TRIGGER_MODE        
   | r == c'DC1394_INVALID_TRIGGER_SOURCE     = INVALID_TRIGGER_SOURCE      
   | r == c'DC1394_INVALID_ISO_SPEED          = INVALID_ISO_SPEED           
   | r == c'DC1394_INVALID_IIDC_VERSION       = INVALID_IIDC_VERSION        
   | r == c'DC1394_INVALID_COLOR_CODING       = INVALID_COLOR_CODING        
   | r == c'DC1394_INVALID_COLOR_FILTER       = INVALID_COLOR_FILTER        
   | r == c'DC1394_INVALID_CAPTURE_POLICY     = INVALID_CAPTURE_POLICY      
   | r == c'DC1394_INVALID_ERROR_CODE         = INVALID_ERROR_CODE          
   | r == c'DC1394_INVALID_BAYER_METHOD       = INVALID_BAYER_METHOD        
   | r == c'DC1394_INVALID_VIDEO1394_DEVICE   = INVALID_VIDEO1394_DEVICE    
   | r == c'DC1394_INVALID_OPERATION_MODE     = INVALID_OPERATION_MODE      
   | r == c'DC1394_INVALID_TRIGGER_POLARITY   = INVALID_TRIGGER_POLARITY    
   | r == c'DC1394_INVALID_FEATURE_MODE       = INVALID_FEATURE_MODE        
   | r == c'DC1394_INVALID_LOG_TYPE           = INVALID_LOG_TYPE            
   | r == c'DC1394_INVALID_BYTE_ORDER         = INVALID_BYTE_ORDER          
   | r == c'DC1394_INVALID_STEREO_METHOD      = INVALID_STEREO_METHOD       
   | r == c'DC1394_BASLER_NO_MORE_SFF_CHUNKS  = BASLER_NO_MORE_SFF_CHUNKS   
   | r == c'DC1394_BASLER_CORRUPTED_SFF_CHUNK = BASLER_CORRUPTED_SFF_CHUNK  
   | r == c'DC1394_BASLER_UNKNOWN_SFF_CHUNK   = BASLER_UNKNOWN_SFF_CHUNK  

data VideoMode = Mode_160x120_YUV444 
     | Mode_320x240_YUV422  
     | Mode_640x480_YUV411  
     | Mode_640x480_YUV422  
     | Mode_640x480_RGB8  
     | Mode_640x480_MONO8  
     | Mode_640x480_MONO16  
     | Mode_800x600_YUV422  
     | Mode_800x600_RGB8  
     | Mode_800x600_MONO8  
     | Mode_1024x768_YUV422  
     | Mode_1024x768_RGB8  
     | Mode_1024x768_MONO8  
     | Mode_800x600_MONO16  
     | Mode_1024x768_MONO16  
     | Mode_1280x960_YUV422  
     | Mode_1280x960_RGB8  
     | Mode_1280x960_MONO8  
     | Mode_1600x1200_YUV422  
     | Mode_1600x1200_RGB8  
     | Mode_1600x1200_MONO8  
     | Mode_1280x960_MONO16  
     | Mode_1600x1200_MONO16  
     | Mode_EXIF  
     | Mode_FORMAT7_0  
     | Mode_FORMAT7_1  
     | Mode_FORMAT7_2  
     | Mode_FORMAT7_3  
     | Mode_FORMAT7_4  
     | Mode_FORMAT7_5  
     | Mode_FORMAT7_6  
     | Mode_FORMAT7_7 
      deriving (Show,Eq)

fromVideoMode Mode_160x120_YUV444 = c'DC1394_VIDEO_MODE_160x120_YUV444  
fromVideoMode Mode_320x240_YUV422 = c'DC1394_VIDEO_MODE_320x240_YUV422  
fromVideoMode Mode_640x480_YUV411 = c'DC1394_VIDEO_MODE_640x480_YUV411  
fromVideoMode Mode_640x480_YUV422 = c'DC1394_VIDEO_MODE_640x480_YUV422  
fromVideoMode Mode_640x480_RGB8 = c'DC1394_VIDEO_MODE_640x480_RGB8  
fromVideoMode Mode_640x480_MONO8 = c'DC1394_VIDEO_MODE_640x480_MONO8  
fromVideoMode Mode_640x480_MONO16 = c'DC1394_VIDEO_MODE_640x480_MONO16  
fromVideoMode Mode_800x600_YUV422 = c'DC1394_VIDEO_MODE_800x600_YUV422  
fromVideoMode Mode_800x600_RGB8 = c'DC1394_VIDEO_MODE_800x600_RGB8  
fromVideoMode Mode_800x600_MONO8 = c'DC1394_VIDEO_MODE_800x600_MONO8  
fromVideoMode Mode_1024x768_YUV422 = c'DC1394_VIDEO_MODE_1024x768_YUV422  
fromVideoMode Mode_1024x768_RGB8 = c'DC1394_VIDEO_MODE_1024x768_RGB8  
fromVideoMode Mode_1024x768_MONO8 = c'DC1394_VIDEO_MODE_1024x768_MONO8  
fromVideoMode Mode_800x600_MONO16 = c'DC1394_VIDEO_MODE_800x600_MONO16  
fromVideoMode Mode_1024x768_MONO16 = c'DC1394_VIDEO_MODE_1024x768_MONO16  
fromVideoMode Mode_1280x960_YUV422 = c'DC1394_VIDEO_MODE_1280x960_YUV422  
fromVideoMode Mode_1280x960_RGB8 = c'DC1394_VIDEO_MODE_1280x960_RGB8  
fromVideoMode Mode_1280x960_MONO8 = c'DC1394_VIDEO_MODE_1280x960_MONO8  
fromVideoMode Mode_1600x1200_YUV422 = c'DC1394_VIDEO_MODE_1600x1200_YUV422  
fromVideoMode Mode_1600x1200_RGB8 = c'DC1394_VIDEO_MODE_1600x1200_RGB8  
fromVideoMode Mode_1600x1200_MONO8 = c'DC1394_VIDEO_MODE_1600x1200_MONO8  
fromVideoMode Mode_1280x960_MONO16 = c'DC1394_VIDEO_MODE_1280x960_MONO16  
fromVideoMode Mode_1600x1200_MONO16 = c'DC1394_VIDEO_MODE_1600x1200_MONO16  
fromVideoMode Mode_EXIF = c'DC1394_VIDEO_MODE_EXIF  
fromVideoMode Mode_FORMAT7_0 = c'DC1394_VIDEO_MODE_FORMAT7_0  
fromVideoMode Mode_FORMAT7_1 = c'DC1394_VIDEO_MODE_FORMAT7_1  
fromVideoMode Mode_FORMAT7_2 = c'DC1394_VIDEO_MODE_FORMAT7_2  
fromVideoMode Mode_FORMAT7_3 = c'DC1394_VIDEO_MODE_FORMAT7_3  
fromVideoMode Mode_FORMAT7_4 = c'DC1394_VIDEO_MODE_FORMAT7_4  
fromVideoMode Mode_FORMAT7_5 = c'DC1394_VIDEO_MODE_FORMAT7_5  
fromVideoMode Mode_FORMAT7_6 = c'DC1394_VIDEO_MODE_FORMAT7_6  
fromVideoMode Mode_FORMAT7_7 = c'DC1394_VIDEO_MODE_FORMAT7_7 

toVideoMode i 
  | i == Mode_160x120_YUV444 = c'DC1394_VIDEO_MODE_160x120_YUV444  
  | i == Mode_320x240_YUV422 = c'DC1394_VIDEO_MODE_320x240_YUV422  
  | i == Mode_640x480_YUV411 = c'DC1394_VIDEO_MODE_640x480_YUV411  
  | i == Mode_640x480_YUV422 = c'DC1394_VIDEO_MODE_640x480_YUV422  
  | i == Mode_640x480_RGB8 = c'DC1394_VIDEO_MODE_640x480_RGB8  
  | i == Mode_640x480_MONO8 = c'DC1394_VIDEO_MODE_640x480_MONO8  
  | i == Mode_640x480_MONO16 = c'DC1394_VIDEO_MODE_640x480_MONO16  
  | i == Mode_800x600_YUV422 = c'DC1394_VIDEO_MODE_800x600_YUV422  
  | i == Mode_800x600_RGB8 = c'DC1394_VIDEO_MODE_800x600_RGB8  
  | i == Mode_800x600_MONO8 = c'DC1394_VIDEO_MODE_800x600_MONO8  
  | i == Mode_1024x768_YUV422 = c'DC1394_VIDEO_MODE_1024x768_YUV422  
  | i == Mode_1024x768_RGB8 = c'DC1394_VIDEO_MODE_1024x768_RGB8  
  | i == Mode_1024x768_MONO8 = c'DC1394_VIDEO_MODE_1024x768_MONO8  
  | i == Mode_800x600_MONO16 = c'DC1394_VIDEO_MODE_800x600_MONO16  
  | i == Mode_1024x768_MONO16 = c'DC1394_VIDEO_MODE_1024x768_MONO16  
  | i == Mode_1280x960_YUV422 = c'DC1394_VIDEO_MODE_1280x960_YUV422  
  | i == Mode_1280x960_RGB8 = c'DC1394_VIDEO_MODE_1280x960_RGB8  
  | i == Mode_1280x960_MONO8 = c'DC1394_VIDEO_MODE_1280x960_MONO8  
  | i == Mode_1600x1200_YUV422 = c'DC1394_VIDEO_MODE_1600x1200_YUV422  
  | i == Mode_1600x1200_RGB8 = c'DC1394_VIDEO_MODE_1600x1200_RGB8  
  | i == Mode_1600x1200_MONO8 = c'DC1394_VIDEO_MODE_1600x1200_MONO8  
  | i == Mode_1280x960_MONO16 = c'DC1394_VIDEO_MODE_1280x960_MONO16  
  | i == Mode_1600x1200_MONO16 = c'DC1394_VIDEO_MODE_1600x1200_MONO16  
  | i == Mode_EXIF = c'DC1394_VIDEO_MODE_EXIF  
  | i == Mode_FORMAT7_0 = c'DC1394_VIDEO_MODE_FORMAT7_0  
  | i == Mode_FORMAT7_1 = c'DC1394_VIDEO_MODE_FORMAT7_1  
  | i == Mode_FORMAT7_2 = c'DC1394_VIDEO_MODE_FORMAT7_2  
  | i == Mode_FORMAT7_3 = c'DC1394_VIDEO_MODE_FORMAT7_3  
  | i == Mode_FORMAT7_4 = c'DC1394_VIDEO_MODE_FORMAT7_4  
  | i == Mode_FORMAT7_5 = c'DC1394_VIDEO_MODE_FORMAT7_5  
  | i == Mode_FORMAT7_6 = c'DC1394_VIDEO_MODE_FORMAT7_6  
  | i == Mode_FORMAT7_7 = c'DC1394_VIDEO_MODE_FORMAT7_7 

data Framerate = Rate_1_875 
     | Rate_3_75  
     | Rate_7_5  
     | Rate_15  
     | Rate_30  
     | Rate_60  
     | Rate_120  
     | Rate_240 
      deriving (Show,Eq)

fromFramerate Rate_1_875 = c'DC1394_FRAMERATE_1_875  
fromFramerate Rate_3_75 = c'DC1394_FRAMERATE_3_75  
fromFramerate Rate_7_5 = c'DC1394_FRAMERATE_7_5  
fromFramerate Rate_15 = c'DC1394_FRAMERATE_15  
fromFramerate Rate_30 = c'DC1394_FRAMERATE_30  
fromFramerate Rate_60 = c'DC1394_FRAMERATE_60  
fromFramerate Rate_120 = c'DC1394_FRAMERATE_120  
fromFramerate Rate_240 = c'DC1394_FRAMERATE_240 

toFramerate i 
  | i == Rate_1_875 = c'DC1394_FRAMERATE_1_875  
  | i == Rate_3_75 = c'DC1394_FRAMERATE_3_75  
  | i == Rate_7_5 = c'DC1394_FRAMERATE_7_5  
  | i == Rate_15 = c'DC1394_FRAMERATE_15  
  | i == Rate_30 = c'DC1394_FRAMERATE_30  
  | i == Rate_60 = c'DC1394_FRAMERATE_60  
  | i == Rate_120 = c'DC1394_FRAMERATE_120  
  | i == Rate_240 = c'DC1394_FRAMERATE_240 

