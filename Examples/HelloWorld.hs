{-#LANGUAGE ScopedTypeVariables#-}
module Main where

import Bindings.DC1394
import CV.ColourUtils
import CV.Conversions
import CV.Image
import Control.Applicative
import Control.Exception
import Data.Array.CArray
import Data.Array.IArray
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.Tuple
import qualified Data.ByteString.Char8 as B



getIds camList = peekArray (fromIntegral $ c'dc1394camera_list_t'num camList)
                           (c'dc1394camera_list_t'ids camList)

getCameras dc = alloca $ \list -> bracket 
            (c'dc1394_camera_enumerate dc list)
            (\_ -> peek list >>= c'dc1394_camera_free_list)
            (\_ -> peek list >>= peek >>= getIds) -- TODO: Errors

getFrame camera = alloca $ \framePtr -> do
    c'dc1394_capture_dequeue camera  c'DC1394_CAPTURE_POLICY_WAIT framePtr
    dataPtr <- c'dc1394video_frame_t'image <$> (peek framePtr >>= peek)
    return $ unsafe8UC3FromPtr (640,480) dataPtr

main = do
    dc <- c'dc1394_new 
    (e:_) <- getCameras dc
    print e
    let guid = c'dc1394camera_id_t'guid e
    print ("Trying camera", guid)
    camera <- c'dc1394_camera_new dc guid
    print camera
    c'dc1394_video_set_iso_speed camera c'DC1394_ISO_SPEED_400 
    c'dc1394_video_set_mode camera c'DC1394_VIDEO_MODE_640x480_RGB8
    c'dc1394_video_set_framerate camera  c'DC1394_FRAMERATE_7_5
    c'dc1394_capture_setup camera 4  c'DC1394_CAPTURE_FLAGS_DEFAULT
    c'dc1394_video_set_transmission camera c'DC1394_ON


    getFrame camera >>= saveImage "testShot2-1.png"
    getFrame camera >>= saveImage "testShot2-2.png"
    getFrame camera >>= saveImage "testShot2-3.png"

    --peekArray (3*640*480) dataPtr >>= print
--    bs <- B.packCStringLen (dataPtr,640*480*3)
--    B.writeFile "test.ppm" $ B.pack "P6\n640 480\n255\n"
--    B.appendFile "test.ppm" bs
--    ca :: CArray (Int,Int) (CChar,CChar, CChar) 
--        <- createCArray ((0,0),(639,479)) 
--            (\ptr -> c'memcpy (castPtr dataPtr) (castPtr ptr) 
--                              (fromIntegral $ 640*480*sizeOf (0::CChar,0::CChar, 0::CChar)))
--    print $ toList ca
--    saveImage "testShot.png" $ stretchHistogram $ copyFCArrayToImage $ amap (\(r,g,b) -> fromIntegral (r+g+b) / 3) ca
--
    w <- malloc
    h <- malloc
    c'dc1394_get_image_size_from_video_mode camera c'DC1394_VIDEO_MODE_640x480_RGB8 w h
    print "Size"
    peek w >>= print 
    peek h >>= print 
    print "eSize"


    c'dc1394_video_set_transmission camera c'DC1394_OFF
    c'dc1394_capture_stop camera
    c'dc1394_camera_free camera
    c'dc1394_free dc


    

