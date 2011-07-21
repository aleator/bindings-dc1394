{-# LANGUAGE ForeignFunctionInterface #-}

#include <bindings.dsl.h>
#include <dc1394/dc1394.h>

module Bindings.DC1394 where
import Data.Word
#strict_import

-- camera_id_t 
#starttype dc1394camera_id_t
#field unit , Word16
#field guid , Word64
#stoptype

-- camera_list_t 
#starttype dc1394camera_list_t
#field num , Word32
#field ids , Ptr <dc1394camera_id_t>
#stoptype

-- dc1394switch_t
#num DC1394_OFF
#num DC1394_ON

-- utils
#ccall memcpy, Ptr () -> Ptr () -> Word32 -> IO ()

-- video_frame_t
#starttype dc1394video_frame_t
#field image,                  Ptr Word8 

#field size,                   Ptr Word32                 
#field position,               Ptr Word32                  
#field color_coding,           CInt
#field color_filter,           CInt
#field yuv_byte_order,         Word32                 
#field data_depth,             Word32                 

#field stride,                 Word32                 
#field video_mode,             CInt
#field total_bytes,            Word64                 

#field image_bytes,            Word32                 
#field padding_bytes,          Word32                 
#field packet_size,            Word32                 
#field packets_per_frame,      Word32                 
#field timestamp,              Word64                 

#field frames_behind,          Word32                 
#field camera,                 Ptr <dc1394camera_t>
#field id,                     Word32                 
#field allocated_image_bytes,  Word64                 
#field little_endian,          CInt             
                                                    
#field data_in_padding,        CInt             
#stoptype

-- dc1394camera_t
#starttype dc1394camera_t
# field guid,                                          Word64            
# field unit,                                          CInt                 
# field unit_spec_ID,                                  Word32            
# field unit_sw_version,                               Word32            
# field unit_sub_sw_version,                           Word32            
# field command_registers_base,                        Word32            
# field unit_directory,                                Word32            
# field unit_dependent_directory,                      Word32            
# field advanced_features_csr,                         Word64            
# field PIO_control_csr,                               Word64            
# field SIO_control_csr,                               Word64            
# field strobe_control_csr,                            Word64            
# field format7_csr[DC1394_VIDEO_MODE_FORMAT7_NUM],    Word64            
# field iidc_version,                                  CInt
# field vendor,                                        Ptr CChar               
# field model,                                         Ptr CChar
# field vendor_id,                                     Word32            
# field model_id,                                      Word32            
# field bmode_capable,                                 CInt       
# field one_shot_capable,                              CInt       
# field multi_shot_capable,                            CInt       
# field can_switch_on_off,                             CInt       
# field has_vmode_error_status,                        CInt       
# field has_feature_error_status,                      CInt       
# field max_mem_channel,                               CInt                 

# field flags,                                         Word32            
#stoptype

#opaque_t dc1394_t


-- * Creating dc1394 context
#ccall dc1394_new                 , IO (Ptr <dc1394_t>)
#ccall dc1394_free                , Ptr <dc1394_t> -> IO ()

-- * Creating cameras
#ccall dc1394_camera_new, Ptr <dc1394_t> -> Word64 -> IO (Ptr <dc1394camera_t>)
#ccall dc1394_camera_free  , Ptr <dc1394camera_t> -> IO ()
#ccall dc1394_camera_enumerate, Ptr <dc1394_t> -> Ptr (Ptr <dc1394camera_list_t>) -> IO CInt
#ccall dc1394_camera_free_list    , (Ptr <dc1394camera_list_t>) -> IO ()


-- * Setting camera properties
#ccall dc1394_video_set_transmission , Ptr <dc1394camera_t> -> CInt -> IO CInt
#ccall dc1394_video_set_iso_speed , Ptr <dc1394camera_t> -> CInt -> IO CInt

#ccall dc1394_video_set_mode  , Ptr <dc1394camera_t> -> CInt -> IO CInt
#ccall dc1394_video_get_mode  , Ptr <dc1394camera_t> -> Ptr CInt -> IO CInt

#ccall dc1394_video_set_framerate , Ptr <dc1394camera_t> -> CInt -> IO CInt

-- * Features
-- #ccall dc1394_feature_get_all, Ptr <dc1394camera_t> -> 

-- * Capturing frames
#ccall dc1394_capture_stop , Ptr <dc1394camera_t> -> IO CInt
#ccall dc1394_capture_setup       , Ptr <dc1394camera_t> -> CInt -> CInt -> IO CInt
#ccall dc1394_capture_dequeue , Ptr <dc1394camera_t> -> CInt -> Ptr (Ptr <dc1394video_frame_t>) -> IO CInt
#ccall dc1394_capture_enqueue , Ptr <dc1394camera_t> -> (Ptr <dc1394video_frame_t>) -> IO CInt
#ccall dc1394_capture_is_frame_corrupt, Ptr <dc1394camera_t> -> Ptr <dc1394video_frame_t> -> IO CInt

-- * Auxiliary
#ccall dc1394_get_image_size_from_video_mode , Ptr <dc1394camera_t> -> CInt -> Ptr Word32 -> Ptr Word32 -> IO CInt

#num DC1394_ISO_SPEED_100
#num DC1394_ISO_SPEED_200
#num DC1394_ISO_SPEED_400
#num DC1394_ISO_SPEED_800
#num DC1394_ISO_SPEED_1600
#num DC1394_ISO_SPEED_3200
#num DC1394_ISO_SPEED_MIN                   
#num DC1394_ISO_SPEED_MAX                  
#num DC1394_ISO_SPEED_NUM    

#num DC1394_VIDEO_MODE_160x120_YUV444
#num DC1394_VIDEO_MODE_320x240_YUV422
#num DC1394_VIDEO_MODE_640x480_YUV411
#num DC1394_VIDEO_MODE_640x480_YUV422
#num DC1394_VIDEO_MODE_640x480_RGB8
#num DC1394_VIDEO_MODE_640x480_MONO8
#num DC1394_VIDEO_MODE_640x480_MONO16
#num DC1394_VIDEO_MODE_800x600_YUV422
#num DC1394_VIDEO_MODE_800x600_RGB8
#num DC1394_VIDEO_MODE_800x600_MONO8
#num DC1394_VIDEO_MODE_1024x768_YUV422
#num DC1394_VIDEO_MODE_1024x768_RGB8
#num DC1394_VIDEO_MODE_1024x768_MONO8
#num DC1394_VIDEO_MODE_800x600_MONO16
#num DC1394_VIDEO_MODE_1024x768_MONO16
#num DC1394_VIDEO_MODE_1280x960_YUV422
#num DC1394_VIDEO_MODE_1280x960_RGB8
#num DC1394_VIDEO_MODE_1280x960_MONO8
#num DC1394_VIDEO_MODE_1600x1200_YUV422
#num DC1394_VIDEO_MODE_1600x1200_RGB8
#num DC1394_VIDEO_MODE_1600x1200_MONO8
#num DC1394_VIDEO_MODE_1280x960_MONO16
#num DC1394_VIDEO_MODE_1600x1200_MONO16
#num DC1394_VIDEO_MODE_EXIF
#num DC1394_VIDEO_MODE_FORMAT7_0
#num DC1394_VIDEO_MODE_FORMAT7_1
#num DC1394_VIDEO_MODE_FORMAT7_2
#num DC1394_VIDEO_MODE_FORMAT7_3
#num DC1394_VIDEO_MODE_FORMAT7_4
#num DC1394_VIDEO_MODE_FORMAT7_5
#num DC1394_VIDEO_MODE_FORMAT7_6
#num DC1394_VIDEO_MODE_FORMAT7_7


#num DC1394_FRAMERATE_1_875
#num DC1394_FRAMERATE_3_75
#num DC1394_FRAMERATE_7_5
#num DC1394_FRAMERATE_15
#num DC1394_FRAMERATE_30
#num DC1394_FRAMERATE_60
#num DC1394_FRAMERATE_120
#num DC1394_FRAMERATE_240
#num DC1394_FRAMERATE_MIN
#num DC1394_FRAMERATE_MAX

#num DC1394_CAPTURE_FLAGS_CHANNEL_ALLOC   
#num DC1394_CAPTURE_FLAGS_BANDWIDTH_ALLOC 
#num DC1394_CAPTURE_FLAGS_DEFAULT         
#num DC1394_CAPTURE_FLAGS_AUTO_ISO        

#num DC1394_CAPTURE_POLICY_WAIT
#num DC1394_CAPTURE_POLICY_POLL
#num DC1394_CAPTURE_POLICY_MIN
#num DC1394_CAPTURE_POLICY_MAX
#num DC1394_CAPTURE_POLICY_NUM   

#num DC1394_COLOR_CODING_MONO8
#num DC1394_COLOR_CODING_YUV411
#num DC1394_COLOR_CODING_YUV422
#num DC1394_COLOR_CODING_YUV444
#num DC1394_COLOR_CODING_RGB8
#num DC1394_COLOR_CODING_MONO16
#num DC1394_COLOR_CODING_RGB16
#num DC1394_COLOR_CODING_MONO16S
#num DC1394_COLOR_CODING_RGB16S
#num DC1394_COLOR_CODING_RAW8
#num DC1394_COLOR_CODING_RAW16
#num DC1394_COLOR_CODING_MIN    
#num DC1394_COLOR_CODING_MAX     

#num DC1394_COLOR_FILTER_RGGB
#num DC1394_COLOR_FILTER_GBRG
#num DC1394_COLOR_FILTER_GRBG
#num DC1394_COLOR_FILTER_BGGR
#num DC1394_COLOR_FILTER_MIN        
#num DC1394_COLOR_FILTER_MAX       

#num DC1394_IIDC_VERSION_1_04
#num DC1394_IIDC_VERSION_1_20
#num DC1394_IIDC_VERSION_PTGREY
#num DC1394_IIDC_VERSION_1_30
#num DC1394_IIDC_VERSION_1_31
#num DC1394_IIDC_VERSION_1_32
#num DC1394_IIDC_VERSION_1_33
#num DC1394_IIDC_VERSION_1_34
#num DC1394_IIDC_VERSION_1_35
#num DC1394_IIDC_VERSION_1_36
#num DC1394_IIDC_VERSION_1_37
#num DC1394_IIDC_VERSION_1_38
#num DC1394_IIDC_VERSION_1_39


#num DC1394_SUCCESS                     
#num DC1394_FAILURE                     
#num DC1394_NOT_A_CAMERA                
#num DC1394_FUNCTION_NOT_SUPPORTED      
#num DC1394_CAMERA_NOT_INITIALIZED      
#num DC1394_MEMORY_ALLOCATION_FAILURE   
#num DC1394_TAGGED_REGISTER_NOT_FOUND   
#num DC1394_NO_ISO_CHANNEL              
#num DC1394_NO_BANDWIDTH                
#num DC1394_IOCTL_FAILURE               
#num DC1394_CAPTURE_IS_NOT_SET          
#num DC1394_CAPTURE_IS_RUNNING          
#num DC1394_RAW1394_FAILURE             
#num DC1394_FORMAT7_ERROR_FLAG_1        
#num DC1394_FORMAT7_ERROR_FLAG_2        
#num DC1394_INVALID_ARGUMENT_VALUE      
#num DC1394_REQ_VALUE_OUTSIDE_RANGE     
#num DC1394_INVALID_FEATURE             
#num DC1394_INVALID_VIDEO_FORMAT        
#num DC1394_INVALID_VIDEO_MODE          
#num DC1394_INVALID_FRAMERATE           
#num DC1394_INVALID_TRIGGER_MODE        
#num DC1394_INVALID_TRIGGER_SOURCE      
#num DC1394_INVALID_ISO_SPEED           
#num DC1394_INVALID_IIDC_VERSION        
#num DC1394_INVALID_COLOR_CODING        
#num DC1394_INVALID_COLOR_FILTER        
#num DC1394_INVALID_CAPTURE_POLICY      
#num DC1394_INVALID_ERROR_CODE          
#num DC1394_INVALID_BAYER_METHOD        
#num DC1394_INVALID_VIDEO1394_DEVICE    
#num DC1394_INVALID_OPERATION_MODE      
#num DC1394_INVALID_TRIGGER_POLARITY    
#num DC1394_INVALID_FEATURE_MODE        
#num DC1394_INVALID_LOG_TYPE            
#num DC1394_INVALID_BYTE_ORDER          
#num DC1394_INVALID_STEREO_METHOD       
#num DC1394_BASLER_NO_MORE_SFF_CHUNKS   
#num DC1394_BASLER_CORRUPTED_SFF_CHUNK  
#num DC1394_BASLER_UNKNOWN_SFF_CHUNK    

