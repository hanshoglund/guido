
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, BangPatterns #-}

module Guido where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C 
import Data.Word
import Data.Bits 

import Graphics.UI.WX hiding (alignment)
import Graphics.UI.WXCore.WxcObject
import Graphics.UI.WXCore.WxcClassTypes

type ErrCode   = CInt
type ARHandler      = Ptr ARHandler_
type GRHandler      = Ptr GRHandler_
type VGSystem       = Ptr VGSystem_
type VGDevice       = Ptr VGDevice_

data ARHandler_
data GRHandler_                                             
data VGSystem_
data VGDevice_

data InitDesc       = InitDesc VGDevice String String
    deriving (Eq, Ord, Show)
data OnDrawDesc     = OnDrawDesc GRHandler VGDevice Int (Maybe (Int,Int,Int,Int)) (Int,Int) Float (Int,Int) Bool
    deriving (Eq, Ord, Show)
data LayoutSettings

foreign import ccall "GuidoInit"           cInit           :: Ptr InitDesc -> IO ErrCode;
foreign import ccall "GuidoShutdown"       cShutdown       :: IO ()
foreign import ccall "GuidoGetVersionStr"  cGetVersionStr  :: IO CString
foreign import ccall "GuidoGetErrorString" cGetErrorString :: ErrCode -> CString

foreign import ccall "GuidoParseFile"      cParseFile      :: CString -> Ptr ARHandler -> IO ErrCode
foreign import ccall "GuidoParseString"    cParseString    :: CString -> Ptr ARHandler -> IO ErrCode

foreign import ccall "GuidoCGetVersion" cCGetVersion :: IO Int

foreign import ccall "GuidoAR2GR"       cAR2GR  :: 
    ARHandler -> Ptr LayoutSettings -> Ptr GRHandler -> IO ErrCode

foreign import ccall "GuidoOnDraw" cGuidoOnDraw ::
    Ptr OnDrawDesc -> IO ErrCode

foreign import ccall "GuidoCCreateSystem" cGuidoCCreateSystem :: IO VGSystem
foreign import ccall "GuidoCCreateSVGSystem" cGuidoCCreateSVGSystem :: IO VGSystem
foreign import ccall "GuidoCCreateDisplayDevice" cGuidoCCreateDisplayDevice :: VGSystem -> IO VGDevice
foreign import ccall "GuidoCCreateMemoryDevice" cGuidoCCreateMemoryDevice :: VGSystem -> Int -> Int -> IO VGDevice

-- window -> device -> IO ()
foreign import ccall "GuidoCNativePaint" cGuidoCNativePaint :: VGDevice -> IO (Ptr Word32)

nativePaint :: VGDevice -> IO (Ptr Word32)
nativePaint = cGuidoCNativePaint



-- struct GuidoInitDesc { 
--    VGDevice* graphicDevice;
--    void* reserved;
--    const char* musicFont;
--    const char*  textFont;
-- };

-- defInitDesc :: IO (Ptr InitDesc)
-- defInitDesc = do
--     ptr <- mallocBytes $ 4 * ptrSize
--     pokeElemOff ptr 0 $ nullPtr
--     pokeElemOff ptr 2 $ nullPtr
--     pokeElemOff ptr 3 $ nullPtr
--     return $ castPtr ptr

-- defInitDesc :: InitDesc
-- defInitDesc = InitDesc nullPtr "Guido2" "Times"

instance Storable InitDesc where
    sizeOf _ = 4 * ptrSize    
    alignment _ = ptrAlignment
    peek = error "InitDesc: no peek"    
    poke ptr (InitDesc device musicFont textFont) = do
        cMusicFont <- newCString musicFont
        cTextFont  <- newCString textFont
        pokeByteOff ptr (ptrSize*0) $ castPtr $ device
        pokeByteOff ptr (ptrSize*2) $ castPtr $ cMusicFont
        pokeByteOff ptr (ptrSize*3) $ castPtr $ cTextFont

-- struct GuidoOnDrawDesc {
--     GRHandler handle;
--     VGDevice * hdc;
--     int page;
--     struct {
--          bool	erase;
--          int		left;
--          int		top;
--          int		right;
--          int		bottom;
--     } updateRegion;
--     int scrollx, scrolly;
--     float reserved;
--     int sizex, sizey;
--     int isprint;
-- };


instance Storable OnDrawDesc where
    sizeOf _ = sz
      where
        handle_         = 0
        hdc_            = handle_       + ptrSize
        page_           = hdc_          + ptrSize
        updateRegion_   = page_         + intSize
        scrollX_        = updateRegion_ + boolSize + 4 * intSize
        scrollY_        = scrollX_      + intSize
        reserved_       = scrollY_      + intSize
        sizeX_          = reserved_     + floatSize
        sizeY_          = sizeX_        + intSize
        isPrint_        = sizeY_        + intSize
        sz              = isPrint_      + intSize

    alignment _ = ptrAlignment
    poke ptr (OnDrawDesc handle device page updateRegion scroll reserved size isPrint) = do
        pokeByteOff ptr handle_        $ handle
        pokeByteOff ptr hdc_           $ device
        pokeByteOff ptr page_          $ page
        pokeByteOff ptr updateRegion_  $ (1::CInt) -- TODO use value
        pokeByteOff ptr scrollX_       $ fst scroll
        pokeByteOff ptr scrollY_       $ snd scroll
        pokeByteOff ptr reserved_      $ reserved
        pokeByteOff ptr sizeX_         $ fst size
        pokeByteOff ptr sizeY_         $ snd size
        pokeByteOff ptr isPrint_       $ if isPrint then (1::CInt) else 0
      where
        handle_         = 0
        hdc_            = handle_       + ptrSize
        page_           = hdc_          + ptrSize
        updateRegion_   = page_         + intSize
        scrollX_        = updateRegion_ + boolSize + 4 * intSize
        scrollY_        = scrollX_      + intSize
        reserved_       = scrollY_      + intSize
        sizeX_          = reserved_     + floatSize
        sizeY_          = sizeX_        + intSize
        isPrint_        = sizeY_        + intSize
        sz              = isPrint_      + intSize

-- struct GuidoLayoutSettings {
--  float systemsDistance;
--  int systemsDistribution;
--  float systemsDistribLimit;
--  float   force;
--  float   spring;
--  int neighborhoodSpacing;
--  int optimalPageFill;
-- };
 










getErrorString :: ErrCode -> IO String
getErrorString = peekCString . cGetErrorString

checkErr :: a -> ErrCode -> IO a
checkErr a e = case e of
    0 -> return a
    _ -> getErrorString e >>= \e -> error (": " ++ e)

initialize :: InitDesc -> IO ()
initialize = flip with $ (checkErr () =<<) . cInit

shutdown :: IO ()
shutdown = cShutdown

getVersionString :: IO String
getVersionString = peekCString =<< cGetVersionStr

parseFile :: FilePath -> IO ARHandler
parseFile path = do
    cPath     <- newCString path
    handleRef <- mallocBytes ptrSize
    err       <- cParseFile cPath handleRef
    handle    <- deref handleRef
    free cPath               
    free handleRef
    checkErr handle err

-- TODO use layout settings
ar2gr :: Maybe LayoutSettings -> ARHandler -> IO GRHandler
ar2gr _ ar = do
    grRef <- mallocBytes ptrSize
    err   <- cAR2GR ar nullPtr grRef
    gr    <- deref grRef
    free grRef
    checkErr gr err

draw :: OnDrawDesc -> IO ()
draw = flip with $ (checkErr () =<<) . cGuidoOnDraw

-- foreign import ccall "GuidoOnDraw" cGuidoOnDraw ::
    -- Ptr OnDrawDesc -> IO ErrCode

-- GuidoErrCode GuidoParseFile(const char * filename, ARHandler* ar);
-- GuidoErrCode GuidoParseString(const char * str, ARHandler* ar);
-- GuidoErrCode GuidoFactoryOpen( ARFactoryHandler * outFactory );
-- void         GuidoFactoryClose( ARFactoryHandler inFactory );
-- GuidoErrCode GuidoFactoryOpenMusic( ARFactoryHandler inFactory );
-- ARHandler        GuidoFactoryCloseMusic( ARFactoryHandler inFactory );
-- GuidoErrCode GuidoFactoryOpenVoice( ARFactoryHandler inFactory );
-- GuidoErrCode GuidoFactoryCloseVoice( ARFactoryHandler inFactory );
-- GuidoErrCode GuidoFactoryOpenChord( ARFactoryHandler inFactory );
-- GuidoErrCode GuidoFactoryCloseChord( ARFactoryHandler inFactory );
-- GuidoErrCode GuidoFactoryInsertCommata( ARFactoryHandler inFactory ); // is it a correct name ?
-- GuidoErrCode GuidoFactoryOpenEvent( ARFactoryHandler inFactory, const char * inEventName );
-- GuidoErrCode GuidoFactoryCloseEvent( ARFactoryHandler inFactory );
-- GuidoErrCode GuidoFactoryAddSharp( ARFactoryHandler inFactory );
-- GuidoErrCode GuidoFactoryAddFlat( ARFactoryHandler inFactory );
-- GuidoErrCode     GuidoFactorySetEventDots( ARFactoryHandler inFactory, int dots );
-- GuidoErrCode     GuidoFactorySetEventAccidentals( ARFactoryHandler inFactory, int accident );
-- GuidoErrCode GuidoFactorySetOctave( ARFactoryHandler inFactory, int octave );
-- GuidoErrCode     GuidoFactorySetDuration( ARFactoryHandler inFactory, int numerator, int denominator );

kDim = (800,1600)
 
setupTest :: IO (VGDevice, GRHandler)
setupTest = do              
    sys <- cGuidoCCreateSystem
    -- dev <- cGuidoCCreateDisplayDevice sys
    dev <- cGuidoCCreateMemoryDevice sys (fst kDim) (snd kDim)

    initialize $ InitDesc dev "Guido2" "Arial"

    ar <- parseFile "test.gmn" -- parse requires initialize (don't ask!)    
    gr <- ar2gr Nothing ar

    putStrLn $ show ar
    putStrLn $ show gr
    return (dev, gr)

drawTest :: Window a -> VGDevice -> GRHandler -> IO (Ptr Word32)
drawTest win dev gr = do
    draw $ OnDrawDesc gr dev 1 Nothing (0,0) 1 kDim False
    nativePaint dev

getRGBA :: Word32 -> (Word8,Word8,Word8,Word8)
getRGBA w = (
    cv $ w `shiftR` r, 
    cv $ w `shiftR` g, 
    cv $ w `shiftR` b, 
    cv $ w `shiftR` a
    )
    where                                                                           
        (r,g,b,a) = (0,8,16,24)
        cv x = fromIntegral (x .&. 0xff)

rgbaToColor :: Word32 -> Color
rgbaToColor w = rgb r g b where (r,g,b,_) = getRGBA w

monoToColor :: Word32 -> Color
monoToColor w = rgb a' a' a' 
    where 
        (r,g,b,a) = getRGBA w
        a' = 255 - a


fromRaw :: Int -> Int -> Ptr Word32 -> IO (Image ())    
fromRaw w h ptr = do
    !xs <- peekArray (w*h) ptr
    -- putStrLn $ show $ xs
    imageCreateFromPixels (sz w h) (fmap monoToColor xs)


-- instance Storable Color where
    -- peek ptr = do
    --     peekWord32 ptr



gui = do
    frame <- frame [text := "Guido Test", size := sz 800 800]
          
    (!dev,!gr) <- setupTest

    set frame [on paint := \dc dim -> do
        putStrLn $ show dim
        
        rawImg <- drawTest frame dev gr
        img <- fromRaw (fst kDim) (snd kDim) rawImg
        -- img <- imageCreateFromFile "/Users/hans/Desktop/grav.png"
        -- img <- imageCreateFromPixels (sz 16 16) $ replicate (16^2) black

        drawRect dc (rect (pt 0 0) (sz (fst kDim) (snd kDim))) 
            [                                               
                penKind := PenTransparent,
                brushKind := BrushSolid, brushColor := green
            ]
        drawImage dc img (pt 0 0) []
        

        return ()
        ]
    return ()

main = start gui








ptrAlignment = alignment nullPtr
ptrSize = sizeOf nullPtr
intSize = sizeOf (undefined::CInt)
boolSize = sizeOf (undefined::CInt) -- TODO Hopefully correct
floatSize = sizeOf (undefined::CFloat)
doubleSize = sizeOf (undefined::CDouble)

deref :: Ptr (Ptr a) -> IO (Ptr a)
deref = peek
