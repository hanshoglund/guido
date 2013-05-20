
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, BangPatterns #-}

module Guido (
    ErrCode,
    AbstractRepr,
    GraphicRepr,
    GraphicsSystem,    
    GraphicsDevice,
    InitParams(..),      
    DrawParams(..),
    LayoutSettings,
    
    getErrorString,
    initialize,
    initialize',
    shutdown,
    getVersionString,
    
    parseFile,  
    parseString,
    abstractToGraphicRepr,
    
    getPageCount,
    draw,
    draw',
    
    main -- TODO
) where

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
type AbstractRepr      = Ptr AbstractRepr_
type GraphicRepr      = Ptr GraphicRepr_
type GraphicsSystem       = Ptr GraphicsSystem_
type GraphicsDevice       = Ptr GraphicsDevice_

data AbstractRepr_
data GraphicRepr_                                             
data GraphicsSystem_
data GraphicsDevice_

data InitParams 
    = InitParams 
        GraphicsDevice 
        String 
        String
    deriving (Eq, Ord, Show)

data DrawParams 
    = DrawParams 
        GraphicRepr 
        GraphicsDevice 
        Int 
        (Maybe (Int,Int,Int,Int)) 
        (Int,Int) 
        Float 
        (Int,Int) 
        Bool
    deriving (Eq, Ord, Show)

-- TODO
data LayoutSettings

foreign import ccall "GuidoInit"            
    cInit                                       :: Ptr InitParams -> IO ErrCode

foreign import ccall "GuidoShutdown"        
    cShutdown                                   :: IO ()

foreign import ccall "GuidoCGetVersion"     
    cCGetVersion                                :: IO CString

foreign import ccall "GuidoGetVersionStr"   
    cGetVersionStr                              :: IO CString

foreign import ccall "GuidoGetErrorString"  
    cGetErrorString                             :: ErrCode -> CString

foreign import ccall "GuidoParseFile"       
    cParseFile                                  :: CString -> Ptr AbstractRepr -> IO ErrCode

foreign import ccall "GuidoParseString"     
    cParseString                                :: CString -> Ptr AbstractRepr -> IO ErrCode

foreign import ccall "GuidoAR2GR"           
    cAR2GR                                      :: AbstractRepr -> Ptr LayoutSettings -> Ptr GraphicRepr -> IO ErrCode

foreign import ccall "GuidoOnDraw"
    cGuidoOnDraw                                :: Ptr DrawParams -> IO ErrCode

foreign import ccall "GuidoCCreateSystem"         
    cGuidoCCreateSystem                         :: IO GraphicsSystem

foreign import ccall "GuidoCCreateSVGSystem"      
    cGuidoCCreateSVGSystem                      :: IO GraphicsSystem

foreign import ccall "GuidoCCreateDisplayDevice"  
    cGuidoCCreateDisplayDevice                  :: GraphicsSystem -> IO GraphicsDevice

foreign import ccall "GuidoCCreateMemoryDevice"   
    cGuidoCCreateMemoryDevice                   :: GraphicsSystem -> Int -> Int -> IO GraphicsDevice

foreign import ccall "GuidoCGraphicDeviceSetRasterMode" 
    cGuidoCGraphicDeviceSetRasterMode           :: GraphicsDevice -> Int -> IO ()

foreign import ccall "GuidoCNativePaint" 
    cGuidoCNativePaint                          :: GraphicsDevice -> IO (Ptr Word32)

foreign import ccall "GuidoGetPageCount" 
    cGuidoGetPageCount                          :: GraphicRepr -> IO CInt


nativePaint :: GraphicsDevice -> IO (Ptr Word32)
nativePaint = cGuidoCNativePaint


instance Storable InitParams where
    sizeOf _ = 4 * ptrSize    
    alignment _ = ptrAlignment
    peek = error "InitParams: no peek"    
    poke ptr (InitParams device musicFont textFont) = do
        cMusicFont <- newCString musicFont
        cTextFont  <- newCString textFont
        pokeByteOff ptr (ptrSize*0) $ castPtr $ device
        pokeByteOff ptr (ptrSize*2) $ castPtr $ cMusicFont
        pokeByteOff ptr (ptrSize*3) $ castPtr $ cTextFont

instance Storable DrawParams where
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
    poke ptr (DrawParams handle device page updateRegion scroll reserved size isPrint) = do
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

-- | Fail if the given error code is non-zero, otherwise return the given value.
checkErr :: a -> ErrCode -> IO a
checkErr a e = case e of
    0 -> return a
    _ -> getErrorString e >>= \e -> error (": " ++ e)

-- | 
-- Initialises the Guido Engine. Must be called before any attempt to read a Guido file or to use the
-- Guido Factory. The given device must be used throghout the session.
initialize :: GraphicsDevice -> String -> String -> IO ()
initialize dev musicFont textFont = initialize' $ InitParams dev musicFont textFont

-- | 
-- Initialises the Guido Engine. Must be called before any attempt to read a Guido file or to use the
-- Guido Factory. The given device must be used throghout the session.
initialize' :: InitParams -> IO ()
initialize' = flip with $ (checkErr () =<<) . cInit

-- | Shutdown Guido engine.
shutdown :: IO ()
shutdown = cShutdown

-- | Get verison of the Guido engine.
getVersionString :: IO String
getVersionString = peekCString =<< cGetVersionStr

-- | Parse a Guido (.gmn) file.
parseFile :: FilePath -> IO AbstractRepr
parseFile path = do
    cPath     <- newCString path
    handleRef <- mallocBytes ptrSize
    err       <- cParseFile cPath handleRef
    handle    <- deref handleRef
    free cPath               
    free handleRef
    checkErr handle err

-- | Parse a Guido string.
parseString :: String -> IO AbstractRepr
parseString str = error "Not implemented"


-- | Convert an abstract to a graphic representation.
abstractToGraphicRepr :: Maybe LayoutSettings -> AbstractRepr -> IO GraphicRepr
abstractToGraphicRepr _ ar = do
    grRef <- mallocBytes ptrSize
    err   <- cAR2GR ar nullPtr grRef
    gr    <- deref grRef
    free grRef
    checkErr gr err

-- | Gives the number of score pages of the graphic representation. 
getPageCount :: GraphicRepr -> IO CInt
getPageCount = id `fmap` cGuidoGetPageCount

-- | Draw a graphic representation to a device.
draw :: GraphicRepr -> GraphicsDevice -> Int -> (Maybe (Int,Int,Int,Int)) -> (Int,Int) -> Float -> (Int,Int) -> Bool -> IO ()
draw dev a b c d e f g = draw' $ DrawParams dev a b c d e f g

-- | Draw a graphic representation to a device.
draw' :: DrawParams -> IO ()
draw' = flip with $ (checkErr () =<<) . cGuidoOnDraw


-- foreign import ccall "GuidoOnDraw" cGuidoOnDraw ::
    -- Ptr DrawParams -> IO ErrCode

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
 
setupTest :: IO (GraphicsDevice, GraphicRepr)
setupTest = do              
    sys <- cGuidoCCreateSystem
    -- dev <- cGuidoCCreateDisplayDevice sys
    dev <- cGuidoCCreateMemoryDevice sys (fst kDim) (snd kDim)

    initialize dev "Guido2" "Arial"
    ar <- parseFile "test.gmn" -- parse requires initialize' (don't ask!)    
    gr <- abstractToGraphicRepr Nothing ar
    return (dev, gr)

drawTest :: Window a -> GraphicsDevice -> GraphicRepr -> IO (Ptr Word32)
drawTest win dev gr = do
    draw gr dev 1 Nothing (0,0) 1 kDim False
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
    imageCreateFromPixels (sz w h) (fmap monoToColor xs)



gui = do
    frame <- frame [text := "Guido Test", size := sz 800 800]
          
    (!dev,!gr) <- setupTest

    let paintHandler = \dc dim -> do                   
        pages <- getPageCount gr
        putStrLn $ "Dimensions: " ++ show dim
        putStrLn $ "Pages:      " ++ show pages
        
        dcClear dc
        
        rawImg <- drawTest frame dev gr
        img <- fromRaw (fst kDim) (snd kDim) rawImg

        drawRect dc (rect (pt 0 0) (sz (fst kDim) (snd kDim))) 
            [                                               
                penKind := PenTransparent,
                brushKind := BrushSolid, brushColor := black
            ]
        drawImage dc img (pt 10 10) []
        return ()

    set frame [on paint := paintHandler]
    repaint frame
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
