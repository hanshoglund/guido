
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module Guido where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C

type ErrCode   = CInt
type ARHandler      = Ptr ARHandler_
type GRHandler      = Ptr GRHandler_
type VGSystem       = Ptr VGSystem_
type VGDevice       = Ptr VGDevice_

data ARHandler_
data GRHandler_                                             
data VGSystem_
data VGDevice_

data InitDesc
data OnDrawDesc
data LayoutSettings

foreign import ccall "GuidoInit"           cInit           :: Ptr InitDesc -> IO ErrCode;
foreign import ccall "GuidoShutdown"       cShutdown       :: IO ()
foreign import ccall "GuidoGetVersionStr"  cGetVersionStr  :: IO CString
foreign import ccall "GuidoGetErrorString" cGetErrorString :: ErrCode -> CString

foreign import ccall "GuidoParseFile"      cParseFile      :: CString -> Ptr ARHandler -> IO ErrCode
foreign import ccall "GuidoParseString"    cParseString    :: CString -> Ptr ARHandler -> IO ErrCode

foreign import ccall "GuidoCGetVersion" cCGetVersion :: IO Int

-- struct GuidoInitDesc { 
--    VGDevice* graphicDevice;
--    void* reserved;
--    const char* musicFont;
--    const char*  textFont;
-- };

defInitDesc :: IO (Ptr InitDesc)
defInitDesc = do
    desc <- mallocBytes $ 4 * ptrSize
    pokeElemOff desc 0 $ nullPtr
    pokeElemOff desc 2 $ nullPtr
    pokeElemOff desc 3 $ nullPtr
    return $ castPtr desc

newInitDesc :: VGDevice -> String -> String -> IO (Ptr InitDesc)
newInitDesc device musicFont textFont = do
    desc   <- mallocBytes $ 4 * ptrSize
    cMusicFont <- newCString musicFont
    cTextFont  <- newCString textFont
    pokeElemOff desc 0 $ castPtr $ device
    pokeElemOff desc 2 $ castPtr $ cMusicFont
    pokeElemOff desc 3 $ castPtr $ cTextFont
    return $ castPtr desc

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

newOnDrawDesc :: 
    GRHandler -> VGDevice -> Int -> Maybe (Int,Int,Int,Int) -> (Int,Int) -> Float -> (Int,Int) -> Bool
    -> IO (Ptr OnDrawDesc)
newOnDrawDesc 
    handle device page updateRegion scroll reserved size isPrint
    = do
    desc <- mallocBytes             $ sz
   
    pokeByteOff desc handle_        $ handle
    pokeByteOff desc hdc_           $ device
    pokeByteOff desc page_          $ page
    pokeByteOff desc updateRegion_  $ (0::CInt) -- TODO use value
    pokeByteOff desc scrollX_       $ fst scroll
    pokeByteOff desc scrollY_       $ snd scroll
    pokeByteOff desc reserved_      $ reserved
    pokeByteOff desc sizeX_         $ fst size
    pokeByteOff desc sizeY_         $ snd size
    pokeByteOff desc isPrint_       $ if isPrint then (1::CInt) else 0
        
    return $ castPtr desc
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

initialize :: (Ptr InitDesc) -> IO ()
initialize d = cInit d >>= checkErr ()

shutdown :: IO ()
shutdown = cShutdown

getVersionString :: IO String
getVersionString = cGetVersionStr >>= peekCString

parseFile :: FilePath -> IO ARHandler
parseFile path = do
    cPath     <- newCString path
    handleRef <- mallocBytes ptrSize
    err       <- cParseFile cPath handleRef
    handle    <- deref handleRef
    free cPath               
    free handleRef
    checkErr handle err


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

 


main = do
    defInitDesc >>= initialize
    ar <- parseFile "test.guido"
    return ()



ptrSize = sizeOf nullPtr
intSize = sizeOf (undefined::CInt)
boolSize = sizeOf (undefined::CInt) -- TODO Hopefully correct
floatSize = sizeOf (undefined::CFloat)
doubleSize = sizeOf (undefined::CDouble)

deref :: Ptr (Ptr a) -> IO (Ptr a)
deref = peek
