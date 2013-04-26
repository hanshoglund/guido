
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module Guido where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C

type GuidoErrCode   = CInt
type ARHandler      = Ptr ARHandlerStruct
type GRHandler      = Ptr GRHandlerStruct

data ARHandlerStruct
data GRHandlerStruct                                             
data GuidoInitDesc
data GuidoOnDrawDesc
data GuidoLayoutSettings

foreign import ccall "GuidoInit"           cGuidoInit           :: Ptr GuidoInitDesc -> IO GuidoErrCode;
foreign import ccall "GuidoShutdown"       cGuidoShutdown       :: IO ()
foreign import ccall "GuidoGetVersionStr"  cGuidoGetVersionStr  :: IO CString
foreign import ccall "GuidoGetErrorString" cGuidoGetErrorString :: GuidoErrCode -> CString

foreign import ccall "GuidoParseFile"      cGuidoParseFile      :: CString -> Ptr ARHandler -> IO GuidoErrCode
foreign import ccall "GuidoParseString"    cGuidoParseString    :: CString -> Ptr ARHandler -> IO GuidoErrCode

foreign import ccall "GuidoCGetVersion" cGuidoCGetVersion :: IO Int

-- struct GuidoInitDesc { 
--    VGDevice* graphicDevice;
--    void* reserved;
--    const char* musicFont;
--    const char*  textFont;
-- };

defInitDesc :: IO (Ptr GuidoInitDesc)
defInitDesc = do
    ptr <- mallocBytes $ 4 * ptrSize :: IO (Ptr (Ptr a))
    pokeElemOff ptr 0 nullPtr
    pokeElemOff ptr 2 nullPtr
    pokeElemOff ptr 3 nullPtr
    return $ castPtr ptr

-- struct GuidoOnDrawDesc {
--     GRHandler handle;
--     VGDevice * hdc;
--     int page;
--     GPaintStruct updateRegion;
--     int scrollx, scrolly;
--     float reserved;
--     int sizex, sizey;
--     int isprint;
-- };

-- struct GuidoLayoutSettings {
--  float systemsDistance;
--  int systemsDistribution;
--  float systemsDistribLimit;
--  float   force;
--  float   spring;
--  int neighborhoodSpacing;
--  int optimalPageFill;
-- };
 










getErrorString :: GuidoErrCode -> IO String
getErrorString = peekCString . cGuidoGetErrorString

checkErr :: a -> GuidoErrCode -> IO a
checkErr a e = case e of
    0 -> return a
    _ -> getErrorString e >>= \e -> error ("Guido: " ++ e)

initialize :: IO ()
initialize = defInitDesc >>= cGuidoInit >>= checkErr ()

shutdown :: IO ()
shutdown = cGuidoShutdown

getVersionString :: IO String
getVersionString = cGuidoGetVersionStr >>= peekCString

parseFile :: FilePath -> IO ARHandler
parseFile path = do
    cPath     <- newCString path
    handleRef <- mallocBytes ptrSize
    err       <- cGuidoParseFile cPath handleRef
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
    initialize
    ar <- parseFile "test.guido"
    return ()



ptrSize = sizeOf nullPtr
deref :: Ptr (Ptr a) -> IO (Ptr a)
deref = peek
