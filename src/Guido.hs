
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module Guido where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C

type ErrCode   = CInt
type ARHandler      = Ptr ARHandlerStruct
type GRHandler      = Ptr GRHandlerStruct
type VGSystem       = Ptr VGSystemStruct
type VGDevice       = Ptr VGDeviceStruct
type InitDesc  = Ptr InitDescStruct

data ARHandlerStruct
data GRHandlerStruct                                             
data VGSystemStruct
data VGDeviceStruct
data InitDescStruct
data OnDrawDescStruct
data LayoutSettingsStruct

foreign import ccall "GuidoInit"           cInit           :: InitDesc -> IO ErrCode;
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

defInitDesc :: IO InitDesc
defInitDesc = do
    desc <- mallocBytes $ 4 * ptrSize
    pokeElemOff desc 0 $ nullPtr
    pokeElemOff desc 2 $ nullPtr
    pokeElemOff desc 3 $ nullPtr
    return $ castPtr desc

newInitDesc :: VGDevice -> String -> String -> IO InitDesc
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
 










getErrorString :: ErrCode -> IO String
getErrorString = peekCString . cGetErrorString

checkErr :: a -> ErrCode -> IO a
checkErr a e = case e of
    0 -> return a
    _ -> getErrorString e >>= \e -> error (": " ++ e)

initialize :: InitDesc -> IO ()
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
deref :: Ptr (Ptr a) -> IO (Ptr a)
deref = peek
