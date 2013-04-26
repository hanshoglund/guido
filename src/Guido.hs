
{-# LANGUAGE ForeignFunctionInterface #-}

module Guido where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Foreign.C

data GuidoInitDesc
type GuidoErrCode  = CInt                                             

foreign import ccall "GuidoInit"           cGuidoInit     :: Ptr GuidoInitDesc -> IO GuidoErrCode;
foreign import ccall "GuidoShutdown"       cGuidoShutdown :: IO ()
foreign import ccall "GuidoGetErrorString" cGuidoGetErrorString :: GuidoErrCode -> CString

defaultInitDesc :: IO (Ptr GuidoInitDesc)
defaultInitDesc = do
    ptr <- mallocBytes $ 4 * ptrSize :: IO (Ptr (Ptr a))
    pokeElemOff ptr 0 nullPtr   -- graphicDevice
    pokeElemOff ptr 2 nullPtr   -- musicFont
    pokeElemOff ptr 3 nullPtr   -- textFont
    return $ castPtr ptr
    where
        ptrSize = sizeOf nullPtr

getErrorString :: GuidoErrCode -> IO String
getErrorString = peekCString . cGuidoGetErrorString

initialize :: IO GuidoErrCode
initialize = defaultInitDesc >>= cGuidoInit

shutdown :: IO ()
shutdown = cGuidoShutdown

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
    putStrLn . show $ pi
    putStrLn . show $ sin pi
