
/**
    Wrapper for some C++-speficic parts of the GUIDO engine.
    Created for the Haskell bindings by Hans Höglund.

    Copyright (c) Hans Höglund 2013. BSD-like license.
 */

typedef void CSystem;
typedef void CDevice;

#ifdef __cplusplus
extern "C" {
#endif
    
    int GuidoCGetVersion();

    // Create a default system
    CSystem * GuidoCCreateSystem();
    void GuidoCFreeSystem(CSystem * system);
    
    // Create an SVG system
    CSystem * GuidoCCreateSVGSystem();
    void GuidoCFreeSVGSystem(CSystem * system);

    CDevice * GuidoCCreateDisplayDevice(CSystem * a);
    CDevice * GuidoCCreateMemoryDevice(CSystem * a, int width, int height);
    CDevice * GuidoCCreateMemoryDevicePath(CSystem * a, const char* path);
    CDevice * GuidoCCreatePrinterDevice(CSystem * a);
    
    
    void GuidoCBeginContext();
    void GuidoCEndContext();

#ifdef __cplusplus
}
#endif