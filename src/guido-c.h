
/**
    Wrapper for some C++-speficic parts of the GUIDO engine.
    Created for the Haskell bindings by Hans Höglund.

    Copyright (c) Hans Höglund 2013. BSD-like license.
 */

#ifdef __cplusplus
#include <GuidoEngine/VGSystem.h>
#include <GuidoEngine/VGDevice.h>
typedef VGSystem CVGSystem;
typedef VGDevice CVGDevice;
#else
typedef void CVGSystem;
typedef void CVGDevice;
#endif


#ifdef __cplusplus
extern "C" {
#endif
    

int GuidoCGetVersion();

CVGSystem * GuidoCCreateSystem();
CVGSystem * GuidoCCreateSVGSystem();

void GuidoCFreeSystem(CVGSystem * system);
void GuidoCFreeSVGSystem(CVGSystem * system);

CVGDevice * GuidoCCreateDisplayDevice(CVGSystem * a);
CVGDevice * GuidoCCreateMemoryDevice(CVGSystem * a, int width, int height);
CVGDevice * GuidoCCreateMemoryDevicePath(CVGSystem * a, const char* path);
CVGDevice * GuidoCCreatePrinterDevice(CVGSystem * a);


uint32_t* GuidoCNativePaint(void * device);



#ifdef __cplusplus
}
#endif