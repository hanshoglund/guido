
/**
    Pure C wrapper for thethe GUIDO engine.
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
    

/** Return the version of guido-c (not to be confused with the guido version).
 */
int GuidoCGetVersion();

/** Create the default system.
 */
CVGSystem * GuidoCCreateSystem();
void        GuidoCFreeSystem(CVGSystem * system);

/** Create an SVG system.
 */
CVGSystem * GuidoCCreateSVGSystem();
void        GuidoCFreeSVGSystem(CVGSystem * system);

CVGDevice * GuidoCCreateDisplayDevice(CVGSystem * a);
CVGDevice * GuidoCCreateMemoryDevice(CVGSystem * a, int width, int height);
CVGDevice * GuidoCCreateMemoryDevicePath(CVGSystem * a, const char* path);
CVGDevice * GuidoCCreatePrinterDevice(CVGSystem * a);

uint32_t*   GuidoCNativePaint(CVGDevice * device);
void        GuidoCPrintDeviceInfo(CVGDevice * device);





#ifdef __cplusplus
}
#endif