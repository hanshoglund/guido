
/**
    Pure C wrapper for thethe GUIDO engine.
    Created for the Haskell bindings by Hans Höglund.

    Copyright (c) Hans Höglund 2013. BSD-like license.
 */

#ifdef __cplusplus
#include <GuidoEngine/VGSystem.h>
#include <GuidoEngine/VGDevice.h>
typedef VGSystem                    GuidoCGraphicSystem;
typedef VGDevice                    GuidoCGraphicDevice;
typedef VGDevice::VRasterOpMode     GuidoCRasterMode;
#else
typedef void                        GuidoCGraphicSystem;
typedef void                        GuidoCGraphicDevice;
typedef int                         GuidoCRasterMode;
#endif


#ifdef __cplusplus
extern "C" {
#endif
    

/** Return the version of guido-c (not to be confused with the guido version).
 */
const char*           GuidoCGetVersion();

/** Create the default system.
 */
GuidoCGraphicSystem * GuidoCCreateSystem();
void                  GuidoCFreeSystem(GuidoCGraphicSystem * system);

/** Create an SVG system.
 */
GuidoCGraphicSystem * GuidoCCreateSVGSystem();
void                  GuidoCFreeSVGSystem(GuidoCGraphicSystem * system);

GuidoCGraphicDevice * GuidoCCreateDisplayDevice(GuidoCGraphicSystem * a);
GuidoCGraphicDevice * GuidoCCreateMemoryDevice(GuidoCGraphicSystem * a, int width, int height);
GuidoCGraphicDevice * GuidoCCreateMemoryDevicePath(GuidoCGraphicSystem * a, const char* path);
GuidoCGraphicDevice * GuidoCCreatePrinterDevice(GuidoCGraphicSystem * a);

uint32_t*             GuidoCNativePaint(GuidoCGraphicDevice * device);
void                  GuidoCPrintDeviceInfo(GuidoCGraphicDevice * device);

/** Set raster mode
 */
void		          GuidoCGraphicDeviceSetRasterMode(GuidoCGraphicDevice * device, GuidoCRasterMode mode);




#ifdef __cplusplus
}
#endif