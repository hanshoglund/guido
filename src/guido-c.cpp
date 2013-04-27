
#include <GUIDOEngine/GUIDOEngine.h>
#include <GUIDOEngine/GDeviceOSX.h>
#include <GUIDOEngine/GFontOSX.h>
#include <GUIDOEngine/GMemoryDeviceOSX.h>
#include <GUIDOEngine/GPrinterDeviceOSX.h>
#include <GUIDOEngine/GSystemOSX.h>
#include <GUIDOEngine/SVGSystem.h>

#include <guido-c.h>

int GuidoCGetVersion() {
    return 2;
}

// System

CSystem * GuidoCCreateSystem() {
    // FIXME need to pass CGContextRef as first param here
    /*
       In a WX paint handler:
        wxPaintDC MyDC(this);
        CGContextRef context = ((wxMacCGContext*)MyDC.GetGraphicContext())->GetNativeContext();
     */
	CSystem * system = (CSystem*) new GSystemOSX(0,0);
    return system;
}

CSystem * GuidoCCreateSVGSystem() {
    CSystem * system = (CSystem*) new SVGSystem();
    return system;
}

void GuidoCFreeSystem(CSystem * system) {
	delete (GSystemOSX*) system;
}

void GuidoCFreeSVGSystem(CSystem * system) {
	delete (SVGSystem*) system;
}


CDevice * GuidoCCreateDisplayDevice(CSystem * system) {
    return (CDevice*) ((VGSystem*) system)->CreateDisplayDevice();
}

CDevice * GuidoCCreateMemoryDevice(CSystem * system, int width, int height) {
    return (CDevice*) ((VGSystem*) system)->CreateMemoryDevice(width, height);
}

CDevice * GuidoCCreateMemoryDevicePath(CSystem * system, const char* path) {
    return (CDevice*) ((VGSystem*) system)->CreateMemoryDevice(path);
}

CDevice * GuidoCCreatePrinterDevice(CSystem * system) {
    return (CDevice*) ((VGSystem*) system)->CreatePrinterDevice();
}


