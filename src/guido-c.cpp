
#include <GUIDOEngine/GUIDOEngine.h>
#include <GUIDOEngine/GDeviceOSX.h>
#include <GUIDOEngine/GFontOSX.h>
#include <GUIDOEngine/GMemoryDeviceOSX.h>
#include <GUIDOEngine/GPrinterDeviceOSX.h>
#include <GUIDOEngine/GSystemOSX.h>
#include <GUIDOEngine/SVGSystem.h>

#include <wx/dcclient.h>

#include <guido-c.h>

int GuidoCGetVersion() {
    return 2;
}

// System

CVGSystem * GuidoCCreateSystem() {
    // FIXME need to pass CGContextRef as first param here
    /*
       In a WX paint handler:
        wxPaintDC MyDC(this);
        CGContextRef context = ((wxMacCGContext*)MyDC.GetGraphicContext())->GetNativeContext();
     */
	CVGSystem * system = (CVGSystem*) new GSystemOSX(0,0);
    return system;
}

void GuidoCPaint(void * window, Unary * callback, void * data) {
    wxPaintDC MyDC((wxWindow*)0);
    callback(data);
}

CVGSystem * GuidoCCreateSVGSystem() {
    CVGSystem * system = (CVGSystem*) new SVGSystem();
    return system;
}

void GuidoCFreeSystem(CVGSystem * system) {
	delete (GSystemOSX*) system;
}

void GuidoCFreeSVGSystem(CVGSystem * system) {
	delete (SVGSystem*) system;
}


CVGDevice * GuidoCCreateDisplayDevice(CVGSystem * system) {
    return (CVGDevice*) ((VGSystem*) system)->CreateDisplayDevice();
}

CVGDevice * GuidoCCreateMemoryDevice(CVGSystem * system, int width, int height) {
    return (CVGDevice*) ((VGSystem*) system)->CreateMemoryDevice(width, height);
}

CVGDevice * GuidoCCreateMemoryDevicePath(CVGSystem * system, const char* path) {
    return (CVGDevice*) ((VGSystem*) system)->CreateMemoryDevice(path);
}

CVGDevice * GuidoCCreatePrinterDevice(CVGSystem * system) {
    return (CVGDevice*) ((VGSystem*) system)->CreatePrinterDevice();
}


