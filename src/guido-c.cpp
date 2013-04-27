
#include <GUIDOEngine/GUIDOEngine.h>
#include <GUIDOEngine/GDeviceOSX.h>
#include <GUIDOEngine/GFontOSX.h>
#include <GUIDOEngine/GMemoryDeviceOSX.h>
#include <GUIDOEngine/GPrinterDeviceOSX.h>
#include <GUIDOEngine/GSystemOSX.h>
#include <GUIDOEngine/SVGSystem.h>

// #include <wx/dcclient.h>

#include <guido-c.h>

int GuidoCGetVersion() {
    return 2;
}

// System

CVGSystem * GuidoCCreateSystem() {
    fprintf(stderr, "Creating system!\n");

    // FIXME need to pass CGContextRef as first param here
    /*
       In a WX paint handler:
        wxPaintDC MyDC(this);
        CGContextRef context = ((wxMacCGContext*)MyDC.GetGraphicContext())->GetNativeContext();
     */
	CVGSystem * system = (CVGSystem*) new GSystemOSX(0,0);
    return system;
}

uint32_t* GuidoCNativePaint(void * device) {
    fprintf(stderr, "Repainting!\n");

    // wxPaintDC MyDC((wxWindow*)0);
    CGContextRef context = CGContextRef(((VGDevice*) device)->GetNativeContext());
    uint32_t* data = (uint32_t*)::CGBitmapContextGetData(context); 

    int n = CGBitmapContextGetBitsPerPixel(context);
    int w = CGBitmapContextGetWidth(context);
    int h = CGBitmapContextGetHeight(context);

    fprintf(stderr, "Data format: n=%d w=%d h=%d\n", n, w, h);
    return data;
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


