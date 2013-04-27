
#include <GUIDOEngine/GUIDOEngine.h>
#include <GUIDOEngine/GDeviceOSX.h>
#include <GUIDOEngine/GFontOSX.h>
#include <GUIDOEngine/GMemoryDeviceOSX.h>
#include <GUIDOEngine/GPrinterDeviceOSX.h>
#include <GUIDOEngine/GSystemOSX.h>
#include <GUIDOEngine/SVGSystem.h>

#include <guido-c.h>

// const char *byte_to_binary(int x);

const char* GuidoCGetVersion() 
{
        return "0.5.0";
}

// System

GuidoCGraphicSystem * GuidoCCreateSystem() 
{
        fprintf(stderr, "Creating system!\n");

        // FIXME need to pass CGContextRef as first param here
        /*
           In a WX paint handler:
            wxPaintDC MyDC(this);
            CGContextRef context = ((wxMacCGContext*)MyDC.GetGraphicContext())->GetNativeContext();
         */
    	GuidoCGraphicSystem * system = (GuidoCGraphicSystem*) new GSystemOSX(0,0);
        return system;
}

uint32_t* GuidoCNativePaint(GuidoCGraphicDevice * device) 
{
        CGContextRef context = CGContextRef(((VGDevice*) device)->GetNativeContext());
        uint32_t* data = (uint32_t*)::CGBitmapContextGetData(context); 
        return data;
}

void GuidoCPrintDeviceInfo(GuidoCGraphicDevice * device) 
{
        CGContextRef context = CGContextRef(((VGDevice*) device)->GetNativeContext());
        uint32_t* data = (uint32_t*)::CGBitmapContextGetData(context); 

        int m = CGBitmapContextGetBitsPerComponent(context);
        int n = CGBitmapContextGetBitsPerPixel(context);
        int w = CGBitmapContextGetWidth(context);
        int h = CGBitmapContextGetHeight(context);

        unsigned i = CGBitmapContextGetBitmapInfo(context);
        unsigned a = CGBitmapContextGetAlphaInfo(context);

        fprintf(stderr, "Data format: bitsPerComp=%d bitsPerPixel=%d width=%d height=%d\n", m, n, w, h);
        // fprintf(stderr, "Info: %s\n", byte_to_binary(i));
        // fprintf(stderr, "Alpha info: %s\n", byte_to_binary(a));

        if (i & kCGBitmapFloatComponents)   fprintf(stderr, "The components are floats\n");
        if ((i & kCGBitmapByteOrderMask) == kCGBitmapByteOrderDefault) fprintf(stderr, "The byte order is the default\n");
        if ((i & kCGBitmapByteOrderMask) == kCGBitmapByteOrder16Little) fprintf(stderr, "The byte order is 16LE\n");
        if ((i & kCGBitmapByteOrderMask) == kCGBitmapByteOrder32Little) fprintf(stderr, "The byte order is 32LE\n");
        if ((i & kCGBitmapByteOrderMask) == kCGBitmapByteOrder16Big)    fprintf(stderr, "The byte order is 16BE\n");
        if ((i & kCGBitmapByteOrderMask) == kCGBitmapByteOrder32Big)    fprintf(stderr, "The byte order is 32BE\n");

        if (a == kCGImageAlphaNone)                  fprintf(stderr, "Alpha: kCGImageAlphaNone\n");
        if (a == kCGImageAlphaPremultipliedLast)     fprintf(stderr, "Alpha: kCGImageAlphaPremultipliedLast\n");
        if (a == kCGImageAlphaPremultipliedFirst)    fprintf(stderr, "Alpha: kCGImageAlphaPremultipliedFirst\n");
        if (a == kCGImageAlphaLast)                  fprintf(stderr, "Alpha: kCGImageAlphaLast\n");
        if (a == kCGImageAlphaFirst)                 fprintf(stderr, "Alpha: kCGImageAlphaFirst\n");
        if (a == kCGImageAlphaNoneSkipLast)          fprintf(stderr, "Alpha: kCGImageAlphaNoneSkipLast\n");
        if (a == kCGImageAlphaNoneSkipFirst)         fprintf(stderr, "Alpha: kCGImageAlphaNoneSkipFirst\n");   
}

GuidoCGraphicSystem * GuidoCCreateSVGSystem() 
{
        GuidoCGraphicSystem * system = (GuidoCGraphicSystem*) new SVGSystem();
        return system;
}

void GuidoCFreeSystem(GuidoCGraphicSystem * system) 
{
    	delete (GSystemOSX*) system;
}

void GuidoCFreeSVGSystem(GuidoCGraphicSystem * system) 
{
    	delete (SVGSystem*) system;
}

GuidoCGraphicDevice * GuidoCCreateDisplayDevice(GuidoCGraphicSystem * system) 
{
        return (GuidoCGraphicDevice*) ((VGSystem*) system)->CreateDisplayDevice();
}

GuidoCGraphicDevice * GuidoCCreateMemoryDevice(GuidoCGraphicSystem * system, int width, int height) 
{
        return (GuidoCGraphicDevice*) ((VGSystem*) system)->CreateMemoryDevice(width, height);
}

GuidoCGraphicDevice * GuidoCCreateMemoryDevicePath(GuidoCGraphicSystem * system, const char* path) 
{
        return (GuidoCGraphicDevice*) ((VGSystem*) system)->CreateMemoryDevice(path);
}

GuidoCGraphicDevice * GuidoCCreatePrinterDevice(GuidoCGraphicSystem * system) 
{
        return (GuidoCGraphicDevice*) ((VGSystem*) system)->CreatePrinterDevice();
}




// const char *byte_to_binary(int x)
// {
//     static char b[9];
//     b[0] = '\0';
//     int z;
//     for (z = 128; z > 0; z >>= 1)
//         strcat(b, ((x & z) == z) ? "1" : "0");
//     return b;
// }
