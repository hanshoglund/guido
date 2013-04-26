
#include <GUIDOEngine/GUIDOEngine.h>
#include <GUIDOEngine/GDeviceOSX.h>
#include <GUIDOEngine/GFontOSX.h>
#include <GUIDOEngine/GMemoryDeviceOSX.h>
#include <GUIDOEngine/GPrinterDeviceOSX.h>
#include <GUIDOEngine/GSystemOSX.h>

#include <guido-c.h>

// #define EXPORT __attribute__((visibility("default")))

int ThisIsAFunction() {
    GuidoInit(0);
    return 42;
}