
#import <QuartzCore/QuartzCore.h>
#import <Cocoa/Cocoa.h>

#include <guido-c.h>

static CGContextRef gImageContext;
static NSImage *kResult;

void GuidoCBeginContext() {
    // CGSize size;
    // size.width  = 400;
    // size.height = 400;
    // UIGraphicsBeginImageContext(&size);
    // gImageContext = UIGraphicsGetCurrentContext();
}

void GuidoCEndContext() {
    // kResult = UIGraphicsGetImageFromCurrentImageContext();
    // UIGraphicsEndImageContext();
}
