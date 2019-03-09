// Borrowed liberally (as allowed by license) from
// http://developer.apple.com/samplecode/SerialPortSample/index.html
// This really is a slash-and-burn; no attempt was to make it very "C++-iike"

// Apple's copyright blob:
/*
    Copyright:		© Copyright 2000-2005 Apple Computer, Inc. All rights reserved.

    Disclaimer:		IMPORTANT:  This Apple software is supplied to you by Apple Computer, Inc.
					("Apple") in consideration of your agreement to the following terms, and your
					use, installation, modification or redistribution of this Apple software
					constitutes acceptance of these terms.  If you do not agree with these terms,
					please do not use, install, modify or redistribute this Apple software.

					In consideration of your agreement to abide by the following terms, and subject
					to these terms, Apple grants you a personal, non-exclusive license, under AppleÕs
					copyrights in this original Apple software (the "Apple Software"), to use,
					reproduce, modify and redistribute the Apple Software, with or without
					modifications, in source and/or binary forms; provided that if you redistribute
					the Apple Software in its entirety and without modifications, you must retain
					this notice and the following text and disclaimers in all such redistributions of
					the Apple Software.  Neither the name, trademarks, service marks or logos of
					Apple Computer, Inc. may be used to endorse or promote products derived from the
					Apple Software without specific prior written permission from Apple.  Except as
					expressly stated in this notice, no other rights or licenses, express or implied,
					are granted by Apple herein, including but not limited to any patent rights that
					may be infringed by your derivative works or by other works in which the Apple
					Software may be incorporated.

					The Apple Software is provided by Apple on an "AS IS" basis.  APPLE MAKES NO
					WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE IMPLIED
					WARRANTIES OF NON-INFRINGEMENT, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
					PURPOSE, REGARDING THE APPLE SOFTWARE OR ITS USE AND OPERATION ALONE OR IN
					COMBINATION WITH YOUR PRODUCTS.

					IN NO EVENT SHALL APPLE BE LIABLE FOR ANY SPECIAL, INDIRECT, INCIDENTAL OR
					CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
					GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
					ARISING IN ANY WAY OUT OF THE USE, REPRODUCTION, MODIFICATION AND/OR DISTRIBUTION
					OF THE APPLE SOFTWARE, HOWEVER CAUSED AND WHETHER UNDER THEORY OF CONTRACT, TORT
					(INCLUDING NEGLIGENCE), STRICT LIABILITY OR OTHERWISE, EVEN IF APPLE HAS BEEN
					ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

*/

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <errno.h>
#include <paths.h>
#include <termios.h>
#include <sysexits.h>
#include <sys/param.h>
#include <sys/select.h>
#include <sys/time.h>
#include <time.h>
#include <AvailabilityMacros.h>

#include <CoreFoundation/CoreFoundation.h>

#include <IOKit/IOKitLib.h>
#include <IOKit/serial/IOSerialKeys.h>
#if defined(MAC_OS_X_VERSION_10_3) && (MAC_OS_X_VERSION_MIN_REQUIRED >= MAC_OS_X_VERSION_10_3)
#include <IOKit/serial/ioss.h>
#endif
#include <IOKit/IOBSD.h>

#include "mainwindow.h"

// Function prototypes
static kern_return_t FindModems(io_iterator_t* matchingServices);
// static kern_return_t GetModemPath(io_iterator_t serialPortIterator, char *bsdPath, CFIndex maxPathSize);
// static int OpenSerialPort(const char *bsdPath);

// Returns an iterator across all known modems. Caller is responsible for
// releasing the iterator when iteration is complete.
static kern_return_t FindModems(io_iterator_t* matchingServices)
{
  kern_return_t			kernResult;
  CFMutableDictionaryRef	classesToMatch;

  /*! @function IOServiceMatching
      @abstract Create a matching dictionary that specifies an IOService class match.
      @discussion A very common matching criteria for IOService is based on its class. IOServiceMatching will create a matching dictionary that specifies any IOService of a class, or its subclasses. The class is specified by C-string name.
      @param name The class name, as a const C-string. Class matching is successful on IOService's of this class or any subclass.
      @result The matching dictionary created, is returned on success, or zero on failure. The dictionary is commonly passed to IOServiceGetMatchingServices or IOServiceAddNotification which will consume a reference, otherwise it should be released with CFRelease by the caller. */

  // Serial devices are instances of class IOSerialBSDClient
  classesToMatch = IOServiceMatching(kIOSerialBSDServiceValue);
  if (classesToMatch == NULL) {
    printf("IOServiceMatching returned a NULL dictionary.\n");
  } else {
    /*!
    	@function CFDictionarySetValue
    	Sets the value of the key in the dictionary.
    	@param theDict The dictionary to which the value is to be set. If this
    		parameter is not a valid mutable CFDictionary, the behavior is
    		undefined. If the dictionary is a fixed-capacity dictionary and
    		it is full before this operation, and the key does not exist in
    		the dictionary, the behavior is undefined.
    	@param key The key of the value to set into the dictionary. If a key
    		which matches this key is already present in the dictionary, only
    		the value is changed ("add if absent, replace if present"). If
    		no key matches the given key, the key-value pair is added to the
    		dictionary. If added, the key is retained by the dictionary,
    		using the retain callback provided
    		when the dictionary was created. If the key is not of the sort
    		expected by the key retain callback, the behavior is undefined.
    	@param value The value to add to or replace into the dictionary. The value
    		is retained by the dictionary using the retain callback provided
    		when the dictionary was created, and the previous value if any is
    		released. If the value is not of the sort expected by the
    		retain or release callbacks, the behavior is undefined.
    */
//        CFDictionarySetValue(classesToMatch,
//                             CFSTR(kIOSerialBSDTypeKey),
//                             CFSTR(kIOSerialBSDModemType));

    // Each serial device object has a property with key
    // kIOSerialBSDTypeKey and a value that is one of kIOSerialBSDAllTypes,
    // kIOSerialBSDModemType, or kIOSerialBSDRS232Type. You can experiment with the
    // matching by changing the last parameter in the above call to CFDictionarySetValue.

    // As shipped, this sample is only interested in modems,
    // so add this property to the CFDictionary we're matching on.
    // This will find devices that advertise themselves as modems,
    // such as built-in and USB modems. However, this match won't find serial modems.
  }

  /*! @function IOServiceGetMatchingServices
      @abstract Look up registered IOService objects that match a matching dictionary.
      @discussion This is the preferred method of finding IOService objects currently registered by IOKit. IOServiceAddNotification can also supply this information and install a notification of new IOServices. The matching information used in the matching dictionary may vary depending on the class of service being looked up.
      @param masterPort The master port obtained from IOMasterPort().
      @param matching A CF dictionary containing matching information, of which one reference is consumed by this function. IOKitLib can contruct matching dictionaries for common criteria with helper functions such as IOServiceMatching, IOOpenFirmwarePathMatching.
      @param existing An iterator handle is returned on success, and should be released by the caller when the iteration is finished.
      @result A kern_return_t error code. */

  kernResult = IOServiceGetMatchingServices(kIOMasterPortDefault, classesToMatch, matchingServices);
  if (KERN_SUCCESS != kernResult) {
    printf("IOServiceGetMatchingServices returned %d\n", kernResult);
    goto exit;
  }

exit:
  return kernResult;
}

// Given an iterator across a set of modems, return the BSD path to the first one.
// If no modems are found the path name is set to an empty string.
static kern_return_t GetModemPath(io_iterator_t serialPortIterator, char* bsdPath, CFIndex maxPathSize, QComboBox* box)
{
  io_object_t		modemService;
  kern_return_t	kernResult = KERN_FAILURE;
  Boolean			modemFound = false;

  // Initialize the returned path
  *bsdPath = '\0';

  // Iterate across all modems found. In this example, we bail after finding the first modem.

  while ((modemService = IOIteratorNext(serialPortIterator)) && !modemFound) {
    CFTypeRef	bsdPathAsCFString;

    // Get the callout device's path (/dev/cu.xxxxx). The callout device should almost always be
    // used: the dialin device (/dev/tty.xxxxx) would be used when monitoring a serial port for
    // incoming calls, e.g. a fax listener.

    bsdPathAsCFString = IORegistryEntryCreateCFProperty(modemService,
                        CFSTR(kIOCalloutDeviceKey),
                        kCFAllocatorDefault,
                        0);
    if (bsdPathAsCFString) {
      Boolean result;

      // Convert the path from a CFString to a C (NUL-terminated) string for use
      // with the POSIX open() call.

      result = CFStringGetCString((const __CFString*) bsdPathAsCFString,
                                  bsdPath,
                                  maxPathSize,
                                  kCFStringEncodingUTF8);
      CFRelease(bsdPathAsCFString);

      if (result) {
        box->addItem(bsdPath);
      }
    }

    // Release the io_service_t now that we are done with it.

//		(void) IOObjectRelease(modemService);
  }

  return kernResult;
}

#include "mainwindow.h"
void MainWindow::osLoadDeviceNameCombos(QComboBox* box)
{
  kern_return_t       kernResult;
  io_iterator_t       serialPortIterator;
  char                bsdPath[MAXPATHLEN];

  kernResult = FindModems(&serialPortIterator);
  kernResult = GetModemPath(serialPortIterator, bsdPath, sizeof(bsdPath), box);

}
