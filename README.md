# Bond Keevil's MQTT Component Pack for Lazarus

MQTT client and server component pack with demo applications written in Lazarus/FPC.

Project Goals: 

1. Implements the MQTT protocol version 3.1.1 completely and accurately
2. Implements all QoS levels
3. Is not dependent on a specific networking component or transport mechanism 
4. Has good [documentation](docs/Main.MD)

For the client and server demo applications I have been using the LNet components because they seem to run well on both Windows and Linux.

There is a GUI client application and server application as well as a server command line application. These demonstrate the features of the component set and are useful for testing MQTT based applications.

MQTT Brokers are not implemented and there is no security authentication provided by this component. You must provide your own authentication methods by implementing appropriate event handlers

## Project Milestones

**Version 0.9 (CURRENT RELEASE)**
 * First release of the code that can be considered stable.
 
**Version 0.95 is in progress and will add:**
 * Rough draft of documentation written in markdown (i.e. available on the Github Wiki pages for this project).
 * I am presently adding two new palette bar components, [TMQTTClientSubscription](docs/TMQTTClientSubscription.MD) and [TMQTTClientPublisher](docs/TMQTTClientPublisher.MD).
 * Added SSL/TLS support to the demo applications.
 * Remove the dependencies on my other packages.
 
**Version 1.0 will be the first major release of this component set. In this release I want to have:**

1. Figure out how to include the component set in the online package manager.
2. Go through the MQTT 3.1.1 specs one final time to verify everything has been implemented correctly.
3. Final draft of documentation
 
## Installation

To build this package you will need to check out and install my "bkutils" package on which this package depends. This is my general utilities package. This package provides my TBuffer class which this component set uses extensively. I hope to remove this dependency in a future release

Older versions of my bkUtils package also depend on my CryptoPKG package. The new version, which will be released shortly will depend on the DCPCrypt library. My CryptoPKG was a modified version of DCPCrypt but those modifications will be incorporated into my bkUtils package.

To build the demo applications you will also need to install LNet. For me, the only version of LNet that works is the one available in the Lazarus online package manager, I am not sure why.

## Documentation

The documentation is being rewritten in markdown and is [available in the docs directory](docs/Main.MD).

## License

This component is released under the GNU LGPL license agreement. You may share it, extend it, or use it for any commercial purposes provided you share any improvements you make to the code. It is not necessary to distribute this source code with your binaries.
