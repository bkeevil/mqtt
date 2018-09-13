# mqtt

MQTT component pack and Client/Server demo applications in FPC/Lazarus

The goal of this project is to create a Lazarus/FPC component package that 

1. Implements the MQTT protocol completely and accurately
2. Is not dependent on a specific networking component.  
3. Has good documentation

For the client and server demo applications I have been using the LNet components because that component seems to run well on both Windows and Linux.

There is a GUI client application and server application as well as a server command line application.

The BROKERCONNECT packet is not implemented but may be at some future date.

## Project Status

 * The components set is complete and ready for use in projects 
 * There are mostly complete FPDocs available in the Docs directory

## Installation

To build this package you will need to check out and install my "bkutils" package on which this package depends. This package provides my buffer class. At some future date I plan to remove this dependency.
