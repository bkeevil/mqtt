# mqtt
MQTT Client/Server in FPC/Lazarus

The goal of this project is to create a Lazarus/FPC component package that implements MQTT 3.1.1 completely and that is not dependent on a specific networking component.  

Currently this component package requires my crypto package and bkUtils package.  I plan to eliminate these dependencies in the near future.

For the client and server demo applications I am using the LNet components because that component seems to run the best on both Windows and Linux.

Currently the client and server demo applications are working but need further testing and verification against the standards before it can be considered done.  

## Installation

To build this package you will need to check out and install my "crypto" package and my "bkutils" package.
