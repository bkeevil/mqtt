# mqtt
MQTT Client/Server in FPC/Lazarus

The goal of this project is to create a Lazarus/FPC component package that implements MQTT 3.1.1 completely and that is not dependent on a specific networking component.  

For the client and server demo applications I am using the LNet components because that component seems to run the best on both Windows and Linux.

Currently the client and server demo applications are working but need further testing and verification against the standards before it can be considered done.  

I have a command line version of the server demo app about done and will be uploading that shortly.  I am developing on my own private svn server and uploading here when I have something relatively stable.

To build this package you will need to place buffers.pas, logging.pas and passwordman.pas from my lazarus repository into the mqttcomponents package and remove the dependency on the bkutils.lpk package.
