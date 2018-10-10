# Bond Keevil's MQTT Component Pack for Lazarus

MQTT component pack and Client/Server demo applications in FPC/Lazarus

The goal of this project is to create a Lazarus/FPC component package that 

1. Implements the MQTT protocol completely and accurately
2. Is not dependent on a specific networking component or transport mechanism 
3. Has good documentation
4. Supports SSL/TLS

For the client and server demo applications I have been using the LNet components because they seem to run well on both Windows and Linux.

There is a GUI client application and server application as well as a server command line application.

MQTT Brokers are not implemented and there is no security authentication provided by this component. You must provide your own authentication methods by implementing appropriate event handlers

## Project Status

 * The components set is complete and ready for use in projects 
 * There are mostly complete FPDocs available in the Docs directory but they are out of date
 
## Current Development Efforts
 * I am in the process of redoing the docs in Markdown.
 * I am presently adding two new components, TMQTTClientSubscription and TMQTTClientPublisher. These changes will break the code from v0.9
 * I have tested the component set with LNet's SSL implementation and it works. I am adding SSL support to the demo applications.
 * I would like to develop the demo applications into a full featured set of applications to aid in MQTT protocol debugging.

## Installation

To build this package you will need to check out and install my "bkutils" package on which this package depends. This is my general utilities package. This package provides my TBuffer class which this component set uses extensively. I hope to remove this dependency in a future release

Older versions of my bkUtils package also depend on my CryptoPKG package. The new version, which will be released shortly will depend on the DCPCrypt library. My CryptoPKG was a modified version of DCPCrypt but those modifications will be incorporated into my bkUtils package.

To build the demo applications you will also need to install LNet. For me, the only version of LNet that works is the one available in the Lazarus online package manager, I am not sure why.

## Documentation

The documentation is being rewritten in markdown. Check out the docs directory in the development branch. 

## Command line server demo application

The username/password database is not implemented. This is considered outside the scope of the component.

For help with command line options, run the program with the --help switch.

Values specified in command line options override values loaded from configuration file.

The server first looks for the file 'mqttservercli.ini' in the current working directory. If that is not found then it looks for a file '/etc/mqttservercli.ini'.

The file is read in using the Lazarus IniFile unit. 

Valid options and default values are:

```
[Server]
RequireAuthentication=False
AllowNullClientIDs=True
StrictClientIDValidation=False
ResetPAcketTimeout=2
MaxResendAttempts=3
MaxSubscriptionAge=1080
MaxSessionAge=1080
MaximumQOS=2
Host="0.0.0.0"
Port=1883
```

On Linux, the command line server application was crashing after several days of running. I have fixed one thing that might cause this problem but it needs to be verified that it is stable. 
