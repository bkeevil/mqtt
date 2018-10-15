# Server INI File Parameters

The Server INI file is created using the Lazarus IniFiles unit. There are four sections: 

* General - General application settings
* MQTT - MQTT protocol settings
* Server - Normal unencrypted TCP/IP listener settings
* TLS - Encrypted SSL/TLS session listener settings

## [General]

Parameter | Datatype | Default | Description
:--- | :---: | :--- | :---
Debug | Boolean | 0 | Set to 1 to enable debug messages in the logs 
LogFilename | String | "" | Specify a log file name or leave blank to disable logging to a file
Title | String | "" | Specify a title for the main application window or leave blank to use the default title

## [MQTT]

Parameter | Datatype | Default | Description
:--- | :---: | :--- | :---
Enabled | Boolean | True | Set to 0 to start the server application in disabled mode. Used for using the application to create an INI file without starting the server.
RequireAuthentication | Boolean | False | Set to 1 to require clients to authenticate using a username and password
AllowNullClientIDs | Boolean | False | Set to 1 to allow clients to connect with null clientIDs. This will cause them to be assigned a randomly generated clientID.
StrictClientIDValidation | Boolean | False | Set to 1 to enable strict ClientID validation
StoreOfflineQoS0Messages | Boolean | False | Set to 1 to store QoS0 messages dispatched to offline clients for when they reconnect
KeepAlive | Integer | 30 | Disconnect the client if it hasn't been heard from in 1.5 times this many seconds
MaxSessionAge | Integer | 1080 | The number of minutes to retain session information for disconnected sessions
MaxSubscriptionAge | Integer | 1080 | The number of minutes to retain subscriptions for disconnected clients
MaximumQOS | Integer | 2 | The maximum QoS level provided by the server. Any packets received at a higher QoS level will be downgraded. 

## [Server]

Parameter | Datatype | Default | Description
:--- | :---: | :--- | :---
Listen | Boolean | 1 | Set to 0 to disable listening for non-SSL connections
Interface | String | "0.0.0.0" | Specify an interface to listen on by IP address. "0.0.0.0" means listen on all interfaces.
Port | Integer | 1883 | Specify a port to listen for incomming unencrypted connections on

## [TLS]

Parameter | Datatype | Default | Description
:--- | :---: | :--- | :---
Listen | Boolean | 0 | Set to 1 to enable listening TLS connections
Interface | String | "0.0.0.0" | Specify an interface to listen on by IP address. "0.0.0.0" means listen on all interfaces.
Port | Integer | 8883 | Specify a port to listen for incomming TLS encrypted connections
Key | String | "" | Specify a filename that holds the server's private key. The key should be in PEM format.
Certificate | String | "" | Specify a certificate that holds the server's certificate. This should be in PEM format.
PasswordEnc | String | "" | Specifies a password for decrypting the private key. Stored in encrypted form. Use the GUI to set it and save a configuration file
