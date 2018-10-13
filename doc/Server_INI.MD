# Server INI File Parameters

The Server INI file is created using the Lazarus IniFiles unit. There are four sections: 

* General - General application settings
* MQTT - MQTT protocol settings
* Server - Normal unencrypted TCP/IP listener settings
* SSL - Encrypted SSL/TLS session listener settings

## [General]

Parameter | Datatype | Default | Description
:--- | :---: | :--- | :---
Debug | Boolean | 0 | Set to 1 to enable debug messages in the logs 
LogFilename | String | "" | Specify a log file name or leave blank to disable logging to a file
Title | String | "" | Specify a title for the main application window or leave blank to use the default title

## [MQTT]

Parameter | Datatype | Default | Description
:--- | :---: | :--- | :---
RequireAuthentication | Boolean | False | Set to 1 to require clients to authenticate using a username and password
AllowNullClientIDs | Boolean | True | Set to 0 to prevent clients from connecting with null clientIDs
StrictClientIDValidation | Boolean | False | Set to 1 to enable strict ClientID validation
KeepAlive | Integer | 30 | Disconnect the client if it hasn't been heard from in this many seconds
MaxSessionAge | Integer | 1080 | The number of minutes to retain session information for disconnected sessions
MaxSubscriptionAge | Integer | 1080 | The number of minutes to retain subscriptions for disconnected clients
MaxResendAttempts | Integer | 3 | The number of times to try to resend a packet before giving up and failing
ResendPacketTimeout | Integer | 2 | The number of seconds to wait for a response before retransmitting a packet
MaximumQOS | Integer | 2 | The maximum QoS level provided by the server. Any packets received at a higher QoS level will be downgraded. 

## [Server]

Parameter | Datatype | Default | Description
:--- | :---: | :--- | :---
Listen | Boolean | 1 | Set to 0 to disable listening for non-SSL connections
BindAddress | String | "0.0.0.0" | Specify an interface to listen on by IP address. "0.0.0.0" means listen on all interfaces.
Port | Integer | 1883 | Specify a port to listen for incomming unencrypted connections on

## [SSL]

Parameter | Datatype | Default | Description
:--- | :---: | :--- | :---
Listen | Boolean | 0 | Set to 1 to enable listening SSL/TLS connections
BindAddress | String | "0.0.0.0" | Specify an interface to listen on by IP address. "0.0.0.0" means listen on all interfaces.
Port | Integer | 8883 | Specify a port to listen for incomming SSL/TLS encrypted connections
Key | String | "" | Specify a filename that holds the server's private key. The key should be in PEM format.
Certificate | String | "" | Specify a certificate that holds the server's certificate. This should be in PEM format.
Password | String | "" | Specify the password for decrypting the private key. Use with caution! 
