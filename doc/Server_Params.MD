# Server Demo Application Command Line Parameters

Short Switch | Long Switch | Parameter | Description
:--- | :--- | :--- | :---
-l | --log-file | *filename* | Specify a log filename to use
N/A | --title | *string* | Specify an alternate application title
-c | --config | *filename* | Specify a configuration ini file. Default is /etc/mqtt/mqttserver.ini or mqttserver.ini in the current working directory if that is not found
N/A | --passfile | *filename* | Specify a password file. This file can be created using the server GUI.
-a | --authenticate | (none) | When specified, clients are authenticated against the password database.  Default behavior is to only authenticate clients when they connect using a username and password. 
-n | --null-clientid | (none) | Allow clients to connect with a null ClientID. When no ClientID is provided by a client, generate a unique one automatically.  The default behavior is to reject the connection if no ClientID has been provided
-s | --strict-clientid | (none) | Validate Client IDs to ensure they contain only letters and numbers and are less than 23 characters in length. Default behavior is to accepts any character or length.
N/A | --store-qos0 | (none) | Store QoS0 messages that are sent to a client while it is disconnected and transmit it when the client reconnects. The default behavior is to only store QoS1 and QoS2 messages for offline clients.
N/A | --debug | (none) | Turn on logging of debug messages
N/A | --disabled | (none) | Starts the server in disabled state. Use to create a config ini file using the GUI
-t | --tls | (none) | Enable TLS connections. Requires --pkey and --cert to be specified
N/A | --tls-only | (none) | Only listen for TLS connections
-i | --interface | *IPAddress* | Specify an IP address to listen for normal unencrypted connections on. Default is all interfaces
-p | --port | *Number* | Specify a port number to listen for normal unencrypted connections on. Default 1883
N/A | --tls-interface | *IPAddress* | Specify an IP address to listen for TLS connections on. Default is all interfaces
N/A | --tls-port | *Number* | Specify a port to listen for TLS connections on. Default is 8883
N/A | --pkey | *Filename* | Specify the filename of a private key file in PEM format for TLS
N/A | --cert | *Filename* | Specify the filename of a public key (certificate) file in PEM format for TLS
N/A | --pkey-password | *String* | Specify the password that will be used to decrypt the private key, if any. 
-k | --keepalive | *Seconds* | Specify an alternate keepalive value. Default is 30 seconds
N/A | --max-session-age | *Minutes* | Specify a maximum session age. Default is 1080 minutes
N/A | --max-subs-age | *Minutes* | Specify a maximum subscription age. Default is 1080 minutes
-q | --max-qos | *0,1,2* | Specify the maximum QoS level supported by the server. Default is QoS2
-h | --help | (none) | Display a command line parameter help message with program version info

## Security Note:

If your private key is protected by a password then you can provide a small degree of security by adding that password in the GUI interface and then saving the configuration to an INI file. This will ensure the password is saved in encrypted form. However, since the code is open source and the encryption key is stored in the code it is still not very secure. If you are going to use this feature then be sure to change the SYSTEM_PASSWORD constant in ServerFM.pas to something else.
