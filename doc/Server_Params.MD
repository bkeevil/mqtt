# Server Demo Application Command Line Parameters

Short Switch | Long Switch | Parameter | Description
:--- | :--- | :--- | :---
-l | --log-file | *filename* | Specify a log filename to use
N/A | --title | *string* | Specify an alternate application title
-c | --config | *filename* | Specify a configuration ini file. Default is /etc/mqtt/mqttserver.ini or mqttserver.ini in the current working directory if that is not found
-a | --authenticate | (none) | When specified, clients are authenticated against the password database.  Default is no authentication
-n | --null-clientid | (none) | When no ClientID is provided by a client, generate a unique one automatically.  Default rejects connection
-s | --strict-clientid | (none) | Validate Client IDs to ensure they contain only letters and numbers. Default accepts any character
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
N/A | --pkey-password | *String* | Specify the password that will be used to decrypt the private key, if any. Use with caution obviously.
-k | --keepalive | *Seconds* | Specify an alternate keepalive value. Default is 30 seconds
N/A | --max-session-age | *Minutes* | Specify a maximum session age. Default is 1080 minutes
N/A | --max-subs-age | *Minutes* | Specify a maximum subscription age. Default is 1080 minutes
-q | --max-qos | *0,1,2* | Specify the maximum QoS level supported by the server. Default is QoS2
-h | --help | (none) | Display a command line parameter help message with program version info
