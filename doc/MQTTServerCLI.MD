# Command line server demo application

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
