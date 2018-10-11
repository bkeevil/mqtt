# TMQTTClientPublisher

[Back to Documentation Index](Main.MD)

_MQTTClientPublisher_ stores the parameters and data of an MQTT PUBLISH command sent from a client. The same functionality can be obtained by calling the _Publish_ method of a _TMQTTClient_ object. This component stores the parameters for such a command in the application's resource file as a convenience.

## Properties

Name | Visibility | Access | Description
:--- | :---: | :---: | :---
Enabled | published | RW | Determines whether this publisher is Enabled. Default is *true*. If set to *false* the _Publish_ method won't publish the message or trigger the _OnPublish_ event.
AutoPublish | published | RW | Boolean value. If set to *true*, any *change* to the _Data_ property will cause the component to call _Publish_. Default *false*.
Client | published | RW | The _TMQTTClient_ components used for publishing data. This property is required or the component will not be able to send messages to the server. When the _Client_ property is changed, it adds/removes the object from the corresponding _TMQTTClient.Publishers_ lists.
Topic | published | RW | A UTF-8 String representing the topic to publish to. Internally stored by a _TMQTTTokenizer_ object that enforces some validity constraints on the Topic.
Data | published | RW | A text string containing the data to be sent to the server. If the _AutoPublish_ property is *true*, then setting the Data property will cause the _Publish_ command to execute but only if the Data has changed.
QOS | published | RW | A _TMQTTQOSType_ (qtAT_MOST_ONCE, qtAT_LEAST_ONCE, qtEXACTLY_ONCE). These correspond to QOS0, QOS1, and QOS2. Determines the quality of service used to deliver the message to the server. See the MQTT specs for more info. The default is qtAT_LEAST_ONCE (QOS1).
Retain | published | RW | Whether the published message should be retained by the server. Any client subscribing to a topic will recieve any matching retained messages that match the topic filter.
Modified | public | RW | This property is set to *true* whenever any of the properties of the component are changed. Used for updating GUI displays or data stores. Set it to *false* in code to reset the flag.

## Methods

Name | Visibility | Description
:--- | :---: | :---
Create | public | Creates the component and sets its default values
Destroy | public | Destroys the component and removes it from the Client's _Publishers_ list if the _Client_ property has been assigned.
Publish | public | Calls the _Publish_ command of _Client_, passing the properties of this component as parameters to this command
Clear | public | Clears the properties of data and resets it to default values. After a call to _Clear_, the component will be in almost the same state it was as when it was created. The _Client_ property is NOT cleared. The _Modified_ property is set to *false*.
Changed | protected | Virtual method that can be overridden. In this class this method calls the _OnChanged_ event handler.
Assign | public | Assign the properties and events from another object of class _TMQTTClientPublisher_

## Events

Name | Description
:--- | :---
OnChanged | A TNotifyEvent that is called whenever any of the properties of the component are changed. Intended for notifying GUI displays or data stores about updates.
OnPublish | A TNotifyEvent that is called right before the Publish command is sent. Provides a means for applications to modify the properties of the command or enforce constraints before it is sent. Raise an EAbort exception to prevent the publishing of the message. If can also be used to provide an alternative means of publishing a message when the _Client_ property is *nil*.
