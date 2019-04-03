# TMQTTClientSubscription

[Back to Documentation Index](Main.MD)

Represents a client's MQTT subscription. Subscriptions are automatically sent to the server by the _TMQTTClient_ component's _InitSession_ method that fires (if needed) during the Connection process. Duplicate topic filters are allowed. When a message is received by the client, all subscriptions with a matching topic _Filter_ have their _HandleMessage_ method called.

## Properties

Name | Visibility | Access | Description
:--- | :---: | :---: | :---
AutoSubscribe | published | RW | If true, the subscription will automatically be sent to the server when the client connects. If false then you must call Subscribe/Unsubscribe manually.
Enabled | published | RW | Is this subscription enabled. Defaults to *true*. If set to *false* then the _OnMessage_ event handler won't fire.
Filter | published | RW | The topic filter as a UTF-8 encoded string. Internally this is parsed and stored as a TMQTTTokenizer object which preforms some basic validation of the text.
QOS | published | RW | In most cases the server will send messages matching this topic filter using this requested QoS level. Default QoS is QOS1 (qtAT_LEAST_ONCE).
Client | published | RW  | The MQTTClient object that this subscription is sent over and through which matching messages are received.  This property is required or the component will not be able to communicate with the server. When the _Client_ property is changed, it adds/removes the object from the corresponding _TMQTTClient.Subscriptions_ lists.
~~Modified | public | RW | This property is set to *true* whenever any of the properties of the component are changed. Used for updating GUI displays or data stores. Set it to *false* in code to reset the flag.~~
LastUpdated | public | RO | This is set to the current timestamp whenever a message is received. Used to keep track of how long ago data was received.

## Methods

Name | Visibility | Description
:--- | :---: | :---
Create | public | Creates the component and sets its default values
Destroy | public | Destroys the component and removes it from the Client's _Subscriptions_ list if the _Client_ property has been assigned.
Clear | public | Clears the properties of data and resets it to default values. After a call to _Clear_, the component will be in almost the same state it was as when it was created. The _Client_ property is NOT cleared. The _Modified_ property is set to *false*.
Subscribe | public | Sends a SUBSCRIBE packet to the server based on the properties of this object
Unsubscribe | public | Sends an UNSUBSCRIBE packet to the server based on the properties of this object
~~Changed | protected | Virtual method that can be overridden. In this class this method calls the _OnChanged_ event handler.~~
Assign | public | Assign the properties and events from another object of class _TMQTTClientSubscription_

## Events

Name | Description
:--- | :---
~~OnChanged | A TNotifyEvent that is called whenever any of the properties of the component are changed. Intended for notifying GUI displays or data stores about updates.~~
OnMessage | This event is called when the client receives a message matching this subscriptions topic filter. All TMQTTClientSubscription components which match the message will have their OnMessage event handler triggered.
OnSendSubscription | This event is called just before this subscription is added to the list of subscriptions the client will be sending to the server. Raise an EAbort exception to cause this subscription to be ignored.
