# Documentation Index

[Format of the MQTT Server INI file](Server_INI.MD)

[Server command line parameters](Server_CMD.MD)

[A checklist of MQTT 3.1.1 specification requirements](Checklist.MD)

## Source Code Documentation

Document | Description
:--- | :---
[MQTTConsts.pas](MQTTConsts.MD) | Global types, error messages and string constants
[TMQTTClient](TMQTTClient.MD) | The main MQTT client component
[TMQTTClientSubscriptionList](TMQTTClientSubscriptionList.MD) | Maintains the list of client subscriptions 
[TMQTTClientSubscription](TMQTTClientSubscription.MD) | A component that represents a client subscription
[TMQTTClientPublisherList](TMQTTClientPublisherList.MD) | Maintains the list of client publishers
[TMQTTClientPublisher](TMQTTClientPublisher.MD) | A component that represents a client publisher
[TMQTTTokenizer](TMQTTTokenizer.MD) | The topic name/filter parser and validator
[TMQTTToken](TMQTTToken.MD) | A token used by the TMQTTTokenizer
[TMQTTSubscriptionList](TMQTTSubscriptionList.MD) | A set of TMQTTSubscription objects
[TMQTTSubscription](TMQTTSubscription.MD) | Represents a subscription on the server
[MQTTPackets.pas](MQTTPackets.MD) | Defines MQTT packet base classes and utility classes. Used internally for parsing packets
[MQTTPacketDefs.pas](MQTTPacketDefs.MD) | Defines MQTT packet structure and validation constraints according to MQTT 3.1.1 
