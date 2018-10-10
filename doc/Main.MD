# Bond Keevil's MQTT Package

Implements the MQTT protocol version 3.1.1 in a way that is transport mechanism and authentication mechanism agnostic.

## License

This component is being released under the GNU GPL license agreement. You may share it, extend it, or use it for any commercial purposes provided you share any improvements you make to the code.

## Dependencies

This package depends on the logging.pas and buffers.pas units in my bkUtils package. 

The logging framework can be used to obtain and filter error, warning, info and debug messages sent by this component package. The TBuffer object provides FIFO buffers that are used to buffer communications to/from from your transport mechanism and this component set. 

## Status

All components are believed to be stable and tested and I am using them in projects. There were some interface changes between versions 0.8x and version 1.0 that may break some people's code.

## General Client Useage

Place a TMQTTClient on your datamodule and follow the examples of the MQTT client application to implement the events that enable the component to send and receive data through a transport mechanism. In the demo apps, the transport mechanism is the lnet component set.

Set the properties for the client and your transport mechanism and test that it works.

For each subscription you wish to create, place a TMQTTClientSubscription component on the datamodule and set its properties. These subscriptions will be automatically sent to the server when needed.

Alternatively, you can implement the OnInitSession event handler, manually create a TMQTTSubscriptionList object at runtime, and call Subscribe() with this list.

For each message you wish to publish, you can either call the TMQTTClient.Publish() method directly or use TMQTTPublisher components provided.
 
## General Server Useage

Follow the examples in the demo applications. 

There is currently no validation of usernames/passwords, your server must provide these using the component's event handlers. 

## Documentation

There are older docs in the FPDocs format that are mixed in with the newer documentation in the docs directory. The newer documentation will be done in Markdown. I intend to delete the FPDocs stuff once this migration is complete.

Document | Description
:--- | :---
[MQTTConsts.pas](MQTTConsts.MD) | Global types, error messages and string constants
[TMQTTClient](TMQTTClient.MD) | The main MQTT client component
[TMQTTClientSubscriptionList](TMQTTClientSubscriptionList.MD) | Maintains the list of client subscriptions 
[TMQTTClientSubscription](TMQTTClientSubscription) | A component that represents a client subscription
[TMQTTClientPublisherList](TMQTTClientPublisherList.MD) | Maintains the list of client publishers
[TMQTTClientPublisher](TMQTTClientPublisher.MD) | A component that represents a client publisher
[TMQTTTokenizer](TMQTTTokenizer.MD) | The topic name/filter parser and validator.
[TMQTTToken](TMQTTToken.MD) | A token used by the TMQTTTokenizer
[TMQTTSubscriptionList](TMQTTSubscriptionList.MD) | A set of TMQTTSubscription objects
[TMQTTSubscription](TMQTTSubscription.MD) | Represents a subscription on the server


