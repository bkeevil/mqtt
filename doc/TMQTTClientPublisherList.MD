# Description

The type of the _TMQTTClient.Publishers_ property. Maintains a list of TMQTTClientPublisher components that have been instantiated and are assigned to this TMQTTClient object. This list is maintained automatically. 

Use this list to inspect or find TMQTTClientPublisher components for an MQTT client connection. _TMQTTClientPublisher_ objects are added/removed from this list by setting their _Client_ property or by calling their destructors.

## Properties

Name | Visibility | Access | Description
:--- | :---: | :---: | :---
Count | public | RO | The number of items in the list
Items[] | public | RO | An indexed array of the items in the list. Default property

## Methods

Name | Visibility | Description
:--- | :---: | :---
Add | public | Adds _AItem_ to the list. Ensures duplicates are ignored.
Remove | public | Removes _AItem_ from the list, if it exists in the list.
           
