# Description

The type of the _TMQTTClient.Subscriptions_ property. Maintains a list of TMQTTClientSubscription components that have been instantiated and are assigned to this TMQTTClient object. This list is maintained automatically. 

When a session is established and it is a clean session or the server indicates it did not find an existing connection in its datastores, then the Client.InitSession method is called. This method uses the Subscriptions property to automatically subscribe to various topic filters.

Use this list to inspect or find TMQTTClientSubscription components for an MQTT client connection. _TMQTTClientSubscription_ objects are added/removed from this list by setting their _Client_ property or by calling their destructors.

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
           
