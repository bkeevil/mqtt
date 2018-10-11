# Description

Holds a set of subscriptions use by both _TMQTTServers_ and _TMQTTClients_ and performs set operations on these lists of subscriptions. 

Clients can subscribe to topics either by creating _TMQTTClientSubscription_ components and linking them to _TMQTTClient_, or by constructing a _TMQTTSubscriptionList_ object and passing it to _TMQTTClient.Subscribe()_

Server sessions use TMQTTSubscriptionList to keep track of what topics each client is subscribed to.

## Properties

Name | Visibility | Access | Description
:--- | :---: | :---: | :---
Count | public | RO | The number of subscriptions in the Items[] property
Items[] | public | RO | An indexed array of TMQTTSubscription objects


## Methods

Name | Visibility | Description
:--- | :---: | :---
Assign | public | If Source is a TMQTTSubscriptionList object, then clear the list and copy the items from _Source_
Clear | public | Clears the list of subscriptions
Find | public | Find a subscription by Filter string. Returns nil if nothing found.
New | public | Factory method to create a new TMQTTSubscription object and add it to the list. Use this method to quickly construct a Subscription list.
Update | public | Find a subscription object with a matching Filter and replace it with the object provided. Free's the replaced object.
Remove | public | Remove a subscription from the list and free it from memory
Delete | public | Delete the subscription at the indexth position in the list and free it from memory
MergeList | public | Merge's another subscription list into this list without creating duplicates. If two subscriptions share the same Filter text then the existing one is replaced by the one provided.
DeleteList | public | Delete's a list of subscriptions from this subscription list. Any subscription with a matching Filter are removed and destroyed.
RemoveDuplicates | public | Removes duplicate subscriptions, preserving the copy with the highes QoS value. Must be called after RemoveInvalidSubscriptions. Returns the number of duplicate subscriptions removed.
RemoveInvalidSubscriptions | public | Remove any subscription that could not be parsed into tokens by the _TMQTTTokenizer_ or that contain invalid tokens according to the MQTT specifications. Returns the number of invalid subscriptions removed.
