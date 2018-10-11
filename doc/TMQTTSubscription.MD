# Description

Represents a subscription on a client or a server. Subscriptions are normally held in sets using the TMQTTSubscriptionList class.

## Properties

Name | Visibility | Access | Description
:--- | :---: | :---: | :---
Age | public | RW | Keeps track of the age of the subscription. The server automatically ages subscriptions and deletes them if they haven't been used after a certain amount of time.
QoS | public | RW | The desired QoS level
Tokens | public | RO | The tokenizer giving access to a list of parsed Filter tokens. Exposed so applications can check if a given filter string was successfully parsed.
Filter | public | RW | The filter string for the component. Setting Filter also updates Tokens.


## Methods

Name | Visibility | Description
:--- | :---: | :---
Assign | public | If Source is a TMQTTSubscription then copy its properties into this object

