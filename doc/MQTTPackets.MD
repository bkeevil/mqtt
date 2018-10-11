# MQTTPackets.pas

This unit defines several packet base classes and helper classes that are used by the TMQTTPacketDefs unit. This unit is primarily responsible for parsing and validating packets from an incomming data buffer.

Nothing in this unit needs to be directly accessed by users of this component package, these classes are only used internally.

This unit is required by both the client and server components.

## Base Classes

Name | Description
:--- | :---
TMQTTPacket | The abastract base class from which all other packet types descend. Introduces methods for reading/writing a packet to/from a TBuffer, and for validating the data received.
TMQTTPacketIDPacket | Descending from TMQTTPacket, this class provides PacketIDs that are used by many packet types.
TMQTTQueuedPacket | Descendants from this base class can be stored in a packet queue. Not actually a queue, it was misnamed.

## Utility Classes

Name | Description
:--- | :---
TMQTTPacketQueue | A basic list of packets. Not a Queue. Only descendants of TMQTTQueuedPacket can be stored in this list
TMQTTPacketIDManager | Issues and releases unique PacketIDs. Used by both the client and the server.
TMQTTPacketList | This class was created to hold a list of all packets that have been created. It was needed to find a bug where packets weren't getting freed. Its not required for the operation of the component set.

## See Also
[MQTTPacketDefs.pas](MQTTPacketDefs.MD)