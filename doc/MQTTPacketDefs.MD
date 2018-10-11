# MQTTPacketDefs.pas

This unit contains the definitions of various packet formats used by the protocol. It also provides the TMQTTWillMessage class used by the TMQTTCONNECTPacket. Other than the structure of this WillMessage, nothing in this unit needs to be understood by component users.

## TMQTTWillMessage

This is a property of the client. It specifies a message that the server will send out when its connection with a client is lost.

```pascal
  TMQTTWillMessage = class(TPersistent)
    public
      procedure Clear;
      procedure Assign(ASource: TPersistent); override;
      function DisplayText: String;
      function AsString: String;
      property Retain: Boolean read FRetain write FRetain;
      property QOS: TMQTTQOSType read FQOS write FQOS;
      property Enabled: Boolean read FEnabled write FEnabled;
      property Topic: UTF8String read FTopic write FTopic;
      property Message: UTF8String read FMessage write FMessage;
  end;
```

The `DisplayText` and `AsString` methods are for used for logging and debugging purposes. Calling `Clear` will reset the object to its initial state. 

## Packet Classes

The following packet classes are defined, which should correspond to the packets defined by the MQTT 3.1.1 protocol specifications.

```pascal
TMQTTCONNECTPacket = class(TMQTTPacket)
TMQTTCONNACKPacket = class(TMQTTPacket)
TMQTTPUBLISHPacket = class(TMQTTQueuedPacket)
TMQTTPUBACKPacket = class(TMQTTPacketIDPacket)
TMQTTPUBRECPacket = class(TMQTTQueuedPacket)
TMQTTPUBRELPacket = class(TMQTTQueuedPacket)
TMQTTPUBCOMPPacket = class(TMQTTPacketIDPacket)
TMQTTSUBSCRIBEPacket = class(TMQTTQueuedPacket)
TMQTTSUBACKPacket = class(TMQTTPacketIDPacket)
TMQTTUNSUBSCRIBEPacket = class(TMQTTQueuedPacket)
TMQTTUNSUBACKPacket = class(TMQTTPacketIDPacket)
TMQTTPINGREQPacket = class(TMQTTPacket)
TMQTTPINGRESPPacket = class(TMQTTPacket)
TMQTTDISCONNECTPacket = class(TMQTTPacket)
```
## See Also
[MQTTPackets.pas](MQTTPackets.MD)