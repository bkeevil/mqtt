# Description

This file contains basic types, strings and a basic mechanism for localizing the component. If anyone wants error messages in another language they ought to be able to do so by customizing this file.

It used to contain configuration constant values but those have since been moved to appropriate classes. i.e. The file is named mqttconsts.pas for historical reasons.

## Types

Add an additional language to this enumeration type and then add appropriate functions in the implementation section to return strings in that language.

```pascal
type TMQTTLanguagePack = (lpEnglish);
```

The MQTT connection state. Should be self-explanatory.

```pascal
type TMQTTConnectionState = (csNew,csConnecting,csConnected,csDisconnecting,csDisconnected);
```

The MQTT packet type. See the MQTT standards for descriptions.

```pascal
type TMQTTPacketType = (mtReserved0,ptBROKERCONNECT,ptCONNECT,ptCONNACK,ptPUBLISH,ptPUBACK,ptPUBREC,
  ptPUBREL,ptPUBCOMP,ptSUBSCRIBE,ptSUBACK,ptUNSUBSCRIBE,ptUNSUBACK,ptPINGREQ,ptPINGRESP,ptDISCONNECT,ptReserved15);
```  

The MQTT QoS types. See MQTT standards for descriptions.

```pascal
type TMQTTQOSType = (qtAT_MOST_ONCE,qtAT_LEAST_ONCE,qtEXACTLY_ONCE,qtReserved3);
```

## Variables

Setting this variable selects the default language. It can be changed at runtime:

`var MQTTLanguagePack: TMQTTLanguagePack = lpEnglish;`

## Functions

Returns display text for a log message in the selected language.

`function GetLogMessage(MessageCode: Word): String;`

Returns display text for a numeric MQTT error code in the selected language.

`function GetMQTTErrorMessage(ErrCode: Word): String;`

Returns display text in the selected language for a connection state name:

`function GetConnectionStateName(ConnectionState: TMQTTConnectionState): String;`

Returns display text for a packet type name. This function is not localizeable as the names are defined by the MQTT standards:

`function GetPacketTypeName(PacketType: TMQTTPacketType): String;`

Returns display text in the selected language for a QOS type name:

`function GetQOSTypeName(QOSType: TMQTTQOSType): String;`

