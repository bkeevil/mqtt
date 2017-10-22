unit mqttconsts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  LISTEN_RETRY_DELAY            = 15000;
  MQTT_DEFAULT_CONFIG_FILENAME1 = '/etc/mqtt/mqtt.ini';
  MQTT_DEFAULT_CONFIG_FILENAME2 = 'mqttserver.ini';
  MQTT_DEFAULT_KEEPALIVE        =   60; // Seconds.  0 to 65535
  MQTT_DEFAULT_PING_INTERVAL    =   45; // Seconds
  MQTT_RESEND_PACKET_TIMEOUT    =    2; // Seconds
  MQTT_MAX_PACKET_RESEND_TRIES  =    3;
  MQTT_MAX_SUBSCRIPTION_AGE     = 1080; // Minutes. 1080=18 hours
  MQTT_MAX_SESSION_AGE          = 1080; // Minutes. 1080=18 hours

var
  MQTTStrictClientIDValidationChars: set of char = ['0'..'9','a'..'z','A'..'Z'];

type
  //  Packet type
  TMQTTPacketType =
  (
//    mtReserved0,    //        0	Reserved
    ptBROKERCONNECT,  //        0	Broker request to connect to Broker
    ptCONNECT,        //	1	Client request to connect to Broker
    ptCONNACK,        //	2	Connect Acknowledgment
    ptPUBLISH,        //	3	Publish Packet
    ptPUBACK,         //	4	Publish Acknowledgment
    ptPUBREC,         //	5	Publish Received (assured delivery part 1)
    ptPUBREL,         //	6	Publish Release (assured delivery part 2)
    ptPUBCOMP,        //	7	Publish Complete (assured delivery part 3)
    ptSUBSCRIBE,      //	8	Client Subscribe request
    ptSUBACK,         //	9	Subscribe Acknowledgment
    ptUNSUBSCRIBE,    //        10	Client Unsubscribe request
    ptUNSUBACK,       //        11	Unsubscribe Acknowledgment
    ptPINGREQ,        //        12	PING Request
    ptPINGRESP,       //        13	PING Response
    ptDISCONNECT,     //        14	Client is Disconnecting
    ptReserved15      //        15
  );

  TMQTTQOSType =
  (
    qtAT_MOST_ONCE,   //  0 At most once Fire and Forget        <=1
    qtAT_LEAST_ONCE,  //  1 At least once Acknowledged delivery >=1
    qtEXACTLY_ONCE,   //  2 Exactly once Assured delivery       =1
    qtReserved3	      //  3 Reserved
  );

const
  MQTTPacketTypeNames : array [TMQTTPacketType] of string =
  (
    'BROKERCONNECT',	//      0	Broker request to connect to Broker
    'CONNECT',          //	1	Client request to connect to Broker
    'CONNACK',          //	2	Connect Acknowledgment
    'PUBLISH',          //	3	Publish Packet
    'PUBACK',           //	4	Publish Acknowledgment
    'PUBREC',           //	5	Publish Received (assured delivery part 1)
    'PUBREL',           //	6	Publish Release (assured delivery part 2)
    'PUBCOMP',          //	7	Publish Complete (assured delivery part 3)
    'SUBSCRIBE',        //	8	Client Subscribe request
    'SUBACK',           //	9	Subscribe Acknowledgment
    'UNSUBSCRIBE',      //      10	Client Unsubscribe request
    'UNSUBACK',         //      11	Unsubscribe Acknowledgment
    'PINGREQ',          //      12	PING Request
    'PINGRESP',         //      13	PING Response
    'DISCONNECT',       //      14	Client is Disconnecting
    'Reserved15'        //      15
  );

  MQTTQOSTypeNames : array [TMQTTQOSType] of string =
  (
    'AT MOST ONCE',   //  0 At most once Fire and Forget        <=1
    'AT LEAST ONCE',  //  1 At least once Acknowledged delivery >=1
    'EXACTLY ONCE',   //  2 Exactly once Assured delivery       =1
    'RESERVED'	      //  3	Reserved
  );

  // CONNACK RETURN CODES
  MQTT_CONNACK_SUCCESS                  = 0;
  MQTT_CONNACK_UNACCEPTABLE_PROTOCOL    = 1;
  MQTT_CONNACK_CLIENTID_REJECTED        = 2;
  MQTT_CONNACK_SERVER_UNAVAILABLE       = 3;
  MQTT_CONNACK_BAD_USERNAME_PASSWORD    = 4;
  MQTT_CONNACK_NOT_AUTHORIZED           = 5;

  // ERROR CODES
  MQTT_ERROR_NONE                         = 0;
  MQTT_ERROR_ALREADY_CONNECTED            = 101;
  MQTT_ERROR_NOT_CONNECTED                = 102;
  MQTT_ERROR_INSUFFICIENT_DATA            = 103;
  MQTT_ERROR_REMAINING_LENGTH_ENCODING    = 104;
  MQTT_ERROR_INVALID_PACKET_FLAGS         = 105;
  MQTT_ERROR_PACKET_INVALID               = 106;  // Packet was parsed successfully but failed final validation
  MQTT_ERROR_PAYLOAD_INVALID              = 107;
  MQTT_ERROR_VARHEADER_INVALID            = 108;
  MQTT_ERROR_UNACCEPTABLE_PROTOCOL        = 109;
  MQTT_ERROR_CLIENTID_REJECTED            = 110;
  MQTT_ERROR_SERVER_UNAVAILABLE           = 111;
  MQTT_ERROR_BAD_USERNAME_PASSWORD        = 112;
  MQTT_ERROR_NOT_AUTHORIZED               = 113;
  MQTT_ERROR_NO_CLIENTID                  = 114;
  MQTT_ERROR_WILLMESSAGE_INVALID          = 115;
  MQTT_ERROR_NO_PING_RESPONSE             = 116;
  MQTT_ERROR_UNHANDLED_PACKETTYPE         = 117;
  MQTT_ERROR_NO_SUBSCRIPTION_LIST         = 118;
  MQTT_ERROR_INVALID_SUBSCRIPTION_ENTRIES = 119;
  MQTT_ERROR_INVALID_RETURN_CODES         = 120;
  MQTT_ERROR_CONNECT_TIMEOUT              = 121;

  MQTT_ERROR_UNKNOWN                      = 1000;

function GetMQTTErrorMessage(ErrCode: Word): String;

implementation

function GetMQTTErrorMessage(ErrCode: Word): String;
begin
  case ErrCode of
    MQTT_ERROR_NONE                       : Result := 'Success';
    MQTT_ERROR_ALREADY_CONNECTED          : Result := 'Tried to CONNECT while a session was already connected';
    MQTT_ERROR_NOT_CONNECTED              : Result := 'First packet sent must be a CONNECT packet';
    MQTT_ERROR_INSUFFICIENT_DATA          : Result := 'Insufficient data for packet';
    MQTT_ERROR_REMAINING_LENGTH_ENCODING  : Result := 'Invalid remaining length encoding';
    MQTT_ERROR_INVALID_PACKET_FLAGS       : Result := 'Invalid packet flags';
    MQTT_ERROR_PACKET_INVALID             : Result := 'Packet was parsed successfully but failed final validation';
    MQTT_ERROR_PAYLOAD_INVALID            : Result := 'Invalid packet payload';
    MQTT_ERROR_VARHEADER_INVALID          : Result := 'Invalid variable header';
    MQTT_ERROR_UNACCEPTABLE_PROTOCOL      : Result := 'Server says protocol version is unsupported';
    MQTT_ERROR_CLIENTID_REJECTED          : Result := 'Server rejected client identifier';
    MQTT_ERROR_SERVER_UNAVAILABLE         : Result := 'Server is temporarily offline';
    MQTT_ERROR_BAD_USERNAME_PASSWORD      : Result := 'Invalid username or password';
    MQTT_ERROR_NOT_AUTHORIZED             : Result := 'Access is unauthorized';
    MQTT_ERROR_NO_CLIENTID                : Result := 'A client id is required';
    MQTT_ERROR_WILLMESSAGE_INVALID        : Result := 'The will message is invalid';
    MQTT_ERROR_NO_PING_RESPONSE           : Result := 'Connection timed out.  No ping response received from server.';
    MQTT_ERROR_UNHANDLED_PACKETTYPE       : Result := 'Unhandled packet type';
    MQTT_ERROR_NO_SUBSCRIPTION_LIST       : Result := 'No subscription list provided';
    MQTT_ERROR_INVALID_SUBSCRIPTION_ENTRIES : Result := 'Invalid entries in subscription list';
    MQTT_ERROR_INVALID_RETURN_CODES         : Result := 'Return codes are invalid';
    MQTT_ERROR_CONNECT_TIMEOUT              : Result := 'Timed out waiting for connect';
  else
    Result := 'An unknown error ocurred ('+IntToStr(ErrCode)+')';
  end;
end;

end.

