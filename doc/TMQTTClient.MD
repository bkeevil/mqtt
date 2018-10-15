# Description

This is the main component to implement a MQTT client. Drop one on a datamodule and add TMQTTClientSubscription or TMQTTClientPublisher objects to publish or subscribe to data.

## CleanSession, InitSession

If the CleanSession property is false while connecting to the server, the server will attempt to restore the previous session used by the given ClientID, which includes subscription information and any pending unsent messages. If an existing session was found and was used, the server indicates this in the CONNACK packet. Therefore clients don't always need to initialize their session if CleanSession is false. The OnInitSession event handler only fires when a session <u>has NOT been resumed</u>.

Rather than implementing an OnInitSession event handler to send subscriptions to the server, you can create TMQTTClientSubscriptions components on your form or datamodule and link them to the client. The protected method `TMQTTClient.InitSession` that triggers the OnInitSession event also looks through the list of these components (`Subscriptions: TMQTTClientSubscriptionList`) and automatically subscribes whatever it finds. 

## OnConnected, OnDisconnect, OnError

It is not necessary to implement these messages if all you want to do is log these events. Instead see the logging mechanism provided by `logging.pas`. This logging framework allows you to filter and log messages from the component to a file, CRT, A Memo, or any other arbitrary component. You can also write a bridge to forward logging messages to some other logging system that you prefer.

## Sending Data to a Transport Mechanism.

1. `Peek()` reads the data from `Client.SendBuffer` but leaves it in the buffer.
2. Whatever data is successfully sent over the transport mechanism is then removed from the buffer by issueing a `Read()` command and ignoring the results
3. The method should be invoked both by the TMQTTClient.OnSendData event and by transport mechanism events that fire when the transport mechanism can resume sending data. 

This shows why the `Socket` property exists: So the MQTT client can keep track of an arbitrary socket (Socket is a pointer to a TObject) that it can call in the OnSendData event handler.

```pascal
procedure TClientForm.ClientSendData(AClient: TMQTTClient);
begin
  TCPCanSend(Client.Socket as TLSocket);
end;    

procedure TClientForm.TCPCanSend(aSocket: TLSocket);
var
  Data: Pointer;
  Sent,Size: Integer;
begin
  Size := Client.SendBuffer.Size;
  GetMem(Data,Size);
  try
    Client.SendBuffer.Peek(Data,Size);
    Sent := TCP.Send(Data^,Size);
    Client.SendBuffer.Read(Data,Sent);
  finally
    FreeMem(Data,Size);
  end;
end;
```

## Receiving Data from the transport mechanism

When the tranport mechanism recieves data, it should implement an event handler that copies that data into the TMQTTClient.RecvBuffer and then call TMQTTClient.DataAvailable.

```pascal
procedure TClientForm.TCPReceive(aSocket: TLSocket);
var
  Data: Pointer;
  Size: Integer;
  Buffer: TBuffer;
begin
  GetMem(Data,32);
  try
    Buffer := TBuffer.Create;
    try
      repeat
        Size := aSocket.Get(Data^,32);
        if Size > 0 then
          Buffer.Write(Data,Size);
      until Size < 32;
      Client.RecvBuffer.WriteBuffer(Buffer);
      Client.DataAvailable;
    finally
      Buffer.Free;
    end;
  finally
    FreeMem(Data,32);
  end;
end;
```

##OnReceiveMessage

There are two ways to subscribe to and receive messages from the server.

1. Subscribe to topics by building a TMQTTSubscriptionList object in the OnInitSession event handler and then calling Subscribe() passing this list. In this case, you will probably need to implement an OnReceiveMessage event handler to receive these messages from the server. 
2. Just drop a TMQTTClientSubscription component onto your form or datamodule and set its properties. Use the events provided in this component to perform actions when messages matching its filter are received.

## Properties

The only properties you need to set are ClientID, Username and Password. Everything else has reasonable defaults.

Name | Visibility | Access | Description
:--- | :---: | :---: | :---
PacketIDManager | protected | RO | This class assigns randomly generated packedIDs required by the protocol and keeps track of those that have been assigned to ensure packet IDs are unique.
Socket | public | RW | A reference to an arbitrary transport mechanism socket. Any object descending from TObject can be stored here. Applications are expected to make use of this in the OnSendData event handler as per the example above. 
Publishers | public | RO | Provides access to the TMQTTClientPublisherList that this client is associated with
Subscriptions | public | RO | Provides access to the TMQTTClientSubscriptionList that this client is associated with
SendBuffer | public | RO | A buffer containing data that is ready to be sent over the transport mechanism. In the `OnSendData` event handler, your application should read data from this buffer and send it. 
RecvBuffer | public | RO | A buffer containing data that has been received by the transport mechanism and is waiting to be processed. When your transport mechanism has data available, it should write it into this buffer and then call `TMQTTClient.DataAvailable()` to process that data.
State | public | RO | The state of the MQTT connection. Can be one of `csNew,csConnecting,csConnected,csDisconnecting,csDisconnected`
ResendPacketTimeout | published | R@ | The number of seconds to wait for a response from the server before retransmitting a packet. Default=2
MaxResendAttempts | published | RW | The maximum number of times to resend a packet before giving up. Default=3
ClientID | published | RW |  A UTF8String containing the unique ClientID to be used for the connection. If the server is dropping your connection right away, first check to ensure ClientID is unique.
Username | published | RW | A UTF8String containing the Username to send as part of the CONNECT packet
Password | published | RW | An AnsiString containing the Password to send as part of the CONNECT packet
WillMessage | published | RW | A will message structure containing info about a message that the server will send out when this connection is lost or closed.
CleanSession | published | RW | Whether or not to establish a new clean session.
KeepAlive | published | RW | The keepalive value to send to the server. The server will expect to hear at least a PINGREQ message from the client within 1.5 times this KeepAlive interval and will disconnect the connection if it expires.
PingInterval | published | RW | After this number of seconds of inactivity, the client will send a PINGREQ message to the server to keep the connection alive. PingInterval needs to be less then KeepAlive obviously but the component does not enforce this constraint.

## Methods

Name | Visibility | Description
:--- | :---: | :---
Reset | public | Resets a connection so that a client can reconnect to a server. This is called internally.
Connect | public | Sends a CONNECT packet to the server, initiating the connection. Returns true when the CONNECT packet has been sent successfully. The connection is not established until a CONNACK packet is received back from the server, at which point the component will fire the OnConnected event. If this function returns false it is probably due to invalid parameters.
Disconnect | public | Send a DISCONNECT packet to the server, gracefully disconnecting the connection.
Publish | public | Send a PUBLISH packet to the server. You could use a TMQTTClientPublisher component which ultimately calls this method.
Disconnected | public | Called when the server drops the connection or the underlying transport mechanism fails. An ungraceful disconnect.
Bail | public | Called interally to drop the connection in the event of a protocol error. The MQTT protocol specs say the connection should be dropped rather than the application performing any sort of error recovery.
Subscribe | public | Pass a TMQTTSubscriptionList object containing subscriptions to this method. Multiple subscriptions can be sent in one SUBSCRIBE packet. Or you can place a TMQTTClientSubscription component on your form or datamodule and it will subscribe itself at the appropriate time.
Unsubscribe | public | Pass a TMQTTSubscriptionList object containing topics to unsubscribe from.
DataAvailable | public | This virtual method should be called by your application **after** data that has been received from the transport mechanism and has been copied into RecvBuffer.
Connected | protected | Virtual method that is the equivelant of the OnConnected event handler.
SendData | protected | Virtual method that is the equivelant of the OnSendData event handler.
InitSession | protected | Virtual method that is the equivelant of the OnInitSession event handler.
ReceiveMessage | protected | Virtual method that is the equivelant of the OnReceiveMessage event handler.

Descendant classes designed to work with specific transport mechanisms can be created by overriding Connected, SendData, InitSession and ReceiveMessage.

## Events

Name | Description
:--- | :---
OnConnected | Fires when a CONNACK packet is received from the server indicating the connection has been successfully established
OnDisconnect | Fires when the server sends a DISCONNECT package to the client to gracefully terminate the connection
OnDisconnected | Fires when there is a transport mechanism error that causes the connection to be closed ungracefully
OnInitSession | Fires when the client is expected to set up a new clean session. Does not fire in the case where an existing session has been resumed.
OnError | Fires when an error occurs
OnSendData | Fires when there is data in `SendBuffer` waiting to be sent on the transport mechanism
OnReceiveMessage | Fires when the client receives a PUBLISH message from the server (In the case of QoS0 or QoS1 messages). In the case of QoS2 messages, it fires when the PUBREL message is received. 

