## Description

The TMQTTTokenizer class parses a filter or topic string into tokens and validates it. The parsing and validation operation is initiated by the constructor.

## Example

```pascal
  Tokens := TMQTTTokenizer.Create('some/mqtt/path/#',true);`
  if Tokens.Valid then
    ShowMessage(Tokens.AsString);
```

## Properties

Name | Visibility | Access | Description
:--- | :---: | :---: | :---
Valid | public | RO | Returns True if all the tokens in the list are valid
Count | public | RO | The number of tokens in the Items[] array
Items[] | public | RO | The list of parsed tokens, which are TMQTTToken objects

## Methods

Name | Visibility | Description
:--- | :---: | :---
Create | public | Constructs a tokenizer and parses Str, which is a filter or topic string. If Filter is true then Str is validated as a Filter string for a subscription. If Filter is false then Str is validated as a topic string for a PUBLISH message.
Destroy | public | Destroys the Tokenizer and any tokens it contains
AsString | public | Reconstructs a text representation of a topic or a filter string from the tokens

## See Also

[TMQTTToken](TMQTTToken.MD)
