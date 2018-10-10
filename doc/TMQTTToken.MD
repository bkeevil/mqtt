## Description

Represents a token in a filter or topic string and validates it as per the MQTT specs.

## Types

The type of the token that has been parsed. If the type is tkValid then the Text property contains the text of the token.

`type TMQTTTokenKind = (tkInvalid,tkValid,tkMultiLevel,tkSingleLevel);`

## Properties

Name | Visibility | Access | Description
:--- | :---: | :---: | :---
Kind | public | RO | The type of this token. See _TMQTTTokenKind_
Text | public | RO | The text of this token if Kind = tkValid, undefined otherwise.

## Methods

These methods are both private. They are used by the [TMQTTTokenizer][TMQTTTokenizer.MD] class.

Name | Visibility | Description
:--- | :---: | :---
ValidateTopicName | private | An empty string is valid. Any token containing a '+' or '#' is invalid.
ValidateTopicFilter | private | An empty string is valid. Any token not containing a special car is valid. The hash and plus chars must appear on their own. The hash char must be the last token in the list.

## See Also

[TMQTTTokenizer](TMQTTTokenizer.MD)
