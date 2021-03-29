codec_sequencer
===============

An OTP library for sequencing the encoding/decoding of structured data.

Build
-----

    $ rebar3 compile


Overview
--------

Given a callback to encode/decode opaque data elements identified by a tag name,
this library manage the organization of these elements and the way they are
packed/unpacked into list, maps and records.

This is done by defining a specification that is used for either encoding or
decoding data.

e.g.

Given encoding/decoding callback supporting 'bool' and 'char', the following
specification pack/unpack a data composed of bool and char element into a map:

	```
	Spec = map(required, #{}, [
        codec(required, bool, foo),
        codec(optional, bool, buz),
        codec(required, char, bar)
    ])
    ```


Specification
-------------


Codec
.....


Objects
.......


List
....


Option
......


Group
.....


Loop
....