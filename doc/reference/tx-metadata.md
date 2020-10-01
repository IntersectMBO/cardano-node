# Transaction Metadata

Transaction metadata gives us the ability to put arbitrarily structured data onto the chain. The metadata is not involved in transaction validation and is intended to be consumed by applications (i.e is application specific). It is only metadata from the point of view that it is carried along with transactions. The metadata value is kept outside the transaction body but the metadata hash is in the transaction body allowing for integrity checking and authentication of the metadata.

## Metadata Schemas/Mappings/Formats

On the chain, within transactions, the metadata is encoded as CBOR. When
building transactions the metadata can be supplied as pre-encoded CBOR.
Alternatively, metadata can be supplied as JSON which will be converted
into the internal format. We provide two different mappings between tx
metadata and JSON, useful for different purposes.
In both schemes, the top level JSON is an object indexed by integers which
are mapped to JSON values.

### No schema

```json
{
    "17802948329108123211": {},
    "945845007538436815": "0x4ebc4ea3b43bb0cc76bb326f17a30d8f",
    "1351859328329939190": {
        "0x0e": "0x3bdefda92265",
        "0x14ff8d": -1.3139667629422286119e19
    },
    "7505166164059511819": "rceHlUxXlWmZJcxYd",
    "7274669146951118819": -1.4814972676680046432e19,
    "1302243434517352162": ["UJB3",-1.6236436627090480302e19]
}
```
This mapping allows almost any JSON value to be converted into
tx metadata. This does not require a specific JSON schema for the
input but it does not expose the full representation capability of tx
metadata. In the "no schema" mapping, the idea is that (almost) any JSON can be
turned into tx metadata and then converted back, without loss. The approach for this mapping is to use the most compact tx metadata representation.
In particular:
* JSON lists and maps represented as CBOR lists and maps
* JSON strings represented as CBOR strings
* JSON hex strings with \"0x\" prefix represented as CBOR byte strings
* JSON integer numbers represented as CBOR signed or unsigned numbers
* JSON maps with string keys that parse as numbers or hex byte strings, represented as CBOR map keys that are actually numbers or byte strings.
* JSON `Null` or `Bool` are not allowed

The string length limit depends on whether the hex string representation
is used or not.
* Text string limit: 64 bytes for the UTF8
representation of the text string.
* Byte string limit: 64 bytes
for the raw byte form (**i.e. not the input hex, but after hex decoding**).

### Detailed Schema

```json
{
    "10504143639544897702": {
        "int": -1.4304053759886015514e19
    },
    "17329656595257689515": {
        "string": "yQNttsok3EQ"
    },
    "15345559452353729335": {
        "bytes": "fa1212030dd02612eccb"
    },
    "593828266493176337": {
        "list": [
            {
                "string": "HaYsLNx7"
            },
            {
                "int": -1.537136810304170744e19
            }
        ]
    },
    "17200655244803120463": {
        "map": [
            {
                "k": {
                    "map": []
                },
                "v": {
                    "string": "G"
                }
            },
            {
                "k": {
                    "string": "zNXD7qk"
                },
                "v": {
                    "list": []
                }
            }
        ]
    }
}
```

The "detailed schema" is a mapping that exposes the full representation capability of tx metadata, but relies on a specific JSON schema for the input JSON.
In the "detailed schema" mapping, the idea is that we expose the representation capability of the tx metadata in the form of a JSON schema. This means the full representation is available and can be controlled precisely. This also means any tx metadata can be converted into the JSON and back without loss. That is we can round-trip the tx metadata via the JSON and round-trip schema-compliant JSON via tx metadata. NB: `Null` and `Bool` JSON values are still not allowed.

Detailed Schema:
* "int": Any integer
* "bytes": Hexidecimal
* "string": Any valid JSON string
* "list": List of objects
* "map": List of objects with key "k" and value "v" which both contain objects.