# Cardano Trace Documentation

## Table Of Contents

### [Trace Messages](#trace-messages)

1. [No ns](#no ns)
1. [No ns](#no ns)
1. [No ns](#no ns)
1. __node2\9443__
\t1. [BlockFromFuture](#node2blockfromfuture)
\t1. [SlotIsImmutable](#node2slotisimmutable)
\t1. [StartLeadershipCheck](#node2startleadershipcheck)

### [Metrics](#metrics)

### [Datapoints](#datapoints)

### [Configuration](#configuration)

## Trace Messages

__Warning__: namespace missing

> Start of the leadership check
>
> We record the current slot number.

Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`

From current configuration:

No backends found
Filtered Invisible by config value: `Silence`

__Warning__: namespace missing

> Leadership check failed: the tip of the ImmutableDB inhabits the
> current slot
>
> This might happen in two cases.
>
> 1. the clock moved backwards, on restart we ignored everything from the
>    VolatileDB since it's all in the future, and now the tip of the
>    ImmutableDB points to a block produced in the same slot we're trying
>    to produce a block in
>
> 2. k = 0 and we already adopted a block from another leader of the same
>    slot.
>
> We record both the current slot number as well as the tip of the
> ImmutableDB.
>
> See also <https://github.com/input-output-hk/ouroboros-network/issues/1462>

Severity:  `Error`
Privacy:   `Public`
Details:   `DNormal`

From current configuration:

No backends found
Filtered Invisible by config value: `Silence`

__Warning__: namespace missing

> Leadership check failed: the current chain contains a block from a slot
> /after/ the current slot
>
> This can only happen if the system is under heavy load.
>
> We record both the current slot number as well as the slot number of the
> block at the tip of the chain.
>
> See also <https://github.com/input-output-hk/ouroboros-network/issues/1462>

Severity:  `Error`
Privacy:   `Public`
Details:   `DNormal`

From current configuration:

No backends found
Filtered Invisible by config value: `Silence`

### node2.BlockFromFuture

> Leadership check failed: the current chain contains a block from a slot
> /after/ the current slot
>
> This can only happen if the system is under heavy load.
>
> We record both the current slot number as well as the slot number of the
> block at the tip of the chain.
>
> See also <https://github.com/input-output-hk/ouroboros-network/issues/1462>

Severity:  `Error`
Privacy:   `Public`
Details:   `DNormal`

From current configuration:

Backends:
      `Stdout MachineFormat`
Filtered `Visible` by config value: `Info`

### node2.SlotIsImmutable

> Leadership check failed: the tip of the ImmutableDB inhabits the
> current slot
>
> This might happen in two cases.
>
> 1. the clock moved backwards, on restart we ignored everything from the
>    VolatileDB since it's all in the future, and now the tip of the
>    ImmutableDB points to a block produced in the same slot we're trying
>    to produce a block in
>
> 2. k = 0 and we already adopted a block from another leader of the same
>    slot.
>
> We record both the current slot number as well as the tip of the
> ImmutableDB.
>
> See also <https://github.com/input-output-hk/ouroboros-network/issues/1462>

Severity:  `Error`
Privacy:   `Public`
Details:   `DNormal`

From current configuration:

Backends:
      `Stdout MachineFormat`
Filtered `Visible` by config value: `Info`

### node2.StartLeadershipCheck

> Start of the leadership check
>
> We record the current slot number.

Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`

From current configuration:

Backends:
      `Stdout MachineFormat`
Filtered `Visible` by config value: `Info`

## Metrics

## Datapoints

## Configuration

```
{
    \"TraceOptionForwarder\": {
        \"tofConnQueueSize\": 100,
        \"tofDisconnQueueSize\": 1000,
        \"tofVerbosity\": \"Minimum\"
    },
    \"TraceOptionNodeName\": null,
    \"TraceOptionPeerFrequency\": null,
    \"TraceOptionResourceFrequency\": null,
    \"TraceOptions\": {
        \"\": {
            \"severity\": \"Silence\"
        },
        \"node2\": {
            \"severity\": \"Info\"
        }
    }
}
```

6 log messages,
0 metrics,
0 datapoints.

\9443- This is the root of a tracer

\9442- This is the root of a tracer that is silent because of the current configuration

\9436- This is the root of a tracer, that provides metrics
