# Cardano Trace Documentation

## Table Of Contents

### [Trace Messages](#trace-messages)

1. __Node1ⓣⓢ__
    1. [BlockFromFuture](#node1blockfromfuture)
    1. [SlotIsImmutable](#node1slotisimmutable)
    1. [StartLeadershipCheck](#node1startleadershipcheck)
1. __Node2ⓣⓢ__
    1. [BlockFromFuture](#node2blockfromfuture)
    1. [SlotIsImmutable](#node2slotisimmutable)
    1. [StartLeadershipCheck](#node2startleadershipcheck)

### [Metrics](#metrics)


### [Datapoints](#datapoints)


### [Configuration](#configuration)



## Trace Messages

### Node1.BlockFromFuture


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
Filtered Invisible by config value: `Silence`

### Node1.SlotIsImmutable


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
Filtered Invisible by config value: `Silence`

### Node1.StartLeadershipCheck


> Start of the leadership check
>
> We record the current slot number.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `Stdout MachineFormat`
Filtered Invisible by config value: `Silence`

### Node2.BlockFromFuture


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
Filtered Invisible by config value: `Silence`

### Node2.SlotIsImmutable


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
Filtered Invisible by config value: `Silence`

### Node2.StartLeadershipCheck


> Start of the leadership check
>
> We record the current slot number.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `Stdout MachineFormat`
Filtered Invisible by config value: `Silence`
## Metrics


## Datapoints


##

