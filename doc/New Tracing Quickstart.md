# New Tracing Quickstart

From now on old-tracing (based on iohk-monitoring-framework) exists
together with new-tracing (based on trace-dispatcher and cardano-tracer) for a while.

In this transition time new-tracing can be tested and improved. Since we have several hundred trace messages
it is expected that you will find regressions and bugs in the port, please help to find
and fix them.

In this transition time new tracing will for technical reason have a restricted functionality,
as e.g. the reconfiguration of a running node is not available.

To switch to new tracing set the value `UseTraceDispatcher` to true. If you do this, the
config file needs to contain several values for the configuration of new-tracing, as we
describe next.

The current tracing system has two ways to identify the message, a hierarchical name we
call it's `Namespace` and the `Kind field` in machine representation. We base our implementation
on the namespace, and require a one-to-one correspondence between namespaces and messages (bijective mapping).

As we have two mechanisms for the same purpose for historic reasons, we will soon
__deprecate the Kind field__, and it will disappear in the near future. So we strongly
advice to use namespaces for any analysis tools of traces!  

### Configuration of new tracing

1. Specify a filter for the severity of the messages you want to see, e.g.:

  ~~~yaml
  # Show messages of Severity Notice or higher as default
  Node:
      severity: Notice

    # But show ChainDB messages starting from Info
  Node.ChainDB:
      severity: Info
  ~~~


  So the namespaces are used for configuration values, which works
  down to individual messages, and the more specialized value overwrites the more general.

  If you don't want to see any messages from tracers the new severity `Silence`
  exists, which suppresses all messages.


2. Specify in which detail level, the messages get shown.

  ~~~yaml
  Node:
      # Keep this
      severity: Notice      
      # All messages are shown with normal detail level
      detail: DNormal
  ~~~

  Other options would be DMinimal, DDetailed and DMaximum. This has only an effect on messages which support the representation in different ways.

3. Specify limiters for the frequency of messages

    Eliding tracers are not supported in new-tracing, instead you can limit the
    frequency in which messages get shown.

    ~~~yaml
    Node.ChainDB.AddBlockEvent.AddedBlockToQueue:
        # Only show a maximum of 2 of these messages per second
        maxFrequency: 2.0
    ~~~

    The activity of limiters will be written in the traces as well.

4. Specify the backends the messages are routed to.

  ~~~yaml
  Node:
      # Keep this
      severity: Notice      
      # And this
      detail: DNormal  
      # And specify a list of backends to use
      backends:
        - Stdout MachineFormat
        - EKGBackend
        - Forwarder
  ~~~

  These are all the backends currently supported. With Stdout you have the
  options MachineFormat or HumanFormatColoured/HumanFormatUncoloured.
  If messages don't support representation in HumanFormat* they are shown in MachineFormat anyway.

  Forwarder means that messages are send to cardano-tracer

Configuration can be written in JSON and YAML, we have shown the examples in YAML.

### Configuration and use of cardano-tracer

`cardano-tracer` is a part of the new tracing infrastructure. It is a separate service that accepts different messages from the node and handles them.
So it is assumed that if you want to use the new tracing infrastructure - you will use `cardano-tracer`. Please read its [documentation](https://github.com/input-output-hk/cardano-node/blob/master/cardano-tracer/docs/cardano-tracer.md) for more details.

This example describes the simplest case, when the node and `cardano-tracer` on the same machine.

First of all, add `TraceOptionForwarder` section in the node's configuration, this section specifies how the node should work with `cardano-tracer`:

~~~yaml
TraceOptionForwarder:
    mode: Initiator
~~~

Then build and run `cardano-tracer`:

~~~shell
$ cabal build cardano-tracer && cabal install cardano-tracer --installdir=PATH_TO_DIR --overwrite-policy=always
$ cd PATH_TO_DIR
$ ./cardano-tracer --config PATH_TO_CONFIG
~~~

where `PATH_TO_CONFIG` is a path to tracer's configuration file. This is an example of such a configuration:

~~~yaml
---
network:
  tag: AcceptAt
  contents: "/tmp/forwarder.sock"
logging:
- logRoot: "/tmp/cardano-tracer-logs"
  logMode: FileMode
  logFormat: ForMachine
~~~

That's it. After you run the node, it will establish the connection with `cardano-tracer` and will start to forward messages to it.
As a result, you will find log files, in JSON format, in `/tmp/cardano-tracer-logs` directory.

### Development during transition-time

For developing tracers in the transition time we suggest:

1. Don't use strictness annotations for trace types. Trace messages are either
discarded immediately (which happens frequently) or instantly converted to another format
but never stored. So strictness annotations make the code less efficient without any benefit.
As well it doesn't play well together with the required prototypes of messages in the
new framework.

2. If you develop new tracers we suggest that you develop the new tracers first,
and then map to old tracers, as the new tracers will survive. You will find plenty of
examples in cardano-node under Cardano.Node.Tracing.Tracers.

3. Contact the benchmarking team or node-logging channel for any questions and reviews.

### Documentation of trace messages and further documentation

This is a document which is regenerated periodically and documents all trace-messages,  metrics and data-points in cardano-node. It as well displays the handling of these
messages with the current default configuration:

[Cardano Trace Documentation](https://github.com/input-output-hk/cardano-node/blob/master/doc/new-tracing/tracers_doc_generated.md)

This document describes the underlying library trace-dispatcher:

[trace-dispatcher: efficient, simple and flexible program tracing](https://github.com/input-output-hk/cardano-node/blob/master/trace-dispatcher/doc/trace-dispatcher.md)

This document describes a seperate service for logging and monitoring Cardano nodes:

[Cardano Tracer](https://github.com/input-output-hk/cardano-node/blob/master/cardano-tracer/docs/cardano-tracer.md)
