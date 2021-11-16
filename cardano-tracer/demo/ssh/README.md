# SSH Local Forwarding Demo

Run `./run.sh PASS FMODE`, where `PASS` is your ssh password for `localhost`, and `FMODE` is forwarder's mode: `Responder` or `Initiator`.

As a result, `demo-forwarder` and `cardano-tracer` will be launched on _different_ local sockets, so direct connection between them is impossible:

```
+----------------+                    +----------------+
| demo-forwarder |-> sock1    sock2 <-| cardano-tracer |
+----------------+                    +----------------+
```

That's why we initiate ssh local forwarding which connects these local sockets, so `demo-forwarder` can work with `cardano-tracer`:

```
+----------------+                                        +----------------+
| demo-forwarder |-> sock1 <---SSH forwarding---> sock2 <-| cardano-tracer |
+----------------+                                        +----------------+
```

Please note that if you run the script with `Responder` mode, `demo-forwarder` works as a server, and `cardano-tracer` works as a client:

```
+----------------+                  +----------------+
| demo-forwarder |  <====connect====| cardano-tracer |
+----------------+                  +----------------+
```

otherwise, with `Initiator` mode, we have an opposite scenario:

```
+----------------+                  +----------------+
| demo-forwarder |====connect====>  | cardano-tracer |
+----------------+                  +----------------+
```
