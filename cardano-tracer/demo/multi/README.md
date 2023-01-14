# Multi Forwarders Demo

Run `./run.sh PASS FMODE`, where `FMODE` is forwarders' mode: `Responder` or `Initiator`.

As a result, 3 `demo-forwarder`s and one `cardano-tracer` will be launched. It imitates the real-world situation when `N` nodes work with one `cardano-tracer`.

Please note that if you run the script with `Responder` mode, `demo-forwarder`s work as a servers, and `cardano-tracer` works as a client:

```
+------------------+
| demo-forwarder 1 |  <====connect
+------------------+              \
+------------------+               \  +----------------+
| demo-forwarder 2 |  <====connect====| cardano-tracer |
+------------------+               /  +----------------+
+------------------+              /
| demo-forwarder 3 |  <====connect
+------------------+
```

Otherwise, with `Initiator` mode, we have an opposite scenario:

```
+------------------+
| demo-forwarder 1 |====connect==
+------------------+              \
+------------------+               \   +----------------+
| demo-forwarder 2 |====connect=====>  | cardano-tracer |
+------------------+               /   +----------------+
+------------------+              /
| demo-forwarder 3 |====connect==
+------------------+
```
