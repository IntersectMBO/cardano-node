# Demo Programs

There are two demo programs, `demo-forwarder` and `demo-acceptor`. You can run these programs in different terminals and see an interaction between them.

Please see `forwarder.hs` module as an example of how to use `trace-forward` library in the node and `acceptor.hs` module as an example of how to use it in acceptor application (for example, logger or RTView).

## How To Build It

As a result of `cabal build all` command, two demo programs will be built: `demo-forwarder` and `demo-acceptor`.

## How To Run It

### Connection Via Local Pipe

The example command to run `demo-acceptor` program:

```
./demo-acceptor /path/to/demo.sock 1
```

where:

* `/path/to/demo.sock` is the path to the pipe file that will be created and used for connection with the forwarder,
* `1` is a request frequency in seconds (in this example, the acceptor will ask EKG metrics every second).

The example command to run `demo-forwarder` program:

```
./demo-forwarder /path/to/demo.sock 10
```

where:

* `/path/to/demo.sock` is the path to the pipe file that will be created (if needed) and used for connection with the acceptor,
* `10` is a reconnect frequency in seconds (in this example, the forwarder will try to reconnect to the acceptor every 10 seconds).

### Connection Via Remote Socket

The example command to run `demo-acceptor` program:

```
./demo-acceptor 127.0.0.1 3010 0.8
```

where:

* `127.0.0.1` and `3010` are the host and port; the acceptor will listen to them to accept the connection from the forwarder,
* `0.8` is a request frequency in seconds (in this example, the acceptor will ask EKG metrics every 0.8 s).

The example command to run `demo-forwarder` program:

```
./demo-forwarder 127.0.0.1 3010 10
```

where:

* `127.0.0.1` and `3010` are the host and port; the forwarder will use them to establish the connection with the acceptor,
* `10` is a reconnect frequency in seconds (in this example, the forwarder will try to reconnect to the acceptor every 10 seconds).
