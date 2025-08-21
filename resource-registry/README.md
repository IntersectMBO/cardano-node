# `resource-registry`

In some cases, the lifetime of a resource is not suitable for CPS and hence
can't use `bracket`-like functions, for example, the resource might be tracked
inside some other data structure. In this case, the container data structure can
be allocated in a resource registry together with the resources so that an
exception will deallocate the resources in a proper order.