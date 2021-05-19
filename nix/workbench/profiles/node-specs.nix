{ runWorkbenchJqOnly
, backend
, profile
}:

let
  inherit (backend) basePort staggerPorts;

  JSON = runWorkbenchJqOnly
    "node-specs-${profile.name}.json"
    "profile node-specs ${profile.JSON} ${toString basePort} ${toString staggerPorts}";

  value = __fromJSON (__readFile JSON);
in
{
  inherit JSON value;
}
