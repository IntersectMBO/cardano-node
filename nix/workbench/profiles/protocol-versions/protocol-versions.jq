import "v5"         as v5;
import "v6"         as v6;
import "v7"         as v7;
import "v8-preview" as v8;

def protocolVersions:
{ v5:
  { pParams:    v5::pParams
  , costModels: {}
  }
, v6:
  { pParams:    v6::pParams
  , costModels: v6::costModels
  }
, v7:
  { pParams:    v7::pParams
  , costModels: v7::costModels
  }
, v8:
  { pParams:    v8::pParams
  , costModels: v8::costModels
  }
};

def pv:
  protocolVersions;