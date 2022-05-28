To add in node-services.nix

               TraceOptionSeverity  = [
                 {ns = ""; severity = "Info";}
                 {ns = "Cardano.Node.AcceptPolicy"; severity = "Silence";}
                 {ns = "Cardano.Node.ChainDB"; severity = "Debug";}
               ];

                TraceOptionDetail = [
                  {ns = ""; detail = "DNormal";}
                  {ns = "Cardano.Node.BlockFetchClient"; detail = "DMinimal";}
               ];

               TraceOptionBackend = [
                 {ns = ""; backends = ["Stdout HumanFormatColoured"; "Forwarder"; "EKGBackend"];}
                 {ns = "Cardano.Node.ChainDB"; backends = ["Forwarder"];}
               ];

               TraceOptionForwarder = {mode: "Initiator"};
