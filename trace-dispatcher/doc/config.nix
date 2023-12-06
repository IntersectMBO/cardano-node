To add in node-services.nix

               TraceOptionSeverity  = [
                 {ns = ""; severity = "Info";}
                 {ns = "AcceptPolicy"; severity = "Silence";}
                 {ns = "ChainDB"; severity = "Debug";}
               ];

                TraceOptionDetail = [
                  {ns = ""; detail = "DNormal";}
                  {ns = "BlockFetchClient"; detail = "DMinimal";}
               ];

               TraceOptionBackend = [
                 {ns = ""; backends = ["Stdout HumanFormatColoured"; "Forwarder"; "EKGBackend"];}
                 {ns = "ChainDB"; backends = ["Forwarder"];}
               ];
               TraceOptionForwarder = {filePath: "/tmp/forwarder.sock";};
