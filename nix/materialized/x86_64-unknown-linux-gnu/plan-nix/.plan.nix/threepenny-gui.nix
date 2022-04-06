{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = { buildexamples = false; rebug = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "threepenny-gui"; version = "0.9.1.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Heinrich Apfelmus <apfelmus at quantentunnel dot de>";
      author = "Heinrich Apfelmus";
      homepage = "http://wiki.haskell.org/Threepenny-gui";
      url = "";
      synopsis = "GUI framework that uses the web browser as a display.";
      description = "Threepenny-GUI is a GUI framework that uses the web browser as a display.\n\nIt's very easy to install because everyone has a web browser installed.\n\nA program written with Threepenny is essentially a small web server that\ndisplays the user interface as a web page to any browser that connects to it.\nYou can freely manipulate the HTML DOM and handle JavaScript events\nfrom your Haskell code.\n\nStability forecast: This is an experimental release! Send me your feedback!\nSignificant API changes are likely in future versions.\n\nNOTE: This library contains examples, but they are not built by default.\nTo build and install the example, use the @buildExamples@ flag like this\n\n@cabal install threepenny-gui -fbuildExamples@";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [
        "samples/static/css/*.css"
        "samples/static/css/*.png"
        "samples/static/*.html"
        "samples/static/*.png"
        "samples/static/*.wav"
        ];
      extraSrcFiles = [
        "CHANGELOG.md"
        "README.md"
        "samples/README.md"
        "js/*.html"
        "js/*.css"
        "js/*.js"
        "js/lib/*.js"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."safe" or (errorHandler.buildDepError "safe"))
          (hsPkgs."snap-server" or (errorHandler.buildDepError "snap-server"))
          (hsPkgs."snap-core" or (errorHandler.buildDepError "snap-core"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."websockets" or (errorHandler.buildDepError "websockets"))
          (hsPkgs."websockets-snap" or (errorHandler.buildDepError "websockets-snap"))
          (hsPkgs."vault" or (errorHandler.buildDepError "vault"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
        buildable = true;
        modules = [
          "Foreign/JavaScript/CallBuffer"
          "Foreign/JavaScript/EventLoop"
          "Foreign/JavaScript/Include"
          "Foreign/JavaScript/Marshal"
          "Foreign/JavaScript/Resources"
          "Foreign/JavaScript/Server"
          "Foreign/JavaScript/Types"
          "Graphics/UI/Threepenny/Internal"
          "Reactive/Threepenny/Memo"
          "Reactive/Threepenny/Monads"
          "Reactive/Threepenny/PulseLatch"
          "Reactive/Threepenny/Types"
          "Paths_threepenny_gui"
          "Foreign/JavaScript"
          "Foreign/RemotePtr"
          "Graphics/UI/Threepenny"
          "Graphics/UI/Threepenny/Attributes"
          "Graphics/UI/Threepenny/Canvas"
          "Graphics/UI/Threepenny/Core"
          "Graphics/UI/Threepenny/DragNDrop"
          "Graphics/UI/Threepenny/Elements"
          "Graphics/UI/Threepenny/Events"
          "Graphics/UI/Threepenny/JQuery"
          "Graphics/UI/Threepenny/SVG"
          "Graphics/UI/Threepenny/SVG/Attributes"
          "Graphics/UI/Threepenny/SVG/Elements"
          "Graphics/UI/Threepenny/Timer"
          "Graphics/UI/Threepenny/Widgets"
          "Reactive/Threepenny"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "threepenny-examples-bartab" = {
          depends = (pkgs.lib).optionals (flags.buildexamples) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."threepenny-gui" or (errorHandler.buildDepError "threepenny-gui"))
            ];
          buildable = if flags.buildexamples then true else false;
          hsSourceDirs = [ "samples" ];
          mainPath = [ "BarTab.hs" ] ++ [ "" ];
          };
        "threepenny-examples-buttons" = {
          depends = (pkgs.lib).optionals (flags.buildexamples) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."threepenny-gui" or (errorHandler.buildDepError "threepenny-gui"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            ];
          buildable = if flags.buildexamples then true else false;
          modules = [ "Paths_threepenny_gui" "Paths" ];
          hsSourceDirs = [ "samples" ];
          mainPath = [ "Buttons.hs" ] ++ [ "" ];
          };
        "threepenny-examples-canvas" = {
          depends = (pkgs.lib).optionals (flags.buildexamples) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."threepenny-gui" or (errorHandler.buildDepError "threepenny-gui"))
            ];
          buildable = if flags.buildexamples then true else false;
          modules = [ "Paths_threepenny_gui" "Paths" ];
          hsSourceDirs = [ "samples" ];
          mainPath = [ "Canvas.hs" ] ++ [ "" ];
          };
        "threepenny-examples-chat" = {
          depends = (pkgs.lib).optionals (flags.buildexamples) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."threepenny-gui" or (errorHandler.buildDepError "threepenny-gui"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            ];
          buildable = if flags.buildexamples then true else false;
          modules = [ "Paths_threepenny_gui" "Paths" "Data/List/Extra" ];
          hsSourceDirs = [ "samples" ];
          mainPath = [ "Chat.hs" ] ++ [ "" ];
          };
        "threepenny-examples-crud" = {
          depends = (pkgs.lib).optionals (flags.buildexamples) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."threepenny-gui" or (errorHandler.buildDepError "threepenny-gui"))
            ];
          buildable = if flags.buildexamples then true else false;
          hsSourceDirs = [ "samples" ];
          mainPath = [ "CRUD.hs" ] ++ [ "" ];
          };
        "threepenny-examples-currencyconverter" = {
          depends = (pkgs.lib).optionals (flags.buildexamples) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."threepenny-gui" or (errorHandler.buildDepError "threepenny-gui"))
            (hsPkgs."safe" or (errorHandler.buildDepError "safe"))
            ];
          buildable = if flags.buildexamples then true else false;
          hsSourceDirs = [ "samples" ];
          mainPath = [ "CurrencyConverter.hs" ] ++ [ "" ];
          };
        "threepenny-examples-dragndropexample" = {
          depends = (pkgs.lib).optionals (flags.buildexamples) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."threepenny-gui" or (errorHandler.buildDepError "threepenny-gui"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            ];
          buildable = if flags.buildexamples then true else false;
          modules = [ "Paths_threepenny_gui" "Paths" ];
          hsSourceDirs = [ "samples" ];
          mainPath = [ "DragNDropExample.hs" ] ++ [ "" ];
          };
        "threepenny-examples-drummachine" = {
          depends = (pkgs.lib).optionals (flags.buildexamples) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."threepenny-gui" or (errorHandler.buildDepError "threepenny-gui"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            ];
          buildable = if flags.buildexamples then true else false;
          modules = [ "Paths_threepenny_gui" "Paths" ];
          hsSourceDirs = [ "samples" ];
          mainPath = [ "DrumMachine.hs" ] ++ [ "" ];
          };
        "threepenny-examples-svg" = {
          depends = (pkgs.lib).optionals (flags.buildexamples) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."threepenny-gui" or (errorHandler.buildDepError "threepenny-gui"))
            ];
          buildable = if flags.buildexamples then true else false;
          hsSourceDirs = [ "samples" ];
          mainPath = [ "Svg.hs" ] ++ [ "" ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "13";
      rev = "minimal";
      sha256 = "";
      }) // {
      url = "13";
      rev = "minimal";
      sha256 = "";
      };
    }