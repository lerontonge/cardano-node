{ pkgs, lib, cardanoLib
, workbenchNix
}:

let
  mkProfileJson = { profileName }:
    workbenchNix.runWorkbench "profile-${profileName}.json"
      "profile json ${profileName}";

  mkTopologyFiles = { profileName, profileJson }:
    pkgs.runCommand "workbench-topology-${profileName}"
      { requiredSystemFeatures = [ "benchmark" ];
        nativeBuildInputs = with pkgs.haskellPackages; with pkgs;
          [ bash cardano-cli coreutils gnused jq moreutils workbenchNix.workbench ];
      }
      ''
      mkdir $out
      wb topology make ${profileJson} $out
      ''
  ;

  mkNodeSpecsJson = { profileName, profileJson }:
    workbenchNix.runWorkbench "node-specs-${profileName}.json"
                 "profile node-specs ${profileName} ${mkTopologyFiles {inherit profileName profileJson;}}";

  mkGenesisFiles = { profileName, profileJson, nodeSpecsJson }:
    pkgs.runCommand "workbench-profile-genesis-cache-${profileName}"
      { requiredSystemFeatures = [ "benchmark" ];
        nativeBuildInputs = with pkgs.haskellPackages; with pkgs;
          [ bash cardano-cli coreutils gnused jq moreutils workbenchNix.workbench ];
      }
      ''
      mkdir $out

      cache_key_input=$(wb genesis profile-cache-key-input ${profileJson})
      cache_key=$(      wb genesis profile-cache-key       ${profileJson})

      genesis_keepalive() {
        while test ! -e $out/profile; do echo 'genesis_keepalive for Hydra'; sleep 10s; done
      }
      genesis_keepalive &
      __genesis_keepalive_pid=$!
      __genesis_keepalive_termination() {
        kill $__genesis_keepalive_pid 2>/dev/null || true
      }
      trap __genesis_keepalive_termination EXIT

      args=(
        genesis actually-genesis
        ${profileJson}
        ${nodeSpecsJson}
        $out
        "$cache_key_input"
        "$cache_key"
      )
      time wb "''${args[@]}"

      touch done

      ln -s ${profileJson}   $out
      ln -s ${nodeSpecsJson} $out
      ''
  ;

  jsonFilePretty = name: x: workbenchNix.runJq name ''--null-input --sort-keys
                                         --argjson x '${x}'
                                       '' "$x";

  mkServices = { profile, nodeSpecs, topologyFiles, backend, profiling }:
    rec {
      inherit
        (pkgs.callPackage
          ../service/nodes.nix
          {
            inherit backend profile nodeSpecs;
            inherit topologyFiles profiling;
            inherit workbenchNix;
            inherit jsonFilePretty;
            baseNodeConfig = cardanoLib.environments.testnet.nodeConfig;
          })
        node-services;

      inherit
        (pkgs.callPackage
          ../service/generator.nix
          {
            inherit backend profile nodeSpecs;
            inherit node-services;
            inherit jsonFilePretty;
          })
        generator-service;

      inherit
        (pkgs.callPackage
          ../service/tracer.nix
          {
            inherit backend profile nodeSpecs;
            inherit jsonFilePretty;
          })
        tracer-service;

      inherit
        (pkgs.callPackage
          ../service/healthcheck.nix
          {
            inherit backend profile nodeSpecs;
          })
        healthcheck-service;

      inherit
        (pkgs.callPackage
          ../service/latency.nix
          {})
        latency-service;
    };

  materialise-profile =
    profileArgs@{ profileName, backend, profiling }:
      if ! builtins.elem profileName workbenchNix.profile-names
      then
        throw "No such profile: ${profileName}; Known profiles: ${toString workbenchNix.profile-names}"
      else
        let
          profileJson = mkProfileJson { inherit profileName; };
          profile = __fromJSON (__readFile profileJson);
          topologyFiles =
            mkTopologyFiles { inherit profileName profileJson; }
          ;
          nodeSpecsJson = mkNodeSpecsJson
            { inherit profileName profileJson;};
          nodeSpecs = __fromJSON (__readFile nodeSpecsJson);
          genesisFiles =
            mkGenesisFiles
              { inherit profileName profileJson nodeSpecsJson; }
          ;
          inherit (mkServices
            {
              inherit backend profiling;
              inherit profile;
              inherit nodeSpecs;
              inherit topologyFiles;
            })
            node-services
            generator-service
            tracer-service
            healthcheck-service
            latency-service
          ;
        in
          pkgs.runCommand "workbench-profile-${profileName}"
            { buildInputs = [];
              profileJsonPath = profileJson;
              nodeSpecsJsonPath = nodeSpecsJson;
              topologyJsonPath = "${topologyFiles}/topology.json";
              topologyDotPath  = "${topologyFiles}/topology.dot";
              nodeServices =
                __toJSON
                (lib.flip lib.mapAttrs node-services
                  (name: node-service:
                    with node-service;
                    { inherit name;
                      start          = start.JSON;
                      config         = config.JSON;
                      topology       = topology.JSON;
                    }));
              generatorService =
                with generator-service;
                __toJSON
                { name            = "generator";
                  start           = start.JSON;
                  config          = config.JSON;
                  plutus-redeemer = plutus-redeemer.JSON;
                  plutus-datum    = plutus-datum.JSON;
                };
              tracerService =
                with tracer-service;
                __toJSON
                { name                 = "tracer";
                  start                = start.JSON;
                  config               = config.JSON;
                };
              healthcheckService =
                with healthcheck-service;
                __toJSON
                { name                 = "healthcheck";
                  start                = start.JSON;
                };
              latencyService =
                with healthcheck-service;
                __toJSON
                { name                 = "latency";
                  start                = start.JSON;
                };
              passAsFile =
                [
                  "nodeServices"
                  "generatorService"
                  "tracerService"
                  "healthcheckService"
                  "latencyService"
                  "topologyJson"
                  "topologyDot"
                ];
            }
            ''
            mkdir $out
            cp    $profileJsonPath              $out/profile.json
            cp    $nodeSpecsJsonPath            $out/node-specs.json
            cp    $topologyJsonPath             $out/topology.json
            cp    $topologyDotPath              $out/topology.dot
            cp    $nodeServicesPath             $out/node-services.json
            cp    $generatorServicePath         $out/generator-service.json
            cp    $tracerServicePath            $out/tracer-service.json
            cp    $healthcheckServicePath       $out/healthcheck-service.json
            cp    $latencyServicePath           $out/latency-service.json
            ''
          //
          (
            profile
            //
            {
              inherit profileName;
              JSON = profileJson;
              value = profile;
              topology = {
                files = topologyFiles;
                value = (__fromJSON (__readFile "${topologyFiles}/topology.json"));
              };
              node-specs = {JSON = nodeSpecsJson; value = nodeSpecs;};
              genesis.files = genesisFiles;
              inherit node-services generator-service tracer-service healthcheck-service latency-service;
            }
          )
  ;

in
  {inherit materialise-profile;}
