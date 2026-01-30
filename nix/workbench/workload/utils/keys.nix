{ coreutils
, jq
, cardano-cli
, testnet_magic
}:
''
################################################################################
# Hack: Why pre-create and pre-share keys when you can create them on demand?
#       (ONLY BECAUSE THIS IS A TEST ENVIRONMENT AND WE ARE ALL FRIENDS!)
# Given a prefix and an x, y and z numbers creates always the same address keys.
# The x, y and z are to allow to have different key "levels", for example a key
# for  "node-2", "drep 5" and "proposal 22".
# Supports x=[0..99], y=[0..9999] and z=[0..999999] by adding the Hex chars.
# Returns the file path without the extensions (the ".skey" or ".vkey" part).
# No key is created if the file already exists.
################################################################################
function create_x_y_z_key_files {

  # Function arguments.
  local prefix=$1 # String for the key file name (not for the socket).
  local x_i=$2
  local y_n=$3
  local z_n=$4

  local filename=./"''${prefix}"-"''${y_n}"-"''${z_n}"
  # Now with the extensions.
  local skey="''${filename}".skey
  local vkey="''${filename}".vkey

  # Only create if not already there!
  if ! test -f "''${vkey}"
  then
      ${jq}/bin/jq --null-input \
        --argjson x_n "''${x_n}" \
        --argjson y_n "''${y_n}" \
        --argjson z_n "''${z_n}" \
        '
          {"type": "PaymentSigningKeyShelley_ed25519",
           "description": "Payment Signing Key",
           "cborHex": (
                "5820b02868d722df021278c78be3b7363759b37f5852b8747b488bab"
              + (if   $x_n <=  9
                 then ("0" + ($x_n | tostring))
                 elif $x_n >= 10 and $x_n <= 99
                 then (       $x_n | tostring)
                 else (error ("Node ID above 99"))
                 end
                )
              + (if   $y_n <=      9
                 then (   "000" + ($y_n | tostring))
                 elif $y_n >=   10 and $y_n <=   99
                 then (    "00" + ($y_n | tostring))
                 elif $y_n >=  100 and $y_n <=  999
                 then (     "0" + ($y_n | tostring))
                 elif $y_n >= 1000 and $y_n <= 9999
                 then (           ($y_n | tostring))
                 else (error ("Proposal ID above 9999"))
                 end
                )
              + (if   $z_n <=      9
                 then ( "00000" + ($z_n | tostring))
                 elif $z_n >=     10 and $z_n <=     99
                 then (  "0000" + ($z_n | tostring))
                 elif $z_n >=    100 and $z_n <=    999
                 then (   "000" + ($z_n | tostring))
                 elif $z_n >=   1000 and $z_n <=   9999
                 then (    "00" + ($z_n | tostring))
                 elif $z_n >=  10000 and $z_n <=  99999
                 then (     "0" + ($z_n | tostring))
                 elif $z_n >= 100000 and $z_n <= 999999
                 then (           ($z_n | tostring))
                 else (error ("DRep ID above 999999"))
                 end
                )
            )
          }
        ' \
    > "''${skey}"
    ${cardano-cli}/bin/cardano-cli conway key verification-key         \
      --signing-key-file      "''${skey}"                              \
      --verification-key-file "''${vkey}"
  fi
  ${coreutils}/bin/echo "''${filename}"
}

################################################################################
# Get address of the x-y-z key combination!
# Creates the key if it does not already exist.
################################################################################
function build_x_y_z_address {

  # Function arguments.
  local prefix=$1
  local x_n=$2
  local y_n=$3
  local z_n=$4

  local filename addr
  filename="$(create_x_y_z_key_files "''${prefix}" "''${x_n}" "''${y_n}" "''${z_n}")"
  addr="''${filename}.addr"
  # Only create if not already there!
  if ! test -f "''${addr}"
  then
    local vkey="''${filename}".vkey
      ${cardano-cli}/bin/cardano-cli address build  \
        --testnet-magic ${toString testnet_magic}   \
        --payment-verification-key-file "''${vkey}" \
    > "''${addr}"
  fi
  ${coreutils}/bin/cat "''${addr}"
}
''

