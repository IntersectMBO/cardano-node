{ pkgs ? import <nixpkgs> {} }:

let

  # Single File Validation.
  # Executable named `validate-file` will be included in the shell.
  validateFile = pkgs.writeShellScriptBin "validate-file"
    ''
    # Usage: validate-file <schema> [file].
    # exec replaces the shell process with the validator for efficiency
    # If [file] is omitted, it reads the JSON payload from standard input.
    exec ${pkgs.check-jsonschema}/bin/check-jsonschema --schemafile "$1" "''${2:-/dev/stdin}"
    ''
  ;

  # Directory Traversal Validator.
  # Executable named `validate-dir` will be included in the shell.
  validateDir = pkgs.writeShellScriptBin "validate-dir"
    ''
    # Usage: validate-dir <schema> <dir>.
  
    # Priority: Argument -> Environment Variable -> Default.
    SCHEMA_FILE="''${1:-''${SCHEMA_FILE:-$DEFAULT_NAMESPACE_SCHEMA_SCHEMA}}"
    TARGET_DIR="''${2:-''${TARGET_DIR:-$DEFAULT_NAMESPACE_SCHEMA_DIR}}"

    # Checks.
    if ! test -f "$SCHEMA_FILE"; then
      echo "Error: Schema file not found at: $SCHEMA_FILE"
      exit 1
    fi
    if ! test -d "$TARGET_DIR"; then
      echo "Error: No directory found at: $TARGET_DIR"
      exit 1
    fi

    echo "Schema: $SCHEMA_FILE"
    echo "Searching in: $TARGET_DIR"
    echo "----------------------------------------"

    find "$TARGET_DIR" -type f -name "*.json" | {
      count=0
      has_errors=0

      while read -r file
      do
        count=$((count + 1))
        # Check validation (silenced)
        if ${validateFile}/bin/validate-file "$SCHEMA_FILE" "$file" > /dev/null 2>&1; then
          echo "Valid: $file"
        else
          echo "Invalid: $file"
          # Re-run to display specific errors
          ${validateFile}/bin/validate-file "$SCHEMA_FILE" "$file"
          echo "----------------------------------------"
          has_errors=1
        fi
      done

      # Summary.
      if test "$count" -eq 0
      then
          echo "No JSON files found to validate."
      elif test "$has_errors" -eq 0
      then
          echo "Success! All $count files passed validation."
      else
          echo "Failure. See errors above."
          exit 1
      fi
    }
    ''
  ;

  # Log File Validator.
  # Parses a log file, extracts the namespace per line, and validates the message.
  # Executable named `validate-log` will be available in the shell.
  validateLog = pkgs.writeShellScriptBin "validate-log"
    ''
    # Usage: validate-log <file> <dir>.

    LOG_FILE="$1"
    # Priority: Argument -> Environment Variable -> Default.
    TARGET_DIR="''${2:-''${TARGET_DIR:-$DEFAULT_NAMESPACE_SCHEMA_DIR}}"

    # Checks.
    if ! test -f "$LOG_FILE"; then
      echo "Error: Log file not found at: $LOG_FILE"
      exit 1
    fi
    if ! test -d "$TARGET_DIR"; then
      echo "Error: No directory found at: $TARGET_DIR"
      exit 1
    fi

    echo "Processing log: $LOG_FILE"
    echo "Using schema directory: $TARGET_DIR"
    echo "----------------------------------------"

    # Filter for valid JSON traces and process.
    grep '^{"at":' "$LOG_FILE" | {
      has_errors=0
      count=0

      while read -r line; do
        # Extract namespace using jq
        ns=$(echo "$line" | ${pkgs.jq}/bin/jq -r '.ns' 2>/dev/null)

        if test -z "$ns" || test "$ns" = "null"; then
          continue
        fi

        # Transform namespace A.B.C to A/B/C.schema.json and append to schema dir
        SCHEMA_PATH="$TARGET_DIR/$(echo "$ns" | ${pkgs.gnused}/bin/sed 's|\.|/|g').schema.json"

        if ! test -f "$SCHEMA_PATH"; then
          echo "Skip: No schema found at $SCHEMA_PATH for namespace $ns"
          continue
        fi

        count=$((count + 1))

        # Validate the specific line by piping standard input directly to validate-file
        if echo "$line" | ${validateFile}/bin/validate-file "$SCHEMA_PATH" > /dev/null 2>&1; then
          echo "Valid [$ns]"
        else
          echo "Invalid [$ns]: $line"
          echo "$line" | ${validateFile}/bin/validate-file "$SCHEMA_PATH"
          echo "----------------------------------------"
          has_errors=1
        fi
      done

      # Summary
      if test "$count" -eq 0; then
          echo "No valid log entries found to validate."
      elif test "$has_errors" -eq 0; then
          echo "Success! All $count log entries passed validation."
      else
          echo "Failure. See errors above."
          exit 1
      fi
    }
    ''
  ;

in pkgs.mkShell
  { buildInputs =
      [ # Tools
        pkgs.check-jsonschema
        pkgs.findutils
        pkgs.jq
        pkgs.gnused
        # Custom Scripts
        validateFile
        validateDir
        validateLog
      ]
    ;
    DEFAULT_MESSAGE_SCHEMA="${builtins.toString ./.}/TraceMessage.schema.json";
    DEFAULT_NAMESPACE_SCHEMA_SCHEMA="${builtins.toString ./.}/meta.schema.json";
    DEFAULT_NAMESPACE_SCHEMA_DIR="${builtins.toString ./.}/messages";
    shellHook =
      ''
      echo "JSON Schema Validation Shell"
      echo "----------------------------"
      echo "Commands available:"
      echo "  - validate-file <schema.json>  [file.json]                       (Single file validation)"
      echo "  - validate-dir  <schema.json>  <dir/*.json>                      (Recursive directory validation)"
      echo "  - validate-log  <log_file>     <namespace_dir/A/B/C.schema.json> (Log trace validation)"
      echo ""
      echo "Defaults are relative to the location of this "shell.nix" file:"
      echo "DEFAULT_MESSAGE_SCHEMA=\"''${DEFAULT_MESSAGE_SCHEMA}\""
      echo "DEFAULT_NAMESPACE_SCHEMA_SCHEMA=\"''${DEFAULT_NAMESPACE_SCHEMA_SCHEMA}\""
      echo "DEFAULT_NAMESPACE_SCHEMA_DIR=\"''${DEFAULT_NAMESPACE_SCHEMA_DIR}\""
      ''
    ;
  }

