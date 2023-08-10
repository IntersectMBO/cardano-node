{ pkgs
, bashInteractive
, coreutils
, openssh_hacks
}:

with pkgs.lib;

let
  pid_file             = "/local/run/current/ssh/sshd.pid";
  listen_address       = "0.0.0.0";
  port                 = 32000;
  host_key             = "/local/run/current/ssh/sshd.id_ed25519";
  # Inside SRE's Nomad Clients, runs as 'nobody' and group ID '65534':
  # $ nomad alloc exec XXX /nix/store/XXX-coreutils-9.1/bin/whoami
  # nobody
  # $ nomad alloc exec XXX /nix/store/XXX-coreutils-9.1/bin/groups
  # /nix/store/XXX-coreutils-9.1/bin/groups: cannot find name for group ID 65534
  # 65534
  user                 = "nobody";
  authorized_keys_file = "/local/run/current/ssh/%u.id_ed25519.pub";
in {

  start = rec {
    JSON = pkgs.writeScript "startup-ssh.sh" value;
    value = ''
      #!${bashInteractive}/bin/sh

      # Strict runtime
      ################
      # e:        Immediately exit if any command has a non-zero exit status
      # u:        Reference to non previously defined variables is an error
      # pipefail: Any failed command in a pipeline is used as return code
      set -euo pipefail

      # Running as ...
      ${coreutils}/bin/echo "Running as (whoami): $(${coreutils}/bin/whoami)"

      # -D
      # When this option is specified, sshd will not detach and does not become
      # a daemon. This allows easy monitoring of sshd.
      # -e
      # Write debug logs to standard error instead of the system log so
      # supervisord can use it the same it does with the other programs.
      # -f config_file
      # Specifies the name  of the configuration file. The default is
      # /etc/ssh/sshd_config. sshd refuses to start if there is no configuration
      # file.
      ${openssh_hacks}/bin/sshd -D -e -f /local/run/current/ssh/sshd_config
    '';
  };

  config = rec {
    JSON = pkgs.writeScript "sshd_config" value;
    # To run inside the isolated environment created by Nomad it needs:
    # - StrictModes no:      This or setting a proper home for the `nobody`
    #                        user.
    # - ShellPath BASH:      Use a patched OpenSSH that allows to bypass the
    #                        "/bin/nologin" like shells in /etc/passdw.
    # - PermitLockedAccount: Use a patched OpenSSH that allows to bypass locked
    #                        accounts (passwords with "!", "LK", etc).
    value = ''
      # Specifies what environment variables sent by the client will be
      # copied into the session's environ(7). See SendEnv and SetEnv in
      # ssh_config(5) for how to configure the client. The TERM environment
      # variable is always accepted whenever the client requests a
      # pseudo-terminal as it is required by the protocol. Variables are
      # specified by name, which may contain  the  wildcard  characters `*'
      # and `?'. Multiple environment variables may be separated by
      # whitespace or spread across multiple AcceptEnv directives. Be warned
      # that some environment variables could be used to bypass restricted
      # user environments. For this reason, care should be taken in the use
      # of this directive. The default is not to accept any environment
      # variables.
      AcceptEnv LANG LC_*

      # Specifies  which  address  family should be used by sshd(8). Valid
      # arguments are any (the default), inet (use IPv4 only), or inet6 (use
      # IPv6 only).
      AddressFamily inet

      # Specifies whether ssh-agent(1) forwarding is permitted. The default
      # is "yes". Note that disabling agent forwarding does not improve
      # security unless users are also denied shell access, as they can
      # always install their own for‐warders.
      AllowAgentForwarding no

      # This keyword can be followed by a list of group name patterns,
      # separated by spaces. If specified, login is allowed only for users
      # whose primary group or supplementary group list matches one of the
      # patterns. Only group names are valid; a numerical group ID is not
      # recognized. By default, login is allowed for all groups. The
      # allow/deny groups directives are processed in the following order:
      # DenyGroups, AllowGroups.
      # See PATTERNS in ssh_config(5) for more information on patterns.
      # AllowGroups

      # Specifies whether StreamLocal (Unix-domain socket) forwarding is
      # permitted. The available options are yes (the default) or all to
      # allow StreamLocal forwarding, no to prevent all StreamLocal
      # forwarding, local to allow local (from the perspective of ssh(1))
      # forwarding only or remote to allow remote forwarding only. Note that
      # disabling StreamLocal forwarding does not improve security unless
      # users are also denied shell access, as they can always install their
      # own forwarders.
      AllowStreamLocalForwarding no

      # Specifies whether TCP forwarding is permitted. The available options
      # are yes (the default) or all to allow TCP forwarding, no to prevent
      # all TCP forwarding, local to allow local (from the perspective of
      # ssh(1)) forwarding only or remote to allow remote forwarding only.
      # Note that disabling TCP forwarding does not improve security unless
      # users are also denied shell access, as they can always install their
      # own forwarders.
      AllowTcpForwarding no

      # This keyword can be followed by a list of user name patterns,
      # separated by spaces. If specified, login is allowed only for user
      # names that match one of the patterns. Only user names are valid; a
      # numerical user ID is not recognized. By default, login is allowed
      # for all users. If the pattern takes the form USER@HOST then USER and
      # HOST are separately checked, restricting logins to particular users
      # from particular hosts. HOST criteria may additionally contain
      # addresses to match in CIDR address/masklen format. The allow/deny
      # users directives are processed in the following order: DenyUsers,
      # AllowUsers.
      # See PATTERNS in ssh_config(5) for more information on patterns.
      AllowUsers ${user}

      # Specifies the authentication methods that must be successfully
      # completed for a user to be granted access. This option must be
      # followed by one or more lists of comma-separated authentication
      # method names, or by the single string any to indicate the default
      # behaviour of accepting any single authentication method. If the
      # default is overridden, then successful authentication requires
      # completion of every method in at least one of these lists.
      # For example, Qq publickey,password publickey,keyboard-interactive
      # would require the user to complete public key authentication,
      # followed by either password or keyboard interactive authentication.
      # Only methods that are next in one or more lists are offered at each
      # stage, so for this example it  would not be possible to attempt
      # password or keyboard-interactive authentication before public key.
      # For keyboard interactive authentication it is also possible to
      # restrict authentication to a specific device by appending a colon
      # followed by the device identifier bsdauth or pam. depending on the
      # server configuration. For example, Qq keyboard-interactive:bsdauth
      # would restrict keyboard interactive authentication to the bsdauth
      # device.
      # If the publickey method is listed more than once, sshd(8) verifies
      # that keys that have been used successfully are not reused for
      # subsequent authentications. For example, Qq publickey,publickey
      # requires successful authentication using two different public keys.
      # Note that each authentication method listed should also be
      # explicitly enabled in the configuration.
      # The available authentication methods are: Qq gssapi-with-mic,
      # Qq hostbased, Qq keyboard-interactive, Qq none (used for access to
      # password-less accounts when PermitEmptyPasswords is enabled),
      # Qq password and Qq publickey.
      AuthenticationMethods publickey

      # AuthorizedKeysCommand
      # AuthorizedKeysCommandUser

      # Specifies the file that contains the public keys used for user
      # authentication. The format is described in the AUTHORIZED_KEYS FILE
      # FORMAT section of sshd(8). Arguments to AuthorizedKeysFile accept
      # the tokens described in the TOKENS section. After expansion,
      # AuthorizedKeysFile is taken to be an absolute path or one relative
      # to the user's home directory. Multiple files may be listed,
      # separated by whitespace. Alternately this option may be set to none
      # to skip checking for user keys in files. The default is
      # Qq  .ssh/authorized_keys .ssh/authorized_keys2 .
      AuthorizedKeysFile ${authorized_keys_file}

      # AuthorizedPrincipalsCommand
      # AuthorizedPrincipalsCommandUser
      # AuthorizedPrincipalsFile

      # The contents of the specified file are sent to the remote user
      # before authentication is allowed. If the argument is none then no
      # banner is displayed.
      # By default, no banner is displayed.
      Banner none

      # CASignatureAlgorithms

      # Specifies the pathname of a directory to chroot(2) to after
      # authentication. At session startup sshd(8) checks that all
      # components of the pathname are root-owned directories which are not
      # writable by any other user or group. After the chroot, sshd(8)
      # changes the working directory to the user's home directory.
      # Arguments to ChrootDirectory accept the tokens described in the
      # TOKENS section.
      # The ChrootDirectory must contain the necessary files and directories
      # to support the user's session. For an interactive session this
      # requires at least a shell, typically sh(1), and basic /dev nodes
      # such as null(4), zero(4), stdin(4), stdout(4), stderr(4), and tty(4)
      # devices. For file transfer sessions using SFTP no additional
      # configuration of the environment is necessary if the in-process
      # sftp-server is used, though sessions which use logging may require
      # /dev/log inside the chroot directory on some operating systems (see
      # sftp-server(8) for details).
      # For safety, it is very important that the directory hierarchy be
      # prevented from modification by other processes on the system
      # (especially those outside the jail). Misconfiguration can lead to
      # unsafe environments which sshd(8) cannot detect.
      ChrootDirectory none

      # Ciphers

      # Sets the number of client alive messages which may be sent without
      # sshd(8) receiving any messages back from the client. If this
      # threshold is reached while client alive messages are being sent,
      # sshd will disconnect the client, terminating the session. It is
      # important to note that the use of client alive messages is very
      # different from TCPKeepAlive. The client alive messages are sent
      # through the encrypted channel and therefore will not be spoofable.
      # The TCP keepalive option enabled by TCPKeepAlive is spoofable. The
      # client alive mechanism is valuable when the client or server depend
      # on knowing when a connection has become unresponsive.
      # The default value is 3. If ClientAliveInterval is set to 15, and
      # ClientAliveCountMax is left at the default, unresponsive SSH clients
      # will be disconnected after approximately 45 seconds. Setting a zero
      # ClientAliveCountMax disables connection termination.
      ClientAliveCountMax 3

      # Sets a timeout interval in seconds after which if no data has been
      # received from the client, sshd(8) will send a message through the
      # encrypted channel to request a response from the client. The default
      # is 0, indicating that  these messages will not be sent to the
      # client.
      ClientAliveInterval 30

      # Specifies whether compression is enabled after the user has
      # authenticated successfully. The argument must be yes, delayed (a
      # legacy synonym for yes) or no. The default is yes.
      Compression yes

      # DenyGroups
      # DenyUsers

      # Disables all forwarding features, including X11, ssh-agent(1), TCP
      # and StreamLocal. This option overrides all other forwarding-related
      # options and may simplify restricted configurations.
      DisableForwarding yes

      # ExposeAuthInfo
      # FingerprintHash

      # Forces the execution of the command specified by ForceCommand,
      # ignoring any command supplied by the client and ~/.ssh/rc if
      # present. The command is invoked by using the user's login shell
      # with the -c option. This applies to shell, command, or subsystem
      # execution. It is most useful inside a Match block. The command
      # originally supplied by the client is available in the
      # SSH_ORIGINAL_COMMAND environment variable. Specifying a command of
      # internal-sftp will force the use of an in-process SFTP server that
      # requires no support files when used with ChrootDirectory. The
      # default is none.
      # TODO:
      # ForceCommand ${bashInteractive}/bin/sh -c "''${SSH_ORIGINAL_COMMAND}"

      # GatewayPorts
      # GSSAPIAuthentication
      # GSSAPICleanupCredentials
      # GSSAPIStrictAcceptorCheck
      # HostbasedAcceptedAlgorithms

      # Specifies whether rhosts or /etc/hosts.equiv authentication together
      # with successful public key client host authentication is allowed
      # (host-based authentication). The default is no.
      HostbasedAuthentication no

      # HostbasedUsesNameFromPacketOnly
      # HostCertificate

      # Specifies a file containing a private host key used by SSH. The
      # defaults are /etc/ssh/ssh_host_ecdsa_key,
      # /etc/ssh/ssh_host_ed25519_key and /etc/ssh/ssh_host_rsa_key.
      # Note that sshd(8) will refuse to use a file if it is
      # group/world-accessible and that the HostKeyAlgorithms option
      # restricts which of the keys are actually used by sshd(8).
      # It is possible to have multiple host key files. It is also possible
      # to specify public host key files instead. In this case operations on
      # the  private key will be delegated to an ssh-agent(1).
      HostKey ${host_key}

      # HostKeyAgent
      # HostKeyAlgorithms
      # IgnoreRhosts
      # IgnoreUserKnownHosts
      # Include
      # IPQoS

      # Specifies whether to allow keyboard-interactive authentication. All
      # authentication styles from login.conf(5) are supported. The default
      # is yes. The argument to this keyword must be yes or no.
      # ChallengeResponseAuthentication is a deprecated alias for this.
      KbdInteractiveAuthentication no

      # KerberosAuthentication
      # KerberosGetAFSToken
      # KerberosOrLocalPasswd
      # KerberosTicketCleanup
      # KexAlgorithms

      # Specifies the local addresses sshd(8) should listen on. The
      # following forms may be used:
      #  ListenAddress hostname | address [rdomain domain]
      #  ListenAddress hostname:port [rdomain domain]
      #  ListenAddress IPv4_address:port [rdomain domain]
      #  ListenAddress [hostname | address]:port [rdomain domain]
      # The optional rdomain qualifier requests sshd(8) listen in an
      # explicit routing domain. If port is not specified, sshd will listen
      # on the address and all Port options specified. The default is to
      # listen on all local addresses on the current default routing domain.
      # Multiple ListenAddress options are permitted. For more information
      # on routing domains, see rdomain(4).
      ListenAddress ${listen_address}

      # LoginGraceTime 8
      # LogLevel
      # LogVerbose
      # MACs
      # Match
      # MaxAuthTries

      # Specifies the maximum number of open shell, login or subsystem
      # (e.g. sftp) sessions permitted per network connection. Multiple
      # sessions may be established by clients that support connection
      # multiplexing. Setting MaxSessions to 1 will effectively disable
      # session multiplexing, whereas setting it to 0 will prevent all
      # shell, login and subsystem sessions while still permitting
      # forwarding. The default is 10.
      MaxSessions 10

      # MaxStartups 2:30:10
      # ModuliFile

      # Specifies whether password authentication is allowed. The default is
      # yes.
      PasswordAuthentication no

      # PermitEmptyPasswords
      # PermitListen
      # PermitOpen

      # Specifies whether root can log in using ssh(1). The argument must be
      # yes, prohibit-password, forced-commands-only, or no. The default is
      # prohibit-password.
      # If this option is set to prohibit-password (or its deprecated alias,
      # without-password), password and keyboard-interactive authentication
      # are disabled for root.
      # If this option is set to forced-commands-only, root login with
      # public key authentication will be allowed, but only if the command
      # option has been specified (which may be useful for taking remote
      # backups even if root login is normally not allowed). All other
      # authentication methods are disabled for root.
      # If this option is set to no, root is not allowed to log in.
      PermitRootLogin no

      # PermitTTY yes
      # PermitTunnel
      # PermitUserEnvironment
      # PermitUserRC
      # PerSourceMaxStartups
      # PerSourceNetBlockSize

      # Specifies the file that contains the process ID of the SSH daemon,
      # or none to not write one.  The default is /run/sshd.pid.
      PidFile ${pid_file}

      # Specifies the port number that sshd(8) listens on. The default is
      # 22. Multiple options of this type are permitted. See also
      # ListenAddress.
      Port ${toString port}

      # PrintLastLog
      # PrintMotd
      # PubkeyAcceptedAlgorithms
      # PubkeyAuthOptions

      # Specifies whether public key authentication is allowed. The default
      # is yes.
      PubkeyAuthentication yes

      # RekeyLimit
      # RequiredRSASize
      # RevokedKeys
      # RDomain
      # SecurityKeyProvider

      # Specifies one or more environment variables to set in child sessions
      # started by sshd(8) as NAME=VALUE. The environment value may be
      # quoted (e.g. if it contains whitespace characters). Environment
      # variables set by SetEnv override the default environment and any
      # variables specified by the user via AcceptEnv or
      # PermitUserEnvironment.
      # SetEnv

      # StreamLocalBindMask
      # StreamLocalBindUnlink

      # Specifies whether sshd(8) should check file modes and ownership of
      # the user's files and home directory before accepting login. This is
      # normally desirable because novices sometimes accidentally leave
      # their directory or files world-writable. The default is yes. Note
      # that this does not apply to ChrootDirectory, whose permissions and
      # ownership are checked unconditionally.
      StrictModes no

      # Configures an external subsystem (e.g. file transfer daemon).
      # Arguments should be a subsystem name and a command (with optional
      # arguments) to execute upon subsystem request.
      # The command sftp-server implements the SFTP file transfer subsystem.
      # Alternately the name internal-sftp implements an in-process SFTP
      # server. This may simplify configurations using ChrootDirectory to
      # force  a  different filesystem root on clients.
      # By default no subsystems are defined.
      # Subsystem sftp /usr/lib/openssh/sftp-server
      # TODO:
      # Subsystem nomad ${bashInteractive}/bin/sh -c "''${SSH_ORIGINAL_COMMAND}"

      # SyslogFacility

      # Specifies whether the system should send TCP keepalive messages to
      # the other side. If they are sent, death of the connection or crash
      # of one of the machines will be properly noticed. However, this means
      # that connections will die if the route is down temporarily, and some
      # people find it annoying. On the other hand, if TCP keepalives are
      # not sent, sessions may hang indefinitely on the server, leaving Qq
      # ghost users and consuming server resources.
      # The default is "yes" (to send TCP keepalive messages), and the
      # server will notice if the network goes down or the client host
      # crashes. This avoids infinitely hanging sessions.
      # To disable TCP keepalive messages, the value should be set to no.
      TCPKeepAlive no

      # TrustedUserCAKeys
      # UseDNS
      # UsePAM
      # VersionAddendum
      # X11DisplayOffset
      # X11Forwarding
      # X11UseLocalhost
      # XAuthLocation

      # From https://github.com/fmaste/openssh-portable-hacks
      Match User nobody
        ShellPath ${bashInteractive}/bin/bash
        PermitLockedAccount yes
    '';
  };

}
