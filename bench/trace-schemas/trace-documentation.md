# Cardano Trace Documentation

## Table Of Contents

### [Trace Messages](#trace-messages)

1. [BlockFetchⓜ](#blockfetchclientacknowledgedfetchrequest)
	1. [Clientⓣⓢ](#blockfetchclientacknowledgedfetchrequest)
	1. [Decisionⓣⓜ](#blockfetchdecisionaccept)
	1. [Remoteⓣⓢ](#blockfetchremotereceivebatchdone)
		1. [Serialisedⓣⓢ](#blockfetchremoteserialisedreceivebatchdone)
	1. [Serverⓣⓜ](#blockfetchserversendblock)
1. [BlockchainTimeⓣ](#blockchaintimecurrentslotunknown)
1. [ChainDBⓣⓜ](#chaindbaddblockeventaddblockvalidationinvalidblock)
	1. [ReplayBlockⓣⓜ](#chaindbreplayblockledgerreplay)
1. [ChainSyncⓜ](#chainsyncclientaccessingforecasthorizon)
	1. [Clientⓣ](#chainsyncclientaccessingforecasthorizon)
	1. [Localⓣⓢ](#chainsynclocalreceiveawaitreply)
	1. [Remoteⓣⓢ](#chainsyncremotereceiveawaitreply)
		1. [Serialisedⓣⓢ](#chainsyncremoteserialisedreceiveawaitreply)
	1. [ServerBlockⓣⓢⓜ](#chainsyncserverblockupdate)
	1. [ServerHeaderⓣⓢⓜ](#chainsyncserverheaderupdate)
1. [Consensusⓜ](#consensuscsjbecomingobjector)
	1. [CSJⓣⓢ](#consensuscsjbecomingobjector)
	1. [DevotedBlockFetchⓣⓢ](#consensusdevotedblockfetchrotateddynamo)
	1. [GDDⓣⓢ](#consensusgddtracegddevent)
	1. [GSMⓣⓜ](#consensusgsmentercaughtup)
	1. [SanityCheckⓣ](#consensussanitychecksanitycheckissue)
	1. [Startupⓣ](#consensusstartupconsensusstartupexception)
1. [Forgeⓣⓜ](#forgeloopadoptedblock)
	1. [Loopⓣⓜ](#forgeloopadoptedblock)
	1. [ThreadStatsⓣⓢⓜ](#forgethreadstatsforgingstats)
1. [Mempoolⓣⓢⓜ](#mempooladdedtx)
1. [Netⓣⓢ](#netacceptpolicyconnectionhardlimit)
	1. [AcceptPolicyⓣ](#netacceptpolicyconnectionhardlimit)
	1. [ConnectionManagerⓜ](#netconnectionmanagerlocalconnect)
		1. [Localⓣⓜ](#netconnectionmanagerlocalconnect)
		1. [Remoteⓣⓜ](#netconnectionmanagerremoteconnect)
		1. [Transitionⓣⓢ](#netconnectionmanagertransitiontransition)
	1. [InboundGovernorⓜ](#netinboundgovernorlocaldemotedtocoldremote)
		1. [Localⓣⓜ](#netinboundgovernorlocaldemotedtocoldremote)
		1. [Remoteⓣⓜ](#netinboundgovernorremotedemotedtocoldremote)
		1. [Transitionⓣ](#netinboundgovernortransitiontransition)
	1. [Muxⓜ](#netmuxlocalcleanexit)
		1. [Localⓣⓜ](#netmuxlocalcleanexit)
		1. [Remoteⓣⓜ](#netmuxremotecleanexit)
	1. [PeerSelectionⓜ](#netpeerselectionactionsconnectionerror)
		1. [Actionsⓣ](#netpeerselectionactionsconnectionerror)
		1. [Countersⓣⓢⓜ](#netpeerselectioncounterscounters)
		1. [Initiatorⓣⓢ](#netpeerselectioninitiatorgovernorstate)
		1. [Responderⓣⓢ](#netpeerselectionrespondergovernorstate)
		1. [Selectionⓣⓜ](#netpeerselectionselectionbigledgerpeersfailure)
	1. [Peersⓜ](#netpeersledgerdisabledledgerpeers)
		1. [Ledgerⓣ](#netpeersledgerdisabledledgerpeers)
		1. [LocalRootⓣ](#netpeerslocalrootlocalrootdnsmap)
		1. [PublicRootⓣ](#netpeerspublicrootpublicrootdomains)
	1. [Serverⓜ](#netserverlocalacceptconnection)
		1. [Localⓣ](#netserverlocalacceptconnection)
		1. [Remoteⓣ](#netserverremoteacceptconnection)
1. [NodeStateⓣ](#nodestatenodeaddblock)
1. [RPCⓣⓜ](#rpcerror)
1. [Reflectionⓣⓜ](#reflectionmetricsinfo)
1. [Shutdownⓣ](#shutdownabnormal)
1. [Startupⓣⓜ](#startupblockforgingblocktypemismatch)
	1. [DiffusionInitⓣ](#startupdiffusioninitconfiguringlocalsocket)
1. [StateQueryServerⓣ](#statequeryserverreceiveacquire)
1. [TxSubmissionⓜ](#txsubmissionlocalreceiveaccepttx)
	1. [Localⓣⓢ](#txsubmissionlocalreceiveaccepttx)
	1. [LocalServerⓣⓢ](#txsubmissionlocalserverreceivedtx)
	1. [MonitorClientⓣⓢ](#txsubmissionmonitorclientreceiveacquire)
	1. [Remoteⓣⓢ](#txsubmissionremotereceivedone)
	1. [TxInboundⓣⓜ](#txsubmissiontxinboundaddedtomempool)
	1. [TxOutboundⓣⓢ](#txsubmissiontxoutboundcontrolmessage)
1. [Versionⓣⓢⓜ](#versionnodeversion)

### [Metrics](#metrics)

1. [Forge](#forgeabout-to-lead)
	1. [about-to-lead](#forgeabout-to-lead)
	1. [adopted](#forgeadopted)
	1. [adoption-thread-died](#forgeadoption-thread-died)
	1. [block-from-future](#forgeblock-from-future)
	1. [could-not-forge](#forgecould-not-forge)
	1. [didnt-adopt](#forgedidnt-adopt)
	1. [forged](#forgeforged)
	1. [forged-invalid](#forgeforged-invalid)
	1. [node-is-leader](#forgenode-is-leader)
	1. [node-not-leader](#forgenode-not-leader)
	1. [slot-is-immutable](#forgeslot-is-immutable)
1. [GSM](#gsmstate)
	1. [state](#gsmstate)
1. [Mem](#memresident)
	1. [resident](#memresident)
1. [RTS](#rtsalloc)
	1. [alloc](#rtsalloc)
	1. [gcHeapBytes](#rtsgcheapbytes)
	1. [gcLiveBytes](#rtsgclivebytes)
	1. [gcMajorNum](#rtsgcmajornum)
	1. [gcMinorNum](#rtsgcminornum)
	1. [gcticks](#rtsgcticks)
	1. [mutticks](#rtsmutticks)
	1. [threads](#rtsthreads)
1. [Stat](#statcputicks)
	1. [cputicks](#statcputicks)
	1. [fsRd](#statfsrd)
	1. [fsWr](#statfswr)
	1. [netRd](#statnetrd)
	1. [netWr](#statnetwr)
1. [SuppressedMessages](#suppressedmessages)
	1. [](#suppressedmessages)
		1. [](#suppressedmessages)
1. [blockNum](#blocknum)
1. [blockReplayProgress](#blockreplayprogress)
1. [blockfetchclient](#blockfetchclientblockdelay)
	1. [blockdelay](#blockfetchclientblockdelay)
		1. [cdfFive](#blockfetchclientblockdelaycdffive)
		1. [cdfOne](#blockfetchclientblockdelaycdfone)
		1. [cdfThree](#blockfetchclientblockdelaycdfthree)
	1. [blocksize](#blockfetchclientblocksize)
	1. [lateblocks](#blockfetchclientlateblocks)
1. [blocksForged](#blocksforged)
1. [cardano_build_info](#cardano_build_info)
1. [cardano_version_major](#cardano_version_major)
1. [cardano_version_minor](#cardano_version_minor)
1. [cardano_version_patch](#cardano_version_patch)
1. [connectedPeers](#connectedpeers)
1. [connectionManager](#connectionmanagerduplexconns)
	1. [duplexConns](#connectionmanagerduplexconns)
	1. [fullDuplexConns](#connectionmanagerfullduplexconns)
	1. [inboundConns](#connectionmanagerinboundconns)
	1. [outboundConns](#connectionmanageroutboundconns)
	1. [prunableConns](#connectionmanagerprunableconns)
	1. [unidirectionalConns](#connectionmanagerunidirectionalconns)
1. [currentKESPeriod](#currentkesperiod)
1. [delegMapSize](#delegmapsize)
1. [density](#density)
1. [epoch](#epoch)
1. [forgedSlotLast](#forgedslotlast)
1. [forging_enabled](#forging_enabled)
1. [forks](#forks)
1. [haskell_compiler_major](#haskell_compiler_major)
1. [haskell_compiler_minor](#haskell_compiler_minor)
1. [haskell_compiler_patch](#haskell_compiler_patch)
1. [inboundGovernor](#inboundgovernorcold)
	1. [Cold](#inboundgovernorcold)
	1. [Hot](#inboundgovernorhot)
	1. [Idle](#inboundgovernoridle)
	1. [Warm](#inboundgovernorwarm)
1. [localInboundGovernor](#localinboundgovernorcold)
	1. [cold](#localinboundgovernorcold)
	1. [hot](#localinboundgovernorhot)
	1. [idle](#localinboundgovernoridle)
	1. [warm](#localinboundgovernorwarm)
1. [mempoolBytes](#mempoolbytes)
1. [node](#nodestarttime)
	1. [start](#nodestarttime)
		1. [time](#nodestarttime)
1. [nodeCannotForge](#nodecannotforge)
1. [nodeIsLeader](#nodeisleader)
1. [operationalCertificateExpiryKESPeriod](#operationalcertificateexpirykesperiod)
1. [operationalCertificateStartKESPeriod](#operationalcertificatestartkesperiod)
1. [peerSelection](#peerselectionactivebigledgerpeers)
	1. [ActiveBigLedgerPeers](#peerselectionactivebigledgerpeers)
	1. [ActiveBigLedgerPeersDemotions](#peerselectionactivebigledgerpeersdemotions)
	1. [ActiveBootstrapPeers](#peerselectionactivebootstrappeers)
	1. [ActiveBootstrapPeersDemotions](#peerselectionactivebootstrappeersdemotions)
	1. [ActiveLocalRootPeers](#peerselectionactivelocalrootpeers)
	1. [ActiveLocalRootPeersDemotions](#peerselectionactivelocalrootpeersdemotions)
	1. [ActiveNonRootPeers](#peerselectionactivenonrootpeers)
	1. [ActiveNonRootPeersDemotions](#peerselectionactivenonrootpeersdemotions)
	1. [ActivePeers](#peerselectionactivepeers)
	1. [ActivePeersDemotions](#peerselectionactivepeersdemotions)
	1. [Cold](#peerselectioncold)
	1. [ColdBigLedgerPeers](#peerselectioncoldbigledgerpeers)
	1. [ColdBigLedgerPeersPromotions](#peerselectioncoldbigledgerpeerspromotions)
	1. [ColdBootstrapPeersPromotions](#peerselectioncoldbootstrappeerspromotions)
	1. [ColdNonRootPeersPromotions](#peerselectioncoldnonrootpeerspromotions)
	1. [ColdPeersPromotions](#peerselectioncoldpeerspromotions)
	1. [EstablishedBigLedgerPeers](#peerselectionestablishedbigledgerpeers)
	1. [EstablishedBootstrapPeers](#peerselectionestablishedbootstrappeers)
	1. [EstablishedLocalRootPeers](#peerselectionestablishedlocalrootpeers)
	1. [EstablishedNonRootPeers](#peerselectionestablishednonrootpeers)
	1. [EstablishedPeers](#peerselectionestablishedpeers)
	1. [Hot](#peerselectionhot)
	1. [HotBigLedgerPeers](#peerselectionhotbigledgerpeers)
	1. [KnownBigLedgerPeers](#peerselectionknownbigledgerpeers)
	1. [KnownBootstrapPeers](#peerselectionknownbootstrappeers)
	1. [KnownLocalRootPeers](#peerselectionknownlocalrootpeers)
	1. [KnownNonRootPeers](#peerselectionknownnonrootpeers)
	1. [KnownPeers](#peerselectionknownpeers)
	1. [LocalRoots](#peerselectionlocalroots)
	1. [RootPeers](#peerselectionrootpeers)
	1. [Warm](#peerselectionwarm)
	1. [WarmBigLedgerPeers](#peerselectionwarmbigledgerpeers)
	1. [WarmBigLedgerPeersDemotions](#peerselectionwarmbigledgerpeersdemotions)
	1. [WarmBigLedgerPeersPromotions](#peerselectionwarmbigledgerpeerspromotions)
	1. [WarmBootstrapPeersDemotions](#peerselectionwarmbootstrappeersdemotions)
	1. [WarmBootstrapPeersPromotions](#peerselectionwarmbootstrappeerspromotions)
	1. [WarmLocalRootPeersPromotions](#peerselectionwarmlocalrootpeerspromotions)
	1. [WarmNonRootPeersDemotions](#peerselectionwarmnonrootpeersdemotions)
	1. [WarmNonRootPeersPromotions](#peerselectionwarmnonrootpeerspromotions)
	1. [WarmPeersDemotions](#peerselectionwarmpeersdemotions)
	1. [WarmPeersPromotions](#peerselectionwarmpeerspromotions)
	1. [churn](#peerselectionchurndecreasedactivebigledgerpeersduration)
		1. [DecreasedActiveBigLedgerPeers](#peerselectionchurndecreasedactivebigledgerpeersduration)
			1. [duration](#peerselectionchurndecreasedactivebigledgerpeersduration)
		1. [DecreasedActivePeers](#peerselectionchurndecreasedactivepeersduration)
			1. [duration](#peerselectionchurndecreasedactivepeersduration)
		1. [DecreasedEstablishedBigLedgerPeers](#peerselectionchurndecreasedestablishedbigledgerpeersduration)
			1. [duration](#peerselectionchurndecreasedestablishedbigledgerpeersduration)
		1. [DecreasedEstablishedPeers](#peerselectionchurndecreasedestablishedpeersduration)
			1. [duration](#peerselectionchurndecreasedestablishedpeersduration)
		1. [DecreasedKnownBigLedgerPeers](#peerselectionchurndecreasedknownbigledgerpeersduration)
			1. [duration](#peerselectionchurndecreasedknownbigledgerpeersduration)
		1. [DecreasedKnownPeers](#peerselectionchurndecreasedknownpeersduration)
			1. [duration](#peerselectionchurndecreasedknownpeersduration)
1. [remainingKESPeriods](#remainingkesperiods)
1. [rpc](#rpcrequestqueryservicereadparams)
	1. [request](#rpcrequestqueryservicereadparams)
		1. [QueryService](#rpcrequestqueryservicereadparams)
			1. [ReadParams](#rpcrequestqueryservicereadparams)
			1. [ReadUtxos](#rpcrequestqueryservicereadutxos)
		1. [SubmitService](#rpcrequestsubmitservicesubmittx)
			1. [SubmitTx](#rpcrequestsubmitservicesubmittx)
1. [served](#servedblock)
	1. [block](#servedblock)
		1. [latest](#servedblocklatest)
	1. [header](#servedheader)
1. [slotInEpoch](#slotinepoch)
1. [slotNum](#slotnum)
1. [slotsMissed](#slotsmissed)
1. [submissions](#submissionsaccepted)
	1. [accepted](#submissionsaccepted)
	1. [rejected](#submissionsrejected)
	1. [submitted](#submissionssubmitted)
1. [systemStartTime](#systemstarttime)
1. [tipBlock](#tipblock)
1. [txsInMempool](#txsinmempool)
1. [txsMempoolTimeoutHard](#txsmempooltimeouthard)
1. [txsMempoolTimeoutSoft](#txsmempooltimeoutsoft)
1. [txsProcessedNum](#txsprocessednum)
1. [txsSyncDuration](#txssyncduration)
1. [utxoSize](#utxosize)

### [Datapoints](#datapoints)

1. [NodeInfo](#nodeinfo)
1. [NodeStartupInfo](#nodestartupinfo)

### [Configuration](#configuration)



## Trace Messages

### BlockFetch.Client.AcknowledgedFetchRequest


> Mark the point when the fetch client picks up the request added by the block fetch decision thread. Note that this event can happen fewer times than the 'AddedFetchRequest' due to fetch request merging.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### BlockFetch.Client.AddedFetchRequest


> The block fetch decision thread has added a new fetch instruction consisting of one or more individual request ranges.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### BlockFetch.Client.ClientMetrics




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### BlockFetch.Client.ClientTerminating


> The client is terminating.  Log the number of outstanding requests.


Severity:  `Notice`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### BlockFetch.Client.CompletedBlockFetch




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`
Limiter `BlockFetch.Client.CompletedBlockFetch` with frequency `2.0`

### BlockFetch.Client.CompletedFetchBatch


> Mark the successful end of receiving a streaming batch of blocks.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### BlockFetch.Client.RejectedFetchBatch


> If the other peer rejects our request then we have this event instead of 'StartedFetchBatch' and 'CompletedFetchBatch'.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### BlockFetch.Client.SendFetchRequest


> Mark the point when fetch request for a fragment is actually sent over the wire.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### BlockFetch.Client.StartedFetchBatch


> Mark the start of receiving a streaming batch of blocks. This will be followed by one or more 'CompletedBlockFetch' and a final 'CompletedFetchBatch'


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### BlockFetch.Decision.Accept


> Throughout the decision making process we accumulate reasons to decline to fetch any blocks. This message carries the intermediate and final results.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### BlockFetch.Decision.Decline


> Throughout the decision making process we accumulate reasons to decline to fetch any blocks. This message carries the intermediate and final results.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### BlockFetch.Decision.EmptyPeersFetch


> Throughout the decision making process we accumulate reasons to decline to fetch any blocks. This message carries the intermediate and final results.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### BlockFetch.Remote.Receive.BatchDone


> End of block streaming.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### BlockFetch.Remote.Receive.Block


> Stream a single block.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### BlockFetch.Remote.Receive.ClientDone


> Client termination message.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### BlockFetch.Remote.Receive.NoBlocks


> Respond that there are no blocks.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### BlockFetch.Remote.Receive.RequestRange


> Request range of blocks.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### BlockFetch.Remote.Receive.StartBatch


> Start block streaming.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### BlockFetch.Remote.Send.BatchDone


> End of block streaming.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### BlockFetch.Remote.Send.Block


> Stream a single block.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### BlockFetch.Remote.Send.ClientDone


> Client termination message.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### BlockFetch.Remote.Send.NoBlocks


> Respond that there are no blocks.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### BlockFetch.Remote.Send.RequestRange


> Request range of blocks.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### BlockFetch.Remote.Send.StartBatch


> Start block streaming.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### BlockFetch.Remote.Serialised.Receive.BatchDone


> End of block streaming.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### BlockFetch.Remote.Serialised.Receive.Block


> Stream a single block.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### BlockFetch.Remote.Serialised.Receive.ClientDone


> Client termination message.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### BlockFetch.Remote.Serialised.Receive.NoBlocks


> Respond that there are no blocks.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### BlockFetch.Remote.Serialised.Receive.RequestRange


> Request range of blocks.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### BlockFetch.Remote.Serialised.Receive.StartBatch


> Start block streaming.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### BlockFetch.Remote.Serialised.Send.BatchDone


> End of block streaming.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### BlockFetch.Remote.Serialised.Send.Block


> Stream a single block.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### BlockFetch.Remote.Serialised.Send.ClientDone


> Client termination message.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### BlockFetch.Remote.Serialised.Send.NoBlocks


> Respond that there are no blocks.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### BlockFetch.Remote.Serialised.Send.RequestRange


> Request range of blocks.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### BlockFetch.Remote.Serialised.Send.StartBatch


> Start block streaming.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### BlockFetch.Server.SendBlock


> The server sent a block to the peer.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### BlockchainTime.CurrentSlotUnknown


> Current slot is not yet known
>  This happens when the tip of our current chain is so far in the past that we cannot translate the current wallclock to a slot number, typically during syncing. Until the current slot number is known, we cannot produce blocks. Seeing this message during syncing therefore is normal and to be expected.
>  We record the current time (the time we tried to translate to a 'SlotNo') as well as the 'PastHorizonException', which provides detail on the bounds between which we /can/ do conversions. The distance between the current time and the upper bound should rapidly decrease with consecutive 'CurrentSlotUnknown' messages during syncing.


Severity:  `Warning`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### BlockchainTime.StartTimeInTheFuture


> The start time of the blockchain time is in the future
>  We have to block (for 'NominalDiffTime') until that time comes.


Severity:  `Warning`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### BlockchainTime.SystemClockMovedBack


> The system clock moved back an acceptable time span, e.g., because of an NTP sync.
>  The system clock moved back such that the new current slot would be smaller than the previous one. If this is within the configured limit, we trace this warning but *do not change the current slot*. The current slot never decreases, but the current slot may stay the same longer than expected.
>  When the system clock moved back more than the configured limit, we shut down with a fatal exception.


Severity:  `Warning`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### ChainDB.AddBlockEvent.AddBlockValidation.InvalidBlock


> An event traced during validating performed while adding a block. A point was found to be invalid.


Severity:  `Notice`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered Invisible by config value: `Silence`

### ChainDB.AddBlockEvent.AddBlockValidation.UpdateLedgerDb




Severity:  `Notice`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered Invisible by config value: `Silence`

### ChainDB.AddBlockEvent.AddBlockValidation.ValidCandidate


> An event traced during validating performed while adding a block. A candidate chain was valid.


Severity:  `Notice`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered Invisible by config value: `Silence`
Limiter `ChainDB.AddBlockEvent.AddBlockValidation.ValidCandidate` with frequency `2.0`

### ChainDB.AddBlockEvent.AddedBlockToQueue


> The block was added to the queue and will be added to the ChainDB by the background thread. The size of the queue is included..


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`
Limiter `ChainDB.AddBlockEvent.AddedBlockToQueue` with frequency `2.0`

### ChainDB.AddBlockEvent.AddedBlockToVolatileDB


> A block was added to the Volatile DB


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`
Limiter `ChainDB.AddBlockEvent.AddedBlockToVolatileDB` with frequency `2.0`

### ChainDB.AddBlockEvent.AddedReprocessLoEBlocksToQueue




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.AddBlockEvent.AddedToCurrentChain


> The new block fits onto the current chain (first fragment) and we have successfully used it to extend our (new) current chain (second fragment).


Severity:  `Notice`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.AddBlockEvent.ChainSelectionLoEDebug




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.AddBlockEvent.ChangingSelection


> The new block fits onto the current chain (first fragment) and we have successfully used it to extend our (new) current chain (second fragment).


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.AddBlockEvent.IgnoreBlockAlreadyInVolatileDB


> A block that is already in the Volatile DB was ignored.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.AddBlockEvent.IgnoreBlockOlderThanK


> A block with a 'BlockNo' more than @k@ back than the current tip was ignored.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.AddBlockEvent.IgnoreInvalidBlock


> A block that is invalid was ignored.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.AddBlockEvent.PipeliningEvent.OutdatedTentativeHeader


> We selected a new (better) chain, which cleared the previous tentative header.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.AddBlockEvent.PipeliningEvent.SetTentativeHeader


> A new tentative header got set


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.AddBlockEvent.PipeliningEvent.TrapTentativeHeader


> The body of tentative block turned out to be invalid.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.AddBlockEvent.PoppedBlockFromQueue




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.AddBlockEvent.PoppedReprocessLoEBlocksFromQueue




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.AddBlockEvent.PoppingFromQueue




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.AddBlockEvent.StoreButDontChange


> The block fits onto some fork, we'll try to switch to that fork (if it is preferable to our chain).


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.AddBlockEvent.SwitchedToAFork


> The new block fits onto some fork and we have switched to that fork (second fragment), as it is preferable to our (previous) current chain (first fragment).


Severity:  `Notice`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.AddBlockEvent.TryAddToCurrentChain


> The block fits onto the current chain, we'll try to use it to extend our chain.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.AddBlockEvent.TrySwitchToAFork


> The block fits onto some fork, we'll try to switch to that fork (if it is preferable to our chain)


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.AddPerasCertEvent.AddedPerasCertToQueue


> Peras certificate added to processing queue


Severity:  `Debug`
Privacy:   `Public`
Details:   `DDetailed`


From current configuration:
Details:   `DNormal`
Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.AddPerasCertEvent.ChainSelectionForBoostedBlock


> Perform chain selection for block boosted by Peras certificate


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.AddPerasCertEvent.IgnorePerasCertTooOld


> Peras certificate ignored as it is too old compared to immutable slot


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.AddPerasCertEvent.PerasCertBoostsBlockNotYetReceived


> Peras certificate boosts a block not yet received


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.AddPerasCertEvent.PerasCertBoostsCurrentChain


> Peras certificate boosts a block on the current selection


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.AddPerasCertEvent.PerasCertBoostsGenesis


> Peras certificate boosts the Genesis point


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.AddPerasCertEvent.PoppedPerasCertFromQueue


> Peras certificate popped from processing queue


Severity:  `Debug`
Privacy:   `Public`
Details:   `DDetailed`


From current configuration:
Details:   `DNormal`
Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.ChainSelStarvationEvent


> ChainSel is waiting for a next block to process, but there is no block in the queue. Despite the name, it is a pretty normal (and frequent) event.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.CopyToImmutableDBEvent.CopiedBlockToImmutableDB


> A block was successfully copied to the ImmDB.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`
Limiter `ChainDB.CopyToImmutableDBEvent.CopiedBlockToImmutableDB` with frequency `2.0`

### ChainDB.CopyToImmutableDBEvent.NoBlocksToCopyToImmutableDB


> There are no block to copy to the ImmDB.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.FollowerEvent.FollowerNewImmIterator


> The follower is in the 'FollowerInImmutableDB' state but the iterator is exhausted while the ImmDB has grown, so we open a new iterator to stream these blocks too.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.FollowerEvent.FollowerNoLongerInMem


> The follower was in 'FollowerInMem' state and is switched to the 'FollowerInImmutableDB' state.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.FollowerEvent.FollowerSwitchToMem


> The follower was in the 'FollowerInImmutableDB' state and is switched to the 'FollowerInMem' state.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.FollowerEvent.NewFollower


> A new follower was created.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.GCEvent.PerformedGC


> A garbage collection for the given 'SlotNo' was performed.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.GCEvent.ScheduledGC


> A garbage collection for the given 'SlotNo' was scheduled to happen at the given time.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.ImmDbEvent.CacheEvent.CurrentChunkHit


> Current chunk found in the cache.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.ImmDbEvent.CacheEvent.PastChunkEvict


> The least recently used past chunk was evicted because the cache was full.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.ImmDbEvent.CacheEvent.PastChunkExpired




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.ImmDbEvent.CacheEvent.PastChunkHit


> Past chunk found in the cache


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.ImmDbEvent.CacheEvent.PastChunkMiss


> Past chunk was not found in the cache


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.ImmDbEvent.ChunkFileDoesntFit


> The hash of the last block in the previous epoch doesn't match the previous hash of the first block in the current epoch


Severity:  `Warning`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.ImmDbEvent.ChunkValidation.InvalidChunkFile




Severity:  `Warning`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.ImmDbEvent.ChunkValidation.InvalidPrimaryIndex




Severity:  `Warning`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.ImmDbEvent.ChunkValidation.InvalidSecondaryIndex




Severity:  `Warning`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.ImmDbEvent.ChunkValidation.MissingChunkFile




Severity:  `Warning`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.ImmDbEvent.ChunkValidation.MissingPrimaryIndex




Severity:  `Warning`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.ImmDbEvent.ChunkValidation.MissingSecondaryIndex




Severity:  `Warning`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.ImmDbEvent.ChunkValidation.RewritePrimaryIndex




Severity:  `Warning`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.ImmDbEvent.ChunkValidation.RewriteSecondaryIndex




Severity:  `Warning`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.ImmDbEvent.ChunkValidation.StartedValidatingChunk




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.ImmDbEvent.ChunkValidation.ValidatedChunk




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.ImmDbEvent.DBAlreadyClosed




Severity:  `Error`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.ImmDbEvent.DBClosed


> Closing the immutable DB


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.ImmDbEvent.DeletingAfter


> Delete after


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.ImmDbEvent.Migrating


> Performing a migration of the on-disk files.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.ImmDbEvent.NoValidLastLocation


> No valid last location was found


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.ImmDbEvent.ValidatedLastLocation


> The last location was validatet


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.InitChainSelEvent.InitialChainSelected


> A garbage collection for the given 'SlotNo' was performed.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.InitChainSelEvent.StartedInitChainSelection


> A garbage collection for the given 'SlotNo' was scheduled to happen at the given time.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.InitChainSelEvent.Validation.InvalidBlock


> An event traced during validating performed while adding a block. A point was found to be invalid.


Severity:  `Error`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.InitChainSelEvent.Validation.UpdateLedgerDb




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.InitChainSelEvent.Validation.ValidCandidate


> An event traced during validating performed while adding a block. A candidate chain was valid.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.IteratorEvent.BlockGCedFromVolatileDB


> A block is no longer in the VolatileDB and isn't in the ImmDB either; it wasn't part of the current chain.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.IteratorEvent.BlockMissingFromVolatileDB


> A block is no longer in the VolatileDB because it has been garbage collected. It might now be in the ImmDB if it was part of the current chain.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.IteratorEvent.BlockWasCopiedToImmutableDB


> A block that has been garbage collected from the VolatileDB is now found and streamed from the ImmDB.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.IteratorEvent.StreamFromBoth


> Stream from both the VolatileDB and the ImmDB.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.IteratorEvent.StreamFromImmutableDB


> Stream only from the ImmDB.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.IteratorEvent.StreamFromVolatileDB


> Stream only from the VolatileDB.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.IteratorEvent.SwitchBackToVolatileDB


> We have streamed one or more blocks from the ImmDB that were part of the VolatileDB when initialising the iterator. Now, we have to look back in the VolatileDB again because the ImmDB doesn't have the next block we're looking for.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.IteratorEvent.UnknownRangeRequested.ForkTooOld




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.IteratorEvent.UnknownRangeRequested.MissingBlock




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.LastShutdownUnclean


> Last shutdown of the node didn't leave the ChainDB directory in a clean state. Therefore, revalidating all the immutable chunks is necessary to ensure the correctness of the chain.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.LedgerEvent.Flavor.V1.LMDB.BackingStoreEvent.AlreadyClosed


> An LMDB trace


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.LedgerEvent.Flavor.V1.LMDB.BackingStoreEvent.Closed


> An LMDB trace


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.LedgerEvent.Flavor.V1.LMDB.BackingStoreEvent.Closing


> An LMDB trace


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.LedgerEvent.Flavor.V1.LMDB.BackingStoreEvent.Copied


> An LMDB trace


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.LedgerEvent.Flavor.V1.LMDB.BackingStoreEvent.Copying


> An LMDB trace


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.LedgerEvent.Flavor.V1.LMDB.BackingStoreEvent.CreatedValueHandle


> An LMDB trace


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.LedgerEvent.Flavor.V1.LMDB.BackingStoreEvent.CreatingValueHandle


> An LMDB trace


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.LedgerEvent.Flavor.V1.LMDB.BackingStoreEvent.InitialisedFromCopy


> An LMDB trace


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.LedgerEvent.Flavor.V1.LMDB.BackingStoreEvent.InitialisedFromValues


> An LMDB trace


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.LedgerEvent.Flavor.V1.LMDB.BackingStoreEvent.InitialisingFromCopy


> An LMDB trace


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.LedgerEvent.Flavor.V1.LMDB.BackingStoreEvent.InitialisingFromValues


> An LMDB trace


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.LedgerEvent.Flavor.V1.LMDB.BackingStoreEvent.Opened


> An LMDB trace


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.LedgerEvent.Flavor.V1.LMDB.BackingStoreEvent.Opening


> An LMDB trace


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.LedgerEvent.Flavor.V1.LMDB.BackingStoreEvent.ValueHandleTrace.AlreadyClosed


> An LMDB trace


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.LedgerEvent.Flavor.V1.LMDB.BackingStoreEvent.ValueHandleTrace.Closed


> An LMDB trace


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.LedgerEvent.Flavor.V1.LMDB.BackingStoreEvent.ValueHandleTrace.Closing


> An LMDB trace


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.LedgerEvent.Flavor.V1.LMDB.BackingStoreEvent.ValueHandleTrace.RangeRead


> An LMDB trace


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.LedgerEvent.Flavor.V1.LMDB.BackingStoreEvent.ValueHandleTrace.RangeReading


> An LMDB trace


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.LedgerEvent.Flavor.V1.LMDB.BackingStoreEvent.ValueHandleTrace.Read


> An LMDB trace


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.LedgerEvent.Flavor.V1.LMDB.BackingStoreEvent.ValueHandleTrace.Reading


> An LMDB trace


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.LedgerEvent.Flavor.V1.LMDB.BackingStoreEvent.ValueHandleTrace.Statted


> An LMDB trace


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.LedgerEvent.Flavor.V1.LMDB.BackingStoreEvent.ValueHandleTrace.Statting


> An LMDB trace


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.LedgerEvent.Flavor.V1.LMDB.BackingStoreEvent.Writing


> An LMDB trace


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.LedgerEvent.Flavor.V1.LMDB.BackingStoreEvent.Written


> An LMDB trace


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.LedgerEvent.Flavor.V1.LMDB.Initialise


> An LMDB trace


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.LedgerEvent.Flavor.V2.BackendTrace.LSM.LSMLookup




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.LedgerEvent.Flavor.V2.BackendTrace.LSM.LSMOpenSession




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.LedgerEvent.Flavor.V2.BackendTrace.LSM.LSMSnap




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.LedgerEvent.Flavor.V2.BackendTrace.LSM.LSMTrace




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.LedgerEvent.Flavor.V2.BackendTrace.LSM.LSMUpdate




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.LedgerEvent.Flavor.V2.LedgerTablesHandleClose


> Closed a ledger tables handle


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.LedgerEvent.Flavor.V2.LedgerTablesHandleCreate


> Created a ledger tables handle


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.LedgerEvent.Flavor.V2.LedgerTablesHandleCreateFirst


> Creating the first ledger tables handle


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.LedgerEvent.Flavor.V2.LedgerTablesHandleDuplicate


> Duplicating a ledger tables handle


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.LedgerEvent.Flavor.V2.LedgerTablesHandlePush


> Pushing to a ledger tables handle


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.LedgerEvent.Flavor.V2.LedgerTablesHandleRead


> Reading from ledger tables handle


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.LedgerEvent.Forker.Close


> A forker was closed


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered Invisible by config value: `Silence`

### ChainDB.LedgerEvent.Forker.Open


> A forker is being opened


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered Invisible by config value: `Silence`

### ChainDB.LedgerEvent.Forker.Push


> A forker is pushing a new ledger state


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered Invisible by config value: `Silence`

### ChainDB.LedgerEvent.Forker.RangeRead


> A forker is range reading values


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered Invisible by config value: `Silence`

### ChainDB.LedgerEvent.Forker.Read


> A forker is reading values


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered Invisible by config value: `Silence`

### ChainDB.LedgerEvent.Forker.Statistics


> Statistics were gathered from the forker


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered Invisible by config value: `Silence`

### ChainDB.LedgerEvent.Replay.ReplayProgress.ReplayedBlock


> We replayed the given block (reference) on the genesis snapshot during the initialisation of the LedgerDB.
>  The @blockInfo@ parameter corresponds replayed block and the @replayTo@ parameter corresponds to the block at the tip of the ImmDB, i.e., the last block to replay.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.LedgerEvent.Replay.ReplayStart.ReplayFromGenesis


> There were no LedgerDB snapshots on disk, so we're replaying all blocks starting from Genesis against the initial ledger. The @replayTo@ parameter corresponds to the block at the tip of the ImmDB, i.e., the last block to replay.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.LedgerEvent.Replay.ReplayStart.ReplayFromSnapshot


> There was a LedgerDB snapshot on disk corresponding to the given tip. We're replaying more recent blocks against it. The @replayTo@ parameter corresponds to the block at the tip of the ImmDB, i.e., the last block to replay.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.LedgerEvent.Snapshot.DeletedSnapshot


> A snapshot was deleted from the disk.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.LedgerEvent.Snapshot.InvalidSnapshot


> An on disk snapshot was invalid. Unless it was suffixed or seems to be from an old node or different backend, it will be deleted


Severity:  `Error`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.LedgerEvent.Snapshot.TookSnapshot


> A snapshot is being written to disk. Two events will be traced, one for when the node starts taking the snapshot and another one for when the snapshot has been written to the disk.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.OpenEvent.ClosedDB


> The ChainDB was closed.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.OpenEvent.OpenedDB


> The ChainDB was opened.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.OpenEvent.OpenedImmutableDB


> The ImmDB was opened.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.OpenEvent.OpenedLgrDB


> The LedgerDB was opened.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.OpenEvent.OpenedVolatileDB


> The VolatileDB was opened.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.OpenEvent.StartedOpeningDB


> The ChainDB is being opened.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.OpenEvent.StartedOpeningImmutableDB


> The ImmDB is being opened.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.OpenEvent.StartedOpeningLgrDB


> The LedgerDB is being opened.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.OpenEvent.StartedOpeningVolatileDB


> The VolatileDB is being opened.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.PerasCertDbEvent.AddedPerasCert


> Certificate added to Peras certificate database


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.PerasCertDbEvent.AddingPerasCert


> Adding certificate to Peras certificate database


Severity:  `Debug`
Privacy:   `Public`
Details:   `DDetailed`


From current configuration:
Details:   `DNormal`
Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.PerasCertDbEvent.ClosedPerasCertDB


> Peras certificate database closed


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.PerasCertDbEvent.IgnoredCertAlreadyInDB


> Certificate ignored as it was already in the database


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.PerasCertDbEvent.OpenedPerasCertDB


> Peras certificate database opened


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.ReplayBlock.LedgerReplay


> Counts block replays and calculates the percent.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### ChainDB.VolatileDbEvent.BlockAlreadyHere


> A block was found to be already in the DB.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.VolatileDbEvent.DBAlreadyClosed


> When closing the DB it was found it is closed already.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.VolatileDbEvent.DBClosed


> Closing the volatile DB


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.VolatileDbEvent.InvalidFileNames


> Reports a list of invalid file paths.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainDB.VolatileDbEvent.Truncate


> Truncates a file up to offset because of the error.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### ChainSync.Client.AccessingForecastHorizon


> The slot number, which was previously beyond the forecast horizon, has now entered it


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Warning`

### ChainSync.Client.DownloadedHeader


> While following a candidate chain, we rolled forward by downloading a header.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Warning`

### ChainSync.Client.DrainingThePipe


> The client is draining the pipe of messages


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Warning`

### ChainSync.Client.Exception


> An exception was thrown by the Chain Sync Client.


Severity:  `Warning`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Warning`

### ChainSync.Client.FoundIntersection


> We found an intersection between our chain fragment and the candidate's chain.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Warning`

### ChainSync.Client.GaveLoPToken


> May have added atoken to the LoP bucket of the peer


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Warning`

### ChainSync.Client.JumpResult


> Response to a jump offer (accept or reject)


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Warning`

### ChainSync.Client.JumpingInstructionIs


> The client got its next instruction


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Warning`

### ChainSync.Client.JumpingWaitingForNextInstruction


> The client is waiting for the next instruction


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Warning`

### ChainSync.Client.OfferJump


> Offering a jump to the remote peer


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Warning`

### ChainSync.Client.RolledBack


> While following a candidate chain, we rolled back to the given point.


Severity:  `Notice`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Warning`

### ChainSync.Client.Termination


> The client has terminated.


Severity:  `Notice`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Warning`

### ChainSync.Client.ValidatedHeader


> The header has been validated


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Warning`

### ChainSync.Client.WaitingBeyondForecastHorizon


> The slot number is beyond the forecast horizon


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Warning`

### ChainSync.Local.Receive.AwaitReply


> Acknowledge the request but require the consumer to wait for the next update. This means that the consumer is synced with the producer, and the producer is waiting for its own chain state to change.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Local.Receive.Done


> We have to explain to the framework what our states mean, in terms of which party has agency in each state. 
>  Idle states are where it is for the client to send a message, busy states are where the server is expected to send a reply.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Local.Receive.FindIntersect


> Ask the producer to try to find an improved intersection point between the consumer and producer's chains. The consumer sends a sequence of points and it is up to the producer to find the first intersection point on its chain and send it back to the consumer.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Local.Receive.IntersectFound


> The reply to the consumer about an intersection found. The consumer can decide weather to send more points. 
>  The message also tells the consumer about the head point of the producer.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Local.Receive.IntersectNotFound


> The reply to the consumer that no intersection was found: none of the points the consumer supplied are on the producer chain. 
>  The message also tells the consumer about the head point of the producer.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Local.Receive.RequestNext


> Request the next update from the producer. The response can be a roll forward, a roll back or wait.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Local.Receive.RollBackward


> Tell the consumer to roll back to a given point on their chain. 
>  The message also tells the consumer about the head point of the producer.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Local.Receive.RollForward


> Tell the consumer to extend their chain with the given header. 
>  The message also tells the consumer about the head point of the producer.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Local.Send.AwaitReply


> Acknowledge the request but require the consumer to wait for the next update. This means that the consumer is synced with the producer, and the producer is waiting for its own chain state to change.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Local.Send.Done


> We have to explain to the framework what our states mean, in terms of which party has agency in each state. 
>  Idle states are where it is for the client to send a message, busy states are where the server is expected to send a reply.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Local.Send.FindIntersect


> Ask the producer to try to find an improved intersection point between the consumer and producer's chains. The consumer sends a sequence of points and it is up to the producer to find the first intersection point on its chain and send it back to the consumer.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Local.Send.IntersectFound


> The reply to the consumer about an intersection found. The consumer can decide weather to send more points. 
>  The message also tells the consumer about the head point of the producer.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Local.Send.IntersectNotFound


> The reply to the consumer that no intersection was found: none of the points the consumer supplied are on the producer chain. 
>  The message also tells the consumer about the head point of the producer.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Local.Send.RequestNext


> Request the next update from the producer. The response can be a roll forward, a roll back or wait.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Local.Send.RollBackward


> Tell the consumer to roll back to a given point on their chain. 
>  The message also tells the consumer about the head point of the producer.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Local.Send.RollForward


> Tell the consumer to extend their chain with the given header. 
>  The message also tells the consumer about the head point of the producer.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Remote.Receive.AwaitReply


> Acknowledge the request but require the consumer to wait for the next update. This means that the consumer is synced with the producer, and the producer is waiting for its own chain state to change.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Remote.Receive.Done


> We have to explain to the framework what our states mean, in terms of which party has agency in each state. 
>  Idle states are where it is for the client to send a message, busy states are where the server is expected to send a reply.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Remote.Receive.FindIntersect


> Ask the producer to try to find an improved intersection point between the consumer and producer's chains. The consumer sends a sequence of points and it is up to the producer to find the first intersection point on its chain and send it back to the consumer.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Remote.Receive.IntersectFound


> The reply to the consumer about an intersection found. The consumer can decide weather to send more points. 
>  The message also tells the consumer about the head point of the producer.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Remote.Receive.IntersectNotFound


> The reply to the consumer that no intersection was found: none of the points the consumer supplied are on the producer chain. 
>  The message also tells the consumer about the head point of the producer.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Remote.Receive.RequestNext


> Request the next update from the producer. The response can be a roll forward, a roll back or wait.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Remote.Receive.RollBackward


> Tell the consumer to roll back to a given point on their chain. 
>  The message also tells the consumer about the head point of the producer.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Remote.Receive.RollForward


> Tell the consumer to extend their chain with the given header. 
>  The message also tells the consumer about the head point of the producer.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Remote.Send.AwaitReply


> Acknowledge the request but require the consumer to wait for the next update. This means that the consumer is synced with the producer, and the producer is waiting for its own chain state to change.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Remote.Send.Done


> We have to explain to the framework what our states mean, in terms of which party has agency in each state. 
>  Idle states are where it is for the client to send a message, busy states are where the server is expected to send a reply.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Remote.Send.FindIntersect


> Ask the producer to try to find an improved intersection point between the consumer and producer's chains. The consumer sends a sequence of points and it is up to the producer to find the first intersection point on its chain and send it back to the consumer.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Remote.Send.IntersectFound


> The reply to the consumer about an intersection found. The consumer can decide weather to send more points. 
>  The message also tells the consumer about the head point of the producer.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Remote.Send.IntersectNotFound


> The reply to the consumer that no intersection was found: none of the points the consumer supplied are on the producer chain. 
>  The message also tells the consumer about the head point of the producer.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Remote.Send.RequestNext


> Request the next update from the producer. The response can be a roll forward, a roll back or wait.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Remote.Send.RollBackward


> Tell the consumer to roll back to a given point on their chain. 
>  The message also tells the consumer about the head point of the producer.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Remote.Send.RollForward


> Tell the consumer to extend their chain with the given header. 
>  The message also tells the consumer about the head point of the producer.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Remote.Serialised.Receive.AwaitReply


> Acknowledge the request but require the consumer to wait for the next update. This means that the consumer is synced with the producer, and the producer is waiting for its own chain state to change.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Remote.Serialised.Receive.Done


> We have to explain to the framework what our states mean, in terms of which party has agency in each state. 
>  Idle states are where it is for the client to send a message, busy states are where the server is expected to send a reply.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Remote.Serialised.Receive.FindIntersect


> Ask the producer to try to find an improved intersection point between the consumer and producer's chains. The consumer sends a sequence of points and it is up to the producer to find the first intersection point on its chain and send it back to the consumer.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Remote.Serialised.Receive.IntersectFound


> The reply to the consumer about an intersection found. The consumer can decide weather to send more points. 
>  The message also tells the consumer about the head point of the producer.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Remote.Serialised.Receive.IntersectNotFound


> The reply to the consumer that no intersection was found: none of the points the consumer supplied are on the producer chain. 
>  The message also tells the consumer about the head point of the producer.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Remote.Serialised.Receive.RequestNext


> Request the next update from the producer. The response can be a roll forward, a roll back or wait.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Remote.Serialised.Receive.RollBackward


> Tell the consumer to roll back to a given point on their chain. 
>  The message also tells the consumer about the head point of the producer.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Remote.Serialised.Receive.RollForward


> Tell the consumer to extend their chain with the given header. 
>  The message also tells the consumer about the head point of the producer.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Remote.Serialised.Send.AwaitReply


> Acknowledge the request but require the consumer to wait for the next update. This means that the consumer is synced with the producer, and the producer is waiting for its own chain state to change.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Remote.Serialised.Send.Done


> We have to explain to the framework what our states mean, in terms of which party has agency in each state. 
>  Idle states are where it is for the client to send a message, busy states are where the server is expected to send a reply.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Remote.Serialised.Send.FindIntersect


> Ask the producer to try to find an improved intersection point between the consumer and producer's chains. The consumer sends a sequence of points and it is up to the producer to find the first intersection point on its chain and send it back to the consumer.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Remote.Serialised.Send.IntersectFound


> The reply to the consumer about an intersection found. The consumer can decide weather to send more points. 
>  The message also tells the consumer about the head point of the producer.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Remote.Serialised.Send.IntersectNotFound


> The reply to the consumer that no intersection was found: none of the points the consumer supplied are on the producer chain. 
>  The message also tells the consumer about the head point of the producer.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Remote.Serialised.Send.RequestNext


> Request the next update from the producer. The response can be a roll forward, a roll back or wait.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Remote.Serialised.Send.RollBackward


> Tell the consumer to roll back to a given point on their chain. 
>  The message also tells the consumer about the head point of the producer.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.Remote.Serialised.Send.RollForward


> Tell the consumer to extend their chain with the given header. 
>  The message also tells the consumer about the head point of the producer.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.ServerBlock.Update


> A server read has occurred, either for an add block or a rollback


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### ChainSync.ServerHeader.Update


> A server read has occurred, either for an add block or a rollback


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Consensus.CSJ.BecomingObjector


> This peer is becoming the CSJ objector


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Consensus.CSJ.BlockedOnJump


> This peer is blocked on a CSJ jump


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Consensus.CSJ.InitializedAsDynamo


> This peer has been initialized as the CSJ dynamo


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Consensus.CSJ.NoLongerDynamo


> This peer no longer is the CSJ dynamo


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Consensus.CSJ.NoLongerObjector


> This peer no longer is the CSJ objector


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Consensus.CSJ.SentJumpInstruction


> This peer has been instructed to jump via CSJ


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Consensus.DevotedBlockFetch.RotatedDynamo


> The ChainSync Jumping module has been asked to rotate its dynamo


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Consensus.GDD.TraceGDDEvent


> The Genesis Density Disconnection governor has updated its state


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Consensus.GSM.EnterCaughtUp


> Node is caught up


Severity:  `Notice`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Consensus.GSM.InitializedInCaughtUp


> The GSM was initialized in the 'CaughtUp' state


Severity:  `Notice`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Consensus.GSM.InitializedInPreSyncing


> The GSM was initialized in the 'PreSyncing' state


Severity:  `Notice`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Consensus.GSM.LeaveCaughtUp


> Node is not caught up


Severity:  `Warning`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Consensus.GSM.PreSyncingToSyncing


> The Honest Availability Assumption is now satisfied


Severity:  `Notice`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Consensus.GSM.SyncingToPreSyncing


> The Honest Availability Assumption is no longer satisfied


Severity:  `Notice`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Consensus.SanityCheck.SanityCheckIssue




Severity:  `Error`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Consensus.Startup.ConsensusStartupException




Severity:  `Error`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Forge.Loop.AdoptedBlock


> We adopted the block we produced, we also trace the transactions  that were adopted.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Forge.Loop.AdoptionThreadDied


> Block adoption thread died


Severity:  `Error`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Forge.Loop.BlockContext


> We found out to which block we are going to connect the block we are about  to forge.   We record the current slot number, the block number of the block to  connect to and its point.   Note that block number of the block we will try to forge is one more than  the recorded block number.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### Forge.Loop.BlockFromFuture


> Leadership check failed: the current chain contains a block from a slot  /after/ the current slot   This can only happen if the system is under heavy load.   We record both the current slot number as well as the slot number of the  block at the tip of the chain.   See also <https://github.com/IntersectMBO/ouroboros-consensus/issues/732>


Severity:  `Error`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Forge.Loop.DidntAdoptBlock


> We did not adopt the block we produced, but the block was valid. We  must have adopted a block that another leader of the same slot produced  before we got the chance of adopting our own block. This is very rare,  this warrants a warning.


Severity:  `Error`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Forge.Loop.ForgeStateUpdateError


> Updating the forge state failed.   For example, the KES key could not be evolved anymore.   We record the error returned by 'updateForgeState'.


Severity:  `Critical`
Privacy:   `Confidential`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Forge.Loop.ForgeTickedLedgerState




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### Forge.Loop.ForgedBlock


> We forged a block.
>   We record the current slot number, the point of the predecessor, the block  itself, and the total size of the mempool snapshot at the time we produced  the block (which may be significantly larger than the block, due to  maximum block size)
>   This will be followed by one of three messages:
>   * AdoptedBlock (normally)
>   * DidntAdoptBlock (rarely)
>   * ForgedInvalidBlock (hopefully never, this would indicate a bug)


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Forge.Loop.ForgedInvalidBlock


> We forged a block that is invalid according to the ledger in the  ChainDB. This means there is an inconsistency between the mempool  validation and the ledger validation. This is a serious error!


Severity:  `Error`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Forge.Loop.ForgingMempoolSnapshot




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### Forge.Loop.LedgerState


> We obtained a ledger state for the point of the block we want to  connect to   We record both the current slot number as well as the point of the block  we attempt to connect the new block to (that we requested the ledger  state for).


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### Forge.Loop.LedgerView


> We obtained a ledger view for the current slot number   We record the current slot number.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### Forge.Loop.NoLedgerState


> Leadership check failed: we were unable to get the ledger state for the  point of the block we want to connect to   This can happen if after choosing which block to connect to the node  switched to a different fork. We expect this to happen only rather  rarely, so this certainly merits a warning; if it happens a lot, that  merits an investigation.   We record both the current slot number as well as the point of the block  we attempt to connect the new block to (that we requested the ledger  state for).


Severity:  `Error`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Forge.Loop.NoLedgerView


> Leadership check failed: we were unable to get the ledger view for the  current slot number   This will only happen if there are many missing blocks between the tip of  our chain and the current slot.   We record also the failure returned by 'forecastFor'.


Severity:  `Error`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Forge.Loop.NodeCannotForge


> We did the leadership check and concluded that we should lead and forge  a block, but cannot.   This should only happen rarely and should be logged with warning severity.   Records why we cannot forge a block.


Severity:  `Error`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Forge.Loop.NodeIsLeader


> We did the leadership check and concluded we /are/ the leader
>   The node will soon forge; it is about to read its transactions from the  Mempool. This will be followed by ForgedBlock.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Forge.Loop.NodeNotLeader


> We did the leadership check and concluded we are not the leader   We record the current slot number


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Forge.Loop.SlotIsImmutable


> Leadership check failed: the tip of the ImmutableDB inhabits the  current slot   This might happen in two cases.    1. the clock moved backwards, on restart we ignored everything from the      VolatileDB since it's all in the future, and now the tip of the      ImmutableDB points to a block produced in the same slot we're trying      to produce a block in    2. k = 0 and we already adopted a block from another leader of the same      slot.   We record both the current slot number as well as the tip of the  ImmutableDB.  See also <https://github.com/IntersectMBO/ouroboros-consensus/issues/732>


Severity:  `Error`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Forge.Loop.StartLeadershipCheck


> Start of the leadership check.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Forge.StateInfo


> kesStartPeriod 
> kesEndPeriod is kesStartPeriod + tpraosMaxKESEvo
> kesEvolution is the current evolution or /relative period/.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Forge.ThreadStats.ForgingStats


> nodeCannotForgeNum shows how many times this node could not forge.
> nodeIsLeaderNum shows how many times this node was leader.
> blocksForgedNum shows how many blocks did forge in this node.
> slotsMissed shows how many slots were missed in this node.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### LedgerMetrics


> Periodic trace emitted every Nth slot, approximately 700 milliseconds after slot start.
> It queries the current ledger and chain to report metrics such as UTxO set and delegation map sizes.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Mempool.AddedTx


> New, valid transaction that was added to the Mempool.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered Invisible by config value: `Silence`

### Mempool.AttemptAdd


> Mempool is about to try to validate and add a transaction.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered Invisible by config value: `Silence`

### Mempool.ManuallyRemovedTxs


> Transactions that have been manually removed from the Mempool.


Severity:  `Warning`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered Invisible by config value: `Silence`

### Mempool.RejectedTx


> New, invalid transaction that was rejected and thus not added to the Mempool.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered Invisible by config value: `Silence`

### Mempool.RemoveTxs


> Previously valid transactions that are no longer valid because of changes in the ledger state. These transactions have been removed from the Mempool.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered Invisible by config value: `Silence`

### Mempool.SyncNotNeeded


> The mempool and the LedgerDB are in sync already.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered Invisible by config value: `Silence`

### Mempool.Synced


> The mempool and the LedgerDB are syncing or in sync depending on the argument on the trace.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered Invisible by config value: `Silence`

### Mempool.TipMovedBetweenSTMBlocks


> LedgerDB moved to an alternative fork between two reads during re-sync.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered Invisible by config value: `Silence`

### Net.AcceptPolicy.ConnectionHardLimit


> Hard rate limit reached, waiting until the number of connections drops below n.


Severity:  `Warning`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Net.AcceptPolicy.ConnectionLimitResume




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.AcceptPolicy.ConnectionRateLimiting


> Rate limiting accepting connections, delaying next accept for given time, currently serving n connections.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.ConnectionManager.Local.Connect




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.ConnectionManager.Local.ConnectError




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.ConnectionManager.Local.ConnectionCleanup




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.ConnectionManager.Local.ConnectionExists




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.ConnectionManager.Local.ConnectionFailure




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.ConnectionManager.Local.ConnectionHandler.Error




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.ConnectionManager.Local.ConnectionHandler.HandshakeClientError




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.ConnectionManager.Local.ConnectionHandler.HandshakeQuery




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.ConnectionManager.Local.ConnectionHandler.HandshakeServerError




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.ConnectionManager.Local.ConnectionHandler.HandshakeSuccess




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.ConnectionManager.Local.ConnectionManagerCounters




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.ConnectionManager.Local.ConnectionNotFound




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.ConnectionManager.Local.ConnectionTimeWait




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.ConnectionManager.Local.ConnectionTimeWaitDone




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.ConnectionManager.Local.ForbiddenConnection




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.ConnectionManager.Local.ForbiddenOperation




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.ConnectionManager.Local.IncludeConnection




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.ConnectionManager.Local.PruneConnections




Severity:  `Notice`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Net.ConnectionManager.Local.Shutdown




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.ConnectionManager.Local.State




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.ConnectionManager.Local.TerminatedConnection




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.ConnectionManager.Local.TerminatingConnection




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.ConnectionManager.Local.UnexpectedlyFalseAssertion




Severity:  `Error`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Net.ConnectionManager.Local.UnregisterConnection




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.ConnectionManager.Remote.Connect




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### Net.ConnectionManager.Remote.ConnectError




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.ConnectionManager.Remote.ConnectionCleanup




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### Net.ConnectionManager.Remote.ConnectionExists




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.ConnectionManager.Remote.ConnectionFailure




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.ConnectionManager.Remote.ConnectionHandler.Error




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.ConnectionManager.Remote.ConnectionHandler.HandshakeClientError




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.ConnectionManager.Remote.ConnectionHandler.HandshakeQuery




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.ConnectionManager.Remote.ConnectionHandler.HandshakeServerError




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.ConnectionManager.Remote.ConnectionHandler.HandshakeSuccess




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.ConnectionManager.Remote.ConnectionManagerCounters




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.ConnectionManager.Remote.ConnectionNotFound




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### Net.ConnectionManager.Remote.ConnectionTimeWait




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### Net.ConnectionManager.Remote.ConnectionTimeWaitDone




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.ConnectionManager.Remote.ForbiddenConnection




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.ConnectionManager.Remote.ForbiddenOperation




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.ConnectionManager.Remote.IncludeConnection




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### Net.ConnectionManager.Remote.PruneConnections




Severity:  `Notice`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.ConnectionManager.Remote.Shutdown




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.ConnectionManager.Remote.State




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.ConnectionManager.Remote.TerminatedConnection




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### Net.ConnectionManager.Remote.TerminatingConnection




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### Net.ConnectionManager.Remote.UnexpectedlyFalseAssertion




Severity:  `Error`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.ConnectionManager.Remote.UnregisterConnection




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### Net.ConnectionManager.Transition.Transition




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.InboundGovernor.Local.DemotedToColdRemote


> All mini-protocols terminated.  The boolean is true if this connection was not used by p2p-governor, and thus the connection will be terminated.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Warning`

### Net.InboundGovernor.Local.DemotedToWarmRemote


> All mini-protocols terminated.  The boolean is true if this connection was not used by p2p-governor, and thus the connection will be terminated.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Warning`

### Net.InboundGovernor.Local.Inactive




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Warning`

### Net.InboundGovernor.Local.InboundGovernorCounters




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Warning`

### Net.InboundGovernor.Local.InboundGovernorError




Severity:  `Error`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Warning`

### Net.InboundGovernor.Local.MaturedConnections




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Warning`

### Net.InboundGovernor.Local.MuxCleanExit




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Warning`

### Net.InboundGovernor.Local.MuxErrored




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Warning`

### Net.InboundGovernor.Local.NewConnection




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Warning`

### Net.InboundGovernor.Local.PromotedToHotRemote




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Warning`

### Net.InboundGovernor.Local.PromotedToWarmRemote




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Warning`

### Net.InboundGovernor.Local.RemoteState




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Warning`

### Net.InboundGovernor.Local.ResponderErrored




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Warning`

### Net.InboundGovernor.Local.ResponderRestarted




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Warning`

### Net.InboundGovernor.Local.ResponderStartFailure




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Warning`

### Net.InboundGovernor.Local.ResponderStarted




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Warning`

### Net.InboundGovernor.Local.ResponderTerminated




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Warning`

### Net.InboundGovernor.Local.UnexpectedlyFalseAssertion




Severity:  `Error`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Warning`

### Net.InboundGovernor.Local.WaitIdleRemote




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Warning`

### Net.InboundGovernor.Remote.DemotedToColdRemote


> All mini-protocols terminated.  The boolean is true if this connection was not used by p2p-governor, and thus the connection will be terminated.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.InboundGovernor.Remote.DemotedToWarmRemote


> All mini-protocols terminated.  The boolean is true if this connection was not used by p2p-governor, and thus the connection will be terminated.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.InboundGovernor.Remote.Inactive




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### Net.InboundGovernor.Remote.InboundGovernorCounters




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.InboundGovernor.Remote.InboundGovernorError




Severity:  `Error`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.InboundGovernor.Remote.MaturedConnections




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.InboundGovernor.Remote.MuxCleanExit




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### Net.InboundGovernor.Remote.MuxErrored




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.InboundGovernor.Remote.NewConnection




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### Net.InboundGovernor.Remote.PromotedToHotRemote




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.InboundGovernor.Remote.PromotedToWarmRemote




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.InboundGovernor.Remote.RemoteState




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### Net.InboundGovernor.Remote.ResponderErrored




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.InboundGovernor.Remote.ResponderRestarted




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### Net.InboundGovernor.Remote.ResponderStartFailure




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.InboundGovernor.Remote.ResponderStarted




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### Net.InboundGovernor.Remote.ResponderTerminated




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### Net.InboundGovernor.Remote.UnexpectedlyFalseAssertion




Severity:  `Error`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.InboundGovernor.Remote.WaitIdleRemote




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### Net.InboundGovernor.Transition.Transition




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Warning`

### Net.KeepAliveClient


> A server read has occurred, either for an add block or a rollback


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.Mux.Local.CleanExit


> Miniprotocol terminated cleanly.


Severity:  `Notice`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Net.Mux.Local.ExceptionExit


> Miniprotocol terminated with exception.


Severity:  `Notice`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Net.Mux.Local.NewMux


> New Mux


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.Mux.Local.StartEagerly


> Eagerly started.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.Mux.Local.StartOnDemand


> Preparing to start.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.Mux.Local.StartOnDemandAny


> Start whenever any other protocol has started.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.Mux.Local.StartedOnDemand


> Started on demand.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.Mux.Local.Starting


> Mux Starting


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.Mux.Local.State


> State.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.Mux.Local.Stopped


> Mux shutdown.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.Mux.Local.Stopping


> Mux shutdown.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.Mux.Local.Terminating


> Terminating.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.Mux.Remote.CleanExit


> Miniprotocol terminated cleanly.


Severity:  `Notice`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.Mux.Remote.ExceptionExit


> Miniprotocol terminated with exception.


Severity:  `Notice`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.Mux.Remote.NewMux


> New Mux


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### Net.Mux.Remote.StartEagerly


> Eagerly started.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### Net.Mux.Remote.StartOnDemand


> Preparing to start.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### Net.Mux.Remote.StartOnDemandAny


> Start whenever any other protocol has started.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### Net.Mux.Remote.StartedOnDemand


> Started on demand.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### Net.Mux.Remote.Starting


> Mux Starting


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### Net.Mux.Remote.State


> State.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.Mux.Remote.Stopped


> Mux shutdown.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### Net.Mux.Remote.Stopping


> Mux shutdown.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### Net.Mux.Remote.Terminating


> Terminating.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### Net.PeerSelection.Actions.ConnectionError




Severity:  `Error`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Actions.MonitoringError




Severity:  `Error`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Actions.MonitoringResult




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### Net.PeerSelection.Actions.PeerHotDuration


> Reports how long the outbound connection was in hot state


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Actions.StatusChangeFailure




Severity:  `Error`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Actions.StatusChanged




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Counters.Counters


> Peer selection peer counters


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### Net.PeerSelection.Initiator.GovernorState


> Outbound peer selection internal state


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### Net.PeerSelection.Responder.GovernorState


> Outbound peer selection internal state


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### Net.PeerSelection.Selection.BigLedgerPeersFailure




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.BigLedgerPeersRequest




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.BigLedgerPeersResults




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.BootstrapPeersFlagChangedWhilstInSensitiveState




Severity:  `Warning`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.ChurnAction




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.ChurnTimeout




Severity:  `Notice`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.ChurnWait




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.DebugState


> peer selection internal state


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.DemoteAsynchronous




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.DemoteBigLedgerPeersAsynchronous




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.DemoteHotBigLedgerPeerDone




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.DemoteHotBigLedgerPeerFailed




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.DemoteHotBigLedgerPeerFailed.CoolingToColdTimeout


> Impossible asynchronous demotion timeout


Severity:  `Error`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.DemoteHotBigLedgerPeers




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.DemoteHotDone


> target active, actual active, peer


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.DemoteHotFailed


> target active, actual active, peer, reason


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.DemoteHotFailed.CoolingToColdTimeout


> Impossible asynchronous demotion timeout


Severity:  `Error`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.DemoteHotPeers


> target active, actual active, selected peers


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.DemoteLocalAsynchronous




Severity:  `Warning`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.DemoteLocalHotPeers


> local per-group (target active, actual active), selected peers


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.DemoteWarmBigLedgerPeerDone




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.DemoteWarmBigLedgerPeerFailed




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.DemoteWarmBigLedgerPeerFailed.CoolingToColdTimeout


> Impossible asynchronous demotion timeout


Severity:  `Error`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.DemoteWarmBigLedgerPeers




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.DemoteWarmDone


> target established, actual established, peer


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.DemoteWarmFailed


> target established, actual established, peer, reason


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.DemoteWarmFailed.CoolingToColdTimeout


> Impossible asynchronous demotion timeout


Severity:  `Error`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.DemoteWarmPeers


> target established, actual established, selected peers


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.ForgetBigLedgerPeers




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.ForgetColdPeers


> target known peers, actual known peers, selected peers


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.GovernorWakeup




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.LedgerStateJudgementChanged




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.LocalRootPeersChanged




Severity:  `Notice`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.OnlyBootstrapPeers




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.OutboundGovernorCriticalFailure


> Outbound Governor was killed unexpectedly


Severity:  `Error`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.PeerShareRequests


> target known peers, actual known peers, peers available for gossip, peers selected for gossip


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### Net.PeerSelection.Selection.PeerShareResults




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Info`

### Net.PeerSelection.Selection.PeerShareResultsFiltered




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.PickInboundPeers


> An inbound connection was added to known set of outbound governor


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.PromoteColdBigLedgerPeerDone




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.PromoteColdBigLedgerPeerFailed




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.PromoteColdBigLedgerPeers




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.PromoteColdDone


> target active, actual active, selected peers


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.PromoteColdFailed


> target established, actual established, peer, delay until next promotion, reason


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.PromoteColdLocalPeers


> target local established, actual local established, selected peers


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.PromoteColdPeers


> target established, actual established, selected peers


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.PromoteWarmAborted




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.PromoteWarmBigLedgerPeerAborted




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.PromoteWarmBigLedgerPeerDone




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.PromoteWarmBigLedgerPeerFailed




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.PromoteWarmBigLedgerPeers




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.PromoteWarmDone


> target active, actual active, peer


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.PromoteWarmFailed


> target active, actual active, peer, reason


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.PromoteWarmLocalPeers


> local per-group (target active, actual active), selected peers


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.PromoteWarmPeers


> target active, actual active, selected peers


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.PublicRootsFailure




Severity:  `Error`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.PublicRootsRequest




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.PublicRootsResults




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.TargetsChanged




Severity:  `Notice`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.UseBootstrapPeersChanged




Severity:  `Notice`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.PeerSelection.Selection.VerifyPeerSnapshot




Severity:  `Error`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Net.Peers.Ledger.DisabledLedgerPeers


> Trace for when getting peers from the ledger is disabled, that is DontUseLedger.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.Peers.Ledger.FallingBackToPublicRootPeers




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.Peers.Ledger.FetchingNewLedgerState


> Trace for fetching a new list of peers from the ledger. Int is the number of peers returned.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.Peers.Ledger.NotEnoughBigLedgerPeers




Severity:  `Warning`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Net.Peers.Ledger.NotEnoughLedgerPeers




Severity:  `Warning`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Net.Peers.Ledger.PickedBigLedgerPeer


> Trace for a big ledger peer picked with accumulated and relative stake of its pool.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.Peers.Ledger.PickedBigLedgerPeers


> Trace for the number of big ledger peers we wanted to pick and the list of peers picked.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.Peers.Ledger.PickedLedgerPeer


> Trace for a peer picked with accumulated and relative stake of its pool.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.Peers.Ledger.PickedLedgerPeers


> Trace for the number of peers we wanted to pick and the list of peers picked.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.Peers.Ledger.RequestForPeers


> RequestForPeers (NumberOfPeers 1)


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.Peers.Ledger.ReusingLedgerState




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.Peers.Ledger.TraceLedgerPeersDomains




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.Peers.Ledger.TraceUseLedgerAfter


> Trace UseLedgerAfter value.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.Peers.Ledger.UsingBigLedgerPeerSnapshot


> Trace for when a request for big ledger peers is fulfilled from the snapshot file specified in the topology file.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.Peers.Ledger.WaitingOnRequest




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.Peers.LocalRoot.LocalRootDNSMap




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.Peers.LocalRoot.LocalRootDomains




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.Peers.LocalRoot.LocalRootError




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.Peers.LocalRoot.LocalRootFailure




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.Peers.LocalRoot.LocalRootGroups




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.Peers.LocalRoot.LocalRootReconfigured




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.Peers.LocalRoot.LocalRootWaiting




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.Peers.PublicRoot.PublicRootDomains




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.Peers.PublicRoot.PublicRootRelayAccessPoint




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.Server.Local.AcceptConnection




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.Server.Local.AcceptError




Severity:  `Error`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Net.Server.Local.AcceptPolicy




Severity:  `Notice`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Net.Server.Local.Error




Severity:  `Critical`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Net.Server.Local.Started




Severity:  `Notice`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Net.Server.Local.Stopped




Severity:  `Notice`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Net.Server.Remote.AcceptConnection




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Net.Server.Remote.AcceptError




Severity:  `Error`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Net.Server.Remote.AcceptPolicy




Severity:  `Notice`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Net.Server.Remote.Error




Severity:  `Critical`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Net.Server.Remote.Started




Severity:  `Notice`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Net.Server.Remote.Stopped




Severity:  `Notice`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### NodeState.NodeAddBlock


> Applying block


Severity:  `Notice`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### NodeState.NodeInitChainSelection


> Performing initial chain selection


Severity:  `Notice`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### NodeState.NodeKernelOnline


> Tracing system configured and node kernel is online


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### NodeState.NodeReplays


> Replaying chain


Severity:  `Notice`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### NodeState.NodeShutdown


> Node shutting down


Severity:  `Warning`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### NodeState.NodeStartup


> Node startup


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### NodeState.NodeTracingFailure


> Tracing system experienced a non-fatal failure during startup


Severity:  `Error`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### NodeState.NodeTracingForwardingInterrupted


> Trace/metrics forwarding connection was interrupted


Severity:  `Warning`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### NodeState.NodeTracingOnlineConfiguring


> Tracing system came online, system configuring now


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### NodeState.OpeningDbs


> ChainDB components being opened


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### NodeState.PrometheusSimple.Start


> PrometheusSimple backend is starting


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### NodeState.PrometheusSimple.Stop


> PrometheusSimple backend stopped


Severity:  `Warning`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### RPC.Error


> Normal operation errors such as request errors. Those are not harmful to the RPC server itself.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### RPC.FatalError


> RPC startup critical error.


Severity:  `Error`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### RPC.QueryService.ReadParams.Span


> Span for the ReadParams UTXORPC method.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### RPC.QueryService.ReadUtxos.Span


> Span for the ReadUtxos UTXORPC method.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### RPC.SubmitService.N2cConnectionError


> Node connection error. This should not happen, as this means that there is an issue in cardano-rpc configuration.


Severity:  `Warning`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### RPC.SubmitService.SubmitTx.Span


> Span for the SubmitTx UTXORPC method.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### RPC.SubmitService.TxDecodingError


> A regular request error, when submitted transaction decoding fails.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### RPC.SubmitService.TxValidationError


> A regular request error, when submitted transaction is invalid.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Reflection.MetricsInfo


> Writes out numbers for metrics delivered.
> This internal message can't be filtered by the current configuration


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Reflection.RememberLimiting


> ^ This message remembers of ongoing frequency limiting, and gives the number of messages that has been suppressed
> This internal message can't be filtered by the current configuration


Severity:  `Notice`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Reflection.StartLimiting


> This message indicates the start of frequency limiting
> This internal message can't be filtered by the current configuration


Severity:  `Notice`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Reflection.StopLimiting


> This message indicates the stop of frequency limiting, and gives the number of messages that has been suppressed
> This internal message can't be filtered by the current configuration


Severity:  `Notice`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Reflection.TracerConfigInfo


> Trace the tracer configuration which is effectively used.
> This internal message can't be filtered by the current configuration


Severity:  `Notice`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Reflection.TracerConsistencyWarnings


> Tracer consistency check found errors.
> This internal message can't be filtered by the current configuration


Severity:  `Warning`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Reflection.TracerInfo


> Writes out tracers with metrics and silent tracers.
> This internal message can't be filtered by the current configuration


Severity:  `Notice`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Reflection.UnknownNamespace


> A value was queried for a namespaces from a tracer,which is unknown. This indicates a bug in the tracer implementation.
> This internal message can't be filtered by the current configuration


Severity:  `Error`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Resources




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered Invisible by config value: `Silence`

### Shutdown.Abnormal


> non-isEOFerror shutdown request


Severity:  `Error`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Shutdown.ArmedAt


> Setting up node shutdown at given slot / block.


Severity:  `Warning`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Shutdown.Requested


> Node shutdown was requested.


Severity:  `Warning`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Shutdown.Requesting


> Ringing the node shutdown doorbell


Severity:  `Warning`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Shutdown.UnexpectedInput


> Received shutdown request but found unexpected input in --shutdown-ipc FD: 


Severity:  `Error`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Startup.BlockForgingBlockTypeMismatch




Severity:  `Error`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Startup.BlockForgingUpdate




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Startup.Byron


> _bibSystemStartTime_: 
> _bibSlotLength_: gives the length of a slot as time interval. 
> _bibEpochLength_: gives the number of slots which forms an epoch.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Startup.Common


> _biConfigPath_: is the path to the config in use. 
> _biProtocol_: is the name of the protocol, e.g. "Byron", "Shelley" or "Byron; Shelley". 
> _biVersion_: is the version of the node software running. 
> _biCommit_: is the commit revision of the software running. 
> _biNodeStartTime_: gives the time this node was started.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Startup.DBValidation




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Startup.DiffusionInit.ConfiguringLocalSocket


> ConfiguringLocalSocket


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Startup.DiffusionInit.ConfiguringServerSocket


> ConfiguringServerSocket


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Startup.DiffusionInit.CreateSystemdSocketForSnocketPath


> CreateSystemdSocketForSnocketPath


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Startup.DiffusionInit.CreatedLocalSocket


> CreatedLocalSocket


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Startup.DiffusionInit.CreatingServerSocket


> CreatingServerSocket


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Startup.DiffusionInit.DiffusionErrored


> DiffusionErrored


Severity:  `Critical`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Startup.DiffusionInit.ListeningLocalSocket


> ListeningLocalSocket


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Startup.DiffusionInit.ListeningServerSocket


> ListeningServerSocket


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Startup.DiffusionInit.LocalSocketUp


> LocalSocketUp


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Startup.DiffusionInit.RunLocalServer


> RunLocalServer


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Startup.DiffusionInit.RunServer


> RunServer


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Startup.DiffusionInit.ServerSocketUp


> ServerSocketUp


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Startup.DiffusionInit.SystemdSocketConfiguration


> SystemdSocketConfiguration


Severity:  `Warning`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Startup.DiffusionInit.UnsupportedLocalSystemdSocket


> UnsupportedLocalSystemdSocket


Severity:  `Warning`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Startup.DiffusionInit.UnsupportedReadySocketCase


> UnsupportedReadySocketCase


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Startup.DiffusionInit.UsingSystemdSocket


> UsingSystemdSocket


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Info`

### Startup.Info




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Startup.LedgerPeerSnapshot




Severity:  `Notice`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Startup.LedgerPeerSnapshot.Incompatible




Severity:  `Warning`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Startup.MovedTopLevelOption


> An option was moved from the top level of the config file to a subsection


Severity:  `Warning`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Startup.Network


> _niAddresses_: IPv4 or IPv6 socket ready to accept connectionsor diffusion addresses. 
> _niDiffusionMode_: shows if the node runs only initiator or bothinitiator or responder node. 
> _niDnsProducers_: shows the list of domain names to subscribe to. 
> _niIpProducers_: shows the list of ip subscription addresses.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Startup.NetworkConfig




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Startup.NetworkConfigUpdate




Severity:  `Notice`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Startup.NetworkConfigUpdateError




Severity:  `Error`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Startup.NetworkConfigUpdateUnsupported




Severity:  `Warning`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Startup.NetworkMagic




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Startup.NonP2PWarning




Severity:  `Warning`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Startup.P2PInfo




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Startup.ShelleyBased


> bisEra is the current era, e.g. "Shelley", "Allegra", "Mary" or "Alonzo". 
> _bisSystemStartTime_: 
> _bisSlotLength_: gives the length of a slot as time interval. 
> _bisEpochLength_: gives the number of slots which forms an epoch. 
> _bisSlotsPerKESPeriod_: gives the slots per KES period.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Startup.SocketConfigError




Severity:  `Error`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Startup.Time




Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Startup.WarningDevelopmentNodeToClientVersions




Severity:  `Warning`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### Startup.WarningDevelopmentNodeToNodeVersions




Severity:  `Warning`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### StateQueryServer.Receive.Acquire


> The client requests that the state as of a particular recent point on the server's chain (within K of the tip) be made available to query, and waits for confirmation or failure. 
>  From 'NodeToClient_V8' onwards if the point is not specified, current tip will be acquired.  For previous versions of the protocol 'point' must be given.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### StateQueryServer.Receive.Acquired


> The server can confirm that it has the state at the requested point.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### StateQueryServer.Receive.Done


> The client can terminate the protocol.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### StateQueryServer.Receive.Failure


> The server can report that it cannot obtain the state for the requested point.


Severity:  `Warning`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### StateQueryServer.Receive.Query


> The client can perform queries on the current acquired state.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### StateQueryServer.Receive.ReAcquire


> This is like 'MsgAcquire' but for when the client already has a state. By moving to another state directly without a 'MsgRelease' it enables optimisations on the server side (e.g. moving to the state for the immediate next block). 
>  Note that failure to re-acquire is equivalent to 'MsgRelease', rather than keeping the exiting acquired state. 
>  From 'NodeToClient_V8' onwards if the point is not specified, current tip will be acquired.  For previous versions of the protocol 'point' must be given.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### StateQueryServer.Receive.Release


> The client can instruct the server to release the state. This lets the server free resources.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### StateQueryServer.Receive.Result


> The server must reply with the queries.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### StateQueryServer.Send.Acquire


> The client requests that the state as of a particular recent point on the server's chain (within K of the tip) be made available to query, and waits for confirmation or failure. 
>  From 'NodeToClient_V8' onwards if the point is not specified, current tip will be acquired.  For previous versions of the protocol 'point' must be given.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### StateQueryServer.Send.Acquired


> The server can confirm that it has the state at the requested point.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### StateQueryServer.Send.Done


> The client can terminate the protocol.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### StateQueryServer.Send.Failure


> The server can report that it cannot obtain the state for the requested point.


Severity:  `Warning`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### StateQueryServer.Send.Query


> The client can perform queries on the current acquired state.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### StateQueryServer.Send.ReAcquire


> This is like 'MsgAcquire' but for when the client already has a state. By moving to another state directly without a 'MsgRelease' it enables optimisations on the server side (e.g. moving to the state for the immediate next block). 
>  Note that failure to re-acquire is equivalent to 'MsgRelease', rather than keeping the exiting acquired state. 
>  From 'NodeToClient_V8' onwards if the point is not specified, current tip will be acquired.  For previous versions of the protocol 'point' must be given.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### StateQueryServer.Send.Release


> The client can instruct the server to release the state. This lets the server free resources.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### StateQueryServer.Send.Result


> The server must reply with the queries.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.Local.Receive.AcceptTx


> The server can reply to inform the client that it has accepted the transaction.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.Local.Receive.Done


> The client can terminate the protocol.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.Local.Receive.RejectTx


> The server can reply to inform the client that it has rejected the transaction. A reason for the rejection is included.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.Local.Receive.SubmitTx


> The client submits a single transaction and waits a reply.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.Local.Send.AcceptTx


> The server can reply to inform the client that it has accepted the transaction.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.Local.Send.Done


> The client can terminate the protocol.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.Local.Send.RejectTx


> The server can reply to inform the client that it has rejected the transaction. A reason for the rejection is included.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.Local.Send.SubmitTx


> The client submits a single transaction and waits a reply.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.LocalServer.ReceivedTx


> A transaction was received.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.MonitorClient.Receive.Acquire




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.MonitorClient.Receive.Acquired




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.MonitorClient.Receive.AwaitAcquire




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.MonitorClient.Receive.Done




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.MonitorClient.Receive.GetMeasures




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.MonitorClient.Receive.GetSizes




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.MonitorClient.Receive.HasTx




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.MonitorClient.Receive.NextTx




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.MonitorClient.Receive.Release




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.MonitorClient.Receive.ReplyGetMeasures




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.MonitorClient.Receive.ReplyGetSizes




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.MonitorClient.Receive.ReplyHasTx




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.MonitorClient.Receive.ReplyNextTx




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.MonitorClient.Send.Acquire




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.MonitorClient.Send.Acquired




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.MonitorClient.Send.AwaitAcquire




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.MonitorClient.Send.Done




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.MonitorClient.Send.GetMeasures




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.MonitorClient.Send.GetSizes




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.MonitorClient.Send.HasTx




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.MonitorClient.Send.NextTx




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.MonitorClient.Send.Release




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.MonitorClient.Send.ReplyGetMeasures




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.MonitorClient.Send.ReplyGetSizes




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.MonitorClient.Send.ReplyHasTx




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.MonitorClient.Send.ReplyNextTx




Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.Remote.Receive.Done


> Termination message, initiated by the client when the server is making a blocking call for more transaction identifiers.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.Remote.Receive.MsgInit


> Client side hello message.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.Remote.Receive.ReplyTxIds


> Reply with a list of transaction identifiers for available transactions, along with the size of each transaction. 
>  The list must not be longer than the maximum number requested. 
>  In the 'StTxIds' 'StBlocking' state the list must be non-empty while in the 'StTxIds' 'StNonBlocking' state the list may be empty. 
>  These transactions are added to the notional FIFO of outstanding transaction identifiers for the protocol. 
>  The order in which these transaction identifiers are returned must be the order in which they are submitted to the mempool, to preserve dependent transactions.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.Remote.Receive.ReplyTxs


> Reply with the requested transactions, or implicitly discard.
> Transactions can become invalid between the time the transaction identifier was sent and the transaction being requested. Invalid (including committed) transactions do not need to be sent.
> Any transaction identifiers requested but not provided in this reply should be considered as if this peer had never announced them. (Note that this is no guarantee that the transaction is invalid, it may still be valid and available from another peer).


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.Remote.Receive.RequestTxIds


> Request a non-empty list of transaction identifiers from the client, and confirm a number of outstanding transaction identifiers. 
>  With 'TokBlocking' this is a blocking operation: the response will always have at least one transaction identifier, and it does not expect a prompt response: there is no timeout. This covers the case when there is nothing else to do but wait. For example this covers leaf nodes that rarely, if ever, create and submit a transaction. 
>  With 'TokNonBlocking' this is a non-blocking operation: the response may be an empty list and this does expect a prompt response. This covers high throughput use cases where we wish to pipeline, by interleaving requests for additional transaction identifiers with requests for transactions, which requires these requests not block. 
>  The request gives the maximum number of transaction identifiers that can be accepted in the response. This must be greater than zero in the 'TokBlocking' case. In the 'TokNonBlocking' case either the numbers acknowledged or the number requested must be non-zero. In either case, the number requested must not put the total outstanding over the fixed protocol limit. 
> The request also gives the number of outstanding transaction identifiers that can now be acknowledged. The actual transactions to acknowledge are known to the peer based on the FIFO order in which they were provided. 
>  There is no choice about when to use the blocking case versus the non-blocking case, it depends on whether there are any remaining unacknowledged transactions (after taking into account the ones acknowledged in this message): 
>  * The blocking case must be used when there are zero remaining   unacknowledged transactions. 
>  * The non-blocking case must be used when there are non-zero remaining   unacknowledged transactions.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.Remote.Receive.RequestTxs


> Request one or more transactions corresponding to the given  transaction identifiers.  
>  While it is the responsibility of the replying peer to keep within pipelining in-flight limits, the sender must also cooperate by keeping the total requested across all in-flight requests within the limits. 
> It is an error to ask for transaction identifiers that were not previously announced (via 'MsgReplyTxIds'). 
> It is an error to ask for transaction identifiers that are not outstanding or that were already asked for.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.Remote.Send.Done


> Termination message, initiated by the client when the server is making a blocking call for more transaction identifiers.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.Remote.Send.MsgInit


> Client side hello message.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.Remote.Send.ReplyTxIds


> Reply with a list of transaction identifiers for available transactions, along with the size of each transaction. 
>  The list must not be longer than the maximum number requested. 
>  In the 'StTxIds' 'StBlocking' state the list must be non-empty while in the 'StTxIds' 'StNonBlocking' state the list may be empty. 
>  These transactions are added to the notional FIFO of outstanding transaction identifiers for the protocol. 
>  The order in which these transaction identifiers are returned must be the order in which they are submitted to the mempool, to preserve dependent transactions.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.Remote.Send.ReplyTxs


> Reply with the requested transactions, or implicitly discard.
> Transactions can become invalid between the time the transaction identifier was sent and the transaction being requested. Invalid (including committed) transactions do not need to be sent.
> Any transaction identifiers requested but not provided in this reply should be considered as if this peer had never announced them. (Note that this is no guarantee that the transaction is invalid, it may still be valid and available from another peer).


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.Remote.Send.RequestTxIds


> Request a non-empty list of transaction identifiers from the client, and confirm a number of outstanding transaction identifiers. 
>  With 'TokBlocking' this is a blocking operation: the response will always have at least one transaction identifier, and it does not expect a prompt response: there is no timeout. This covers the case when there is nothing else to do but wait. For example this covers leaf nodes that rarely, if ever, create and submit a transaction. 
>  With 'TokNonBlocking' this is a non-blocking operation: the response may be an empty list and this does expect a prompt response. This covers high throughput use cases where we wish to pipeline, by interleaving requests for additional transaction identifiers with requests for transactions, which requires these requests not block. 
>  The request gives the maximum number of transaction identifiers that can be accepted in the response. This must be greater than zero in the 'TokBlocking' case. In the 'TokNonBlocking' case either the numbers acknowledged or the number requested must be non-zero. In either case, the number requested must not put the total outstanding over the fixed protocol limit. 
> The request also gives the number of outstanding transaction identifiers that can now be acknowledged. The actual transactions to acknowledge are known to the peer based on the FIFO order in which they were provided. 
>  There is no choice about when to use the blocking case versus the non-blocking case, it depends on whether there are any remaining unacknowledged transactions (after taking into account the ones acknowledged in this message): 
>  * The blocking case must be used when there are zero remaining   unacknowledged transactions. 
>  * The non-blocking case must be used when there are non-zero remaining   unacknowledged transactions.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.Remote.Send.RequestTxs


> Request one or more transactions corresponding to the given  transaction identifiers.  
>  While it is the responsibility of the replying peer to keep within pipelining in-flight limits, the sender must also cooperate by keeping the total requested across all in-flight requests within the limits. 
> It is an error to ask for transaction identifiers that were not previously announced (via 'MsgReplyTxIds'). 
> It is an error to ask for transaction identifiers that are not outstanding or that were already asked for.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.TxInbound.AddedToMempool


> Transactions added to the mempool and processing time


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.TxInbound.CanRequestMoreTxs


> There are no replies in flight, but we do know some more txs we can ask for, so lets ask for them and more txids.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.TxInbound.CannotRequestMoreTxs


> There's no replies in flight, and we have no more txs we can ask for so the only remaining thing to do is to ask for more txids. Since this is the only thing to do now, we make this a blocking call.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.TxInbound.Collected


> Number of transactions just about to be inserted.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.TxInbound.Decision


> Decision to advance the protocol


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.TxInbound.Error


> Protocol violation causing connection reset


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.TxInbound.Processed


> Just processed transaction pass/fail breakdown.


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.TxInbound.RejectedFromMempool


> Transactions rejected from mempool and processing time


Severity:  `Debug`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.TxInbound.Terminated


> Server received 'MsgDone'.


Severity:  `Notice`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Visible` by config value: `Notice`

### TxSubmission.TxOutbound.ControlMessage


> Peer selection control instruction


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.TxOutbound.RecvMsgRequest


> The IDs of the transactions requested.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### TxSubmission.TxOutbound.SendMsgReply


> The transactions to be sent in the response.


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`

### Version.NodeVersion


> Node version information


Severity:  `Info`
Privacy:   `Public`
Details:   `DNormal`


From current configuration:

Backends:
      `EKGBackend`,
      `Stdout MachineFormat`,
      `Forwarder`
Filtered `Invisible` by config value: `Notice`
## Metrics

### Forge.about-to-lead



Dispatched by: 
Forge.Loop.StartLeadershipCheck

### Forge.adopted



Dispatched by: 
Forge.Loop.AdoptedBlock

### Forge.adoption-thread-died



Dispatched by: 
Forge.Loop.AdoptionThreadDied

### Forge.block-from-future



Dispatched by: 
Forge.Loop.BlockFromFuture

### Forge.could-not-forge



Dispatched by: 
Forge.Loop.NoLedgerState
Forge.Loop.NoLedgerView
Forge.Loop.NodeCannotForge

### Forge.didnt-adopt



Dispatched by: 
Forge.Loop.DidntAdoptBlock

### Forge.forged

> Counter of forged blocks


Dispatched by: 
Forge.Loop.ForgedBlock

### Forge.forged-invalid



Dispatched by: 
Forge.Loop.ForgedInvalidBlock

### Forge.node-is-leader



Dispatched by: 
Forge.Loop.NodeIsLeader

### Forge.node-not-leader



Dispatched by: 
Forge.Loop.NodeNotLeader

### Forge.slot-is-immutable



Dispatched by: 
Forge.Loop.SlotIsImmutable

### GSM.state

> The state of the Genesis State Machine. 0 = PreSyncing, 1 = Syncing, 2 = CaughtUp.


Dispatched by: 
Consensus.GSM.EnterCaughtUp
Consensus.GSM.InitializedInCaughtUp
Consensus.GSM.InitializedInPreSyncing
Consensus.GSM.LeaveCaughtUp
Consensus.GSM.PreSyncingToSyncing
Consensus.GSM.SyncingToPreSyncing

### Mem.resident

> Kernel-reported RSS (resident set size)


Dispatched by: 
Resources

### RTS.alloc

> RTS-reported bytes allocated


Dispatched by: 
Resources

### RTS.gcHeapBytes

> RTS-reported heap bytes


Dispatched by: 
Resources

### RTS.gcLiveBytes

> RTS-reported live bytes


Dispatched by: 
Resources

### RTS.gcMajorNum

> Major GCs


Dispatched by: 
Resources

### RTS.gcMinorNum

> Minor GCs


Dispatched by: 
Resources

### RTS.gcticks

> RTS-reported CPU ticks spent on GC


Dispatched by: 
Resources

### RTS.mutticks

> RTS-reported CPU ticks spent on mutator


Dispatched by: 
Resources

### RTS.threads

> RTS green thread count


Dispatched by: 
Resources

### Stat.cputicks

> Kernel-reported CPU ticks (1/100th of a second), since process start


Dispatched by: 
Resources

### Stat.fsRd

> FS bytes read


Dispatched by: 
Resources

### Stat.fsWr

> FS bytes written


Dispatched by: 
Resources

### Stat.netRd

> IP packet bytes read


Dispatched by: 
Resources

### Stat.netWr

> IP packet bytes written


Dispatched by: 
Resources

### SuppressedMessages..

> Number of suppressed messages of a certain namespace


Dispatched by: 
Reflection.StartLimiting

### blockNum

> Number of blocks in this chain fragment.


Dispatched by: 
ChainDB.AddBlockEvent.AddedToCurrentChain
ChainDB.AddBlockEvent.SwitchedToAFork

### blockReplayProgress

> Progress in percent


Dispatched by: 
ChainDB.ReplayBlock.LedgerReplay

### blockfetchclient.blockdelay

> delay (s) of the latest block fetch


Dispatched by: 
BlockFetch.Client.ClientMetrics

### blockfetchclient.blockdelay.cdfFive

> probability for block fetch to complete within 5s


Dispatched by: 
BlockFetch.Client.ClientMetrics

### blockfetchclient.blockdelay.cdfOne

> probability for block fetch to complete within 1s


Dispatched by: 
BlockFetch.Client.ClientMetrics

### blockfetchclient.blockdelay.cdfThree

> probability for block fetch to complete within 3s


Dispatched by: 
BlockFetch.Client.ClientMetrics

### blockfetchclient.blocksize

> block size (bytes) of the latest block fetch


Dispatched by: 
BlockFetch.Client.ClientMetrics

### blockfetchclient.lateblocks

> number of block fetches that took longer than 5s


Dispatched by: 
BlockFetch.Client.ClientMetrics

### blocksForged

> How many blocks did this node forge?


Dispatched by: 
Forge.ThreadStats.ForgingStats

### cardano_build_info

> Cardano node build info


Dispatched by: 
Version.NodeVersion

### cardano_version_major

> Cardano node version information


Dispatched by: 
Version.NodeVersion

### cardano_version_minor

> Cardano node version information


Dispatched by: 
Version.NodeVersion

### cardano_version_patch

> Cardano node version information


Dispatched by: 
Version.NodeVersion

### connectedPeers

> Number of connected peers


Dispatched by: 
BlockFetch.Decision.Accept
BlockFetch.Decision.Decline

### connectionManager.duplexConns



Dispatched by: 
Net.ConnectionManager.Remote.ConnectionManagerCounters

### connectionManager.duplexConns



Dispatched by: 
Net.ConnectionManager.Local.ConnectionManagerCounters

### connectionManager.fullDuplexConns



Dispatched by: 
Net.ConnectionManager.Remote.ConnectionManagerCounters

### connectionManager.fullDuplexConns



Dispatched by: 
Net.ConnectionManager.Local.ConnectionManagerCounters

### connectionManager.inboundConns



Dispatched by: 
Net.ConnectionManager.Remote.ConnectionManagerCounters

### connectionManager.inboundConns



Dispatched by: 
Net.ConnectionManager.Local.ConnectionManagerCounters

### connectionManager.outboundConns



Dispatched by: 
Net.ConnectionManager.Remote.ConnectionManagerCounters

### connectionManager.outboundConns



Dispatched by: 
Net.ConnectionManager.Local.ConnectionManagerCounters

### connectionManager.prunableConns



Dispatched by: 
Net.ConnectionManager.Remote.ConnectionManagerCounters

### connectionManager.prunableConns



Dispatched by: 
Net.ConnectionManager.Local.ConnectionManagerCounters

### connectionManager.unidirectionalConns



Dispatched by: 
Net.ConnectionManager.Remote.ConnectionManagerCounters

### connectionManager.unidirectionalConns



Dispatched by: 
Net.ConnectionManager.Local.ConnectionManagerCounters

### currentKESPeriod



Dispatched by: 
Forge.StateInfo

### currentKESPeriod



Dispatched by: 
Forge.Loop.ForgeStateUpdateError

### delegMapSize

> Delegation map size


Dispatched by: 
LedgerMetrics

### density

> The actual number of blocks created over the maximum expected number of blocks that could be created over the span of the last @k@ blocks.


Dispatched by: 
ChainDB.AddBlockEvent.AddedToCurrentChain
ChainDB.AddBlockEvent.SwitchedToAFork

### epoch

> In which epoch is the tip of the current chain.


Dispatched by: 
ChainDB.AddBlockEvent.AddedToCurrentChain
ChainDB.AddBlockEvent.SwitchedToAFork

### forgedSlotLast

> Slot number of the last forged block


Dispatched by: 
Forge.Loop.ForgedBlock

### forging_enabled

> A node without forger credentials or started as non-producing has forging disabled.


Dispatched by: 
Startup.BlockForgingUpdate

### forks

> counter for forks


Dispatched by: 
ChainDB.AddBlockEvent.SwitchedToAFork

### haskell_compiler_major

> Cardano compiler version information


Dispatched by: 
Version.NodeVersion

### haskell_compiler_minor

> Cardano compiler version information


Dispatched by: 
Version.NodeVersion

### haskell_compiler_patch

> Cardano compiler version information


Dispatched by: 
Version.NodeVersion

### inboundGovernor.Cold



Dispatched by: 
Net.InboundGovernor.Remote.InboundGovernorCounters

### inboundGovernor.Cold



Dispatched by: 
Net.InboundGovernor.Local.InboundGovernorCounters

### inboundGovernor.Hot



Dispatched by: 
Net.InboundGovernor.Remote.InboundGovernorCounters

### inboundGovernor.Hot



Dispatched by: 
Net.InboundGovernor.Local.InboundGovernorCounters

### inboundGovernor.Idle



Dispatched by: 
Net.InboundGovernor.Remote.InboundGovernorCounters

### inboundGovernor.Idle



Dispatched by: 
Net.InboundGovernor.Local.InboundGovernorCounters

### inboundGovernor.Warm



Dispatched by: 
Net.InboundGovernor.Remote.InboundGovernorCounters

### inboundGovernor.Warm



Dispatched by: 
Net.InboundGovernor.Local.InboundGovernorCounters

### localInboundGovernor.cold



Dispatched by: 
Net.InboundGovernor.Remote.InboundGovernorCounters

### localInboundGovernor.cold



Dispatched by: 
Net.InboundGovernor.Local.InboundGovernorCounters

### localInboundGovernor.hot



Dispatched by: 
Net.InboundGovernor.Remote.InboundGovernorCounters

### localInboundGovernor.hot



Dispatched by: 
Net.InboundGovernor.Local.InboundGovernorCounters

### localInboundGovernor.idle



Dispatched by: 
Net.InboundGovernor.Remote.InboundGovernorCounters

### localInboundGovernor.idle



Dispatched by: 
Net.InboundGovernor.Local.InboundGovernorCounters

### localInboundGovernor.warm



Dispatched by: 
Net.InboundGovernor.Remote.InboundGovernorCounters

### localInboundGovernor.warm



Dispatched by: 
Net.InboundGovernor.Local.InboundGovernorCounters

### mempoolBytes

> Byte size of the mempool


Dispatched by: 
Mempool.AddedTx
Mempool.ManuallyRemovedTxs
Mempool.RejectedTx
Mempool.RemoveTxs

### node.start.time

> The UTC time this node was started represented in POSIX seconds.


Dispatched by: 
Startup.Common

### nodeCannotForge

> How many times was this node unable to forge [a block]?


Dispatched by: 
Forge.ThreadStats.ForgingStats

### nodeIsLeader

> How many times was this node slot leader?


Dispatched by: 
Forge.ThreadStats.ForgingStats

### operationalCertificateExpiryKESPeriod



Dispatched by: 
Forge.StateInfo

### operationalCertificateExpiryKESPeriod



Dispatched by: 
Forge.Loop.ForgeStateUpdateError

### operationalCertificateStartKESPeriod



Dispatched by: 
Forge.StateInfo

### operationalCertificateStartKESPeriod



Dispatched by: 
Forge.Loop.ForgeStateUpdateError

### peerSelection.ActiveBigLedgerPeers

> Number of active big ledger peers


Dispatched by: 
Net.PeerSelection.Counters.Counters

### peerSelection.ActiveBigLedgerPeersDemotions

> Number of active big ledger peers demotions


Dispatched by: 
Net.PeerSelection.Counters.Counters

### peerSelection.ActiveBootstrapPeers

> Number of active bootstrap peers


Dispatched by: 
Net.PeerSelection.Counters.Counters

### peerSelection.ActiveBootstrapPeersDemotions

> Number of active bootstrap peers demotions


Dispatched by: 
Net.PeerSelection.Counters.Counters

### peerSelection.ActiveLocalRootPeers

> Number of active local root peers


Dispatched by: 
Net.PeerSelection.Counters.Counters

### peerSelection.ActiveLocalRootPeersDemotions

> Number of active local root peers demotions


Dispatched by: 
Net.PeerSelection.Counters.Counters

### peerSelection.ActiveNonRootPeers

> Number of active non root peers


Dispatched by: 
Net.PeerSelection.Counters.Counters

### peerSelection.ActiveNonRootPeersDemotions

> Number of active non root peers demotions


Dispatched by: 
Net.PeerSelection.Counters.Counters

### peerSelection.ActivePeers

> Number of active peers


Dispatched by: 
Net.PeerSelection.Counters.Counters

### peerSelection.ActivePeersDemotions

> Number of active peers demotions


Dispatched by: 
Net.PeerSelection.Counters.Counters

### peerSelection.Cold

> Number of cold peers


Dispatched by: 
Net.PeerSelection.Counters.Counters

### peerSelection.ColdBigLedgerPeers

> Number of cold big ledger peers


Dispatched by: 
Net.PeerSelection.Counters.Counters

### peerSelection.ColdBigLedgerPeersPromotions

> Number of cold big ledger peers promotions


Dispatched by: 
Net.PeerSelection.Counters.Counters

### peerSelection.ColdBootstrapPeersPromotions

> Number of cold bootstrap peers promotions


Dispatched by: 
Net.PeerSelection.Counters.Counters

### peerSelection.ColdNonRootPeersPromotions

> Number of cold non root peers promotions


Dispatched by: 
Net.PeerSelection.Counters.Counters

### peerSelection.ColdPeersPromotions

> Number of cold peers promotions


Dispatched by: 
Net.PeerSelection.Counters.Counters

### peerSelection.EstablishedBigLedgerPeers

> Number of established big ledger peers


Dispatched by: 
Net.PeerSelection.Counters.Counters

### peerSelection.EstablishedBootstrapPeers

> Number of established bootstrap peers


Dispatched by: 
Net.PeerSelection.Counters.Counters

### peerSelection.EstablishedLocalRootPeers

> Number of established local root peers


Dispatched by: 
Net.PeerSelection.Counters.Counters

### peerSelection.EstablishedNonRootPeers

> Number of established non root peers


Dispatched by: 
Net.PeerSelection.Counters.Counters

### peerSelection.EstablishedPeers

> Number of established peers


Dispatched by: 
Net.PeerSelection.Counters.Counters

### peerSelection.Hot

> Number of hot peers


Dispatched by: 
Net.PeerSelection.Counters.Counters

### peerSelection.HotBigLedgerPeers

> Number of hot big ledger peers


Dispatched by: 
Net.PeerSelection.Counters.Counters

### peerSelection.KnownBigLedgerPeers

> Number of known big ledger peers


Dispatched by: 
Net.PeerSelection.Counters.Counters

### peerSelection.KnownBootstrapPeers

> Number of known bootstrap peers


Dispatched by: 
Net.PeerSelection.Counters.Counters

### peerSelection.KnownLocalRootPeers

> Number of known local root peers


Dispatched by: 
Net.PeerSelection.Counters.Counters

### peerSelection.KnownNonRootPeers

> Number of known non root peers


Dispatched by: 
Net.PeerSelection.Counters.Counters

### peerSelection.KnownPeers

> Number of known peers


Dispatched by: 
Net.PeerSelection.Counters.Counters

### peerSelection.LocalRoots

> Numbers of warm & hot local roots


Dispatched by: 
Net.PeerSelection.Counters.Counters

### peerSelection.RootPeers

> Number of root peers


Dispatched by: 
Net.PeerSelection.Counters.Counters

### peerSelection.Warm

> Number of warm peers


Dispatched by: 
Net.PeerSelection.Counters.Counters

### peerSelection.WarmBigLedgerPeers

> Number of warm big ledger peers


Dispatched by: 
Net.PeerSelection.Counters.Counters

### peerSelection.WarmBigLedgerPeersDemotions

> Number of warm big ledger peers demotions


Dispatched by: 
Net.PeerSelection.Counters.Counters

### peerSelection.WarmBigLedgerPeersPromotions

> Number of warm big ledger peers promotions


Dispatched by: 
Net.PeerSelection.Counters.Counters

### peerSelection.WarmBootstrapPeersDemotions

> Number of warm bootstrap peers demotions


Dispatched by: 
Net.PeerSelection.Counters.Counters

### peerSelection.WarmBootstrapPeersPromotions

> Number of warm bootstrap peers promotions


Dispatched by: 
Net.PeerSelection.Counters.Counters

### peerSelection.WarmLocalRootPeersPromotions

> Number of warm local root peers promotions


Dispatched by: 
Net.PeerSelection.Counters.Counters

### peerSelection.WarmNonRootPeersDemotions

> Number of warm non root peers demotions


Dispatched by: 
Net.PeerSelection.Counters.Counters

### peerSelection.WarmNonRootPeersPromotions

> Number of warm non root peers promotions


Dispatched by: 
Net.PeerSelection.Counters.Counters

### peerSelection.WarmPeersDemotions

> Number of warm peers demotions


Dispatched by: 
Net.PeerSelection.Counters.Counters

### peerSelection.WarmPeersPromotions

> Number of warm peers promotions


Dispatched by: 
Net.PeerSelection.Counters.Counters

### peerSelection.churn.DecreasedActiveBigLedgerPeers.duration



Dispatched by: 
Net.PeerSelection.Selection.ChurnAction

### peerSelection.churn.DecreasedActivePeers.duration



Dispatched by: 
Net.PeerSelection.Selection.ChurnAction

### peerSelection.churn.DecreasedEstablishedBigLedgerPeers.duration



Dispatched by: 
Net.PeerSelection.Selection.ChurnAction

### peerSelection.churn.DecreasedEstablishedPeers.duration



Dispatched by: 
Net.PeerSelection.Selection.ChurnAction

### peerSelection.churn.DecreasedKnownBigLedgerPeers.duration



Dispatched by: 
Net.PeerSelection.Selection.ChurnAction

### peerSelection.churn.DecreasedKnownPeers.duration



Dispatched by: 
Net.PeerSelection.Selection.ChurnAction

### remainingKESPeriods



Dispatched by: 
Forge.StateInfo

### remainingKESPeriods



Dispatched by: 
Forge.Loop.ForgeStateUpdateError

### rpc.request.QueryService.ReadParams

> Span for the ReadParams UTXORPC method.


Dispatched by: 
RPC.QueryService.ReadParams.Span

### rpc.request.QueryService.ReadUtxos

> Span for the ReadUtxos UTXORPC method.


Dispatched by: 
RPC.QueryService.ReadUtxos.Span

### rpc.request.SubmitService.SubmitTx

> Span for the SubmitTx UTXORPC method.


Dispatched by: 
RPC.SubmitService.SubmitTx.Span

### served.block

> This counter metric indicates how many blocks this node has served.


Dispatched by: 
BlockFetch.Server.SendBlock

### served.block.latest

> This counter metric indicates how many chain tip blocks this node has served.


Dispatched by: 
BlockFetch.Server.SendBlock

### served.header

> A counter triggered only on header event with falling edge


Dispatched by: 
ChainSync.ServerHeader.Update

### served.header

> A counter triggered only on header event with falling edge


Dispatched by: 
ChainSync.ServerBlock.Update

### slotInEpoch

> Relative slot number of the tip of the current chain within the epoch..


Dispatched by: 
ChainDB.AddBlockEvent.AddedToCurrentChain
ChainDB.AddBlockEvent.SwitchedToAFork

### slotNum

> Number of slots in this chain fragment.


Dispatched by: 
ChainDB.AddBlockEvent.AddedToCurrentChain
ChainDB.AddBlockEvent.SwitchedToAFork

### slotsMissed

> How many slots did this node miss?


Dispatched by: 
Forge.ThreadStats.ForgingStats

### submissions.accepted



Dispatched by: 
TxSubmission.TxInbound.Processed

### submissions.rejected



Dispatched by: 
TxSubmission.TxInbound.Processed

### submissions.submitted



Dispatched by: 
TxSubmission.TxInbound.Collected

### systemStartTime

> The UTC time this node was started.


Dispatched by: 
Startup.Common

### tipBlock

> Values for hash, parent hash and issuer verification key hash


Dispatched by: 
ChainDB.AddBlockEvent.AddedToCurrentChain
ChainDB.AddBlockEvent.SwitchedToAFork

### txsInMempool

> Transactions in mempool


Dispatched by: 
Mempool.AddedTx
Mempool.ManuallyRemovedTxs
Mempool.RejectedTx
Mempool.RemoveTxs

### txsMempoolTimeoutHard

> Transactions that hard timed out in mempool


Dispatched by: 
Net.Mux.Remote.ExceptionExit

### txsMempoolTimeoutHard

> Transactions that hard timed out in mempool


Dispatched by: 
Net.Mux.Local.ExceptionExit

### txsMempoolTimeoutSoft

> Transactions that soft timed out in mempool


Dispatched by: 
Mempool.RejectedTx

### txsProcessedNum



Dispatched by: 
Mempool.ManuallyRemovedTxs

### txsSyncDuration

> Time to sync the mempool in ms after block adoption


Dispatched by: 
Mempool.Synced

### utxoSize

> UTxO set size


Dispatched by: 
LedgerMetrics
## Datapoints

### NodeInfo


> Basic information about this node collected at startup
>
>  _niName_: Name of the node. 
>  _niProtocol_: Protocol which this nodes uses. 
>  _niVersion_: Software version which this node is using. 
>  _niStartTime_: Start time of this node. 
>  _niSystemStartTime_: How long did the start of the node took.


### NodeStartupInfo


> Startup information about this node, required for RTView
>
>  _suiEra_: Name of the current era. 
>  _suiSlotLength_: Slot length, in seconds. 
>  _suiEpochLength_: Epoch length, in slots. 
>  _suiSlotsPerKESPeriod_: KES period length, in slots.

## Configuration: 
```
{
    "AppicationName": null,
    "Forwarder": {
        "maxReconnectDelay": 30,
        "queueSize": 128,
        "verbosity": "Minimum"
    },
    "MetricsPrefix": "cardano.node.metrics.",
    "Options": {
        "": {
            "backends": [
                "EKGBackend",
                "Forwarder",
                "PrometheusSimple 127.0.0.1 12798",
                "Stdout HumanFormatColoured"
            ],
            "detail": "DNormal",
            "severity": "Notice"
        },
        "BlockFetch.Client.CompletedBlockFetch": {
            "maxFrequency": 2
        },
        "BlockFetch.Decision": {
            "severity": "Info"
        },
        "ChainDB": {
            "severity": "Info"
        },
        "ChainDB.AddBlockEvent.AddBlockValidation": {
            "severity": "Silence"
        },
        "ChainDB.AddBlockEvent.AddBlockValidation.ValidCandidate": {
            "maxFrequency": 2
        },
        "ChainDB.AddBlockEvent.AddedBlockToQueue": {
            "maxFrequency": 2
        },
        "ChainDB.AddBlockEvent.AddedBlockToVolatileDB": {
            "maxFrequency": 2
        },
        "ChainDB.CopyToImmutableDBEvent.CopiedBlockToImmutableDB": {
            "maxFrequency": 2
        },
        "ChainDB.LedgerEvent.Forker": {
            "severity": "Silence"
        },
        "ChainSync.Client": {
            "severity": "Warning"
        },
        "Forge.Loop": {
            "severity": "Info"
        },
        "Forge.StateInfo": {
            "severity": "Info"
        },
        "Mempool": {
            "severity": "Silence"
        },
        "Mempool.AttemptAdd": {
            "severity": "Silence"
        },
        "Mempool.SyncNotNeeded": {
            "severity": "Silence"
        },
        "Net.ConnectionManager.Remote": {
            "severity": "Info"
        },
        "Net.InboundGovernor": {
            "severity": "Warning"
        },
        "Net.InboundGovernor.Remote": {
            "severity": "Info"
        },
        "Net.Mux.Remote": {
            "severity": "Info"
        },
        "Net.PeerSelection": {
            "severity": "Info"
        },
        "Resources": {
            "severity": "Silence"
        },
        "Startup.DiffusionInit": {
            "severity": "Info"
        }
    },
    "PrometheusSimpleRun": null
}
```
663 log messages, 
159 metrics,
2 datapoints.

ⓣ- This is the root of a tracer

ⓢ- This is the root of a tracer that is silent because of the current configuration

ⓜ- This is the root of a tracer, that provides metrics

Generated at 2026-04-13 11:05:00.121894722 CEST, git commit hash f28b2ab394330fc5ac35893f1337230d1262b712, node version 10.7.1
