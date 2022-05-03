# Cardano Trace Documentation
# Table Of Contents


## [Trace Messages](#trace-messages)
1. __AcceptPolicy__
	1. [ConnectionHardLimit](#connectionhardlimit)
	1. [ConnectionLimitResume](#acceptpolicyconnectionlimitresume)
	1. [ConnectionRateLimiting](#acceptpolicyconnectionratelimiting)
1. __BlockFetch__
	1. __NodeToNode__
		1. __Recieve__
			1. [BatchDone](#batchdone)
			1. [Block](#blockfetchnodetonoderecieveblock)
			1. [ClientDone](#blockfetchnodetonoderecieveclientdone)
			1. [NoBlocks](#blockfetchnodetonoderecievenoblocks)
			1. [RequestRange](#blockfetchnodetonoderecieverequestrange)
			1. [StartBatch](#blockfetchnodetonoderecievestartbatch)
		1. __Send__
			1. [BatchDone](#batchdone)
			1. [Block](#blockfetchnodetonodesendblock)
			1. [ClientDone](#blockfetchnodetonodesendclientdone)
			1. [NoBlocks](#blockfetchnodetonodesendnoblocks)
			1. [RequestRange](#blockfetchnodetonodesendrequestrange)
			1. [StartBatch](#blockfetchnodetonodesendstartbatch)
1. __BlockFetchClient__
	1. [AcknowledgedFetchRequest](#acknowledgedfetchrequest)
	1. [AddedFetchRequest](#blockfetchclientaddedfetchrequest)
	1. [ClientTerminating](#blockfetchclientclientterminating)
	1. [CompletedBlockFetch](#blockfetchclientcompletedblockfetch)
	1. [CompletedFetchBatch](#blockfetchclientcompletedfetchbatch)
	1. [RejectedFetchBatch](#blockfetchclientrejectedfetchbatch)
	1. [SendFetchRequest](#blockfetchclientsendfetchrequest)
	1. [StartedFetchBatch](#blockfetchclientstartedfetchbatch)
1. [BlockFetchDecision](#blockfetchdecision)
1. __BlockFetchSerialised__
	1. __NodeToNode__
		1. __Recieve__
			1. [BatchDone](#batchdone)
			1. [Block](#blockfetchserialisednodetonoderecieveblock)
			1. [ClientDone](#blockfetchserialisednodetonoderecieveclientdone)
			1. [NoBlocks](#blockfetchserialisednodetonoderecievenoblocks)
			1. [RequestRange](#blockfetchserialisednodetonoderecieverequestrange)
			1. [StartBatch](#blockfetchserialisednodetonoderecievestartbatch)
		1. __Send__
			1. [BatchDone](#batchdone)
			1. [Block](#blockfetchserialisednodetonodesendblock)
			1. [ClientDone](#blockfetchserialisednodetonodesendclientdone)
			1. [NoBlocks](#blockfetchserialisednodetonodesendnoblocks)
			1. [RequestRange](#blockfetchserialisednodetonodesendrequestrange)
			1. [StartBatch](#blockfetchserialisednodetonodesendstartbatch)
1. __BlockFetchServer__
	1. [SendBlock](#sendblock)
1. __BlockchainTime__
	1. [CurrentSlotUnknown](#currentslotunknown)
	1. [StartTimeInTheFuture](#blockchaintimestarttimeinthefuture)
	1. [SystemClockMovedBack](#blockchaintimesystemclockmovedback)
1. __ChainDB__
	1. __AddBlockEvent__
		1. __AddBlockValidation__
			1. [CandidateContainsFutureBlocks](#candidatecontainsfutureblocks)
			1. [CandidateContainsFutureBlocksExceedingClockSkew](#chaindbaddblockeventaddblockvalidationcandidatecontainsfutureblocksexceedingclockskew)
			1. [InvalidBlock](#chaindbaddblockeventaddblockvalidationinvalidblock)
			1. [ValidCandidate](#chaindbaddblockeventaddblockvalidationvalidcandidate)
		1. [AddedBlockToQueue](#addedblocktoqueue)
		1. [AddedBlockToVolatileDB](#chaindbaddblockeventaddedblocktovolatiledb)
		1. [AddedToCurrentChain](#chaindbaddblockeventaddedtocurrentchain)
		1. [BlockInTheFuture](#chaindbaddblockeventblockinthefuture)
		1. [ChainSelectionForFutureBlock](#chaindbaddblockeventchainselectionforfutureblock)
		1. [IgnoreBlockAlreadyInVolatileDB](#chaindbaddblockeventignoreblockalreadyinvolatiledb)
		1. [IgnoreBlockOlderThanK](#chaindbaddblockeventignoreblockolderthank)
		1. [IgnoreInvalidBlock](#chaindbaddblockeventignoreinvalidblock)
		1. [StoreButDontChange](#chaindbaddblockeventstorebutdontchange)
		1. [SwitchedToAFork](#chaindbaddblockeventswitchedtoafork)
		1. [TryAddToCurrentChain](#chaindbaddblockeventtryaddtocurrentchain)
		1. [TrySwitchToAFork](#chaindbaddblockeventtryswitchtoafork)
	1. __TraceCopyToImmutableDBEvent__
		1. [CopiedBlockToImmutableDB](#copiedblocktoimmutabledb)
		1. [NoBlocksToCopyToImmutableDB](#chaindbtracecopytoimmutabledbeventnoblockstocopytoimmutabledb)
	1. __TraceFollowerEvent__
		1. [FollowerNewImmIterator](#followernewimmiterator)
		1. [FollowerNoLongerInMem](#chaindbtracefollowereventfollowernolongerinmem)
		1. [FollowerSwitchToMem](#chaindbtracefollowereventfollowerswitchtomem)
		1. [NewFollower](#chaindbtracefollowereventnewfollower)
	1. __TraceGCEvent__
		1. [PerformedGC](#performedgc)
		1. [ScheduledGC](#chaindbtracegceventscheduledgc)
	1. __TraceImmutableDBEvent__
		1. __CacheEvent__
			1. [CurrentChunkHit](#currentchunkhit)
			1. [PastChunkEvict](#chaindbtraceimmutabledbeventcacheeventpastchunkevict)
			1. [PastChunkExpired](#chaindbtraceimmutabledbeventcacheeventpastchunkexpired)
			1. [PastChunkHit](#chaindbtraceimmutabledbeventcacheeventpastchunkhit)
			1. [PastChunkMiss](#chaindbtraceimmutabledbeventcacheeventpastchunkmiss)
		1. [ChunkFileDoesntFit](#chunkfiledoesntfit)
		1. __ChunkValidation__
			1. [InvalidChunkFile](#invalidchunkfile)
			1. [InvalidPrimaryIndex](#chaindbtraceimmutabledbeventchunkvalidationinvalidprimaryindex)
			1. [InvalidSecondaryIndex](#chaindbtraceimmutabledbeventchunkvalidationinvalidsecondaryindex)
			1. [MissingChunkFile](#chaindbtraceimmutabledbeventchunkvalidationmissingchunkfile)
			1. [MissingPrimaryIndex](#chaindbtraceimmutabledbeventchunkvalidationmissingprimaryindex)
			1. [MissingSecondaryIndex](#chaindbtraceimmutabledbeventchunkvalidationmissingsecondaryindex)
			1. [RewritePrimaryIndex](#chaindbtraceimmutabledbeventchunkvalidationrewriteprimaryindex)
			1. [RewriteSecondaryIndex](#chaindbtraceimmutabledbeventchunkvalidationrewritesecondaryindex)
			1. [StartedValidatingChunk](#chaindbtraceimmutabledbeventchunkvalidationstartedvalidatingchunk)
			1. [ValidatedChunk](#chaindbtraceimmutabledbeventchunkvalidationvalidatedchunk)
		1. [DBAlreadyClosed](#dbalreadyclosed)
		1. [DBClosed](#chaindbtraceimmutabledbeventdbclosed)
		1. [DeletingAfter](#chaindbtraceimmutabledbeventdeletingafter)
		1. [Migrating](#chaindbtraceimmutabledbeventmigrating)
		1. [NoValidLastLocation](#chaindbtraceimmutabledbeventnovalidlastlocation)
		1. [ValidatedLastLocation](#chaindbtraceimmutabledbeventvalidatedlastlocation)
	1. __TraceInitChainSelEvent__
		1. [CandidateContainsFutureBlocks](#candidatecontainsfutureblocks)
		1. [CandidateContainsFutureBlocksExceedingClockSkew](#chaindbtraceinitchainseleventcandidatecontainsfutureblocksexceedingclockskew)
		1. [InitalChainSelected](#chaindbtraceinitchainseleventinitalchainselected)
		1. [InvalidBlock](#chaindbtraceinitchainseleventinvalidblock)
		1. [StartedInitChainSelection](#chaindbtraceinitchainseleventstartedinitchainselection)
		1. [UpdateLedgerDb](#chaindbtraceinitchainseleventupdateledgerdb)
		1. [ValidCandidate](#chaindbtraceinitchainseleventvalidcandidate)
	1. __TraceIteratorEvent__
		1. [BlockGCedFromVolatileDB](#blockgcedfromvolatiledb)
		1. [BlockMissingFromVolatileDB](#chaindbtraceiteratoreventblockmissingfromvolatiledb)
		1. [BlockWasCopiedToImmutableDB](#chaindbtraceiteratoreventblockwascopiedtoimmutabledb)
		1. [StreamFromBoth](#chaindbtraceiteratoreventstreamfromboth)
		1. [StreamFromImmutableDB](#chaindbtraceiteratoreventstreamfromimmutabledb)
		1. [StreamFromVolatileDB](#chaindbtraceiteratoreventstreamfromvolatiledb)
		1. [SwitchBackToVolatileDB](#chaindbtraceiteratoreventswitchbacktovolatiledb)
		1. [UnknownRangeRequested](#chaindbtraceiteratoreventunknownrangerequested)
	1. __TraceLedgerEvent__
		1. [DeletedSnapshot](#deletedsnapshot)
		1. [InvalidSnapshot](#chaindbtraceledgereventinvalidsnapshot)
		1. [TookSnapshot](#chaindbtraceledgereventtooksnapshot)
	1. __TraceLedgerReplayEvent__
		1. [ReplayFromGenesis](#replayfromgenesis)
		1. [ReplayFromSnapshot](#chaindbtraceledgerreplayeventreplayfromsnapshot)
		1. [ReplayedBlock](#chaindbtraceledgerreplayeventreplayedblock)
	1. __TraceOpenEvent__
		1. [ClosedDB](#closeddb)
		1. [OpenedDB](#chaindbtraceopeneventopeneddb)
		1. [OpenedImmutableDB](#chaindbtraceopeneventopenedimmutabledb)
		1. [OpenedLgrDB](#chaindbtraceopeneventopenedlgrdb)
		1. [OpenedVolatileDB](#chaindbtraceopeneventopenedvolatiledb)
		1. [StartedOpeningDB](#chaindbtraceopeneventstartedopeningdb)
		1. [StartedOpeningImmutableDB](#chaindbtraceopeneventstartedopeningimmutabledb)
		1. [StartedOpeningLgrDB](#chaindbtraceopeneventstartedopeninglgrdb)
		1. [StartedOpeningVolatileDB](#chaindbtraceopeneventstartedopeningvolatiledb)
	1. __TraceVolatileDBEvent__
		1. [BlockAlreadyHere](#blockalreadyhere)
		1. [DBAlreadyClosed](#chaindbtracevolatiledbeventdbalreadyclosed)
		1. [InvalidFileNames](#chaindbtracevolatiledbeventinvalidfilenames)
		1. [Truncate](#chaindbtracevolatiledbeventtruncate)
1. __ChainSync__
	1. __NodeToClient__
		1. __Recieve__
			1. [AwaitReply](#awaitreply)
			1. [Done](#chainsyncnodetoclientrecievedone)
			1. [FindIntersect](#chainsyncnodetoclientrecievefindintersect)
			1. [IntersectFound](#chainsyncnodetoclientrecieveintersectfound)
			1. [IntersectNotFound](#chainsyncnodetoclientrecieveintersectnotfound)
			1. [RequestNext](#chainsyncnodetoclientrecieverequestnext)
			1. [RollBackward](#chainsyncnodetoclientrecieverollbackward)
			1. [RollForward](#chainsyncnodetoclientrecieverollforward)
		1. __Send__
			1. [AwaitReply](#awaitreply)
			1. [Done](#chainsyncnodetoclientsenddone)
			1. [FindIntersect](#chainsyncnodetoclientsendfindintersect)
			1. [IntersectFound](#chainsyncnodetoclientsendintersectfound)
			1. [IntersectNotFound](#chainsyncnodetoclientsendintersectnotfound)
			1. [RequestNext](#chainsyncnodetoclientsendrequestnext)
			1. [RollBackward](#chainsyncnodetoclientsendrollbackward)
			1. [RollForward](#chainsyncnodetoclientsendrollforward)
1. __ChainSyncClient__
	1. __ChainSyncClientEvent__
		1. [DownloadedHeader](#downloadedheader)
		1. [Exception](#chainsyncclientchainsyncclienteventexception)
		1. [FoundIntersection](#chainsyncclientchainsyncclienteventfoundintersection)
		1. [RolledBack](#chainsyncclientchainsyncclienteventrolledback)
		1. [Termination](#chainsyncclientchainsyncclienteventtermination)
1. __ChainSyncNode__
	1. __NodeToNode__
		1. __Recieve__
			1. [AwaitReply](#awaitreply)
			1. [Done](#chainsyncnodenodetonoderecievedone)
			1. [FindIntersect](#chainsyncnodenodetonoderecievefindintersect)
			1. [IntersectFound](#chainsyncnodenodetonoderecieveintersectfound)
			1. [IntersectNotFound](#chainsyncnodenodetonoderecieveintersectnotfound)
			1. [RequestNext](#chainsyncnodenodetonoderecieverequestnext)
			1. [RollBackward](#chainsyncnodenodetonoderecieverollbackward)
			1. [RollForward](#chainsyncnodenodetonoderecieverollforward)
		1. __Send__
			1. [AwaitReply](#awaitreply)
			1. [Done](#chainsyncnodenodetonodesenddone)
			1. [FindIntersect](#chainsyncnodenodetonodesendfindintersect)
			1. [IntersectFound](#chainsyncnodenodetonodesendintersectfound)
			1. [IntersectNotFound](#chainsyncnodenodetonodesendintersectnotfound)
			1. [RequestNext](#chainsyncnodenodetonodesendrequestnext)
			1. [RollBackward](#chainsyncnodenodetonodesendrollbackward)
			1. [RollForward](#chainsyncnodenodetonodesendrollforward)
1. __ChainSyncSerialised__
	1. __NodeToNode__
		1. __Recieve__
			1. [AwaitReply](#awaitreply)
			1. [Done](#chainsyncserialisednodetonoderecievedone)
			1. [FindIntersect](#chainsyncserialisednodetonoderecievefindintersect)
			1. [IntersectFound](#chainsyncserialisednodetonoderecieveintersectfound)
			1. [IntersectNotFound](#chainsyncserialisednodetonoderecieveintersectnotfound)
			1. [RequestNext](#chainsyncserialisednodetonoderecieverequestnext)
			1. [RollBackward](#chainsyncserialisednodetonoderecieverollbackward)
			1. [RollForward](#chainsyncserialisednodetonoderecieverollforward)
		1. __Send__
			1. [AwaitReply](#awaitreply)
			1. [Done](#chainsyncserialisednodetonodesenddone)
			1. [FindIntersect](#chainsyncserialisednodetonodesendfindintersect)
			1. [IntersectFound](#chainsyncserialisednodetonodesendintersectfound)
			1. [IntersectNotFound](#chainsyncserialisednodetonodesendintersectnotfound)
			1. [RequestNext](#chainsyncserialisednodetonodesendrequestnext)
			1. [RollBackward](#chainsyncserialisednodetonodesendrollbackward)
			1. [RollForward](#chainsyncserialisednodetonodesendrollforward)
1. __ChainSyncServerBlock__
	1. __ChainSyncServerEvent__
		1. __ServerRead__
			1. [RollBackward](#rollbackward)
			1. [RollForward](#chainsyncserverblockchainsyncservereventserverreadrollforward)
			1. [ServerRead](#chainsyncserverblockchainsyncservereventserverreadserverread)
			1. [ServerReadBlocked](#chainsyncserverblockchainsyncservereventserverreadserverreadblocked)
1. __ChainSyncServerHeader__
	1. __ChainSyncServerEvent__
		1. __ServerRead__
			1. [RollBackward](#rollbackward)
			1. [RollForward](#chainsyncserverheaderchainsyncservereventserverreadrollforward)
			1. [ServerRead](#chainsyncserverheaderchainsyncservereventserverreadserverread)
			1. [ServerReadBlocked](#chainsyncserverheaderchainsyncservereventserverreadserverreadblocked)
1. __ConnectionManager__
	1. [Connect](#connect)
	1. [ConnectError](#connectionmanagerconnecterror)
	1. [ConnectionCleanup](#connectionmanagerconnectioncleanup)
	1. [ConnectionExists](#connectionmanagerconnectionexists)
	1. [ConnectionFailure](#connectionmanagerconnectionfailure)
	1. [ConnectionHandler](#connectionmanagerconnectionhandler)
	1. [ConnectionManagerCounters](#connectionmanagerconnectionmanagercounters)
	1. [ConnectionNotFound](#connectionmanagerconnectionnotfound)
	1. [ConnectionTimeWait](#connectionmanagerconnectiontimewait)
	1. [ConnectionTimeWaitDone](#connectionmanagerconnectiontimewaitdone)
	1. [ForbiddenConnection](#connectionmanagerforbiddenconnection)
	1. [ForbiddenOperation](#connectionmanagerforbiddenoperation)
	1. [ImpossibleConnection](#connectionmanagerimpossibleconnection)
	1. [IncludeConnection](#connectionmanagerincludeconnection)
	1. [PruneConnections](#connectionmanagerpruneconnections)
	1. [Shutdown](#connectionmanagershutdown)
	1. [State](#connectionmanagerstate)
	1. [TerminatedConnection](#connectionmanagerterminatedconnection)
	1. [TerminatingConnection](#connectionmanagerterminatingconnection)
	1. [UnexpectedlyFalseAssertion](#connectionmanagerunexpectedlyfalseassertion)
	1. [UnknownConnection](#connectionmanagerunknownconnection)
	1. [UnregisterConnection](#connectionmanagerunregisterconnection)
1. __ConnectionManagerTransition__
	1. [ConnectionManagerTransition](#connectionmanagertransition)
1. __DNSResolver__
	1. [LookupAAAAError](#lookupaaaaerror)
	1. [LookupAAAAResult](#dnsresolverlookupaaaaresult)
	1. [LookupAError](#dnsresolverlookupaerror)
	1. [LookupAResult](#dnsresolverlookuparesult)
	1. [LookupException](#dnsresolverlookupexception)
	1. [LookupIPv4First](#dnsresolverlookupipv4first)
	1. [LookupIPv6First](#dnsresolverlookupipv6first)
1. __DNSSubscription__
	1. __DNS__
		1. [AllocateSocket](#allocatesocket)
		1. [ApplicationException](#dnssubscriptiondnsapplicationexception)
		1. [CloseSocket](#dnssubscriptiondnsclosesocket)
		1. [ConnectEnd](#dnssubscriptiondnsconnectend)
		1. [ConnectException](#dnssubscriptiondnsconnectexception)
		1. [ConnectStart](#dnssubscriptiondnsconnectstart)
		1. [ConnectionExist](#dnssubscriptiondnsconnectionexist)
		1. [MissingLocalAddress](#dnssubscriptiondnsmissinglocaladdress)
		1. [Restart](#dnssubscriptiondnsrestart)
		1. [SkippingPeer](#dnssubscriptiondnsskippingpeer)
		1. [SocketAllocationException](#dnssubscriptiondnssocketallocationexception)
		1. [Start](#dnssubscriptiondnsstart)
		1. [SubscriptionFailed](#dnssubscriptiondnssubscriptionfailed)
		1. [SubscriptionRunning](#dnssubscriptiondnssubscriptionrunning)
		1. [SubscriptionWaiting](#dnssubscriptiondnssubscriptionwaiting)
		1. [SubscriptionWaitingNewConnection](#dnssubscriptiondnssubscriptionwaitingnewconnection)
		1. [TryConnectToPeer](#dnssubscriptiondnstryconnecttopeer)
		1. [UnsupportedRemoteAddr](#dnssubscriptiondnsunsupportedremoteaddr)
1. __DebugPeerSelection__
	1. __DebugPeerSelection__
		1. [GovernorState](#governorstate)
1. __DebugPeerSelectionResponder__
	1. __DebugPeerSelection__
		1. [GovernorState](#governorstate)
1. __DiffusionInit__
	1. [ConfiguringLocalSocket](#configuringlocalsocket)
	1. [ConfiguringServerSocket](#diffusioninitconfiguringserversocket)
	1. [CreateSystemdSocketForSnocketPath](#diffusioninitcreatesystemdsocketforsnocketpath)
	1. [CreatedLocalSocket](#diffusioninitcreatedlocalsocket)
	1. [CreatingServerSocket](#diffusioninitcreatingserversocket)
	1. [DiffusionErrored](#diffusioninitdiffusionerrored)
	1. [ListeningLocalSocket](#diffusioninitlisteninglocalsocket)
	1. [ListeningServerSocket](#diffusioninitlisteningserversocket)
	1. [LocalSocketUp](#diffusioninitlocalsocketup)
	1. [RunLocalServer](#diffusioninitrunlocalserver)
	1. [RunServer](#diffusioninitrunserver)
	1. [ServerSocketUp](#diffusioninitserversocketup)
	1. [UnsupportedLocalSystemdSocket](#diffusioninitunsupportedlocalsystemdsocket)
	1. [UnsupportedReadySocketCase](#diffusioninitunsupportedreadysocketcase)
	1. [UsingSystemdSocket](#diffusioninitusingsystemdsocket)
1. __ErrorPolicy__
	1. [AcceptException](#acceptexception)
	1. [KeepSuspended](#errorpolicykeepsuspended)
	1. [LocalNodeError](#errorpolicylocalnodeerror)
	1. [ResumeConsumer](#errorpolicyresumeconsumer)
	1. [ResumePeer](#errorpolicyresumepeer)
	1. [ResumeProducer](#errorpolicyresumeproducer)
	1. [SuspendConsumer](#errorpolicysuspendconsumer)
	1. [SuspendPeer](#errorpolicysuspendpeer)
	1. [UnhandledApplicationException](#errorpolicyunhandledapplicationexception)
	1. [UnhandledConnectionException](#errorpolicyunhandledconnectionexception)
1. __Forge__
	1. [AdoptedBlock](#adoptedblock)
	1. [BlockContext](#forgeblockcontext)
	1. [BlockFromFuture](#forgeblockfromfuture)
	1. [DidntAdoptBlock](#forgedidntadoptblock)
	1. [ForgeStateUpdateError](#forgeforgestateupdateerror)
	1. [ForgedBlock](#forgeforgedblock)
	1. [ForgedInvalidBlock](#forgeforgedinvalidblock)
	1. [LedgerState](#forgeledgerstate)
	1. [LedgerView](#forgeledgerview)
	1. [NoLedgerState](#forgenoledgerstate)
	1. [NoLedgerView](#forgenoledgerview)
	1. [NodeCannotForge](#forgenodecannotforge)
	1. [NodeIsLeader](#forgenodeisleader)
	1. [NodeNotLeader](#forgenodenotleader)
	1. [SlotIsImmutable](#forgeslotisimmutable)
	1. [StartLeadershipCheck](#forgestartleadershipcheck)
	1. [StartLeadershipCheckPlus](#forgestartleadershipcheckplus)
1. [ForgeStateInfo](#forgestateinfo)
1. __ForgeStats__
	1. [ForgeStats](#forgestats)
1. __Handshake__
	1. __Receive__
		1. [AcceptVersion](#acceptversion)
		1. [ProposeVersions](#handshakereceiveproposeversions)
		1. [Refuse](#handshakereceiverefuse)
		1. [ReplyVersions](#handshakereceivereplyversions)
	1. __Send__
		1. [AcceptVersion](#acceptversion)
		1. [ProposeVersions](#handshakesendproposeversions)
		1. [Refuse](#handshakesendrefuse)
		1. [ReplyVersions](#handshakesendreplyversions)
1. __InboundGovernor__
	1. [DemotedToColdRemote](#demotedtocoldremote)
	1. [DemotedToWarmRemote](#inboundgovernordemotedtowarmremote)
	1. [InboundGovernorCounters](#inboundgovernorinboundgovernorcounters)
	1. [InboundGovernorError](#inboundgovernorinboundgovernorerror)
	1. [MuxCleanExit](#inboundgovernormuxcleanexit)
	1. [MuxErrored](#inboundgovernormuxerrored)
	1. [NewConnection](#inboundgovernornewconnection)
	1. [PromotedToHotRemote](#inboundgovernorpromotedtohotremote)
	1. [PromotedToWarmRemote](#inboundgovernorpromotedtowarmremote)
	1. [RemoteState](#inboundgovernorremotestate)
	1. [ResponderErrored](#inboundgovernorrespondererrored)
	1. [ResponderRestarted](#inboundgovernorresponderrestarted)
	1. [ResponderStartFailure](#inboundgovernorresponderstartfailure)
	1. [ResponderStarted](#inboundgovernorresponderstarted)
	1. [ResponderTerminated](#inboundgovernorresponderterminated)
	1. [UnexpectedlyFalseAssertion](#inboundgovernorunexpectedlyfalseassertion)
	1. [WaitIdleRemote](#inboundgovernorwaitidleremote)
1. __InboundGovernorTransition__
	1. [InboundGovernorTransition](#inboundgovernortransition)
1. __IpSubscription__
	1. __IP__
		1. [AllocateSocket](#allocatesocket)
		1. [ApplicationException](#ipsubscriptionipapplicationexception)
		1. [CloseSocket](#ipsubscriptionipclosesocket)
		1. [ConnectEnd](#ipsubscriptionipconnectend)
		1. [ConnectException](#ipsubscriptionipconnectexception)
		1. [ConnectStart](#ipsubscriptionipconnectstart)
		1. [ConnectionExist](#ipsubscriptionipconnectionexist)
		1. [MissingLocalAddress](#ipsubscriptionipmissinglocaladdress)
		1. [Restart](#ipsubscriptioniprestart)
		1. [SkippingPeer](#ipsubscriptionipskippingpeer)
		1. [SocketAllocationException](#ipsubscriptionipsocketallocationexception)
		1. [Start](#ipsubscriptionipstart)
		1. [SubscriptionFailed](#ipsubscriptionipsubscriptionfailed)
		1. [SubscriptionRunning](#ipsubscriptionipsubscriptionrunning)
		1. [SubscriptionWaiting](#ipsubscriptionipsubscriptionwaiting)
		1. [SubscriptionWaitingNewConnection](#ipsubscriptionipsubscriptionwaitingnewconnection)
		1. [TryConnectToPeer](#ipsubscriptioniptryconnecttopeer)
		1. [UnsupportedRemoteAddr](#ipsubscriptionipunsupportedremoteaddr)
1. [KeepAliveClient](#keepaliveclient)
1. __LedgerPeers__
	1. [DisabledLedgerPeers](#disabledledgerpeers)
	1. [FallingBackToBootstrapPeers](#ledgerpeersfallingbacktobootstrappeers)
	1. [FetchingNewLedgerState](#ledgerpeersfetchingnewledgerstate)
	1. [PickedPeer](#ledgerpeerspickedpeer)
	1. [PickedPeers](#ledgerpeerspickedpeers)
	1. [RequestForPeers](#ledgerpeersrequestforpeers)
	1. [ReusingLedgerState](#ledgerpeersreusingledgerstate)
	1. [TraceUseLedgerAfter](#ledgerpeerstraceuseledgerafter)
	1. [WaitingOnRequest](#ledgerpeerswaitingonrequest)
1. __LocalConnectionManager__
	1. [Connect](#connect)
	1. [ConnectError](#localconnectionmanagerconnecterror)
	1. [ConnectionCleanup](#localconnectionmanagerconnectioncleanup)
	1. [ConnectionExists](#localconnectionmanagerconnectionexists)
	1. [ConnectionFailure](#localconnectionmanagerconnectionfailure)
	1. [ConnectionHandler](#localconnectionmanagerconnectionhandler)
	1. [ConnectionManagerCounters](#localconnectionmanagerconnectionmanagercounters)
	1. [ConnectionNotFound](#localconnectionmanagerconnectionnotfound)
	1. [ConnectionTimeWait](#localconnectionmanagerconnectiontimewait)
	1. [ConnectionTimeWaitDone](#localconnectionmanagerconnectiontimewaitdone)
	1. [ForbiddenConnection](#localconnectionmanagerforbiddenconnection)
	1. [ForbiddenOperation](#localconnectionmanagerforbiddenoperation)
	1. [ImpossibleConnection](#localconnectionmanagerimpossibleconnection)
	1. [IncludeConnection](#localconnectionmanagerincludeconnection)
	1. [PruneConnections](#localconnectionmanagerpruneconnections)
	1. [Shutdown](#localconnectionmanagershutdown)
	1. [State](#localconnectionmanagerstate)
	1. [TerminatedConnection](#localconnectionmanagerterminatedconnection)
	1. [TerminatingConnection](#localconnectionmanagerterminatingconnection)
	1. [UnexpectedlyFalseAssertion](#localconnectionmanagerunexpectedlyfalseassertion)
	1. [UnknownConnection](#localconnectionmanagerunknownconnection)
	1. [UnregisterConnection](#localconnectionmanagerunregisterconnection)
1. __LocalErrorPolicy__
	1. [AcceptException](#acceptexception)
	1. [KeepSuspended](#localerrorpolicykeepsuspended)
	1. [LocalNodeError](#localerrorpolicylocalnodeerror)
	1. [ResumeConsumer](#localerrorpolicyresumeconsumer)
	1. [ResumePeer](#localerrorpolicyresumepeer)
	1. [ResumeProducer](#localerrorpolicyresumeproducer)
	1. [SuspendConsumer](#localerrorpolicysuspendconsumer)
	1. [SuspendPeer](#localerrorpolicysuspendpeer)
	1. [UnhandledApplicationException](#localerrorpolicyunhandledapplicationexception)
	1. [UnhandledConnectionException](#localerrorpolicyunhandledconnectionexception)
1. __LocalHandshake__
	1. __Receive__
		1. [AcceptVersion](#acceptversion)
		1. [ProposeVersions](#localhandshakereceiveproposeversions)
		1. [Refuse](#localhandshakereceiverefuse)
		1. [ReplyVersions](#localhandshakereceivereplyversions)
	1. __Send__
		1. [AcceptVersion](#acceptversion)
		1. [ProposeVersions](#localhandshakesendproposeversions)
		1. [Refuse](#localhandshakesendrefuse)
		1. [ReplyVersions](#localhandshakesendreplyversions)
1. __LocalInboundGovernor__
	1. [DemotedToColdRemote](#demotedtocoldremote)
	1. [DemotedToWarmRemote](#localinboundgovernordemotedtowarmremote)
	1. [InboundGovernorCounters](#localinboundgovernorinboundgovernorcounters)
	1. [InboundGovernorError](#localinboundgovernorinboundgovernorerror)
	1. [MuxCleanExit](#localinboundgovernormuxcleanexit)
	1. [MuxErrored](#localinboundgovernormuxerrored)
	1. [NewConnection](#localinboundgovernornewconnection)
	1. [PromotedToHotRemote](#localinboundgovernorpromotedtohotremote)
	1. [PromotedToWarmRemote](#localinboundgovernorpromotedtowarmremote)
	1. [RemoteState](#localinboundgovernorremotestate)
	1. [ResponderErrored](#localinboundgovernorrespondererrored)
	1. [ResponderRestarted](#localinboundgovernorresponderrestarted)
	1. [ResponderStartFailure](#localinboundgovernorresponderstartfailure)
	1. [ResponderStarted](#localinboundgovernorresponderstarted)
	1. [ResponderTerminated](#localinboundgovernorresponderterminated)
	1. [UnexpectedlyFalseAssertion](#localinboundgovernorunexpectedlyfalseassertion)
	1. [WaitIdleRemote](#localinboundgovernorwaitidleremote)
1. __LocalRootPeers__
	1. [LocalRootDomains](#localrootdomains)
	1. [LocalRootError](#localrootpeerslocalrooterror)
	1. [LocalRootFailure](#localrootpeerslocalrootfailure)
	1. [LocalRootGroups](#localrootpeerslocalrootgroups)
	1. [LocalRootResult](#localrootpeerslocalrootresult)
	1. [LocalRootWaiting](#localrootpeerslocalrootwaiting)
1. __LocalServer__
	1. [AcceptConnection](#acceptconnection)
	1. [AcceptError](#localserveraccepterror)
	1. [AcceptPolicy](#localserveracceptpolicy)
	1. [Error](#localservererror)
	1. [Started](#localserverstarted)
	1. [Stopped](#localserverstopped)
1. __LocalTxSubmissionServer__
	1. [ReceivedTx](#receivedtx)
1. __Mempool__
	1. [AddedTx](#addedtx)
	1. [ManuallyRemovedTxs](#mempoolmanuallyremovedtxs)
	1. [RejectedTx](#mempoolrejectedtx)
	1. [RemoveTxs](#mempoolremovetxs)
1. __Mux__
	1. [ChannelRecvEnd](#channelrecvend)
	1. [ChannelRecvStart](#muxchannelrecvstart)
	1. [ChannelSendEnd](#muxchannelsendend)
	1. [ChannelSendStart](#muxchannelsendstart)
	1. [CleanExit](#muxcleanexit)
	1. [ExceptionExit](#muxexceptionexit)
	1. [HandshakeClientEnd](#muxhandshakeclientend)
	1. [HandshakeClientError](#muxhandshakeclienterror)
	1. [HandshakeServerEnd](#muxhandshakeserverend)
	1. [HandshakeServerError](#muxhandshakeservererror)
	1. [HandshakeStart](#muxhandshakestart)
	1. [RecvDeltaQObservation](#muxrecvdeltaqobservation)
	1. [RecvDeltaQSample](#muxrecvdeltaqsample)
	1. [RecvEnd](#muxrecvend)
	1. [RecvHeaderEnd](#muxrecvheaderend)
	1. [RecvHeaderStart](#muxrecvheaderstart)
	1. [RecvStart](#muxrecvstart)
	1. [SDUReadTimeoutException](#muxsdureadtimeoutexception)
	1. [SDUWriteTimeoutException](#muxsduwritetimeoutexception)
	1. [SendEnd](#muxsendend)
	1. [SendStart](#muxsendstart)
	1. [Shutdown](#muxshutdown)
	1. [StartEagerly](#muxstarteagerly)
	1. [StartOnDemand](#muxstartondemand)
	1. [StartedOnDemand](#muxstartedondemand)
	1. [State](#muxstate)
	1. [TCPInfo](#muxtcpinfo)
	1. [Terminating](#muxterminating)
1. __MuxLocal__
	1. [ChannelRecvEnd](#channelrecvend)
	1. [ChannelRecvStart](#muxlocalchannelrecvstart)
	1. [ChannelSendEnd](#muxlocalchannelsendend)
	1. [ChannelSendStart](#muxlocalchannelsendstart)
	1. [CleanExit](#muxlocalcleanexit)
	1. [ExceptionExit](#muxlocalexceptionexit)
	1. [HandshakeClientEnd](#muxlocalhandshakeclientend)
	1. [HandshakeClientError](#muxlocalhandshakeclienterror)
	1. [HandshakeServerEnd](#muxlocalhandshakeserverend)
	1. [HandshakeServerError](#muxlocalhandshakeservererror)
	1. [HandshakeStart](#muxlocalhandshakestart)
	1. [RecvDeltaQObservation](#muxlocalrecvdeltaqobservation)
	1. [RecvDeltaQSample](#muxlocalrecvdeltaqsample)
	1. [RecvEnd](#muxlocalrecvend)
	1. [RecvHeaderEnd](#muxlocalrecvheaderend)
	1. [RecvHeaderStart](#muxlocalrecvheaderstart)
	1. [RecvStart](#muxlocalrecvstart)
	1. [SDUReadTimeoutException](#muxlocalsdureadtimeoutexception)
	1. [SDUWriteTimeoutException](#muxlocalsduwritetimeoutexception)
	1. [SendEnd](#muxlocalsendend)
	1. [SendStart](#muxlocalsendstart)
	1. [Shutdown](#muxlocalshutdown)
	1. [StartEagerly](#muxlocalstarteagerly)
	1. [StartOnDemand](#muxlocalstartondemand)
	1. [StartedOnDemand](#muxlocalstartedondemand)
	1. [State](#muxlocalstate)
	1. [TCPInfo](#muxlocaltcpinfo)
	1. [Terminating](#muxlocalterminating)
1. __PeerSelection__
	1. [ChurnMode](#churnmode)
	1. [ChurnWait](#peerselectionchurnwait)
	1. [DemoteAsynchronous](#peerselectiondemoteasynchronous)
	1. [DemoteHotDone](#peerselectiondemotehotdone)
	1. [DemoteHotFailed](#peerselectiondemotehotfailed)
	1. [DemoteHotPeers](#peerselectiondemotehotpeers)
	1. [DemoteLocalHotPeers](#peerselectiondemotelocalhotpeers)
	1. [DemoteWarmDone](#peerselectiondemotewarmdone)
	1. [DemoteWarmFailed](#peerselectiondemotewarmfailed)
	1. [DemoteWarmPeers](#peerselectiondemotewarmpeers)
	1. [ForgetColdPeers](#peerselectionforgetcoldpeers)
	1. [GossipRequests](#peerselectiongossiprequests)
	1. [GossipResults](#peerselectiongossipresults)
	1. [GovernorWakeup](#peerselectiongovernorwakeup)
	1. [LocalRootPeersChanged](#peerselectionlocalrootpeerschanged)
	1. [PromoteColdDone](#peerselectionpromotecolddone)
	1. [PromoteColdFailed](#peerselectionpromotecoldfailed)
	1. [PromoteColdLocalPeers](#peerselectionpromotecoldlocalpeers)
	1. [PromoteColdPeers](#peerselectionpromotecoldpeers)
	1. [PromoteWarmAborted](#peerselectionpromotewarmaborted)
	1. [PromoteWarmDone](#peerselectionpromotewarmdone)
	1. [PromoteWarmFailed](#peerselectionpromotewarmfailed)
	1. [PromoteWarmLocalPeers](#peerselectionpromotewarmlocalpeers)
	1. [PromoteWarmPeers](#peerselectionpromotewarmpeers)
	1. [PublicRootsFailure](#peerselectionpublicrootsfailure)
	1. [PublicRootsRequest](#peerselectionpublicrootsrequest)
	1. [PublicRootsResults](#peerselectionpublicrootsresults)
	1. [TargetsChanged](#peerselectiontargetschanged)
1. __PeerSelectionActions__
	1. [MonitoringError](#monitoringerror)
	1. [MonitoringResult](#peerselectionactionsmonitoringresult)
	1. [StatusChangeFailure](#peerselectionactionsstatuschangefailure)
	1. [StatusChanged](#peerselectionactionsstatuschanged)
1. __PeerSelectionCounters__
	1. [PeerSelectionCounters](#peerselectioncounters)
1. [Peers](#peers)
1. __PublicRootPeers__
	1. __PublicRootPeers__
		1. [PublicRootDomains](#publicrootdomains)
		1. [PublicRootFailure](#publicrootpeerspublicrootpeerspublicrootfailure)
		1. [PublicRootRelayAccessPoint](#publicrootpeerspublicrootpeerspublicrootrelayaccesspoint)
		1. [PublicRootResult](#publicrootpeerspublicrootpeerspublicrootresult)
1. __ReplayBlock__
	1. [LedgerReplay](#ledgerreplay)
1. [Resources](#resources)
1. __Server__
	1. [AcceptConnection](#acceptconnection)
	1. [AcceptError](#serveraccepterror)
	1. [AcceptPolicy](#serveracceptpolicy)
	1. [Error](#servererror)
	1. [Started](#serverstarted)
	1. [Stopped](#serverstopped)
1. __Shutdown__
	1. [AbnormalShutdown](#abnormalshutdown)
	1. [RequestingShutdown](#shutdownrequestingshutdown)
	1. [ShutdownArmedAtSlot](#shutdownshutdownarmedatslot)
	1. [ShutdownRequested](#shutdownshutdownrequested)
	1. [ShutdownUnexpectedInput](#shutdownshutdownunexpectedinput)
1. __Startup__
	1. [Byron](#byron)
	1. [Common](#startupcommon)
	1. [Network](#startupnetwork)
	1. [NetworkConfig](#startupnetworkconfig)
	1. [NetworkConfigUpdate](#startupnetworkconfigupdate)
	1. [NetworkConfigUpdateError](#startupnetworkconfigupdateerror)
	1. [P2PWarning](#startupp2pwarning)
	1. [P2PWarningDevelopementNetworkProtocols](#startupp2pwarningdevelopementnetworkprotocols)
	1. [ShelleyBased](#startupshelleybased)
	1. [StartupDBValidation](#startupstartupdbvalidation)
	1. [StartupInfo](#startupstartupinfo)
	1. [StartupNetworkMagic](#startupstartupnetworkmagic)
	1. [StartupP2PInfo](#startupstartupp2pinfo)
	1. [StartupSocketConfigError](#startupstartupsocketconfigerror)
	1. [StartupTime](#startupstartuptime)
	1. [WarningDevelopmentNetworkProtocols](#startupwarningdevelopmentnetworkprotocols)
1. __StateQueryClient__
	1. __Recieve__
		1. [Acquire](#acquire)
		1. [Acquired](#statequeryclientrecieveacquired)
		1. [Done](#statequeryclientrecievedone)
		1. [Failure](#statequeryclientrecievefailure)
		1. [Query](#statequeryclientrecievequery)
		1. [ReAcquire](#statequeryclientrecievereacquire)
		1. [Release](#statequeryclientrecieverelease)
		1. [Result](#statequeryclientrecieveresult)
	1. __Send__
		1. [Acquire](#acquire)
		1. [Acquired](#statequeryclientsendacquired)
		1. [Done](#statequeryclientsenddone)
		1. [Failure](#statequeryclientsendfailure)
		1. [Query](#statequeryclientsendquery)
		1. [ReAcquire](#statequeryclientsendreacquire)
		1. [Release](#statequeryclientsendrelease)
		1. [Result](#statequeryclientsendresult)
1. __TxInbound__
	1. [TxInboundCanRequestMoreTxs](#txinboundcanrequestmoretxs)
	1. [TxInboundCannotRequestMoreTxs](#txinboundtxinboundcannotrequestmoretxs)
	1. [TxInboundTerminated](#txinboundtxinboundterminated)
	1. [TxSubmissionCollected](#txinboundtxsubmissioncollected)
	1. [TxSubmissionProcessed](#txinboundtxsubmissionprocessed)
1. __TxMonitorClient__
	1. __Recieve__
		1. [Acquire](#acquire)
		1. [Acquired](#txmonitorclientrecieveacquired)
		1. [Done](#txmonitorclientrecievedone)
		1. [Failure](#txmonitorclientrecievefailure)
		1. [Query](#txmonitorclientrecievequery)
		1. [ReAcquire](#txmonitorclientrecievereacquire)
		1. [Release](#txmonitorclientrecieverelease)
		1. [Result](#txmonitorclientrecieveresult)
	1. __Send__
		1. [Acquire](#acquire)
		1. [Acquired](#txmonitorclientsendacquired)
		1. [Done](#txmonitorclientsenddone)
		1. [Failure](#txmonitorclientsendfailure)
		1. [Query](#txmonitorclientsendquery)
		1. [ReAcquire](#txmonitorclientsendreacquire)
		1. [Release](#txmonitorclientsendrelease)
		1. [Result](#txmonitorclientsendresult)
1. __TxOutbound__
	1. [ControlMessage](#controlmessage)
	1. [RecvMsgRequest](#txoutboundrecvmsgrequest)
	1. [SendMsgReply](#txoutboundsendmsgreply)
1. __TxSubmission__
	1. __NodeToNode__
		1. __Recieve__
			1. [Done](#done)
			1. [ReplyTxIds](#txsubmissionnodetonoderecievereplytxids)
			1. [ReplyTxs](#txsubmissionnodetonoderecievereplytxs)
			1. [RequestTxIds](#txsubmissionnodetonoderecieverequesttxids)
			1. [RequestTxs](#txsubmissionnodetonoderecieverequesttxs)
		1. __Send__
			1. [Done](#done)
			1. [ReplyTxIds](#txsubmissionnodetonodesendreplytxids)
			1. [ReplyTxs](#txsubmissionnodetonodesendreplytxs)
			1. [RequestTxIds](#txsubmissionnodetonodesendrequesttxids)
			1. [RequestTxs](#txsubmissionnodetonodesendrequesttxs)
1. __TxSubmission2__
	1. __NodeToNode__
		1. __Recieve__
			1. [Done](#done)
			1. [MsgHello](#txsubmission2nodetonoderecievemsghello)
			1. [ReplyTxIds](#txsubmission2nodetonoderecievereplytxids)
			1. [ReplyTxs](#txsubmission2nodetonoderecievereplytxs)
			1. [RequestTxIds](#txsubmission2nodetonoderecieverequesttxids)
			1. [RequestTxs](#txsubmission2nodetonoderecieverequesttxs)
		1. __Send__
			1. [Done](#done)
			1. [MsgHello](#txsubmission2nodetonodesendmsghello)
			1. [ReplyTxIds](#txsubmission2nodetonodesendreplytxids)
			1. [ReplyTxs](#txsubmission2nodetonodesendreplytxs)
			1. [RequestTxIds](#txsubmission2nodetonodesendrequesttxids)
			1. [RequestTxs](#txsubmission2nodetonodesendrequesttxs)
1. __TxSubmissionClient__
	1. __Recieve__
		1. [AcceptTx](#accepttx)
		1. [Done](#txsubmissionclientrecievedone)
		1. [RejectTx](#txsubmissionclientrecieverejecttx)
		1. [SubmitTx](#txsubmissionclientrecievesubmittx)
	1. __Send__
		1. [AcceptTx](#accepttx)
		1. [Done](#txsubmissionclientsenddone)
		1. [RejectTx](#txsubmissionclientsendrejecttx)
		1. [SubmitTx](#txsubmissionclientsendsubmittx)

## [Metrics](#metrics)
1. [Block replay progress (%)](#block replay progress (%))
1. [blocksForgedNum](#blocksforgednum)
1. __cardano__
	1. __node__
		1. [aboutToLeadSlotLast](#abouttoleadslotlast)
		1. [aboutToLeadSlotLast](#cardanonodeabouttoleadslotlast)
		1. [adoptedSlotLast](#cardanonodeadoptedslotlast)
		1. [blockContext](#cardanonodeblockcontext)
		1. [blockFromFuture](#cardanonodeblockfromfuture)
		1. [blocks](#cardanonodeblocks)
		1. [blocks](#cardanonodeblocks)
		1. __chainSync__
			1. [rollForward](#rollforward)
			1. [rollForward](#cardanonodechainsyncrollforward)
		1. [connectedPeers](#connectedpeers)
		1. __connectionManager__
			1. [duplexConns](#duplexconns)
			1. [duplexConns](#cardanonodeconnectionmanagerduplexconns)
			1. [fullDuplexConns](#cardanonodeconnectionmanagerfullduplexconns)
			1. [fullDuplexConns](#cardanonodeconnectionmanagerfullduplexconns)
			1. [inboundConns](#cardanonodeconnectionmanagerinboundconns)
			1. [inboundConns](#cardanonodeconnectionmanagerinboundconns)
			1. [outboundConns](#cardanonodeconnectionmanageroutboundconns)
			1. [outboundConns](#cardanonodeconnectionmanageroutboundconns)
			1. [unidirectionalConns](#cardanonodeconnectionmanagerunidirectionalconns)
			1. [unidirectionalConns](#cardanonodeconnectionmanagerunidirectionalconns)
		1. [couldNotForgeSlotLast](#couldnotforgeslotlast)
		1. [couldNotForgeSlotLast](#cardanonodecouldnotforgeslotlast)
		1. [currentKESPeriod](#cardanonodecurrentkesperiod)
		1. [delegMapSize](#cardanonodedelegmapsize)
		1. [density](#cardanonodedensity)
		1. [density](#cardanonodedensity)
		1. [epoch](#cardanonodeepoch)
		1. [epoch](#cardanonodeepoch)
		1. [forgedInvalidSlotLast](#cardanonodeforgedinvalidslotlast)
		1. [forgedSlotLast](#cardanonodeforgedslotlast)
		1. [ledgerState](#cardanonodeledgerstate)
		1. [ledgerView](#cardanonodeledgerview)
		1. [mempoolBytes](#cardanonodemempoolbytes)
		1. [mempoolBytes](#cardanonodemempoolbytes)
		1. [mempoolBytes](#cardanonodemempoolbytes)
		1. [mempoolBytes](#cardanonodemempoolbytes)
		1. [nodeCannotForge](#cardanonodenodecannotforge)
		1. [nodeIsLeader](#cardanonodenodeisleader)
		1. [nodeNotLeader](#cardanonodenodenotleader)
		1. [notAdoptedSlotLast](#cardanonodenotadoptedslotlast)
		1. [operationalCertificateExpiryKESPeriod](#cardanonodeoperationalcertificateexpirykesperiod)
		1. [operationalCertificateStartKESPeriod](#cardanonodeoperationalcertificatestartkesperiod)
		1. __peerSelection__
			1. [cold](#cold)
			1. [hot](#cardanonodepeerselectionhot)
			1. [warm](#cardanonodepeerselectionwarm)
		1. [remainingKESPeriods](#remainingkesperiods)
		1. __served__
			1. [block](#block)
		1. [slotInEpoch](#slotinepoch)
		1. [slotInEpoch](#cardanonodeslotinepoch)
		1. [slotIsImmutable](#cardanonodeslotisimmutable)
		1. [slots](#cardanonodeslots)
		1. [slots](#cardanonodeslots)
		1. __submissions__
			1. [accepted](#accepted)
			1. [rejected](#cardanonodesubmissionsrejected)
			1. [submitted](#cardanonodesubmissionssubmitted)
		1. [txsInMempool](#txsinmempool)
		1. [txsInMempool](#cardanonodetxsinmempool)
		1. [txsInMempool](#cardanonodetxsinmempool)
		1. [txsInMempool](#cardanonodetxsinmempool)
		1. [txsProcessedNum](#cardanonodetxsprocessednum)
		1. [utxoSize](#cardanonodeutxosize)
1. __mem__
	1. [resident](#resident)
1. [nodeCannotForgeNum](#nodecannotforgenum)
1. [nodeIsLeaderNum](#nodeisleadernum)
1. [peersFromNodeKernel](#peersfromnodekernel)
1. __rts__
	1. [gcLiveBytes](#gclivebytes)
	1. [gcMajorNum](#rtsgcmajornum)
	1. [gcMinorNum](#rtsgcminornum)
	1. [gcticks](#rtsgcticks)
	1. [mutticks](#rtsmutticks)
	1. [threads](#rtsthreads)
1. [slotsMissed](#slotsmissed)
1. __stat__
	1. [cputicks](#cputicks)

## [Datapoints](#datapoints)
1. [NodeInfo](#nodeinfo)

## Trace Messages
### Cardano.Node.AcceptPolicy.ConnectionHardLimit


***
Hard rate limit reached, waiting until the number of connections drops below n.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.AcceptPolicy.ConnectionLimitResume


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.AcceptPolicy.ConnectionRateLimiting


***
Rate limiting accepting connections, delaying next accept for given time, currently serving n connections.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.BlockFetch.NodeToNode.Recieve.BatchDone


***
End of block streaming.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.BlockFetch.NodeToNode.Recieve.Block


***
Stream a single block.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.BlockFetch.NodeToNode.Recieve.ClientDone


***
Client termination message.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.BlockFetch.NodeToNode.Recieve.NoBlocks


***
Respond that there are no blocks.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.BlockFetch.NodeToNode.Recieve.RequestRange


***
Request range of blocks.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.BlockFetch.NodeToNode.Recieve.StartBatch


***
Start block streaming.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.BlockFetch.NodeToNode.Send.BatchDone


***
End of block streaming.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.BlockFetch.NodeToNode.Send.Block


***
Stream a single block.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.BlockFetch.NodeToNode.Send.ClientDone


***
Client termination message.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.BlockFetch.NodeToNode.Send.NoBlocks


***
Respond that there are no blocks.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.BlockFetch.NodeToNode.Send.RequestRange


***
Request range of blocks.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.BlockFetch.NodeToNode.Send.StartBatch


***
Start block streaming.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.BlockFetchClient.AcknowledgedFetchRequest


***
Mark the point when the fetch client picks up the request added by the block fetch decision thread. Note that this event can happen fewer times than the 'AddedFetchRequest' due to fetch request merging.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.BlockFetchClient.AddedFetchRequest


***
The block fetch decision thread has added a new fetch instruction consisting of one or more individual request ranges.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.BlockFetchClient.ClientTerminating


***
The client is terminating.  Log the number of outstanding requests.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.BlockFetchClient.CompletedBlockFetch


***
Mark the successful end of receiving a streaming batch of blocks.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`
Limiters: Limiter `CompletedBlockFetchLimiter` with frequency `2.0`

### Cardano.Node.BlockFetchClient.CompletedFetchBatch


***
Mark the successful end of receiving a streaming batch of blocks
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.BlockFetchClient.RejectedFetchBatch


***
If the other peer rejects our request then we have this event instead of 'StartedFetchBatch' and 'CompletedFetchBatch'.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.BlockFetchClient.SendFetchRequest


***
Mark the point when fetch request for a fragment is actually sent over the wire.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.BlockFetchClient.StartedFetchBatch


***
Mark the start of receiving a streaming batch of blocks. This will be followed by one or more 'CompletedBlockFetch' and a final 'CompletedFetchBatch'
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.BlockFetchDecision


***
Throughout the decision making process we accumulate reasons to decline to fetch any blocks. This message carries the intermediate and final results.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.BlockFetchSerialised.NodeToNode.Recieve.BatchDone


***
End of block streaming.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.BlockFetchSerialised.NodeToNode.Recieve.Block


***
Stream a single block.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.BlockFetchSerialised.NodeToNode.Recieve.ClientDone


***
Client termination message.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.BlockFetchSerialised.NodeToNode.Recieve.NoBlocks


***
Respond that there are no blocks.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.BlockFetchSerialised.NodeToNode.Recieve.RequestRange


***
Request range of blocks.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.BlockFetchSerialised.NodeToNode.Recieve.StartBatch


***
Start block streaming.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.BlockFetchSerialised.NodeToNode.Send.BatchDone


***
End of block streaming.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.BlockFetchSerialised.NodeToNode.Send.Block


***
Stream a single block.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.BlockFetchSerialised.NodeToNode.Send.ClientDone


***
Client termination message.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.BlockFetchSerialised.NodeToNode.Send.NoBlocks


***
Respond that there are no blocks.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.BlockFetchSerialised.NodeToNode.Send.RequestRange


***
Request range of blocks.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.BlockFetchSerialised.NodeToNode.Send.StartBatch


***
Start block streaming.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.BlockFetchServer.SendBlock


***
The server sent a block to the peer.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.BlockchainTime.CurrentSlotUnknown


***
Current slot is not yet known
 This happens when the tip of our current chain is so far in the past that we cannot translate the current wallclock to a slot number, typically during syncing. Until the current slot number is known, we cannot produce blocks. Seeing this message during syncing therefore is normal and to be expected.
 We record the current time (the time we tried to translate to a 'SlotNo') as well as the 'PastHorizonException', which provides detail on the bounds between which we /can/ do conversions. The distance between the current time and the upper bound should rapidly decrease with consecutive 'CurrentSlotUnknown' messages during syncing.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.BlockchainTime.StartTimeInTheFuture


***
The start time of the blockchain time is in the future
 We have to block (for 'NominalDiffTime') until that time comes.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.BlockchainTime.SystemClockMovedBack


***
The system clock moved back an acceptable time span, e.g., because of an NTP sync.
 The system clock moved back such that the new current slot would be smaller than the previous one. If this is within the configured limit, we trace this warning but *do not change the current slot*. The current slot never decreases, but the current slot may stay the same longer than expected.
 When the system clock moved back more than the configured limit, we shut down with a fatal exception.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainDB.AddBlockEvent.AddBlockValidation.CandidateContainsFutureBlocks


***
An event traced during validating performed while adding a block. Candidate contains headers from the future which do no exceed the clock skew.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.AddBlockEvent.AddBlockValidation.CandidateContainsFutureBlocksExceedingClockSkew


***
An event traced during validating performed while adding a block. Candidate contains headers from the future which exceed the clock skew.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.AddBlockEvent.AddBlockValidation.InvalidBlock


***
An event traced during validating performed while adding a block. A point was found to be invalid.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.AddBlockEvent.AddBlockValidation.ValidCandidate


***
An event traced during validating performed while adding a block. A candidate chain was valid.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`
Limiters: Limiter `ValidCandidateLimiter` with frequency `2.0`

### Cardano.Node.ChainDB.AddBlockEvent.AddedBlockToQueue


***
The block was added to the queue and will be added to the ChainDB by the background thread. The size of the queue is included..
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`
Limiters: Limiter `AddedBlockToQueueLimiter` with frequency `2.0`

### Cardano.Node.ChainDB.AddBlockEvent.AddedBlockToVolatileDB


***
A block was added to the Volatile DB
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`
Limiters: Limiter `AddedBlockToVolatileDBLimiter` with frequency `2.0`

### Cardano.Node.ChainDB.AddBlockEvent.AddedToCurrentChain


***
The new block fits onto the current chain (first fragment) and we have successfully used it to extend our (new) current chain (second fragment).
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.AddBlockEvent.BlockInTheFuture


***
The block is from the future, i.e., its slot number is greater than the current slot (the second argument).
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.AddBlockEvent.ChainSelectionForFutureBlock


***
Run chain selection for a block that was previously from the future. This is done for all blocks from the future each time a new block is added.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.AddBlockEvent.IgnoreBlockAlreadyInVolatileDB


***
A block that is already in the Volatile DB was ignored.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.AddBlockEvent.IgnoreBlockOlderThanK


***
A block with a 'BlockNo' more than @k@ back than the current tip was ignored.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.AddBlockEvent.IgnoreInvalidBlock


***
A block that is already in the Volatile DB was ignored.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.AddBlockEvent.StoreButDontChange


***
The block fits onto some fork, we'll try to switch to that fork (if it is preferable to our chain).
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.AddBlockEvent.SwitchedToAFork


***
The new block fits onto some fork and we have switched to that fork (second fragment), as it is preferable to our (previous) current chain (first fragment).
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.AddBlockEvent.TryAddToCurrentChain


***
The block fits onto the current chain, we'll try to use it to extend our chain.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.AddBlockEvent.TrySwitchToAFork


***
The block fits onto some fork, we'll try to switch to that fork (if it is preferable to our chain)
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceCopyToImmutableDBEvent.CopiedBlockToImmutableDB


***
A block was successfully copied to the ImmDB.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceCopyToImmutableDBEvent.NoBlocksToCopyToImmutableDB


***
There are no block to copy to the ImmDB.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceFollowerEvent.FollowerNewImmIterator


***
The follower is in the 'FollowerInImmutableDB' state but the iterator is exhausted while the ImmDB has grown, so we open a new iterator to stream these blocks too.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceFollowerEvent.FollowerNoLongerInMem


***
The follower was in the 'FollowerInImmutableDB' state and is switched to the 'FollowerInMem' state.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceFollowerEvent.FollowerSwitchToMem


***
The follower was in the 'FollowerInImmutableDB' state and is switched to the 'FollowerInMem' state.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceFollowerEvent.NewFollower


***
A new follower was created.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceGCEvent.PerformedGC


***
There are no block to copy to the ImmDB.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceGCEvent.ScheduledGC


***
There are no block to copy to the ImmDB.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceImmutableDBEvent.CacheEvent.CurrentChunkHit


***
Current chunk found in the cache.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceImmutableDBEvent.CacheEvent.PastChunkEvict


***
The least recently used past chunk was evicted because the cache was full.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceImmutableDBEvent.CacheEvent.PastChunkExpired


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceImmutableDBEvent.CacheEvent.PastChunkHit


***
Past chunk found in the cache
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceImmutableDBEvent.CacheEvent.PastChunkMiss


***
Past chunk was not found in the cache
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceImmutableDBEvent.ChunkFileDoesntFit


***
The hash of the last block in the previous epoch doesn't match the previous hash of the first block in the current epoch
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceImmutableDBEvent.ChunkValidation.InvalidChunkFile


***
Chunk file is invalid
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceImmutableDBEvent.ChunkValidation.InvalidPrimaryIndex


***
The primary index is invalid.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceImmutableDBEvent.ChunkValidation.InvalidSecondaryIndex


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceImmutableDBEvent.ChunkValidation.MissingChunkFile


***
Chunk file is missing
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceImmutableDBEvent.ChunkValidation.MissingPrimaryIndex


***
The primary index is missing.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceImmutableDBEvent.ChunkValidation.MissingSecondaryIndex


***
The secondary index is missing.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceImmutableDBEvent.ChunkValidation.RewritePrimaryIndex


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceImmutableDBEvent.ChunkValidation.RewriteSecondaryIndex


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceImmutableDBEvent.ChunkValidation.StartedValidatingChunk


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceImmutableDBEvent.ChunkValidation.ValidatedChunk


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceImmutableDBEvent.DBAlreadyClosed


***
The immutable DB is already closed
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceImmutableDBEvent.DBClosed


***
Closing the immutable DB
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceImmutableDBEvent.DeletingAfter


***
Delete after
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceImmutableDBEvent.Migrating


***
Performing a migration of the on-disk files.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceImmutableDBEvent.NoValidLastLocation


***
No valid last location was found
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceImmutableDBEvent.ValidatedLastLocation


***
The last location was validatet
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceInitChainSelEvent.CandidateContainsFutureBlocks


***
Candidate contains headers from the future which do not exceed the clock skew.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceInitChainSelEvent.CandidateContainsFutureBlocksExceedingClockSkew


***
Candidate contains headers from the future which exceed the clock skew, making them invalid.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceInitChainSelEvent.InitalChainSelected


***
InitalChainSelected
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceInitChainSelEvent.InvalidBlock


***
A point was found to be invalid.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceInitChainSelEvent.StartedInitChainSelection


***
StartedInitChainSelection
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceInitChainSelEvent.UpdateLedgerDb


***
UpdateLedgerDb
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceInitChainSelEvent.ValidCandidate


***
A candidate chain was valid.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceIteratorEvent.BlockGCedFromVolatileDB


***
A block is no longer in the VolatileDB and isn't in the ImmDB either; it wasn't part of the current chain.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceIteratorEvent.BlockMissingFromVolatileDB


***
A block is no longer in the VolatileDB because it has been garbage collected. It might now be in the ImmDB if it was part of the current chain.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceIteratorEvent.BlockWasCopiedToImmutableDB


***
A block that has been garbage collected from the VolatileDB is now found and streamed from the ImmDB.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceIteratorEvent.StreamFromBoth


***
Stream from both the VolatileDB and the ImmDB.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceIteratorEvent.StreamFromImmutableDB


***
Stream only from the ImmDB.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceIteratorEvent.StreamFromVolatileDB


***
Stream only from the VolatileDB.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceIteratorEvent.SwitchBackToVolatileDB


***
We have streamed one or more blocks from the ImmDB that were part of the VolatileDB when initialising the iterator. Now, we have to look back in the VolatileDB again because the ImmDB doesn't have the next block we're looking for.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceIteratorEvent.UnknownRangeRequested


***
An unknown range was requested, see 'UnknownRange'.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceLedgerEvent.DeletedSnapshot


***
An old or invalid on-disk snapshot was deleted.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceLedgerEvent.InvalidSnapshot


***
An on disk snapshot was skipped because it was invalid.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceLedgerEvent.TookSnapshot


***
A snapshot was written to disk.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceLedgerReplayEvent.ReplayFromGenesis


***
There were no LedgerDB snapshots on disk, so we're replaying all blocks starting from Genesis against the initial ledger. The @replayTo@ parameter corresponds to the block at the tip of the ImmDB, i.e., the last block to replay.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceLedgerReplayEvent.ReplayFromSnapshot


***
There was a LedgerDB snapshot on disk corresponding to the given tip. We're replaying more recent blocks against it. The @replayTo@ parameter corresponds to the block at the tip of the ImmDB, i.e., the last block to replay.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceLedgerReplayEvent.ReplayedBlock


***
We replayed the given block (reference) on the genesis snapshot during the initialisation of the LedgerDB.
 The @blockInfo@ parameter corresponds replayed block and the @replayTo@ parameter corresponds to the block at the tip of the ImmDB, i.e., the last block to replay.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceOpenEvent.ClosedDB


***
The ChainDB was closed.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceOpenEvent.OpenedDB


***
The ChainDB was opened.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceOpenEvent.OpenedImmutableDB


***
The ImmDB was opened.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceOpenEvent.OpenedLgrDB


***
The LedgerDB was opened.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceOpenEvent.OpenedVolatileDB


***
The VolatileDB was opened.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceOpenEvent.StartedOpeningDB


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceOpenEvent.StartedOpeningImmutableDB


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceOpenEvent.StartedOpeningLgrDB


***
The LedgerDB was opened.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceOpenEvent.StartedOpeningVolatileDB


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceVolatileDBEvent.BlockAlreadyHere


***
A block was found to be already in the DB.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceVolatileDBEvent.DBAlreadyClosed


***
When closing the DB it was found itis closed already.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceVolatileDBEvent.InvalidFileNames


***
Reports a list of invalid file paths.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainDB.TraceVolatileDBEvent.Truncate


***
Truncates a file up to offset because of the error.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ChainSync.NodeToClient.Recieve.AwaitReply


***
Acknowledge the request but require the consumer to wait for the nextupdate. This means that the consumer is synced with the producer, andthe producer is waiting for its own chain state to change.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSync.NodeToClient.Recieve.Done


***
We have to explain to the framework what our states mean, in terms ofwhich party has agency in each state.
Idle states are where it is for the client to send a message,busy states are where the server is expected to send a reply.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSync.NodeToClient.Recieve.FindIntersect


***
Ask the producer to try to find an improved intersection point betweenthe consumer and producer's chains. The consumer sends a sequence ofpoints and it is up to the producer to find the first intersection pointon its chain and send it back to the consumer.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSync.NodeToClient.Recieve.IntersectFound


***
The reply to the consumer about an intersection found.The consumer can decide weather to send more points.
The message also tells the consumer about the head point of the producer.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSync.NodeToClient.Recieve.IntersectNotFound


***
The reply to the consumer that no intersection was found: none of thepoints the consumer supplied are on the producer chain.
The message also tells the consumer about the head point of the producer.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSync.NodeToClient.Recieve.RequestNext


***
Request the next update from the producer. The response can be a rollforward, a roll back or wait.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSync.NodeToClient.Recieve.RollBackward


***
Tell the consumer to roll back to a given point on their chain.
The message also tells the consumer about the head point of the producer.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSync.NodeToClient.Recieve.RollForward


***
Tell the consumer to extend their chain with the given header.
The message also tells the consumer about the head point of the producer.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSync.NodeToClient.Send.AwaitReply


***
Acknowledge the request but require the consumer to wait for the nextupdate. This means that the consumer is synced with the producer, andthe producer is waiting for its own chain state to change.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSync.NodeToClient.Send.Done


***
We have to explain to the framework what our states mean, in terms ofwhich party has agency in each state.
Idle states are where it is for the client to send a message,busy states are where the server is expected to send a reply.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSync.NodeToClient.Send.FindIntersect


***
Ask the producer to try to find an improved intersection point betweenthe consumer and producer's chains. The consumer sends a sequence ofpoints and it is up to the producer to find the first intersection pointon its chain and send it back to the consumer.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSync.NodeToClient.Send.IntersectFound


***
The reply to the consumer about an intersection found.The consumer can decide weather to send more points.
The message also tells the consumer about the head point of the producer.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSync.NodeToClient.Send.IntersectNotFound


***
The reply to the consumer that no intersection was found: none of thepoints the consumer supplied are on the producer chain.
The message also tells the consumer about the head point of the producer.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSync.NodeToClient.Send.RequestNext


***
Request the next update from the producer. The response can be a rollforward, a roll back or wait.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSync.NodeToClient.Send.RollBackward


***
Tell the consumer to roll back to a given point on their chain.
The message also tells the consumer about the head point of the producer.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSync.NodeToClient.Send.RollForward


***
Tell the consumer to extend their chain with the given header.
The message also tells the consumer about the head point of the producer.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSyncClient.ChainSyncClientEvent.DownloadedHeader


***
While following a candidate chain, we rolled forward by downloading a header.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSyncClient.ChainSyncClientEvent.Exception


***
An exception was thrown by the Chain Sync Client.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSyncClient.ChainSyncClientEvent.FoundIntersection


***
We found an intersection between our chain fragment and the candidate's chain.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSyncClient.ChainSyncClientEvent.RolledBack


***
While following a candidate chain, we rolled back to the given point.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSyncClient.ChainSyncClientEvent.Termination


***
The client has terminated.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSyncNode.NodeToNode.Recieve.AwaitReply


***
Acknowledge the request but require the consumer to wait for the nextupdate. This means that the consumer is synced with the producer, andthe producer is waiting for its own chain state to change.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSyncNode.NodeToNode.Recieve.Done


***
We have to explain to the framework what our states mean, in terms ofwhich party has agency in each state.
Idle states are where it is for the client to send a message,busy states are where the server is expected to send a reply.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSyncNode.NodeToNode.Recieve.FindIntersect


***
Ask the producer to try to find an improved intersection point betweenthe consumer and producer's chains. The consumer sends a sequence ofpoints and it is up to the producer to find the first intersection pointon its chain and send it back to the consumer.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSyncNode.NodeToNode.Recieve.IntersectFound


***
The reply to the consumer about an intersection found.The consumer can decide weather to send more points.
The message also tells the consumer about the head point of the producer.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSyncNode.NodeToNode.Recieve.IntersectNotFound


***
The reply to the consumer that no intersection was found: none of thepoints the consumer supplied are on the producer chain.
The message also tells the consumer about the head point of the producer.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSyncNode.NodeToNode.Recieve.RequestNext


***
Request the next update from the producer. The response can be a rollforward, a roll back or wait.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSyncNode.NodeToNode.Recieve.RollBackward


***
Tell the consumer to roll back to a given point on their chain.
The message also tells the consumer about the head point of the producer.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSyncNode.NodeToNode.Recieve.RollForward


***
Tell the consumer to extend their chain with the given header.
The message also tells the consumer about the head point of the producer.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSyncNode.NodeToNode.Send.AwaitReply


***
Acknowledge the request but require the consumer to wait for the nextupdate. This means that the consumer is synced with the producer, andthe producer is waiting for its own chain state to change.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSyncNode.NodeToNode.Send.Done


***
We have to explain to the framework what our states mean, in terms ofwhich party has agency in each state.
Idle states are where it is for the client to send a message,busy states are where the server is expected to send a reply.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSyncNode.NodeToNode.Send.FindIntersect


***
Ask the producer to try to find an improved intersection point betweenthe consumer and producer's chains. The consumer sends a sequence ofpoints and it is up to the producer to find the first intersection pointon its chain and send it back to the consumer.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSyncNode.NodeToNode.Send.IntersectFound


***
The reply to the consumer about an intersection found.The consumer can decide weather to send more points.
The message also tells the consumer about the head point of the producer.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSyncNode.NodeToNode.Send.IntersectNotFound


***
The reply to the consumer that no intersection was found: none of thepoints the consumer supplied are on the producer chain.
The message also tells the consumer about the head point of the producer.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSyncNode.NodeToNode.Send.RequestNext


***
Request the next update from the producer. The response can be a rollforward, a roll back or wait.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSyncNode.NodeToNode.Send.RollBackward


***
Tell the consumer to roll back to a given point on their chain.
The message also tells the consumer about the head point of the producer.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSyncNode.NodeToNode.Send.RollForward


***
Tell the consumer to extend their chain with the given header.
The message also tells the consumer about the head point of the producer.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSyncSerialised.NodeToNode.Recieve.AwaitReply


***
Acknowledge the request but require the consumer to wait for the nextupdate. This means that the consumer is synced with the producer, andthe producer is waiting for its own chain state to change.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSyncSerialised.NodeToNode.Recieve.Done


***
We have to explain to the framework what our states mean, in terms ofwhich party has agency in each state.
Idle states are where it is for the client to send a message,busy states are where the server is expected to send a reply.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSyncSerialised.NodeToNode.Recieve.FindIntersect


***
Ask the producer to try to find an improved intersection point betweenthe consumer and producer's chains. The consumer sends a sequence ofpoints and it is up to the producer to find the first intersection pointon its chain and send it back to the consumer.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSyncSerialised.NodeToNode.Recieve.IntersectFound


***
The reply to the consumer about an intersection found.The consumer can decide weather to send more points.
The message also tells the consumer about the head point of the producer.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSyncSerialised.NodeToNode.Recieve.IntersectNotFound


***
The reply to the consumer that no intersection was found: none of thepoints the consumer supplied are on the producer chain.
The message also tells the consumer about the head point of the producer.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSyncSerialised.NodeToNode.Recieve.RequestNext


***
Request the next update from the producer. The response can be a rollforward, a roll back or wait.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSyncSerialised.NodeToNode.Recieve.RollBackward


***
Tell the consumer to roll back to a given point on their chain.
The message also tells the consumer about the head point of the producer.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSyncSerialised.NodeToNode.Recieve.RollForward


***
Tell the consumer to extend their chain with the given header.
The message also tells the consumer about the head point of the producer.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSyncSerialised.NodeToNode.Send.AwaitReply


***
Acknowledge the request but require the consumer to wait for the nextupdate. This means that the consumer is synced with the producer, andthe producer is waiting for its own chain state to change.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSyncSerialised.NodeToNode.Send.Done


***
We have to explain to the framework what our states mean, in terms ofwhich party has agency in each state.
Idle states are where it is for the client to send a message,busy states are where the server is expected to send a reply.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSyncSerialised.NodeToNode.Send.FindIntersect


***
Ask the producer to try to find an improved intersection point betweenthe consumer and producer's chains. The consumer sends a sequence ofpoints and it is up to the producer to find the first intersection pointon its chain and send it back to the consumer.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSyncSerialised.NodeToNode.Send.IntersectFound


***
The reply to the consumer about an intersection found.The consumer can decide weather to send more points.
The message also tells the consumer about the head point of the producer.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSyncSerialised.NodeToNode.Send.IntersectNotFound


***
The reply to the consumer that no intersection was found: none of thepoints the consumer supplied are on the producer chain.
The message also tells the consumer about the head point of the producer.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSyncSerialised.NodeToNode.Send.RequestNext


***
Request the next update from the producer. The response can be a rollforward, a roll back or wait.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSyncSerialised.NodeToNode.Send.RollBackward


***
Tell the consumer to roll back to a given point on their chain.
The message also tells the consumer about the head point of the producer.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSyncSerialised.NodeToNode.Send.RollForward


***
Tell the consumer to extend their chain with the given header.
The message also tells the consumer about the head point of the producer.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSyncServerBlock.ChainSyncServerEvent.ServerRead.RollBackward


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSyncServerBlock.ChainSyncServerEvent.ServerRead.RollForward


***
Roll forward to the given point.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSyncServerBlock.ChainSyncServerEvent.ServerRead.ServerRead


***
A server read has occurred, either for an add block or a rollback
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSyncServerBlock.ChainSyncServerEvent.ServerRead.ServerReadBlocked


***
A server read has blocked, either for an add block or a rollback
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSyncServerHeader.ChainSyncServerEvent.ServerRead.RollBackward


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSyncServerHeader.ChainSyncServerEvent.ServerRead.RollForward


***
Roll forward to the given point.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSyncServerHeader.ChainSyncServerEvent.ServerRead.ServerRead


***
A server read has occurred, either for an add block or a rollback
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ChainSyncServerHeader.ChainSyncServerEvent.ServerRead.ServerReadBlocked


***
A server read has blocked, either for an add block or a rollback
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ConnectionManager.Connect


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ConnectionManager.ConnectError


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ConnectionManager.ConnectionCleanup


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ConnectionManager.ConnectionExists


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ConnectionManager.ConnectionFailure


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ConnectionManager.ConnectionHandler


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ConnectionManager.ConnectionManagerCounters


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ConnectionManager.ConnectionNotFound


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ConnectionManager.ConnectionTimeWait


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ConnectionManager.ConnectionTimeWaitDone


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ConnectionManager.ForbiddenConnection


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ConnectionManager.ForbiddenOperation


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ConnectionManager.ImpossibleConnection


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ConnectionManager.IncludeConnection


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ConnectionManager.PruneConnections


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ConnectionManager.Shutdown


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ConnectionManager.State


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ConnectionManager.TerminatedConnection


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ConnectionManager.TerminatingConnection


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ConnectionManager.UnexpectedlyFalseAssertion


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ConnectionManager.UnknownConnection


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ConnectionManager.UnregisterConnection


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ConnectionManagerTransition.ConnectionManagerTransition


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.DNSResolver.LookupAAAAError


***
AAAA lookup failed with an error.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.DNSResolver.LookupAAAAResult


***
Lookup AAAA result.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.DNSResolver.LookupAError


***
A lookup failed with an error.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.DNSResolver.LookupAResult


***
Lookup A result.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.DNSResolver.LookupException


***
A DNS lookup exception occurred.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.DNSResolver.LookupIPv4First


***
Returning IPv4 address first.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.DNSResolver.LookupIPv6First


***
Returning IPv6 address first.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.DNSSubscription.DNS.AllocateSocket


***
DNS Subscription: Allocate socket to address.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.DNSSubscription.DNS.ApplicationException


***
DNS Subscription: Application Exception occurred.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.DNSSubscription.DNS.CloseSocket


***
DNS Subscription: Closed socket to address.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.DNSSubscription.DNS.ConnectEnd


***
DNS Subscription: Connection Attempt end with destination and outcome.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.DNSSubscription.DNS.ConnectException


***
DNS Subscription: Socket Allocation Exception with destination and the exception.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.DNSSubscription.DNS.ConnectStart


***
DNS Subscription: Connection Attempt Start with destination.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.DNSSubscription.DNS.ConnectionExist


***
DNS Subscription: Connection exists to destination.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.DNSSubscription.DNS.MissingLocalAddress


***
DNS Subscription: Missing local address.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.DNSSubscription.DNS.Restart


***
DNS Subscription: Restarting Subscription after duration with desired valency and current valency.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.DNSSubscription.DNS.SkippingPeer


***
DNS Subscription: Skipping peer with address.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.DNSSubscription.DNS.SocketAllocationException


***
DNS Subscription: Connection Attempt Exception with destination and exception.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.DNSSubscription.DNS.Start


***
DNS Subscription: Starting Subscription Worker with a valency.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.DNSSubscription.DNS.SubscriptionFailed


***
DNS Subscription: Failed to start all required subscriptions.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.DNSSubscription.DNS.SubscriptionRunning


***
DNS Subscription: Required subscriptions started.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.DNSSubscription.DNS.SubscriptionWaiting


***
DNS Subscription: Waiting on address with active connections.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.DNSSubscription.DNS.SubscriptionWaitingNewConnection


***
DNS Subscription: Waiting delay time before attempting a new connection.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.DNSSubscription.DNS.TryConnectToPeer


***
DNS Subscription: Trying to connect to peer with address.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.DNSSubscription.DNS.UnsupportedRemoteAddr


***
DNS Subscription: Unsupported remote target address.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.DebugPeerSelection.DebugPeerSelection.GovernorState


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.DebugPeerSelectionResponder.DebugPeerSelection.GovernorState


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.DiffusionInit.ConfiguringLocalSocket


***
ConfiguringLocalSocket 
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.DiffusionInit.ConfiguringServerSocket


***
ConfiguringServerSocket 
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.DiffusionInit.CreateSystemdSocketForSnocketPath


***
CreateSystemdSocketForSnocketPath 
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.DiffusionInit.CreatedLocalSocket


***
CreatedLocalSocket 
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.DiffusionInit.CreatingServerSocket


***
CreatingServerSocket 
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.DiffusionInit.DiffusionErrored


***
DiffusionErrored 
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.DiffusionInit.ListeningLocalSocket


***
ListeningLocalSocket 
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.DiffusionInit.ListeningServerSocket


***
ListeningServerSocket 
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.DiffusionInit.LocalSocketUp


***
LocalSocketUp 
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.DiffusionInit.RunLocalServer


***
RunLocalServer 
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.DiffusionInit.RunServer


***
RunServer 
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.DiffusionInit.ServerSocketUp


***
ServerSocketUp 
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.DiffusionInit.UnsupportedLocalSystemdSocket


***
UnsupportedLocalSystemdSocket 
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.DiffusionInit.UnsupportedReadySocketCase


***
UnsupportedReadySocketCase 
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.DiffusionInit.UsingSystemdSocket


***
UsingSystemdSocket 
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ErrorPolicy.AcceptException


***
'accept' threw an exception.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ErrorPolicy.KeepSuspended


***
Consumer was suspended until producer will resume.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ErrorPolicy.LocalNodeError


***
caught a local exception.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ErrorPolicy.ResumeConsumer


***
Resume consumer.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ErrorPolicy.ResumePeer


***
Resume a peer (both consumer and producer).
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ErrorPolicy.ResumeProducer


***
Resume producer.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ErrorPolicy.SuspendConsumer


***
Suspending consumer.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ErrorPolicy.SuspendPeer


***
Suspending peer with a given exception.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ErrorPolicy.UnhandledApplicationException


***
An application threw an exception, which was not handled.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ErrorPolicy.UnhandledConnectionException


***
'connect' threw an exception, which was not handled by any 'ErrorPolicy'.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.Forge.AdoptedBlock


***
We adopted the block we produced, we also trace the transactions  that were adopted.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.Forge.BlockContext


***
We found out to which block we are going to connect the block we are about  to forge.   We record the current slot number, the block number of the block to  connect to and its point.   Note that block number of the block we will try to forge is one more than  the recorded block number.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.Forge.BlockFromFuture


***
Leadership check failed: the current chain contains a block from a slot  /after/ the current slot   This can only happen if the system is under heavy load.   We record both the current slot number as well as the slot number of the  block at the tip of the chain.   See also <https://github.com/input-output-hk/ouroboros-network/issues/1462>
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.Forge.DidntAdoptBlock


***
We did not adopt the block we produced, but the block was valid. We  must have adopted a block that another leader of the same slot produced  before we got the chance of adopting our own block. This is very rare,  this warrants a warning.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.Forge.ForgeStateUpdateError


***
Updating the forge state failed.   For example, the KES key could not be evolved anymore.   We record the error returned by 'updateForgeState'.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.Forge.ForgedBlock


***
We forged a block.
  We record the current slot number, the point of the predecessor, the block  itself, and the total size of the mempool snapshot at the time we produced  the block (which may be significantly larger than the block, due to  maximum block size)
  This will be followed by one of three messages:
  * AdoptedBlock (normally)
  * DidntAdoptBlock (rarely)
  * ForgedInvalidBlock (hopefully never -- this would indicate a bug)
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.Forge.ForgedInvalidBlock


***
We forged a block that is invalid according to the ledger in the  ChainDB. This means there is an inconsistency between the mempool  validation and the ledger validation. This is a serious error!
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.Forge.LedgerState


***
We obtained a ledger state for the point of the block we want to  connect to   We record both the current slot number as well as the point of the block  we attempt to connect the new block to (that we requested the ledger  state for).
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.Forge.LedgerView


***
We obtained a ledger view for the current slot number   We record the current slot number.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.Forge.NoLedgerState


***
Leadership check failed: we were unable to get the ledger state for the  point of the block we want to connect to   This can happen if after choosing which block to connect to the node  switched to a different fork. We expect this to happen only rather  rarely, so this certainly merits a warning; if it happens a lot, that  merits an investigation.   We record both the current slot number as well as the point of the block  we attempt to connect the new block to (that we requested the ledger  state for).
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.Forge.NoLedgerView


***
Leadership check failed: we were unable to get the ledger view for the  current slot number   This will only happen if there are many missing blocks between the tip of  our chain and the current slot.   We record also the failure returned by 'forecastFor'.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.Forge.NodeCannotForge


***
We did the leadership check and concluded that we should lead and forge  a block, but cannot.   This should only happen rarely and should be logged with warning severity.   Records why we cannot forge a block.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.Forge.NodeIsLeader


***
We did the leadership check and concluded we /are/ the leader
  The node will soon forge; it is about to read its transactions from the  Mempool. This will be followed by ForgedBlock.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.Forge.NodeNotLeader


***
We did the leadership check and concluded we are not the leader   We record the current slot number
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.Forge.SlotIsImmutable


***
Leadership check failed: the tip of the ImmutableDB inhabits the  current slot   This might happen in two cases.    1. the clock moved backwards, on restart we ignored everything from the      VolatileDB since it's all in the future, and now the tip of the      ImmutableDB points to a block produced in the same slot we're trying      to produce a block in    2. k = 0 and we already adopted a block from another leader of the same      slot.   We record both the current slot number as well as the tip of the  ImmutableDB.  See also <https://github.com/input-output-hk/ouroboros-network/issues/1462>
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.Forge.StartLeadershipCheck


***
Start of the leadership check.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.Forge.StartLeadershipCheckPlus


***
We adopted the block we produced, we also trace the transactions  that were adopted.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.ForgeStateInfo


***
kesStartPeriod 
kesEndPeriod is kesStartPeriod + tpraosMaxKESEvo
kesEvolution is the current evolution or /relative period/.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ForgeStats.ForgeStats


***
nodeCannotForgeNum shows how many times this node could not forge.
nodeIsLeaderNum shows how many times this node was leader.
blocksForgedNum shows how many blocks did forge in this node.
slotsMissed shows how many slots were missed in this node.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Handshake.Receive.AcceptVersion


***
The remote end decides which version to use and sends chosen version.The server is allowed to modify version parameters.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Handshake.Receive.ProposeVersions


***
Propose versions together with version parameters.  It must be encoded to a sorted list..
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Handshake.Receive.Refuse


***
It refuses to run any version.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Handshake.Receive.ReplyVersions


***
`MsgReplyVersions` received as a response to 'MsgProposeVersions'.  It is not supported to explicitly send this message. It can only be received as a copy of 'MsgProposeVersions' in a simultaneous open scenario.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Handshake.Send.AcceptVersion


***
The remote end decides which version to use and sends chosen version.The server is allowed to modify version parameters.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Handshake.Send.ProposeVersions


***
Propose versions together with version parameters.  It must be encoded to a sorted list..
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Handshake.Send.Refuse


***
It refuses to run any version.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Handshake.Send.ReplyVersions


***
`MsgReplyVersions` received as a response to 'MsgProposeVersions'.  It is not supported to explicitly send this message. It can only be received as a copy of 'MsgProposeVersions' in a simultaneous open scenario.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.InboundGovernor.DemotedToColdRemote


***
All mini-protocols terminated.  The boolean is true if this connection was not used by p2p-governor, and thus the connection will be terminated.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.InboundGovernor.DemotedToWarmRemote


***
All mini-protocols terminated.  The boolean is true if this connection was not used by p2p-governor, and thus the connection will be terminated.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.InboundGovernor.InboundGovernorCounters


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.InboundGovernor.InboundGovernorError


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.InboundGovernor.MuxCleanExit


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.InboundGovernor.MuxErrored


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.InboundGovernor.NewConnection


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.InboundGovernor.PromotedToHotRemote


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.InboundGovernor.PromotedToWarmRemote


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.InboundGovernor.RemoteState


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.InboundGovernor.ResponderErrored


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.InboundGovernor.ResponderRestarted


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.InboundGovernor.ResponderStartFailure


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.InboundGovernor.ResponderStarted


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.InboundGovernor.ResponderTerminated


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.InboundGovernor.UnexpectedlyFalseAssertion


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.InboundGovernor.WaitIdleRemote


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.InboundGovernorTransition.InboundGovernorTransition


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.IpSubscription.IP.AllocateSocket


***
IP Subscription: Allocate socket to address.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.IpSubscription.IP.ApplicationException


***
IP Subscription: Application Exception occurred.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.IpSubscription.IP.CloseSocket


***
IP Subscription: Closed socket to address.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.IpSubscription.IP.ConnectEnd


***
IP Subscription: Connection Attempt end with destination and outcome.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.IpSubscription.IP.ConnectException


***
IP Subscription: Socket Allocation Exception with destination and the exception.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.IpSubscription.IP.ConnectStart


***
IP Subscription: Connection Attempt Start with destination.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.IpSubscription.IP.ConnectionExist


***
IP Subscription: Connection exists to destination.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.IpSubscription.IP.MissingLocalAddress


***
IP Subscription: Missing local address.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.IpSubscription.IP.Restart


***
IP Subscription: Restarting Subscription after duration with desired valency and current valency.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.IpSubscription.IP.SkippingPeer


***
IP Subscription: Skipping peer with address.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.IpSubscription.IP.SocketAllocationException


***
IP Subscription: Connection Attempt Exception with destination and exception.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.IpSubscription.IP.Start


***
IP Subscription: Starting Subscription Worker with a valency.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.IpSubscription.IP.SubscriptionFailed


***
IP Subscription: Failed to start all required subscriptions.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.IpSubscription.IP.SubscriptionRunning


***
IP Subscription: Required subscriptions started.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.IpSubscription.IP.SubscriptionWaiting


***
IP Subscription: Waiting on address with active connections.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.IpSubscription.IP.SubscriptionWaitingNewConnection


***
IP Subscription: Waiting delay time before attempting a new connection.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.IpSubscription.IP.TryConnectToPeer


***
IP Subscription: Trying to connect to peer with address.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.IpSubscription.IP.UnsupportedRemoteAddr


***
IP Subscription: Unsupported remote target address.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.KeepAliveClient


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LedgerPeers.DisabledLedgerPeers


***
Trace for when getting peers from the ledger is disabled, that is DontUseLedger.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LedgerPeers.FallingBackToBootstrapPeers


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LedgerPeers.FetchingNewLedgerState


***
Trace for fetching a new list of peers from the ledger. Int is the number of peers returned.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LedgerPeers.PickedPeer


***
Trace for a peer picked with accumulated and relative stake of its pool.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LedgerPeers.PickedPeers


***
Trace for the number of peers we wanted to pick and the list of peers picked.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LedgerPeers.RequestForPeers


***
RequestForPeers (NumberOfPeers 1)
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LedgerPeers.ReusingLedgerState


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LedgerPeers.TraceUseLedgerAfter


***
Trace UseLedgerAfter value.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LedgerPeers.WaitingOnRequest


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalConnectionManager.Connect


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalConnectionManager.ConnectError


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalConnectionManager.ConnectionCleanup


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalConnectionManager.ConnectionExists


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalConnectionManager.ConnectionFailure


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalConnectionManager.ConnectionHandler


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalConnectionManager.ConnectionManagerCounters


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalConnectionManager.ConnectionNotFound


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalConnectionManager.ConnectionTimeWait


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalConnectionManager.ConnectionTimeWaitDone


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalConnectionManager.ForbiddenConnection


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalConnectionManager.ForbiddenOperation


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalConnectionManager.ImpossibleConnection


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalConnectionManager.IncludeConnection


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalConnectionManager.PruneConnections


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalConnectionManager.Shutdown


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalConnectionManager.State


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalConnectionManager.TerminatedConnection


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalConnectionManager.TerminatingConnection


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalConnectionManager.UnexpectedlyFalseAssertion


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalConnectionManager.UnknownConnection


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalConnectionManager.UnregisterConnection


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalErrorPolicy.AcceptException


***
'accept' threw an exception.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.LocalErrorPolicy.KeepSuspended


***
Consumer was suspended until producer will resume.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.LocalErrorPolicy.LocalNodeError


***
caught a local exception.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.LocalErrorPolicy.ResumeConsumer


***
Resume consumer.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.LocalErrorPolicy.ResumePeer


***
Resume a peer (both consumer and producer).
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.LocalErrorPolicy.ResumeProducer


***
Resume producer.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.LocalErrorPolicy.SuspendConsumer


***
Suspending consumer.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.LocalErrorPolicy.SuspendPeer


***
Suspending peer with a given exception.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.LocalErrorPolicy.UnhandledApplicationException


***
An application threw an exception, which was not handled.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.LocalErrorPolicy.UnhandledConnectionException


***
'connect' threw an exception, which was not handled by any 'ErrorPolicy'.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.LocalHandshake.Receive.AcceptVersion


***
The remote end decides which version to use and sends chosen version.The server is allowed to modify version parameters.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalHandshake.Receive.ProposeVersions


***
Propose versions together with version parameters.  It must be encoded to a sorted list..
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalHandshake.Receive.Refuse


***
It refuses to run any version.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalHandshake.Receive.ReplyVersions


***
`MsgReplyVersions` received as a response to 'MsgProposeVersions'.  It is not supported to explicitly send this message. It can only be received as a copy of 'MsgProposeVersions' in a simultaneous open scenario.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalHandshake.Send.AcceptVersion


***
The remote end decides which version to use and sends chosen version.The server is allowed to modify version parameters.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalHandshake.Send.ProposeVersions


***
Propose versions together with version parameters.  It must be encoded to a sorted list..
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalHandshake.Send.Refuse


***
It refuses to run any version.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalHandshake.Send.ReplyVersions


***
`MsgReplyVersions` received as a response to 'MsgProposeVersions'.  It is not supported to explicitly send this message. It can only be received as a copy of 'MsgProposeVersions' in a simultaneous open scenario.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalInboundGovernor.DemotedToColdRemote


***
All mini-protocols terminated.  The boolean is true if this connection was not used by p2p-governor, and thus the connection will be terminated.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalInboundGovernor.DemotedToWarmRemote


***
All mini-protocols terminated.  The boolean is true if this connection was not used by p2p-governor, and thus the connection will be terminated.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalInboundGovernor.InboundGovernorCounters


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalInboundGovernor.InboundGovernorError


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalInboundGovernor.MuxCleanExit


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalInboundGovernor.MuxErrored


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalInboundGovernor.NewConnection


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalInboundGovernor.PromotedToHotRemote


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalInboundGovernor.PromotedToWarmRemote


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalInboundGovernor.RemoteState


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalInboundGovernor.ResponderErrored


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalInboundGovernor.ResponderRestarted


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalInboundGovernor.ResponderStartFailure


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalInboundGovernor.ResponderStarted


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalInboundGovernor.ResponderTerminated


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalInboundGovernor.UnexpectedlyFalseAssertion


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalInboundGovernor.WaitIdleRemote


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalRootPeers.LocalRootDomains


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalRootPeers.LocalRootError


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalRootPeers.LocalRootFailure


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalRootPeers.LocalRootGroups


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalRootPeers.LocalRootResult


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalRootPeers.LocalRootWaiting


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalServer.AcceptConnection


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalServer.AcceptError


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalServer.AcceptPolicy


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalServer.Error


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalServer.Started


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalServer.Stopped


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.LocalTxSubmissionServer.ReceivedTx


***
A transaction was received.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Mempool.AddedTx


***
New, valid transaction that was added to the Mempool.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.Mempool.ManuallyRemovedTxs


***
Transactions that have been manually removed from the Mempool.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.Mempool.RejectedTx


***
New, invalid transaction thas was rejected and thus not added to the Mempool.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.Mempool.RemoveTxs


***
Previously valid transactions that are no longer valid because of changes in the ledger state. These transactions have been removed from the Mempool.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.Mux.ChannelRecvEnd


***
Channel receive end.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Mux.ChannelRecvStart


***
Channel receive start.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Mux.ChannelSendEnd


***
Channel send end.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Mux.ChannelSendStart


***
Channel send start.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Mux.CleanExit


***
Miniprotocol terminated cleanly.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Mux.ExceptionExit


***
Miniprotocol terminated with exception.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Mux.HandshakeClientEnd


***
Handshake client end.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Mux.HandshakeClientError


***
Handshake client error.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Mux.HandshakeServerEnd


***
Handshake server end.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Mux.HandshakeServerError


***
Handshake server error.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Mux.HandshakeStart


***
Handshake start.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Mux.RecvDeltaQObservation


***
Bearer DeltaQ observation.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Mux.RecvDeltaQSample


***
Bearer DeltaQ sample.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Mux.RecvEnd


***
Bearer receive end.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Mux.RecvHeaderEnd


***
Bearer receive header end.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Mux.RecvHeaderStart


***
Bearer receive header start.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Mux.RecvStart


***
Bearer receive start.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Mux.SDUReadTimeoutException


***
Timed out reading SDU.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Mux.SDUWriteTimeoutException


***
Timed out writing SDU.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Mux.SendEnd


***
Bearer send end.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Mux.SendStart


***
Bearer send start.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Mux.Shutdown


***
Mux shutdown.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Mux.StartEagerly


***
Eagerly started.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Mux.StartOnDemand


***
Preparing to start.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Mux.StartedOnDemand


***
Started on demand.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Mux.State


***
State.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Mux.TCPInfo


***
TCPInfo.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Mux.Terminating


***
Terminating.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.MuxLocal.ChannelRecvEnd


***
Channel receive end.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.MuxLocal.ChannelRecvStart


***
Channel receive start.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.MuxLocal.ChannelSendEnd


***
Channel send end.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.MuxLocal.ChannelSendStart


***
Channel send start.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.MuxLocal.CleanExit


***
Miniprotocol terminated cleanly.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.MuxLocal.ExceptionExit


***
Miniprotocol terminated with exception.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.MuxLocal.HandshakeClientEnd


***
Handshake client end.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.MuxLocal.HandshakeClientError


***
Handshake client error.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.MuxLocal.HandshakeServerEnd


***
Handshake server end.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.MuxLocal.HandshakeServerError


***
Handshake server error.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.MuxLocal.HandshakeStart


***
Handshake start.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.MuxLocal.RecvDeltaQObservation


***
Bearer DeltaQ observation.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.MuxLocal.RecvDeltaQSample


***
Bearer DeltaQ sample.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.MuxLocal.RecvEnd


***
Bearer receive end.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.MuxLocal.RecvHeaderEnd


***
Bearer receive header end.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.MuxLocal.RecvHeaderStart


***
Bearer receive header start.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.MuxLocal.RecvStart


***
Bearer receive start.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.MuxLocal.SDUReadTimeoutException


***
Timed out reading SDU.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.MuxLocal.SDUWriteTimeoutException


***
Timed out writing SDU.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.MuxLocal.SendEnd


***
Bearer send end.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.MuxLocal.SendStart


***
Bearer send start.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.MuxLocal.Shutdown


***
Mux shutdown.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.MuxLocal.StartEagerly


***
Eagerly started.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.MuxLocal.StartOnDemand


***
Preparing to start.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.MuxLocal.StartedOnDemand


***
Started on demand.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.MuxLocal.State


***
State.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.MuxLocal.TCPInfo


***
TCPInfo.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.MuxLocal.Terminating


***
Terminating.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.PeerSelection.ChurnMode


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.PeerSelection.ChurnWait


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.PeerSelection.DemoteAsynchronous


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.PeerSelection.DemoteHotDone


***
target active, actual active, peer
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.PeerSelection.DemoteHotFailed


***
target active, actual active, peer, reason
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.PeerSelection.DemoteHotPeers


***
target active, actual active, selected peers
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.PeerSelection.DemoteLocalHotPeers


***
local per-group (target active, actual active), selected peers
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.PeerSelection.DemoteWarmDone


***
target established, actual established, peer
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.PeerSelection.DemoteWarmFailed


***
target established, actual established, peer, reason
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.PeerSelection.DemoteWarmPeers


***
target established, actual established, selected peers
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.PeerSelection.ForgetColdPeers


***
target known peers, actual known peers, selected peers
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.PeerSelection.GossipRequests


***
target known peers, actual known peers, peers available for gossip, peers selected for gossip
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.PeerSelection.GossipResults


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.PeerSelection.GovernorWakeup


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.PeerSelection.LocalRootPeersChanged


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.PeerSelection.PromoteColdDone


***
target active, actual active, selected peers
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.PeerSelection.PromoteColdFailed


***
target established, actual established, peer, delay until next promotion, reason
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.PeerSelection.PromoteColdLocalPeers


***
target local established, actual local established, selected peers
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.PeerSelection.PromoteColdPeers


***
target established, actual established, selected peers
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.PeerSelection.PromoteWarmAborted


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.PeerSelection.PromoteWarmDone


***
target active, actual active, peer
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.PeerSelection.PromoteWarmFailed


***
target active, actual active, peer, reason
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.PeerSelection.PromoteWarmLocalPeers


***
local per-group (target active, actual active), selected peers
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.PeerSelection.PromoteWarmPeers


***
target active, actual active, selected peers
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.PeerSelection.PublicRootsFailure


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.PeerSelection.PublicRootsRequest


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.PeerSelection.PublicRootsResults


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.PeerSelection.TargetsChanged


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.PeerSelectionActions.MonitoringError


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.PeerSelectionActions.MonitoringResult


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.PeerSelectionActions.StatusChangeFailure


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.PeerSelectionActions.StatusChanged


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.PeerSelectionCounters.PeerSelectionCounters


***
Counters for cold, warm and hot peers
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Peers


***
TODO Doc
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.PublicRootPeers.PublicRootPeers.PublicRootDomains


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.PublicRootPeers.PublicRootPeers.PublicRootFailure


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.PublicRootPeers.PublicRootPeers.PublicRootRelayAccessPoint


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.PublicRootPeers.PublicRootPeers.PublicRootResult


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.ReplayBlock.LedgerReplay


***
Counts up the percent of a block replay.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Resources


***
TODO JNF
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Info`

### Cardano.Node.Server.AcceptConnection


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Server.AcceptError


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Server.AcceptPolicy


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Server.Error


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Server.Started


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Server.Stopped


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Shutdown.AbnormalShutdown


***
non-isEOFerror shutdown request
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Shutdown.RequestingShutdown


***
Ringing the node shutdown doorbell
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Shutdown.ShutdownArmedAtSlot


***
Setting up node shutdown at given slot.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Shutdown.ShutdownRequested


***
Node shutdown was requested.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Shutdown.ShutdownUnexpectedInput


***
Received shutdown request but found unexpected input in --shutdown-ipc FD: 
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Startup.Byron


***
_bibSystemStartTime_: TODO JNF 
_bibSlotLength_: gives the length of a slot as time interval. 
_bibEpochLength_: gives the number of slots which forms an epoch.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Startup.Common


***
_biConfigPath_: is the path to the config in use. 
_biProtocol_: is the name of the protocol, e.g. "Byron", "Shelley" or "Byron; Shelley". 
_biVersion_: is the version of the node software running. 
_biCommit_: is the commit revision of the software running. 
_biNodeStartTime_: gives the time this node was started.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Startup.Network


***
_niAddresses_: IPv4 or IPv6 socket ready to accept connectionsor diffusion addresses. 
_niDiffusionMode_: shows if the node runs only initiator or bothinitiator or responder node. 
_niDnsProducers_: shows the list of domain names to subscribe to. 
_niIpProducers_: shows the list of ip subscription addresses.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Startup.NetworkConfig


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Startup.NetworkConfigUpdate


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Startup.NetworkConfigUpdateError


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Startup.P2PWarning


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Startup.P2PWarningDevelopementNetworkProtocols


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Startup.ShelleyBased


***
bisEra is the current era, e.g. "Shelley", "Allegra", "Mary" or "Alonzo". 
_bisSystemStartTime_: TODO JNF 
_bisSlotLength_: gives the length of a slot as time interval. 
_bisEpochLength_: gives the number of slots which forms an epoch. 
_bisSlotsPerKESPeriod_: gives the slots per KES period.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Startup.StartupDBValidation


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Startup.StartupInfo


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Startup.StartupNetworkMagic


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Startup.StartupP2PInfo


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Startup.StartupSocketConfigError


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Startup.StartupTime


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.Startup.WarningDevelopmentNetworkProtocols


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.StateQueryClient.Recieve.Acquire


***
The client requests that the state as of a particular recent point onthe server's chain (within K of the tip) be made available to query,and waits for confirmation or failure.
From 'NodeToClient_V8' onwards if the point is not specified, current tipwill be acquired.  For previous versions of the protocol 'point' must begiven.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.StateQueryClient.Recieve.Acquired


***
The server can confirm that it has the state at the requested point.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.StateQueryClient.Recieve.Done


***
The client can terminate the protocol.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.StateQueryClient.Recieve.Failure


***
The server can report that it cannot obtain the state for therequested point.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.StateQueryClient.Recieve.Query


***
The client can perform queries on the current acquired state.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.StateQueryClient.Recieve.ReAcquire


***
This is like 'MsgAcquire' but for when the client already has astate. By moveing to another state directly without a 'MsgRelease' itenables optimisations on the server side (e.g. moving to the state forthe immediate next block).
Note that failure to re-acquire is equivalent to 'MsgRelease',rather than keeping the exiting acquired state.
From 'NodeToClient_V8' onwards if the point is not specified, current tipwill be acquired.  For previous versions of the protocol 'point' must begiven.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.StateQueryClient.Recieve.Release


***
The client can instruct the server to release the state. This letsthe server free resources.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.StateQueryClient.Recieve.Result


***
The server must reply with the queries.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.StateQueryClient.Send.Acquire


***
The client requests that the state as of a particular recent point onthe server's chain (within K of the tip) be made available to query,and waits for confirmation or failure.
From 'NodeToClient_V8' onwards if the point is not specified, current tipwill be acquired.  For previous versions of the protocol 'point' must begiven.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.StateQueryClient.Send.Acquired


***
The server can confirm that it has the state at the requested point.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.StateQueryClient.Send.Done


***
The client can terminate the protocol.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.StateQueryClient.Send.Failure


***
The server can report that it cannot obtain the state for therequested point.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.StateQueryClient.Send.Query


***
The client can perform queries on the current acquired state.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.StateQueryClient.Send.ReAcquire


***
This is like 'MsgAcquire' but for when the client already has astate. By moveing to another state directly without a 'MsgRelease' itenables optimisations on the server side (e.g. moving to the state forthe immediate next block).
Note that failure to re-acquire is equivalent to 'MsgRelease',rather than keeping the exiting acquired state.
From 'NodeToClient_V8' onwards if the point is not specified, current tipwill be acquired.  For previous versions of the protocol 'point' must begiven.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.StateQueryClient.Send.Release


***
The client can instruct the server to release the state. This letsthe server free resources.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.StateQueryClient.Send.Result


***
The server must reply with the queries.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxInbound.TxInboundCanRequestMoreTxs


***
There are no replies in flight, but we do know some more txs we can ask for, so lets ask for them and more txids.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxInbound.TxInboundCannotRequestMoreTxs


***
There's no replies in flight, and we have no more txs we can ask for so the only remaining thing to do is to ask for more txids. Since this is the only thing to do now, we make this a blocking call.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxInbound.TxInboundTerminated


***
Server received 'MsgDone'.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxInbound.TxSubmissionCollected


***
Number of transactions just about to be inserted.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxInbound.TxSubmissionProcessed


***
Just processed transaction pass/fail breakdown.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxMonitorClient.Recieve.Acquire


***
The client requests that the state as of a particular recent point onthe server's chain (within K of the tip) be made available to query,and waits for confirmation or failure.
From 'NodeToClient_V8' onwards if the point is not specified, current tipwill be acquired.  For previous versions of the protocol 'point' must begiven.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxMonitorClient.Recieve.Acquired


***
The server can confirm that it has the state at the requested point.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxMonitorClient.Recieve.Done


***
The client can terminate the protocol.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxMonitorClient.Recieve.Failure


***
The server can report that it cannot obtain the state for therequested point.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxMonitorClient.Recieve.Query


***
The client can perform queries on the current acquired state.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxMonitorClient.Recieve.ReAcquire


***
This is like 'MsgAcquire' but for when the client already has astate. By moveing to another state directly without a 'MsgRelease' itenables optimisations on the server side (e.g. moving to the state forthe immediate next block).
Note that failure to re-acquire is equivalent to 'MsgRelease',rather than keeping the exiting acquired state.
From 'NodeToClient_V8' onwards if the point is not specified, current tipwill be acquired.  For previous versions of the protocol 'point' must begiven.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxMonitorClient.Recieve.Release


***
The client can instruct the server to release the state. This letsthe server free resources.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxMonitorClient.Recieve.Result


***
The server must reply with the queries.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxMonitorClient.Send.Acquire


***
The client requests that the state as of a particular recent point onthe server's chain (within K of the tip) be made available to query,and waits for confirmation or failure.
From 'NodeToClient_V8' onwards if the point is not specified, current tipwill be acquired.  For previous versions of the protocol 'point' must begiven.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxMonitorClient.Send.Acquired


***
The server can confirm that it has the state at the requested point.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxMonitorClient.Send.Done


***
The client can terminate the protocol.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxMonitorClient.Send.Failure


***
The server can report that it cannot obtain the state for therequested point.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxMonitorClient.Send.Query


***
The client can perform queries on the current acquired state.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxMonitorClient.Send.ReAcquire


***
This is like 'MsgAcquire' but for when the client already has astate. By moveing to another state directly without a 'MsgRelease' itenables optimisations on the server side (e.g. moving to the state forthe immediate next block).
Note that failure to re-acquire is equivalent to 'MsgRelease',rather than keeping the exiting acquired state.
From 'NodeToClient_V8' onwards if the point is not specified, current tipwill be acquired.  For previous versions of the protocol 'point' must begiven.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxMonitorClient.Send.Release


***
The client can instruct the server to release the state. This letsthe server free resources.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxMonitorClient.Send.Result


***
The server must reply with the queries.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxOutbound.ControlMessage


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxOutbound.RecvMsgRequest


***
The IDs of the transactions requested.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxOutbound.SendMsgReply


***
The transactions to be sent in the response.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxSubmission.NodeToNode.Recieve.Done


***
Termination message, initiated by the client when the server is making a blocking call for more transaction identifiers.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxSubmission.NodeToNode.Recieve.ReplyTxIds


***
Reply with a list of transaction identifiers for availabletransactions, along with the size of each transaction.
The list must not be longer than the maximum number requested.
In the 'StTxIds' 'StBlocking' state the list must be non-empty whilein the 'StTxIds' 'StNonBlocking' state the list may be empty.
These transactions are added to the notional FIFO of outstandingtransaction identifiers for the protocol.
The order in which these transaction identifiers are returned must bethe order in which they are submitted to the mempool, to preservedependent transactions.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxSubmission.NodeToNode.Recieve.ReplyTxs


***
Reply with the requested transactions, or implicitly discard.
Transactions can become invalid between the time the transaction identifier was sent and the transaction being requested. Invalid (including committed) transactions do not need to be sent.
Any transaction identifiers requested but not provided in this reply should be considered as if this peer had never announced them. (Note that this is no guarantee that the transaction is invalid, it may still be valid and available from another peer).
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxSubmission.NodeToNode.Recieve.RequestTxIds


***
Request a non-empty list of transaction identifiers from the client,and confirm a number of outstanding transaction identifiers.
With 'TokBlocking' this is a a blocking operation: the response willalways have at least one transaction identifier, and it does not expecta prompt response: there is no timeout. This covers the case when thereis nothing else to do but wait. For example this covers leaf nodes thatrarely, if ever, create and submit a transaction.
With 'TokNonBlocking' this is a non-blocking operation: the responsemay be an empty list and this does expect a prompt response. Thiscovers high throughput use cases where we wish to pipeline, byinterleaving requests for additional transaction identifiers withrequests for transactions, which requires these requests not block.
The request gives the maximum number of transaction identifiers thatcan be accepted in the response. This must be greater than zero in the'TokBlocking' case. In the 'TokNonBlocking' case either the numbersacknowledged or the number requested must be non-zero. In either case,the number requested must not put the total outstanding over the fixedprotocol limit.
The request also gives the number of outstanding transactionidentifiers that can now be acknowledged. The actual transactionsto acknowledge are known to the peer based on the FIFO order in whichthey were provided.
There is no choice about when to use the blocking case versus thenon-blocking case, it depends on whether there are any remainingunacknowledged transactions (after taking into account the onesacknowledged in this message):
* The blocking case must be used when there are zero remaining  unacknowledged transactions.
* The non-blocking case must be used when there are non-zero remaining  unacknowledged transactions.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxSubmission.NodeToNode.Recieve.RequestTxs


***
Request one or more transactions corresponding to the given transaction identifiers. 
While it is the responsibility of the replying peer to keep within pipelining in-flight limits, the sender must also cooperate by keeping the total requested across all in-flight requests within the limits. 
It is an error to ask for transaction identifiers that were not previously announced (via 'MsgReplyTxIds'). 
It is an error to ask for transaction identifiers that are not outstanding or that were already asked for.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxSubmission.NodeToNode.Send.Done


***
Termination message, initiated by the client when the server is making a blocking call for more transaction identifiers.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxSubmission.NodeToNode.Send.ReplyTxIds


***
Reply with a list of transaction identifiers for availabletransactions, along with the size of each transaction.
The list must not be longer than the maximum number requested.
In the 'StTxIds' 'StBlocking' state the list must be non-empty whilein the 'StTxIds' 'StNonBlocking' state the list may be empty.
These transactions are added to the notional FIFO of outstandingtransaction identifiers for the protocol.
The order in which these transaction identifiers are returned must bethe order in which they are submitted to the mempool, to preservedependent transactions.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxSubmission.NodeToNode.Send.ReplyTxs


***
Reply with the requested transactions, or implicitly discard.
Transactions can become invalid between the time the transaction identifier was sent and the transaction being requested. Invalid (including committed) transactions do not need to be sent.
Any transaction identifiers requested but not provided in this reply should be considered as if this peer had never announced them. (Note that this is no guarantee that the transaction is invalid, it may still be valid and available from another peer).
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxSubmission.NodeToNode.Send.RequestTxIds


***
Request a non-empty list of transaction identifiers from the client,and confirm a number of outstanding transaction identifiers.
With 'TokBlocking' this is a a blocking operation: the response willalways have at least one transaction identifier, and it does not expecta prompt response: there is no timeout. This covers the case when thereis nothing else to do but wait. For example this covers leaf nodes thatrarely, if ever, create and submit a transaction.
With 'TokNonBlocking' this is a non-blocking operation: the responsemay be an empty list and this does expect a prompt response. Thiscovers high throughput use cases where we wish to pipeline, byinterleaving requests for additional transaction identifiers withrequests for transactions, which requires these requests not block.
The request gives the maximum number of transaction identifiers thatcan be accepted in the response. This must be greater than zero in the'TokBlocking' case. In the 'TokNonBlocking' case either the numbersacknowledged or the number requested must be non-zero. In either case,the number requested must not put the total outstanding over the fixedprotocol limit.
The request also gives the number of outstanding transactionidentifiers that can now be acknowledged. The actual transactionsto acknowledge are known to the peer based on the FIFO order in whichthey were provided.
There is no choice about when to use the blocking case versus thenon-blocking case, it depends on whether there are any remainingunacknowledged transactions (after taking into account the onesacknowledged in this message):
* The blocking case must be used when there are zero remaining  unacknowledged transactions.
* The non-blocking case must be used when there are non-zero remaining  unacknowledged transactions.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxSubmission.NodeToNode.Send.RequestTxs


***
Request one or more transactions corresponding to the given transaction identifiers. 
While it is the responsibility of the replying peer to keep within pipelining in-flight limits, the sender must also cooperate by keeping the total requested across all in-flight requests within the limits. 
It is an error to ask for transaction identifiers that were not previously announced (via 'MsgReplyTxIds'). 
It is an error to ask for transaction identifiers that are not outstanding or that were already asked for.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxSubmission2.NodeToNode.Recieve.Done


***
Termination message, initiated by the client when the server ismaking a blocking call for more transaction identifiers.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxSubmission2.NodeToNode.Recieve.MsgHello


***
Client side hello message.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxSubmission2.NodeToNode.Recieve.ReplyTxIds


***
Reply with a list of transaction identifiers for availabletransactions, along with the size of each transaction.
The list must not be longer than the maximum number requested.
In the 'StTxIds' 'StBlocking' state the list must be non-empty whilein the 'StTxIds' 'StNonBlocking' state the list may be empty.
These transactions are added to the notional FIFO of outstandingtransaction identifiers for the protocol.
The order in which these transaction identifiers are returned must bethe order in which they are submitted to the mempool, to preservedependent transactions.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxSubmission2.NodeToNode.Recieve.ReplyTxs


***
Reply with the requested transactions, or implicitly discard.
Transactions can become invalid between the time the transactionidentifier was sent and the transaction being requested. Invalid(including committed) transactions do not need to be sent.
Any transaction identifiers requested but not provided in this replyshould be considered as if this peer had never announced them. (Notethat this is no guarantee that the transaction is invalid, it may stillbe valid and available from another peer).
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxSubmission2.NodeToNode.Recieve.RequestTxIds


***
Request a non-empty list of transaction identifiers from the client, and confirm a number of outstanding transaction identifiers. 
With 'TokBlocking' this is a a blocking operation: the response will always have at least one transaction identifier, and it does not expect a prompt response: there is no timeout. This covers the case when there is nothing else to do but wait. For example this covers leaf nodes that rarely, if ever, create and submit a transaction. 
With 'TokNonBlocking' this is a non-blocking operation: the response may be an empty list and this does expect a prompt response. This covers high throughput use cases where we wish to pipeline, by interleaving requests for additional transaction identifiers with requests for transactions, which requires these requests not block. 
The request gives the maximum number of transaction identifiers that can be accepted in the response. This must be greater than zero in the 'TokBlocking' case. In the 'TokNonBlocking' case either the numbers acknowledged or the number requested must be non-zero. In either case, the number requested must not put the total outstanding over the fixed protocol limit. 
The request also gives the number of outstanding transaction identifiers that can now be acknowledged. The actual transactions to acknowledge are known to the peer based on the FIFO order in which they were provided. 
There is no choice about when to use the blocking case versus the non-blocking case, it depends on whether there are any remaining unacknowledged transactions (after taking into account the ones acknowledged in this message): 
* The blocking case must be used when there are zero remaining   unacknowledged transactions. 
* The non-blocking case must be used when there are non-zero remaining   unacknowledged transactions.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxSubmission2.NodeToNode.Recieve.RequestTxs


***
Request one or more transactions corresponding to the given transaction identifiers. 
While it is the responsibility of the replying peer to keep withinpipelining in-flight limits, the sender must also cooperate by keepingthe total requested across all in-flight requests within the limits.
It is an error to ask for transaction identifiers that were notpreviously announced (via 'MsgReplyTxIds').
It is an error to ask for transaction identifiers that are notoutstanding or that were already asked for.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxSubmission2.NodeToNode.Send.Done


***
Termination message, initiated by the client when the server ismaking a blocking call for more transaction identifiers.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxSubmission2.NodeToNode.Send.MsgHello


***
Client side hello message.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxSubmission2.NodeToNode.Send.ReplyTxIds


***
Reply with a list of transaction identifiers for availabletransactions, along with the size of each transaction.
The list must not be longer than the maximum number requested.
In the 'StTxIds' 'StBlocking' state the list must be non-empty whilein the 'StTxIds' 'StNonBlocking' state the list may be empty.
These transactions are added to the notional FIFO of outstandingtransaction identifiers for the protocol.
The order in which these transaction identifiers are returned must bethe order in which they are submitted to the mempool, to preservedependent transactions.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxSubmission2.NodeToNode.Send.ReplyTxs


***
Reply with the requested transactions, or implicitly discard.
Transactions can become invalid between the time the transactionidentifier was sent and the transaction being requested. Invalid(including committed) transactions do not need to be sent.
Any transaction identifiers requested but not provided in this replyshould be considered as if this peer had never announced them. (Notethat this is no guarantee that the transaction is invalid, it may stillbe valid and available from another peer).
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxSubmission2.NodeToNode.Send.RequestTxIds


***
Request a non-empty list of transaction identifiers from the client, and confirm a number of outstanding transaction identifiers. 
With 'TokBlocking' this is a a blocking operation: the response will always have at least one transaction identifier, and it does not expect a prompt response: there is no timeout. This covers the case when there is nothing else to do but wait. For example this covers leaf nodes that rarely, if ever, create and submit a transaction. 
With 'TokNonBlocking' this is a non-blocking operation: the response may be an empty list and this does expect a prompt response. This covers high throughput use cases where we wish to pipeline, by interleaving requests for additional transaction identifiers with requests for transactions, which requires these requests not block. 
The request gives the maximum number of transaction identifiers that can be accepted in the response. This must be greater than zero in the 'TokBlocking' case. In the 'TokNonBlocking' case either the numbers acknowledged or the number requested must be non-zero. In either case, the number requested must not put the total outstanding over the fixed protocol limit. 
The request also gives the number of outstanding transaction identifiers that can now be acknowledged. The actual transactions to acknowledge are known to the peer based on the FIFO order in which they were provided. 
There is no choice about when to use the blocking case versus the non-blocking case, it depends on whether there are any remaining unacknowledged transactions (after taking into account the ones acknowledged in this message): 
* The blocking case must be used when there are zero remaining   unacknowledged transactions. 
* The non-blocking case must be used when there are non-zero remaining   unacknowledged transactions.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxSubmission2.NodeToNode.Send.RequestTxs


***
Request one or more transactions corresponding to the given transaction identifiers. 
While it is the responsibility of the replying peer to keep withinpipelining in-flight limits, the sender must also cooperate by keepingthe total requested across all in-flight requests within the limits.
It is an error to ask for transaction identifiers that were notpreviously announced (via 'MsgReplyTxIds').
It is an error to ask for transaction identifiers that are notoutstanding or that were already asked for.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxSubmissionClient.Recieve.AcceptTx


***
The server can reply to inform the client that it has accepted thetransaction.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxSubmissionClient.Recieve.Done


***
The client can terminate the protocol.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxSubmissionClient.Recieve.RejectTx


***
The server can reply to inform the client that it has rejected thetransaction. A reason for the rejection is included.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxSubmissionClient.Recieve.SubmitTx


***
The client submits a single transaction and waits a reply.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxSubmissionClient.Send.AcceptTx


***
The server can reply to inform the client that it has accepted thetransaction.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxSubmissionClient.Send.Done


***
The client can terminate the protocol.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxSubmissionClient.Send.RejectTx


***
The server can reply to inform the client that it has rejected thetransaction. A reason for the rejection is included.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

### Cardano.Node.TxSubmissionClient.Send.SubmitTx


***
The client submits a single transaction and waits a reply.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered:  because the filter level is `Notice`

## Metrics
### Block replay progress (%)

***
Progress in percent
***


Dispatched by: 
Cardano.Node.ReplayBlock.LedgerReplay

From current configuration:
Filtered:  because the filter level is `Notice`

### blocksForgedNum

***
How many blocks did forge in this node?
***


Dispatched by: 
Cardano.Node.ForgeStats.ForgeStats

From current configuration:
Filtered:  because the filter level is `Notice`

### cardano.node.aboutToLeadSlotLast

***

***


Dispatched by: 
Cardano.Node.Forge.StartLeadershipCheck

From current configuration:
Filtered:  because the filter level is `Info`

### cardano.node.aboutToLeadSlotLast

***

***


Dispatched by: 
Cardano.Node.Forge.StartLeadershipCheckPlus

From current configuration:
Filtered:  because the filter level is `Info`

### cardano.node.adoptedSlotLast

***

***


Dispatched by: 
Cardano.Node.Forge.AdoptedBlock

From current configuration:
Filtered:  because the filter level is `Info`

### cardano.node.blockContext

***

***


Dispatched by: 
Cardano.Node.Forge.BlockContext

From current configuration:
Filtered:  because the filter level is `Info`

### cardano.node.blockFromFuture

***

***


Dispatched by: 
Cardano.Node.Forge.BlockFromFuture

From current configuration:
Filtered:  because the filter level is `Info`

### cardano.node.blocks

***
Number of blocks in this chain fragment.
***


Dispatched by: 
Cardano.Node.ChainDB.AddBlockEvent.AddedToCurrentChain

From current configuration:
Filtered:  because the filter level is `Info`

### cardano.node.blocks

***
Number of blocks in this chain fragment.
***


Dispatched by: 
Cardano.Node.ChainDB.AddBlockEvent.SwitchedToAFork

From current configuration:
Filtered:  because the filter level is `Info`

### cardano.node.chainSync.rollForward

***

***


Dispatched by: 
Cardano.Node.ChainSyncServerHeader.ChainSyncServerEvent.ServerRead.RollForward

From current configuration:
Filtered:  because the filter level is `Notice`

### cardano.node.chainSync.rollForward

***

***


Dispatched by: 
Cardano.Node.ChainSyncServerBlock.ChainSyncServerEvent.ServerRead.RollForward

From current configuration:
Filtered:  because the filter level is `Notice`

### cardano.node.connectedPeers

***
Number of connected peers
***


Dispatched by: 
Cardano.Node.BlockFetchDecision

From current configuration:
Filtered:  because the filter level is `Notice`

### cardano.node.connectionManager.duplexConns

***

***


Dispatched by: 
Cardano.Node.ConnectionManager.ConnectionManagerCounters

From current configuration:
Filtered:  because the filter level is `Notice`

### cardano.node.connectionManager.duplexConns

***

***


Dispatched by: 
Cardano.Node.LocalConnectionManager.ConnectionManagerCounters

From current configuration:
Filtered:  because the filter level is `Notice`

### cardano.node.connectionManager.fullDuplexConns

***

***


Dispatched by: 
Cardano.Node.ConnectionManager.ConnectionManagerCounters

From current configuration:
Filtered:  because the filter level is `Notice`

### cardano.node.connectionManager.fullDuplexConns

***

***


Dispatched by: 
Cardano.Node.LocalConnectionManager.ConnectionManagerCounters

From current configuration:
Filtered:  because the filter level is `Notice`

### cardano.node.connectionManager.inboundConns

***

***


Dispatched by: 
Cardano.Node.ConnectionManager.ConnectionManagerCounters

From current configuration:
Filtered:  because the filter level is `Notice`

### cardano.node.connectionManager.inboundConns

***

***


Dispatched by: 
Cardano.Node.LocalConnectionManager.ConnectionManagerCounters

From current configuration:
Filtered:  because the filter level is `Notice`

### cardano.node.connectionManager.outboundConns

***

***


Dispatched by: 
Cardano.Node.ConnectionManager.ConnectionManagerCounters

From current configuration:
Filtered:  because the filter level is `Notice`

### cardano.node.connectionManager.outboundConns

***

***


Dispatched by: 
Cardano.Node.LocalConnectionManager.ConnectionManagerCounters

From current configuration:
Filtered:  because the filter level is `Notice`

### cardano.node.connectionManager.unidirectionalConns

***

***


Dispatched by: 
Cardano.Node.ConnectionManager.ConnectionManagerCounters

From current configuration:
Filtered:  because the filter level is `Notice`

### cardano.node.connectionManager.unidirectionalConns

***

***


Dispatched by: 
Cardano.Node.LocalConnectionManager.ConnectionManagerCounters

From current configuration:
Filtered:  because the filter level is `Notice`

### cardano.node.couldNotForgeSlotLast

***

***


Dispatched by: 
Cardano.Node.Forge.NoLedgerState

From current configuration:
Filtered:  because the filter level is `Info`

### cardano.node.couldNotForgeSlotLast

***

***


Dispatched by: 
Cardano.Node.Forge.NoLedgerView

From current configuration:
Filtered:  because the filter level is `Info`

### cardano.node.currentKESPeriod

***

***


Dispatched by: 
Cardano.Node.Forge.ForgeStateUpdateError

From current configuration:
Filtered:  because the filter level is `Info`

### cardano.node.delegMapSize

***

***


Dispatched by: 
Cardano.Node.Forge.StartLeadershipCheckPlus

From current configuration:
Filtered:  because the filter level is `Info`

### cardano.node.density

***
The actual number of blocks created over the maximum expected number of blocks that could be created over the span of the last @k@ blocks.
***


Dispatched by: 
Cardano.Node.ChainDB.AddBlockEvent.AddedToCurrentChain

From current configuration:
Filtered:  because the filter level is `Info`

### cardano.node.density

***
The actual number of blocks created over the maximum expected number of blocks that could be created over the span of the last @k@ blocks.
***


Dispatched by: 
Cardano.Node.ChainDB.AddBlockEvent.SwitchedToAFork

From current configuration:
Filtered:  because the filter level is `Info`

### cardano.node.epoch

***
In which epoch is the tip of the current chain.
***


Dispatched by: 
Cardano.Node.ChainDB.AddBlockEvent.AddedToCurrentChain

From current configuration:
Filtered:  because the filter level is `Info`

### cardano.node.epoch

***
In which epoch is the tip of the current chain.
***


Dispatched by: 
Cardano.Node.ChainDB.AddBlockEvent.SwitchedToAFork

From current configuration:
Filtered:  because the filter level is `Info`

### cardano.node.forgedInvalidSlotLast

***

***


Dispatched by: 
Cardano.Node.Forge.ForgedInvalidBlock

From current configuration:
Filtered:  because the filter level is `Info`

### cardano.node.forgedSlotLast

***

***


Dispatched by: 
Cardano.Node.Forge.ForgedBlock

From current configuration:
Filtered:  because the filter level is `Info`

### cardano.node.ledgerState

***

***


Dispatched by: 
Cardano.Node.Forge.LedgerState

From current configuration:
Filtered:  because the filter level is `Info`

### cardano.node.ledgerView

***

***


Dispatched by: 
Cardano.Node.Forge.LedgerView

From current configuration:
Filtered:  because the filter level is `Info`

### cardano.node.mempoolBytes

***
Byte size of the mempool
***


Dispatched by: 
Cardano.Node.Mempool.AddedTx

From current configuration:
Filtered:  because the filter level is `Info`

### cardano.node.mempoolBytes

***
Byte size of the mempool
***


Dispatched by: 
Cardano.Node.Mempool.ManuallyRemovedTxs

From current configuration:
Filtered:  because the filter level is `Info`

### cardano.node.mempoolBytes

***
Byte size of the mempool
***


Dispatched by: 
Cardano.Node.Mempool.RejectedTx

From current configuration:
Filtered:  because the filter level is `Info`

### cardano.node.mempoolBytes

***
Byte size of the mempool
***


Dispatched by: 
Cardano.Node.Mempool.RemoveTxs

From current configuration:
Filtered:  because the filter level is `Info`

### cardano.node.nodeCannotForge

***

***


Dispatched by: 
Cardano.Node.Forge.NodeCannotForge

From current configuration:
Filtered:  because the filter level is `Info`

### cardano.node.nodeIsLeader

***

***


Dispatched by: 
Cardano.Node.Forge.NodeIsLeader

From current configuration:
Filtered:  because the filter level is `Info`

### cardano.node.nodeNotLeader

***

***


Dispatched by: 
Cardano.Node.Forge.NodeNotLeader

From current configuration:
Filtered:  because the filter level is `Info`

### cardano.node.notAdoptedSlotLast

***

***


Dispatched by: 
Cardano.Node.Forge.DidntAdoptBlock

From current configuration:
Filtered:  because the filter level is `Info`

### cardano.node.operationalCertificateExpiryKESPeriod

***

***


Dispatched by: 
Cardano.Node.Forge.ForgeStateUpdateError

From current configuration:
Filtered:  because the filter level is `Info`

### cardano.node.operationalCertificateStartKESPeriod

***

***


Dispatched by: 
Cardano.Node.Forge.ForgeStateUpdateError

From current configuration:
Filtered:  because the filter level is `Info`

### cardano.node.peerSelection.cold

***
Number of cold peers
***


Dispatched by: 
Cardano.Node.PeerSelectionCounters.PeerSelectionCounters

From current configuration:
Filtered:  because the filter level is `Notice`

### cardano.node.peerSelection.hot

***
Number of hot peers
***


Dispatched by: 
Cardano.Node.PeerSelectionCounters.PeerSelectionCounters

From current configuration:
Filtered:  because the filter level is `Notice`

### cardano.node.peerSelection.warm

***
Number of warm peers
***


Dispatched by: 
Cardano.Node.PeerSelectionCounters.PeerSelectionCounters

From current configuration:
Filtered:  because the filter level is `Notice`

### cardano.node.remainingKESPeriods

***

***


Dispatched by: 
Cardano.Node.Forge.ForgeStateUpdateError

From current configuration:
Filtered:  because the filter level is `Info`

### cardano.node.served.block

***

***


Dispatched by: 
Cardano.Node.BlockFetchServer.SendBlock

From current configuration:
Filtered:  because the filter level is `Notice`

### cardano.node.slotInEpoch

***
Relative slot number of the tip of the current chain within theepoch..
***


Dispatched by: 
Cardano.Node.ChainDB.AddBlockEvent.AddedToCurrentChain

From current configuration:
Filtered:  because the filter level is `Info`

### cardano.node.slotInEpoch

***
Relative slot number of the tip of the current chain within theepoch..
***


Dispatched by: 
Cardano.Node.ChainDB.AddBlockEvent.SwitchedToAFork

From current configuration:
Filtered:  because the filter level is `Info`

### cardano.node.slotIsImmutable

***

***


Dispatched by: 
Cardano.Node.Forge.SlotIsImmutable

From current configuration:
Filtered:  because the filter level is `Info`

### cardano.node.slots

***
Number of slots in this chain fragment.
***


Dispatched by: 
Cardano.Node.ChainDB.AddBlockEvent.AddedToCurrentChain

From current configuration:
Filtered:  because the filter level is `Info`

### cardano.node.slots

***
Number of slots in this chain fragment.
***


Dispatched by: 
Cardano.Node.ChainDB.AddBlockEvent.SwitchedToAFork

From current configuration:
Filtered:  because the filter level is `Info`

### cardano.node.submissions.accepted

***

***


Dispatched by: 
Cardano.Node.TxInbound.TxSubmissionProcessed

From current configuration:
Filtered:  because the filter level is `Notice`

### cardano.node.submissions.rejected

***

***


Dispatched by: 
Cardano.Node.TxInbound.TxSubmissionProcessed

From current configuration:
Filtered:  because the filter level is `Notice`

### cardano.node.submissions.submitted

***

***


Dispatched by: 
Cardano.Node.TxInbound.TxSubmissionCollected

From current configuration:
Filtered:  because the filter level is `Notice`

### cardano.node.txsInMempool

***
Transactions in mempool
***


Dispatched by: 
Cardano.Node.Mempool.AddedTx

From current configuration:
Filtered:  because the filter level is `Info`

### cardano.node.txsInMempool

***
Transactions in mempool
***


Dispatched by: 
Cardano.Node.Mempool.ManuallyRemovedTxs

From current configuration:
Filtered:  because the filter level is `Info`

### cardano.node.txsInMempool

***
Transactions in mempool
***


Dispatched by: 
Cardano.Node.Mempool.RejectedTx

From current configuration:
Filtered:  because the filter level is `Info`

### cardano.node.txsInMempool

***
Transactions in mempool
***


Dispatched by: 
Cardano.Node.Mempool.RemoveTxs

From current configuration:
Filtered:  because the filter level is `Info`

### cardano.node.txsProcessedNum

***

***


Dispatched by: 
Cardano.Node.Mempool.ManuallyRemovedTxs

From current configuration:
Filtered:  because the filter level is `Info`

### cardano.node.utxoSize

***

***


Dispatched by: 
Cardano.Node.Forge.StartLeadershipCheckPlus

From current configuration:
Filtered:  because the filter level is `Info`

### mem.resident

***
TODO JNF
***


Dispatched by: 
Cardano.Node.Resources

From current configuration:
Filtered:  because the filter level is `Info`

### nodeCannotForgeNum

***
How many times this node could not forge?
***


Dispatched by: 
Cardano.Node.ForgeStats.ForgeStats

From current configuration:
Filtered:  because the filter level is `Notice`

### nodeIsLeaderNum

***
How many times this node was leader?
***


Dispatched by: 
Cardano.Node.ForgeStats.ForgeStats

From current configuration:
Filtered:  because the filter level is `Notice`

### peersFromNodeKernel

***
TODO Doc
***


Dispatched by: 
Cardano.Node.Peers

From current configuration:
Filtered:  because the filter level is `Notice`

### rts.gcLiveBytes

***
TODO JNF
***


Dispatched by: 
Cardano.Node.Resources

From current configuration:
Filtered:  because the filter level is `Info`

### rts.gcMajorNum

***
TODO JNF
***


Dispatched by: 
Cardano.Node.Resources

From current configuration:
Filtered:  because the filter level is `Info`

### rts.gcMinorNum

***
TODO JNF
***


Dispatched by: 
Cardano.Node.Resources

From current configuration:
Filtered:  because the filter level is `Info`

### rts.gcticks

***
TODO JNF
***


Dispatched by: 
Cardano.Node.Resources

From current configuration:
Filtered:  because the filter level is `Info`

### rts.mutticks

***
TODO JNF
***


Dispatched by: 
Cardano.Node.Resources

From current configuration:
Filtered:  because the filter level is `Info`

### rts.threads

***
TODO JNF
***


Dispatched by: 
Cardano.Node.Resources

From current configuration:
Filtered:  because the filter level is `Info`

### slotsMissed

***
How many slots were missed in this node?
***


Dispatched by: 
Cardano.Node.ForgeStats.ForgeStats

From current configuration:
Filtered:  because the filter level is `Notice`

### stat.cputicks

***
Reports the CPU ticks, sice the process was started
***


Dispatched by: 
Cardano.Node.Resources

From current configuration:
Filtered:  because the filter level is `Info`

## Datapoints
### NodeInfo


***
Basic information about this node collected at startup

 _niName_: Name of the node. 
 _niProtocol_: Protocol which this nodes uses. 
 _niVersion_: Software version which this node is using. 
 _niStartTime_: Start time of this node. 
 _niSystemStartTime_: How long did the start of the node took.
***


Configuration: TraceConfig {tcOptions = fromList [([],[ConfBackend [Stdout MachineFormat,EKGBackend],ConfDetail DNormal,ConfSeverity Notice]),(["Node","AcceptPolicy"],[ConfSeverity Info]),(["Node","BlockFetchClient","CompletedBlockFetch"],[ConfLimiter "CompletedBlockFetchLimiter" 2.0]),(["Node","ChainDB"],[ConfSeverity Info]),(["Node","ChainDB","AddBlockEvent","AddBlockValidation","ValidCandidate"],[ConfLimiter "ValidCandidateLimiter" 2.0]),(["Node","ChainDB","AddBlockEvent","AddedBlockToQueue"],[ConfLimiter "AddedBlockToQueueLimiter" 2.0]),(["Node","ChainDB","AddBlockEvent","AddedBlockToVolatileDB"],[ConfLimiter "AddedBlockToVolatileDBLimiter" 2.0]),(["Node","ChainDB","CopyToImmutableDBEvent","CopiedBlockToImmutableDB"],[ConfLimiter "CopiedBlockToImmutableDBLimiter" 2.0]),(["Node","DNSResolver"],[ConfSeverity Info]),(["Node","DNSSubscription"],[ConfSeverity Info]),(["Node","DiffusionInit"],[ConfSeverity Info]),(["Node","ErrorPolicy"],[ConfSeverity Info]),(["Node","Forge"],[ConfSeverity Info]),(["Node","IpSubscription"],[ConfSeverity Info]),(["Node","LocalErrorPolicy"],[ConfSeverity Info]),(["Node","Mempool"],[ConfSeverity Info]),(["Node","Resources"],[ConfSeverity Info])], tcForwarder = TraceOptionForwarder {tofAddress = LocalSocket "/tmp/forwarder.sock", tofMode = Initiator, tofConnQueueSize = 2000, tofDisconnQueueSize = 200000, tofVerbosity = Minimum}, tcNodeName = Nothing, tcPeerFreqency = Just 2000, tcResourceFreqency = Just 5000}

672 log messages.
Generated at 2022-05-03 11:01:11.142471148 CEST.