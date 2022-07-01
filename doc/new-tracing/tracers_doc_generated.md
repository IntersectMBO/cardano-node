# Cardano Trace Documentation
# Table Of Contents


## [Trace Messages](#trace-messages)
1. __AcceptPolicy__
	1. [ConnectionHardLimit](#acceptpolicyconnectionhardlimit)
	1. [ConnectionLimitResume](#acceptpolicyconnectionlimitresume)
	1. [ConnectionRateLimiting](#acceptpolicyconnectionratelimiting)
1. __BlockFetch__
	1. __NodeToNode__
		1. __Recieve__
			1. [BatchDone](#blockfetchnodetonoderecievebatchdone)
			1. [Block](#blockfetchnodetonoderecieveblock)
			1. [ClientDone](#blockfetchnodetonoderecieveclientdone)
			1. [NoBlocks](#blockfetchnodetonoderecievenoblocks)
			1. [RequestRange](#blockfetchnodetonoderecieverequestrange)
			1. [StartBatch](#blockfetchnodetonoderecievestartbatch)
		1. __Send__
			1. [BatchDone](#blockfetchnodetonodesendbatchdone)
			1. [Block](#blockfetchnodetonodesendblock)
			1. [ClientDone](#blockfetchnodetonodesendclientdone)
			1. [NoBlocks](#blockfetchnodetonodesendnoblocks)
			1. [RequestRange](#blockfetchnodetonodesendrequestrange)
			1. [StartBatch](#blockfetchnodetonodesendstartbatch)
1. __BlockFetchClient__
	1. [AcknowledgedFetchRequest](#blockfetchclientacknowledgedfetchrequest)
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
			1. [BatchDone](#blockfetchserialisednodetonoderecievebatchdone)
			1. [Block](#blockfetchserialisednodetonoderecieveblock)
			1. [ClientDone](#blockfetchserialisednodetonoderecieveclientdone)
			1. [NoBlocks](#blockfetchserialisednodetonoderecievenoblocks)
			1. [RequestRange](#blockfetchserialisednodetonoderecieverequestrange)
			1. [StartBatch](#blockfetchserialisednodetonoderecievestartbatch)
		1. __Send__
			1. [BatchDone](#blockfetchserialisednodetonodesendbatchdone)
			1. [Block](#blockfetchserialisednodetonodesendblock)
			1. [ClientDone](#blockfetchserialisednodetonodesendclientdone)
			1. [NoBlocks](#blockfetchserialisednodetonodesendnoblocks)
			1. [RequestRange](#blockfetchserialisednodetonodesendrequestrange)
			1. [StartBatch](#blockfetchserialisednodetonodesendstartbatch)
1. __BlockFetchServer__
	1. [SendBlock](#blockfetchserversendblock)
1. __BlockchainTime__
	1. [CurrentSlotUnknown](#blockchaintimecurrentslotunknown)
	1. [StartTimeInTheFuture](#blockchaintimestarttimeinthefuture)
	1. [SystemClockMovedBack](#blockchaintimesystemclockmovedback)
1. __ChainDB__
	1. __AddBlockEvent__
		1. __AddBlockValidation__
			1. [CandidateContainsFutureBlocks](#chaindbaddblockeventaddblockvalidationcandidatecontainsfutureblocks)
			1. [CandidateContainsFutureBlocksExceedingClockSkew](#chaindbaddblockeventaddblockvalidationcandidatecontainsfutureblocksexceedingclockskew)
			1. [InvalidBlock](#chaindbaddblockeventaddblockvalidationinvalidblock)
			1. [UpdateLedgerDb](#chaindbaddblockeventaddblockvalidationupdateledgerdb)
			1. [ValidCandidate](#chaindbaddblockeventaddblockvalidationvalidcandidate)
		1. [AddedBlockToQueue](#chaindbaddblockeventaddedblocktoqueue)
		1. [AddedBlockToVolatileDB](#chaindbaddblockeventaddedblocktovolatiledb)
		1. [AddedToCurrentChain](#chaindbaddblockeventaddedtocurrentchain)
		1. [BlockInTheFuture](#chaindbaddblockeventblockinthefuture)
		1. [ChainSelectionForFutureBlock](#chaindbaddblockeventchainselectionforfutureblock)
		1. [IgnoreBlockAlreadyInVolatileDB](#chaindbaddblockeventignoreblockalreadyinvolatiledb)
		1. [IgnoreBlockOlderThanK](#chaindbaddblockeventignoreblockolderthank)
		1. [IgnoreInvalidBlock](#chaindbaddblockeventignoreinvalidblock)
		1. __PipeliningEvent__
			1. [OutdatedTentativeHeader](#chaindbaddblockeventpipeliningeventoutdatedtentativeheader)
			1. [SetTentativeHeader](#chaindbaddblockeventpipeliningeventsettentativeheader)
			1. [TrapTentativeHeader](#chaindbaddblockeventpipeliningeventtraptentativeheader)
		1. [PoppedBlockFromQueue](#chaindbaddblockeventpoppedblockfromqueue)
		1. [StoreButDontChange](#chaindbaddblockeventstorebutdontchange)
		1. [SwitchedToAFork](#chaindbaddblockeventswitchedtoafork)
		1. [TryAddToCurrentChain](#chaindbaddblockeventtryaddtocurrentchain)
		1. [TrySwitchToAFork](#chaindbaddblockeventtryswitchtoafork)
	1. __CopyToImmutableDBEvent__
		1. [CopiedBlockToImmutableDB](#chaindbcopytoimmutabledbeventcopiedblocktoimmutabledb)
		1. [NoBlocksToCopyToImmutableDB](#chaindbcopytoimmutabledbeventnoblockstocopytoimmutabledb)
	1. __FollowerEvent__
		1. [FollowerNewImmIterator](#chaindbfollowereventfollowernewimmiterator)
		1. [FollowerNoLongerInMem](#chaindbfollowereventfollowernolongerinmem)
		1. [FollowerSwitchToMem](#chaindbfollowereventfollowerswitchtomem)
		1. [NewFollower](#chaindbfollowereventnewfollower)
	1. __GCEvent__
		1. [PerformedGC](#chaindbgceventperformedgc)
		1. [ScheduledGC](#chaindbgceventscheduledgc)
	1. __ImmutableDBEvent__
		1. __CacheEvent__
			1. [CurrentChunkHit](#chaindbimmutabledbeventcacheeventcurrentchunkhit)
			1. [PastChunkEvict](#chaindbimmutabledbeventcacheeventpastchunkevict)
			1. [PastChunkExpired](#chaindbimmutabledbeventcacheeventpastchunkexpired)
			1. [PastChunkHit](#chaindbimmutabledbeventcacheeventpastchunkhit)
			1. [PastChunkMiss](#chaindbimmutabledbeventcacheeventpastchunkmiss)
		1. [ChunkFileDoesntFit](#chaindbimmutabledbeventchunkfiledoesntfit)
		1. __ChunkValidation__
			1. [InvalidChunkFile](#chaindbimmutabledbeventchunkvalidationinvalidchunkfile)
			1. [InvalidPrimaryIndex](#chaindbimmutabledbeventchunkvalidationinvalidprimaryindex)
			1. [InvalidSecondaryIndex](#chaindbimmutabledbeventchunkvalidationinvalidsecondaryindex)
			1. [MissingChunkFile](#chaindbimmutabledbeventchunkvalidationmissingchunkfile)
			1. [MissingPrimaryIndex](#chaindbimmutabledbeventchunkvalidationmissingprimaryindex)
			1. [MissingSecondaryIndex](#chaindbimmutabledbeventchunkvalidationmissingsecondaryindex)
			1. [RewritePrimaryIndex](#chaindbimmutabledbeventchunkvalidationrewriteprimaryindex)
			1. [RewriteSecondaryIndex](#chaindbimmutabledbeventchunkvalidationrewritesecondaryindex)
			1. [StartedValidatingChunk](#chaindbimmutabledbeventchunkvalidationstartedvalidatingchunk)
			1. [ValidatedChunk](#chaindbimmutabledbeventchunkvalidationvalidatedchunk)
		1. [DBAlreadyClosed](#chaindbimmutabledbeventdbalreadyclosed)
		1. [DBClosed](#chaindbimmutabledbeventdbclosed)
		1. [DeletingAfter](#chaindbimmutabledbeventdeletingafter)
		1. [Migrating](#chaindbimmutabledbeventmigrating)
		1. [NoValidLastLocation](#chaindbimmutabledbeventnovalidlastlocation)
		1. [ValidatedLastLocation](#chaindbimmutabledbeventvalidatedlastlocation)
	1. __InitChainSelEvent__
		1. [CandidateContainsFutureBlocks](#chaindbinitchainseleventcandidatecontainsfutureblocks)
		1. [CandidateContainsFutureBlocksExceedingClockSkew](#chaindbinitchainseleventcandidatecontainsfutureblocksexceedingclockskew)
		1. [InitalChainSelected](#chaindbinitchainseleventinitalchainselected)
		1. [InvalidBlock](#chaindbinitchainseleventinvalidblock)
		1. [StartedInitChainSelection](#chaindbinitchainseleventstartedinitchainselection)
		1. [UpdateLedgerDb](#chaindbinitchainseleventupdateledgerdb)
		1. [ValidCandidate](#chaindbinitchainseleventvalidcandidate)
	1. __IteratorEvent__
		1. [BlockGCedFromVolatileDB](#chaindbiteratoreventblockgcedfromvolatiledb)
		1. [BlockMissingFromVolatileDB](#chaindbiteratoreventblockmissingfromvolatiledb)
		1. [BlockWasCopiedToImmutableDB](#chaindbiteratoreventblockwascopiedtoimmutabledb)
		1. [StreamFromBoth](#chaindbiteratoreventstreamfromboth)
		1. [StreamFromImmutableDB](#chaindbiteratoreventstreamfromimmutabledb)
		1. [StreamFromVolatileDB](#chaindbiteratoreventstreamfromvolatiledb)
		1. [SwitchBackToVolatileDB](#chaindbiteratoreventswitchbacktovolatiledb)
		1. [UnknownRangeRequested](#chaindbiteratoreventunknownrangerequested)
	1. __LedgerEvent__
		1. [DeletedSnapshot](#chaindbledgereventdeletedsnapshot)
		1. [InvalidSnapshot](#chaindbledgereventinvalidsnapshot)
		1. [TookSnapshot](#chaindbledgereventtooksnapshot)
	1. __LedgerReplayEvent__
		1. [ReplayFromGenesis](#chaindbledgerreplayeventreplayfromgenesis)
		1. [ReplayFromSnapshot](#chaindbledgerreplayeventreplayfromsnapshot)
		1. [ReplayedBlock](#chaindbledgerreplayeventreplayedblock)
	1. __OpenEvent__
		1. [ClosedDB](#chaindbopeneventcloseddb)
		1. [OpenedDB](#chaindbopeneventopeneddb)
		1. [OpenedImmutableDB](#chaindbopeneventopenedimmutabledb)
		1. [OpenedLgrDB](#chaindbopeneventopenedlgrdb)
		1. [OpenedVolatileDB](#chaindbopeneventopenedvolatiledb)
		1. [StartedOpeningDB](#chaindbopeneventstartedopeningdb)
		1. [StartedOpeningImmutableDB](#chaindbopeneventstartedopeningimmutabledb)
		1. [StartedOpeningLgrDB](#chaindbopeneventstartedopeninglgrdb)
		1. [StartedOpeningVolatileDB](#chaindbopeneventstartedopeningvolatiledb)
	1. __VolatileDBEvent__
		1. [BlockAlreadyHere](#chaindbvolatiledbeventblockalreadyhere)
		1. [DBAlreadyClosed](#chaindbvolatiledbeventdbalreadyclosed)
		1. [InvalidFileNames](#chaindbvolatiledbeventinvalidfilenames)
		1. [Truncate](#chaindbvolatiledbeventtruncate)
1. __ChainSync__
	1. __NodeToClient__
		1. __Recieve__
			1. [AwaitReply](#chainsyncnodetoclientrecieveawaitreply)
			1. [Done](#chainsyncnodetoclientrecievedone)
			1. [FindIntersect](#chainsyncnodetoclientrecievefindintersect)
			1. [IntersectFound](#chainsyncnodetoclientrecieveintersectfound)
			1. [IntersectNotFound](#chainsyncnodetoclientrecieveintersectnotfound)
			1. [RequestNext](#chainsyncnodetoclientrecieverequestnext)
			1. [RollBackward](#chainsyncnodetoclientrecieverollbackward)
			1. [RollForward](#chainsyncnodetoclientrecieverollforward)
		1. __Send__
			1. [AwaitReply](#chainsyncnodetoclientsendawaitreply)
			1. [Done](#chainsyncnodetoclientsenddone)
			1. [FindIntersect](#chainsyncnodetoclientsendfindintersect)
			1. [IntersectFound](#chainsyncnodetoclientsendintersectfound)
			1. [IntersectNotFound](#chainsyncnodetoclientsendintersectnotfound)
			1. [RequestNext](#chainsyncnodetoclientsendrequestnext)
			1. [RollBackward](#chainsyncnodetoclientsendrollbackward)
			1. [RollForward](#chainsyncnodetoclientsendrollforward)
1. __ChainSyncClient__
	1. __ChainSyncClientEvent__
		1. [DownloadedHeader](#chainsyncclientchainsyncclienteventdownloadedheader)
		1. [Exception](#chainsyncclientchainsyncclienteventexception)
		1. [FoundIntersection](#chainsyncclientchainsyncclienteventfoundintersection)
		1. [RolledBack](#chainsyncclientchainsyncclienteventrolledback)
		1. [Termination](#chainsyncclientchainsyncclienteventtermination)
1. __ChainSyncNode__
	1. __NodeToNode__
		1. __Recieve__
			1. [AwaitReply](#chainsyncnodenodetonoderecieveawaitreply)
			1. [Done](#chainsyncnodenodetonoderecievedone)
			1. [FindIntersect](#chainsyncnodenodetonoderecievefindintersect)
			1. [IntersectFound](#chainsyncnodenodetonoderecieveintersectfound)
			1. [IntersectNotFound](#chainsyncnodenodetonoderecieveintersectnotfound)
			1. [RequestNext](#chainsyncnodenodetonoderecieverequestnext)
			1. [RollBackward](#chainsyncnodenodetonoderecieverollbackward)
			1. [RollForward](#chainsyncnodenodetonoderecieverollforward)
		1. __Send__
			1. [AwaitReply](#chainsyncnodenodetonodesendawaitreply)
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
			1. [AwaitReply](#chainsyncserialisednodetonoderecieveawaitreply)
			1. [Done](#chainsyncserialisednodetonoderecievedone)
			1. [FindIntersect](#chainsyncserialisednodetonoderecievefindintersect)
			1. [IntersectFound](#chainsyncserialisednodetonoderecieveintersectfound)
			1. [IntersectNotFound](#chainsyncserialisednodetonoderecieveintersectnotfound)
			1. [RequestNext](#chainsyncserialisednodetonoderecieverequestnext)
			1. [RollBackward](#chainsyncserialisednodetonoderecieverollbackward)
			1. [RollForward](#chainsyncserialisednodetonoderecieverollforward)
		1. __Send__
			1. [AwaitReply](#chainsyncserialisednodetonodesendawaitreply)
			1. [Done](#chainsyncserialisednodetonodesenddone)
			1. [FindIntersect](#chainsyncserialisednodetonodesendfindintersect)
			1. [IntersectFound](#chainsyncserialisednodetonodesendintersectfound)
			1. [IntersectNotFound](#chainsyncserialisednodetonodesendintersectnotfound)
			1. [RequestNext](#chainsyncserialisednodetonodesendrequestnext)
			1. [RollBackward](#chainsyncserialisednodetonodesendrollbackward)
			1. [RollForward](#chainsyncserialisednodetonodesendrollforward)
1. __ChainSyncServerBlock__
	1. __ChainSyncServerEvent__
		1. __Update__
			1. [Update](#chainsyncserverblockchainsyncservereventupdateupdate)
1. __ChainSyncServerHeader__
	1. __ChainSyncServerEvent__
		1. __Update__
			1. [Update](#chainsyncserverheaderchainsyncservereventupdateupdate)
1. __ConnectionManager__
	1. [Connect](#connectionmanagerconnect)
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
	1. [ConnectionManagerTransition](#connectionmanagertransitionconnectionmanagertransition)
1. __DNSResolver__
	1. [LookupAAAAError](#dnsresolverlookupaaaaerror)
	1. [LookupAAAAResult](#dnsresolverlookupaaaaresult)
	1. [LookupAError](#dnsresolverlookupaerror)
	1. [LookupAResult](#dnsresolverlookuparesult)
	1. [LookupException](#dnsresolverlookupexception)
	1. [LookupIPv4First](#dnsresolverlookupipv4first)
	1. [LookupIPv6First](#dnsresolverlookupipv6first)
1. __DNSSubscription__
	1. __DNS__
		1. [AllocateSocket](#dnssubscriptiondnsallocatesocket)
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
		1. [GovernorState](#debugpeerselectiondebugpeerselectiongovernorstate)
1. __DebugPeerSelectionResponder__
	1. __DebugPeerSelection__
		1. [GovernorState](#debugpeerselectionresponderdebugpeerselectiongovernorstate)
1. __DiffusionInit__
	1. [ConfiguringLocalSocket](#diffusioninitconfiguringlocalsocket)
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
	1. [AcceptException](#errorpolicyacceptexception)
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
	1. [AdoptedBlock](#forgeadoptedblock)
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
1. [ForgeStats](#forgestats)
1. __Handshake__
	1. __Receive__
		1. [AcceptVersion](#handshakereceiveacceptversion)
		1. [ProposeVersions](#handshakereceiveproposeversions)
		1. [Refuse](#handshakereceiverefuse)
		1. [ReplyVersions](#handshakereceivereplyversions)
	1. __Send__
		1. [AcceptVersion](#handshakesendacceptversion)
		1. [ProposeVersions](#handshakesendproposeversions)
		1. [Refuse](#handshakesendrefuse)
		1. [ReplyVersions](#handshakesendreplyversions)
1. __InboundGovernor__
	1. [DemotedToColdRemote](#inboundgovernordemotedtocoldremote)
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
	1. [InboundGovernorTransition](#inboundgovernortransitioninboundgovernortransition)
1. __IpSubscription__
	1. __IP__
		1. [AllocateSocket](#ipsubscriptionipallocatesocket)
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
	1. [DisabledLedgerPeers](#ledgerpeersdisabledledgerpeers)
	1. [FallingBackToBootstrapPeers](#ledgerpeersfallingbacktobootstrappeers)
	1. [FetchingNewLedgerState](#ledgerpeersfetchingnewledgerstate)
	1. [PickedPeer](#ledgerpeerspickedpeer)
	1. [PickedPeers](#ledgerpeerspickedpeers)
	1. [RequestForPeers](#ledgerpeersrequestforpeers)
	1. [ReusingLedgerState](#ledgerpeersreusingledgerstate)
	1. [TraceUseLedgerAfter](#ledgerpeerstraceuseledgerafter)
	1. [WaitingOnRequest](#ledgerpeerswaitingonrequest)
1. __LocalConnectionManager__
	1. [Connect](#localconnectionmanagerconnect)
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
	1. [AcceptException](#localerrorpolicyacceptexception)
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
		1. [AcceptVersion](#localhandshakereceiveacceptversion)
		1. [ProposeVersions](#localhandshakereceiveproposeversions)
		1. [Refuse](#localhandshakereceiverefuse)
		1. [ReplyVersions](#localhandshakereceivereplyversions)
	1. __Send__
		1. [AcceptVersion](#localhandshakesendacceptversion)
		1. [ProposeVersions](#localhandshakesendproposeversions)
		1. [Refuse](#localhandshakesendrefuse)
		1. [ReplyVersions](#localhandshakesendreplyversions)
1. __LocalInboundGovernor__
	1. [DemotedToColdRemote](#localinboundgovernordemotedtocoldremote)
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
	1. [LocalRootDomains](#localrootpeerslocalrootdomains)
	1. [LocalRootError](#localrootpeerslocalrooterror)
	1. [LocalRootFailure](#localrootpeerslocalrootfailure)
	1. [LocalRootGroups](#localrootpeerslocalrootgroups)
	1. [LocalRootResult](#localrootpeerslocalrootresult)
	1. [LocalRootWaiting](#localrootpeerslocalrootwaiting)
1. __LocalServer__
	1. [AcceptConnection](#localserveracceptconnection)
	1. [AcceptError](#localserveraccepterror)
	1. [AcceptPolicy](#localserveracceptpolicy)
	1. [Error](#localservererror)
	1. [Started](#localserverstarted)
	1. [Stopped](#localserverstopped)
1. __LocalTxSubmissionServer__
	1. [ReceivedTx](#localtxsubmissionserverreceivedtx)
1. __Mempool__
	1. [AddedTx](#mempooladdedtx)
	1. [ManuallyRemovedTxs](#mempoolmanuallyremovedtxs)
	1. [RejectedTx](#mempoolrejectedtx)
	1. [RemoveTxs](#mempoolremovetxs)
1. __Mux__
	1. [ChannelRecvEnd](#muxchannelrecvend)
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
	1. [ChannelRecvEnd](#muxlocalchannelrecvend)
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
	1. [ChurnMode](#peerselectionchurnmode)
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
	1. [MonitoringError](#peerselectionactionsmonitoringerror)
	1. [MonitoringResult](#peerselectionactionsmonitoringresult)
	1. [StatusChangeFailure](#peerselectionactionsstatuschangefailure)
	1. [StatusChanged](#peerselectionactionsstatuschanged)
1. __PeerSelectionCounters__
	1. [PeerSelectionCounters](#peerselectioncounterspeerselectioncounters)
1. [Peers](#peers)
1. __PublicRootPeers__
	1. __PublicRootPeers__
		1. [PublicRootDomains](#publicrootpeerspublicrootpeerspublicrootdomains)
		1. [PublicRootFailure](#publicrootpeerspublicrootpeerspublicrootfailure)
		1. [PublicRootRelayAccessPoint](#publicrootpeerspublicrootpeerspublicrootrelayaccesspoint)
		1. [PublicRootResult](#publicrootpeerspublicrootpeerspublicrootresult)
1. __ReplayBlock__
	1. [LedgerReplay](#replayblockledgerreplay)
1. [Resources](#resources)
1. __Server__
	1. [AcceptConnection](#serveracceptconnection)
	1. [AcceptError](#serveraccepterror)
	1. [AcceptPolicy](#serveracceptpolicy)
	1. [Error](#servererror)
	1. [Started](#serverstarted)
	1. [Stopped](#serverstopped)
1. __Shutdown__
	1. [AbnormalShutdown](#shutdownabnormalshutdown)
	1. [RequestingShutdown](#shutdownrequestingshutdown)
	1. [ShutdownArmedAt](#shutdownshutdownarmedat)
	1. [ShutdownRequested](#shutdownshutdownrequested)
	1. [ShutdownUnexpectedInput](#shutdownshutdownunexpectedinput)
1. __Startup__
	1. [Byron](#startupbyron)
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
		1. [Acquire](#statequeryclientrecieveacquire)
		1. [Acquired](#statequeryclientrecieveacquired)
		1. [Done](#statequeryclientrecievedone)
		1. [Failure](#statequeryclientrecievefailure)
		1. [Query](#statequeryclientrecievequery)
		1. [ReAcquire](#statequeryclientrecievereacquire)
		1. [Release](#statequeryclientrecieverelease)
		1. [Result](#statequeryclientrecieveresult)
	1. __Send__
		1. [Acquire](#statequeryclientsendacquire)
		1. [Acquired](#statequeryclientsendacquired)
		1. [Done](#statequeryclientsenddone)
		1. [Failure](#statequeryclientsendfailure)
		1. [Query](#statequeryclientsendquery)
		1. [ReAcquire](#statequeryclientsendreacquire)
		1. [Release](#statequeryclientsendrelease)
		1. [Result](#statequeryclientsendresult)
1. __TxInbound__
	1. [TxInboundCanRequestMoreTxs](#txinboundtxinboundcanrequestmoretxs)
	1. [TxInboundCannotRequestMoreTxs](#txinboundtxinboundcannotrequestmoretxs)
	1. [TxInboundTerminated](#txinboundtxinboundterminated)
	1. [TxSubmissionCollected](#txinboundtxsubmissioncollected)
	1. [TxSubmissionProcessed](#txinboundtxsubmissionprocessed)
1. __TxMonitorClient__
	1. __Recieve__
		1. [Acquire](#txmonitorclientrecieveacquire)
		1. [Acquired](#txmonitorclientrecieveacquired)
		1. [Done](#txmonitorclientrecievedone)
		1. [Failure](#txmonitorclientrecievefailure)
		1. [Query](#txmonitorclientrecievequery)
		1. [ReAcquire](#txmonitorclientrecievereacquire)
		1. [Release](#txmonitorclientrecieverelease)
		1. [Result](#txmonitorclientrecieveresult)
	1. __Send__
		1. [Acquire](#txmonitorclientsendacquire)
		1. [Acquired](#txmonitorclientsendacquired)
		1. [Done](#txmonitorclientsenddone)
		1. [Failure](#txmonitorclientsendfailure)
		1. [Query](#txmonitorclientsendquery)
		1. [ReAcquire](#txmonitorclientsendreacquire)
		1. [Release](#txmonitorclientsendrelease)
		1. [Result](#txmonitorclientsendresult)
1. __TxOutbound__
	1. [ControlMessage](#txoutboundcontrolmessage)
	1. [RecvMsgRequest](#txoutboundrecvmsgrequest)
	1. [SendMsgReply](#txoutboundsendmsgreply)
1. __TxSubmission2__
	1. __NodeToNode__
		1. __Recieve__
			1. [Done](#txsubmission2nodetonoderecievedone)
			1. [MsgHello](#txsubmission2nodetonoderecievemsghello)
			1. [ReplyTxIds](#txsubmission2nodetonoderecievereplytxids)
			1. [ReplyTxs](#txsubmission2nodetonoderecievereplytxs)
			1. [RequestTxIds](#txsubmission2nodetonoderecieverequesttxids)
			1. [RequestTxs](#txsubmission2nodetonoderecieverequesttxs)
		1. __Send__
			1. [Done](#txsubmission2nodetonodesenddone)
			1. [MsgHello](#txsubmission2nodetonodesendmsghello)
			1. [ReplyTxIds](#txsubmission2nodetonodesendreplytxids)
			1. [ReplyTxs](#txsubmission2nodetonodesendreplytxs)
			1. [RequestTxIds](#txsubmission2nodetonodesendrequesttxids)
			1. [RequestTxs](#txsubmission2nodetonodesendrequesttxs)
1. __TxSubmissionClient__
	1. __Recieve__
		1. [AcceptTx](#txsubmissionclientrecieveaccepttx)
		1. [Done](#txsubmissionclientrecievedone)
		1. [RejectTx](#txsubmissionclientrecieverejecttx)
		1. [SubmitTx](#txsubmissionclientrecievesubmittx)
	1. __Send__
		1. [AcceptTx](#txsubmissionclientsendaccepttx)
		1. [Done](#txsubmissionclientsenddone)
		1. [RejectTx](#txsubmissionclientsendrejecttx)
		1. [SubmitTx](#txsubmissionclientsendsubmittx)

## [Metrics](#metrics)
1. [Block replay progress (%)](#block replay progress (%))
1. [blocksForgedNum](#blocksforgednum)
1. __cardano__
	1. __node__
		1. [aboutToLeadSlotLast](#cardanonodeabouttoleadslotlast)
		1. [aboutToLeadSlotLast](#cardanonodeabouttoleadslotlast)
		1. [adoptedSlotLast](#cardanonodeadoptedslotlast)
		1. [blockContext](#cardanonodeblockcontext)
		1. [blockFromFuture](#cardanonodeblockfromfuture)
		1. [blocks](#cardanonodeblocks)
		1. [blocks](#cardanonodeblocks)
		1. [connectedPeers](#cardanonodeconnectedpeers)
		1. __connectionManager__
			1. [duplexConns](#cardanonodeconnectionmanagerduplexconns)
			1. [duplexConns](#cardanonodeconnectionmanagerduplexconns)
			1. [fullDuplexConns](#cardanonodeconnectionmanagerfullduplexconns)
			1. [fullDuplexConns](#cardanonodeconnectionmanagerfullduplexconns)
			1. [inboundConns](#cardanonodeconnectionmanagerinboundconns)
			1. [inboundConns](#cardanonodeconnectionmanagerinboundconns)
			1. [outboundConns](#cardanonodeconnectionmanageroutboundconns)
			1. [outboundConns](#cardanonodeconnectionmanageroutboundconns)
			1. [unidirectionalConns](#cardanonodeconnectionmanagerunidirectionalconns)
			1. [unidirectionalConns](#cardanonodeconnectionmanagerunidirectionalconns)
		1. [couldNotForgeSlotLast](#cardanonodecouldnotforgeslotlast)
		1. [couldNotForgeSlotLast](#cardanonodecouldnotforgeslotlast)
		1. [currentKESPeriod](#cardanonodecurrentkesperiod)
		1. [delegMapSize](#cardanonodedelegmapsize)
		1. [density](#cardanonodedensity)
		1. [density](#cardanonodedensity)
		1. [epoch](#cardanonodeepoch)
		1. [epoch](#cardanonodeepoch)
		1. [forgedInvalidSlotLast](#cardanonodeforgedinvalidslotlast)
		1. [forgedSlotLast](#cardanonodeforgedslotlast)
		1. __inbound-governor__
			1. [cold](#cardanonodeinbound-governorcold)
			1. [cold](#cardanonodeinbound-governorcold)
			1. [hot](#cardanonodeinbound-governorhot)
			1. [hot](#cardanonodeinbound-governorhot)
			1. [idle](#cardanonodeinbound-governoridle)
			1. [idle](#cardanonodeinbound-governoridle)
			1. [warm](#cardanonodeinbound-governorwarm)
			1. [warm](#cardanonodeinbound-governorwarm)
		1. [ledgerState](#cardanonodeledgerstate)
		1. [ledgerView](#cardanonodeledgerview)
		1. [mempoolBytes](#cardanonodemempoolbytes)
		1. [mempoolBytes](#cardanonodemempoolbytes)
		1. [mempoolBytes](#cardanonodemempoolbytes)
		1. [mempoolBytes](#cardanonodemempoolbytes)
		1. __metrics__
			1. __served__
				1. [header](#cardanonodemetricsservedheader)
		1. [nodeCannotForge](#cardanonodenodecannotforge)
		1. [nodeIsLeader](#cardanonodenodeisleader)
		1. [nodeNotLeader](#cardanonodenodenotleader)
		1. [notAdoptedSlotLast](#cardanonodenotadoptedslotlast)
		1. [operationalCertificateExpiryKESPeriod](#cardanonodeoperationalcertificateexpirykesperiod)
		1. [operationalCertificateStartKESPeriod](#cardanonodeoperationalcertificatestartkesperiod)
		1. __peerSelection__
			1. [cold](#cardanonodepeerselectioncold)
			1. [hot](#cardanonodepeerselectionhot)
			1. [warm](#cardanonodepeerselectionwarm)
		1. [remainingKESPeriods](#cardanonoderemainingkesperiods)
		1. __served__
			1. [block](#cardanonodeservedblock)
		1. [slotInEpoch](#cardanonodeslotinepoch)
		1. [slotInEpoch](#cardanonodeslotinepoch)
		1. [slotIsImmutable](#cardanonodeslotisimmutable)
		1. [slots](#cardanonodeslots)
		1. [slots](#cardanonodeslots)
		1. __submissions__
			1. [accepted](#cardanonodesubmissionsaccepted)
			1. [rejected](#cardanonodesubmissionsrejected)
			1. [submitted](#cardanonodesubmissionssubmitted)
		1. [txsInMempool](#cardanonodetxsinmempool)
		1. [txsInMempool](#cardanonodetxsinmempool)
		1. [txsInMempool](#cardanonodetxsinmempool)
		1. [txsInMempool](#cardanonodetxsinmempool)
		1. [txsProcessedNum](#cardanonodetxsprocessednum)
		1. [utxoSize](#cardanonodeutxosize)
1. __mem__
	1. [resident](#memresident)
1. [nodeCannotForgeNum](#nodecannotforgenum)
1. [nodeIsLeaderNum](#nodeisleadernum)
1. [peersFromNodeKernel](#peersfromnodekernel)
1. __rts__
	1. [gcLiveBytes](#rtsgclivebytes)
	1. [gcMajorNum](#rtsgcmajornum)
	1. [gcMinorNum](#rtsgcminornum)
	1. [gcticks](#rtsgcticks)
	1. [mutticks](#rtsmutticks)
	1. [threads](#rtsthreads)
1. [slotsMissed](#slotsmissed)
1. __stat__
	1. [cputicks](#statcputicks)

## [Datapoints](#datapoints)
1. [NodeInfo](#nodeinfo)
1. [NodePeers](#nodepeers)
1. [NodeState](#nodestate)

## Trace Messages
### AcceptPolicy.ConnectionHardLimit


***
Hard rate limit reached, waiting until the number of connections drops below n.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### AcceptPolicy.ConnectionLimitResume


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### AcceptPolicy.ConnectionRateLimiting


***
Rate limiting accepting connections, delaying next accept for given time, currently serving n connections.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### BlockFetch.NodeToNode.Recieve.BatchDone


***
End of block streaming.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetch.NodeToNode.Recieve.Block


***
Stream a single block.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetch.NodeToNode.Recieve.ClientDone


***
Client termination message.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetch.NodeToNode.Recieve.NoBlocks


***
Respond that there are no blocks.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetch.NodeToNode.Recieve.RequestRange


***
Request range of blocks.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetch.NodeToNode.Recieve.StartBatch


***
Start block streaming.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetch.NodeToNode.Send.BatchDone


***
End of block streaming.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetch.NodeToNode.Send.Block


***
Stream a single block.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetch.NodeToNode.Send.ClientDone


***
Client termination message.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetch.NodeToNode.Send.NoBlocks


***
Respond that there are no blocks.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetch.NodeToNode.Send.RequestRange


***
Request range of blocks.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetch.NodeToNode.Send.StartBatch


***
Start block streaming.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetchClient.AcknowledgedFetchRequest


***
Mark the point when the fetch client picks up the request added by the block fetch decision thread. Note that this event can happen fewer times than the 'AddedFetchRequest' due to fetch request merging.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetchClient.AddedFetchRequest


***
The block fetch decision thread has added a new fetch instruction consisting of one or more individual request ranges.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetchClient.ClientTerminating


***
The client is terminating.  Log the number of outstanding requests.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetchClient.CompletedBlockFetch


***
Mark the successful end of receiving a streaming batch of blocks.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`
Limiters: Limiter `BlockFetchClient.CompletedBlockFetch` with frequency `2.0`

### BlockFetchClient.CompletedFetchBatch


***
Mark the successful end of receiving a streaming batch of blocks
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetchClient.RejectedFetchBatch


***
If the other peer rejects our request then we have this event instead of 'StartedFetchBatch' and 'CompletedFetchBatch'.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetchClient.SendFetchRequest


***
Mark the point when fetch request for a fragment is actually sent over the wire.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetchClient.StartedFetchBatch


***
Mark the start of receiving a streaming batch of blocks. This will be followed by one or more 'CompletedBlockFetch' and a final 'CompletedFetchBatch'
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetchDecision


***
Throughout the decision making process we accumulate reasons to decline to fetch any blocks. This message carries the intermediate and final results.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetchSerialised.NodeToNode.Recieve.BatchDone


***
End of block streaming.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetchSerialised.NodeToNode.Recieve.Block


***
Stream a single block.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetchSerialised.NodeToNode.Recieve.ClientDone


***
Client termination message.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetchSerialised.NodeToNode.Recieve.NoBlocks


***
Respond that there are no blocks.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetchSerialised.NodeToNode.Recieve.RequestRange


***
Request range of blocks.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetchSerialised.NodeToNode.Recieve.StartBatch


***
Start block streaming.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetchSerialised.NodeToNode.Send.BatchDone


***
End of block streaming.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetchSerialised.NodeToNode.Send.Block


***
Stream a single block.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetchSerialised.NodeToNode.Send.ClientDone


***
Client termination message.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetchSerialised.NodeToNode.Send.NoBlocks


***
Respond that there are no blocks.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetchSerialised.NodeToNode.Send.RequestRange


***
Request range of blocks.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetchSerialised.NodeToNode.Send.StartBatch


***
Start block streaming.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetchServer.SendBlock


***
The server sent a block to the peer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockchainTime.CurrentSlotUnknown


***
Current slot is not yet known
 This happens when the tip of our current chain is so far in the past that we cannot translate the current wallclock to a slot number, typically during syncing. Until the current slot number is known, we cannot produce blocks. Seeing this message during syncing therefore is normal and to be expected.
 We record the current time (the time we tried to translate to a 'SlotNo') as well as the 'PastHorizonException', which provides detail on the bounds between which we /can/ do conversions. The distance between the current time and the upper bound should rapidly decrease with consecutive 'CurrentSlotUnknown' messages during syncing.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockchainTime.StartTimeInTheFuture


***
The start time of the blockchain time is in the future
 We have to block (for 'NominalDiffTime') until that time comes.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockchainTime.SystemClockMovedBack


***
The system clock moved back an acceptable time span, e.g., because of an NTP sync.
 The system clock moved back such that the new current slot would be smaller than the previous one. If this is within the configured limit, we trace this warning but *do not change the current slot*. The current slot never decreases, but the current slot may stay the same longer than expected.
 When the system clock moved back more than the configured limit, we shut down with a fatal exception.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainDB.AddBlockEvent.AddBlockValidation.CandidateContainsFutureBlocks


***
An event traced during validating performed while adding a block. Candidate contains headers from the future which do no exceed the clock skew.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.AddBlockEvent.AddBlockValidation.CandidateContainsFutureBlocksExceedingClockSkew


***
An event traced during validating performed while adding a block. Candidate contains headers from the future which exceed the clock skew.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.AddBlockEvent.AddBlockValidation.InvalidBlock


***
An event traced during validating performed while adding a block. A point was found to be invalid.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.AddBlockEvent.AddBlockValidation.UpdateLedgerDb


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.AddBlockEvent.AddBlockValidation.ValidCandidate


***
An event traced during validating performed while adding a block. A candidate chain was valid.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`
Limiters: Limiter `ChainDB.AddBlockEvent.AddBlockValidation.ValidCandidate` with frequency `2.0`

### ChainDB.AddBlockEvent.AddedBlockToQueue


***
The block was added to the queue and will be added to the ChainDB by the background thread. The size of the queue is included..
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`
Limiters: Limiter `ChainDB.AddBlockEvent.AddedBlockToQueue` with frequency `2.0`

### ChainDB.AddBlockEvent.AddedBlockToVolatileDB


***
A block was added to the Volatile DB
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`
Limiters: Limiter `ChainDB.AddBlockEvent.AddedBlockToVolatileDB` with frequency `2.0`

### ChainDB.AddBlockEvent.AddedToCurrentChain


***
The new block fits onto the current chain (first fragment) and we have successfully used it to extend our (new) current chain (second fragment).
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.AddBlockEvent.BlockInTheFuture


***
The block is from the future, i.e., its slot number is greater than the current slot (the second argument).
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.AddBlockEvent.ChainSelectionForFutureBlock


***
Run chain selection for a block that was previously from the future. This is done for all blocks from the future each time a new block is added.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.AddBlockEvent.IgnoreBlockAlreadyInVolatileDB


***
A block that is already in the Volatile DB was ignored.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.AddBlockEvent.IgnoreBlockOlderThanK


***
A block with a 'BlockNo' more than @k@ back than the current tip was ignored.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.AddBlockEvent.IgnoreInvalidBlock


***
A block that is already in the Volatile DB was ignored.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.AddBlockEvent.PipeliningEvent.OutdatedTentativeHeader


***
An event traced during block selection when the tentative header got cleared on chain selection.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.AddBlockEvent.PipeliningEvent.SetTentativeHeader


***
An event traced during block selection when the tentative header (in the context of diffusion pipelining) is set.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.AddBlockEvent.PipeliningEvent.TrapTentativeHeader


***
An event traced during block selection when the body of the tentative header turned out to be invalid.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.AddBlockEvent.PoppedBlockFromQueue


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.AddBlockEvent.StoreButDontChange


***
The block fits onto some fork, we'll try to switch to that fork (if it is preferable to our chain).
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.AddBlockEvent.SwitchedToAFork


***
The new block fits onto some fork and we have switched to that fork (second fragment), as it is preferable to our (previous) current chain (first fragment).
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.AddBlockEvent.TryAddToCurrentChain


***
The block fits onto the current chain, we'll try to use it to extend our chain.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.AddBlockEvent.TrySwitchToAFork


***
The block fits onto some fork, we'll try to switch to that fork (if it is preferable to our chain)
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.CopyToImmutableDBEvent.CopiedBlockToImmutableDB


***
A block was successfully copied to the ImmDB.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`
Limiters: Limiter `ChainDB.CopyToImmutableDBEvent.CopiedBlockToImmutableDB` with frequency `2.0`

### ChainDB.CopyToImmutableDBEvent.NoBlocksToCopyToImmutableDB


***
There are no block to copy to the ImmDB.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.FollowerEvent.FollowerNewImmIterator


***
The follower is in the 'FollowerInImmutableDB' state but the iterator is exhausted while the ImmDB has grown, so we open a new iterator to stream these blocks too.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.FollowerEvent.FollowerNoLongerInMem


***
The follower was in the 'FollowerInImmutableDB' state and is switched to the 'FollowerInMem' state.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.FollowerEvent.FollowerSwitchToMem


***
The follower was in the 'FollowerInImmutableDB' state and is switched to the 'FollowerInMem' state.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.FollowerEvent.NewFollower


***
A new follower was created.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.GCEvent.PerformedGC


***
There are no block to copy to the ImmDB.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.GCEvent.ScheduledGC


***
There are no block to copy to the ImmDB.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.ImmutableDBEvent.CacheEvent.CurrentChunkHit


***
Current chunk found in the cache.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.ImmutableDBEvent.CacheEvent.PastChunkEvict


***
The least recently used past chunk was evicted because the cache was full.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.ImmutableDBEvent.CacheEvent.PastChunkExpired


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.ImmutableDBEvent.CacheEvent.PastChunkHit


***
Past chunk found in the cache
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.ImmutableDBEvent.CacheEvent.PastChunkMiss


***
Past chunk was not found in the cache
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.ImmutableDBEvent.ChunkFileDoesntFit


***
The hash of the last block in the previous epoch doesn't match the previous hash of the first block in the current epoch
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.ImmutableDBEvent.ChunkValidation.InvalidChunkFile


***
Chunk file is invalid
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.ImmutableDBEvent.ChunkValidation.InvalidPrimaryIndex


***
The primary index is invalid.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.ImmutableDBEvent.ChunkValidation.InvalidSecondaryIndex


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.ImmutableDBEvent.ChunkValidation.MissingChunkFile


***
Chunk file is missing
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.ImmutableDBEvent.ChunkValidation.MissingPrimaryIndex


***
The primary index is missing.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.ImmutableDBEvent.ChunkValidation.MissingSecondaryIndex


***
The secondary index is missing.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.ImmutableDBEvent.ChunkValidation.RewritePrimaryIndex


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.ImmutableDBEvent.ChunkValidation.RewriteSecondaryIndex


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.ImmutableDBEvent.ChunkValidation.StartedValidatingChunk


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.ImmutableDBEvent.ChunkValidation.ValidatedChunk


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.ImmutableDBEvent.DBAlreadyClosed


***
The immutable DB is already closed
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.ImmutableDBEvent.DBClosed


***
Closing the immutable DB
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.ImmutableDBEvent.DeletingAfter


***
Delete after
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.ImmutableDBEvent.Migrating


***
Performing a migration of the on-disk files.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.ImmutableDBEvent.NoValidLastLocation


***
No valid last location was found
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.ImmutableDBEvent.ValidatedLastLocation


***
The last location was validatet
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.InitChainSelEvent.CandidateContainsFutureBlocks


***
Candidate contains headers from the future which do not exceed the clock skew.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.InitChainSelEvent.CandidateContainsFutureBlocksExceedingClockSkew


***
Candidate contains headers from the future which exceed the clock skew, making them invalid.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.InitChainSelEvent.InitalChainSelected


***
InitalChainSelected
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.InitChainSelEvent.InvalidBlock


***
A point was found to be invalid.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.InitChainSelEvent.StartedInitChainSelection


***
StartedInitChainSelection
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.InitChainSelEvent.UpdateLedgerDb


***
UpdateLedgerDb
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.InitChainSelEvent.ValidCandidate


***
A candidate chain was valid.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.IteratorEvent.BlockGCedFromVolatileDB


***
A block is no longer in the VolatileDB and isn't in the ImmDB either; it wasn't part of the current chain.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.IteratorEvent.BlockMissingFromVolatileDB


***
A block is no longer in the VolatileDB because it has been garbage collected. It might now be in the ImmDB if it was part of the current chain.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.IteratorEvent.BlockWasCopiedToImmutableDB


***
A block that has been garbage collected from the VolatileDB is now found and streamed from the ImmDB.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.IteratorEvent.StreamFromBoth


***
Stream from both the VolatileDB and the ImmDB.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.IteratorEvent.StreamFromImmutableDB


***
Stream only from the ImmDB.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.IteratorEvent.StreamFromVolatileDB


***
Stream only from the VolatileDB.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.IteratorEvent.SwitchBackToVolatileDB


***
We have streamed one or more blocks from the ImmDB that were part of the VolatileDB when initialising the iterator. Now, we have to look back in the VolatileDB again because the ImmDB doesn't have the next block we're looking for.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.IteratorEvent.UnknownRangeRequested


***
An unknown range was requested, see 'UnknownRange'.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.LedgerEvent.DeletedSnapshot


***
An old or invalid on-disk snapshot was deleted.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.LedgerEvent.InvalidSnapshot


***
An on disk snapshot was skipped because it was invalid.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.LedgerEvent.TookSnapshot


***
A snapshot was written to disk.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.LedgerReplayEvent.ReplayFromGenesis


***
There were no LedgerDB snapshots on disk, so we're replaying all blocks starting from Genesis against the initial ledger. The @replayTo@ parameter corresponds to the block at the tip of the ImmDB, i.e., the last block to replay.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.LedgerReplayEvent.ReplayFromSnapshot


***
There was a LedgerDB snapshot on disk corresponding to the given tip. We're replaying more recent blocks against it. The @replayTo@ parameter corresponds to the block at the tip of the ImmDB, i.e., the last block to replay.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.LedgerReplayEvent.ReplayedBlock


***
We replayed the given block (reference) on the genesis snapshot during the initialisation of the LedgerDB.
 The @blockInfo@ parameter corresponds replayed block and the @replayTo@ parameter corresponds to the block at the tip of the ImmDB, i.e., the last block to replay.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.OpenEvent.ClosedDB


***
The ChainDB was closed.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.OpenEvent.OpenedDB


***
The ChainDB was opened.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.OpenEvent.OpenedImmutableDB


***
The ImmDB was opened.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.OpenEvent.OpenedLgrDB


***
The LedgerDB was opened.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.OpenEvent.OpenedVolatileDB


***
The VolatileDB was opened.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.OpenEvent.StartedOpeningDB


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.OpenEvent.StartedOpeningImmutableDB


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.OpenEvent.StartedOpeningLgrDB


***
The LedgerDB was opened.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.OpenEvent.StartedOpeningVolatileDB


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.VolatileDBEvent.BlockAlreadyHere


***
A block was found to be already in the DB.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.VolatileDBEvent.DBAlreadyClosed


***
When closing the DB it was found itis closed already.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.VolatileDBEvent.InvalidFileNames


***
Reports a list of invalid file paths.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.VolatileDBEvent.Truncate


***
Truncates a file up to offset because of the error.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainSync.NodeToClient.Recieve.AwaitReply


***
Acknowledge the request but require the consumer to wait for the nextupdate. This means that the consumer is synced with the producer, andthe producer is waiting for its own chain state to change.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.NodeToClient.Recieve.Done


***
We have to explain to the framework what our states mean, in terms ofwhich party has agency in each state.
Idle states are where it is for the client to send a message,busy states are where the server is expected to send a reply.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.NodeToClient.Recieve.FindIntersect


***
Ask the producer to try to find an improved intersection point betweenthe consumer and producer's chains. The consumer sends a sequence ofpoints and it is up to the producer to find the first intersection pointon its chain and send it back to the consumer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.NodeToClient.Recieve.IntersectFound


***
The reply to the consumer about an intersection found.The consumer can decide weather to send more points.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.NodeToClient.Recieve.IntersectNotFound


***
The reply to the consumer that no intersection was found: none of thepoints the consumer supplied are on the producer chain.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.NodeToClient.Recieve.RequestNext


***
Request the next update from the producer. The response can be a rollforward, a roll back or wait.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.NodeToClient.Recieve.RollBackward


***
Tell the consumer to roll back to a given point on their chain.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.NodeToClient.Recieve.RollForward


***
Tell the consumer to extend their chain with the given header.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.NodeToClient.Send.AwaitReply


***
Acknowledge the request but require the consumer to wait for the nextupdate. This means that the consumer is synced with the producer, andthe producer is waiting for its own chain state to change.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.NodeToClient.Send.Done


***
We have to explain to the framework what our states mean, in terms ofwhich party has agency in each state.
Idle states are where it is for the client to send a message,busy states are where the server is expected to send a reply.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.NodeToClient.Send.FindIntersect


***
Ask the producer to try to find an improved intersection point betweenthe consumer and producer's chains. The consumer sends a sequence ofpoints and it is up to the producer to find the first intersection pointon its chain and send it back to the consumer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.NodeToClient.Send.IntersectFound


***
The reply to the consumer about an intersection found.The consumer can decide weather to send more points.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.NodeToClient.Send.IntersectNotFound


***
The reply to the consumer that no intersection was found: none of thepoints the consumer supplied are on the producer chain.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.NodeToClient.Send.RequestNext


***
Request the next update from the producer. The response can be a rollforward, a roll back or wait.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.NodeToClient.Send.RollBackward


***
Tell the consumer to roll back to a given point on their chain.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.NodeToClient.Send.RollForward


***
Tell the consumer to extend their chain with the given header.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSyncClient.ChainSyncClientEvent.DownloadedHeader


***
While following a candidate chain, we rolled forward by downloading a header.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSyncClient.ChainSyncClientEvent.Exception


***
An exception was thrown by the Chain Sync Client.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSyncClient.ChainSyncClientEvent.FoundIntersection


***
We found an intersection between our chain fragment and the candidate's chain.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSyncClient.ChainSyncClientEvent.RolledBack


***
While following a candidate chain, we rolled back to the given point.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSyncClient.ChainSyncClientEvent.Termination


***
The client has terminated.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSyncNode.NodeToNode.Recieve.AwaitReply


***
Acknowledge the request but require the consumer to wait for the nextupdate. This means that the consumer is synced with the producer, andthe producer is waiting for its own chain state to change.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSyncNode.NodeToNode.Recieve.Done


***
We have to explain to the framework what our states mean, in terms ofwhich party has agency in each state.
Idle states are where it is for the client to send a message,busy states are where the server is expected to send a reply.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSyncNode.NodeToNode.Recieve.FindIntersect


***
Ask the producer to try to find an improved intersection point betweenthe consumer and producer's chains. The consumer sends a sequence ofpoints and it is up to the producer to find the first intersection pointon its chain and send it back to the consumer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSyncNode.NodeToNode.Recieve.IntersectFound


***
The reply to the consumer about an intersection found.The consumer can decide weather to send more points.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSyncNode.NodeToNode.Recieve.IntersectNotFound


***
The reply to the consumer that no intersection was found: none of thepoints the consumer supplied are on the producer chain.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSyncNode.NodeToNode.Recieve.RequestNext


***
Request the next update from the producer. The response can be a rollforward, a roll back or wait.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSyncNode.NodeToNode.Recieve.RollBackward


***
Tell the consumer to roll back to a given point on their chain.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSyncNode.NodeToNode.Recieve.RollForward


***
Tell the consumer to extend their chain with the given header.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSyncNode.NodeToNode.Send.AwaitReply


***
Acknowledge the request but require the consumer to wait for the nextupdate. This means that the consumer is synced with the producer, andthe producer is waiting for its own chain state to change.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSyncNode.NodeToNode.Send.Done


***
We have to explain to the framework what our states mean, in terms ofwhich party has agency in each state.
Idle states are where it is for the client to send a message,busy states are where the server is expected to send a reply.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSyncNode.NodeToNode.Send.FindIntersect


***
Ask the producer to try to find an improved intersection point betweenthe consumer and producer's chains. The consumer sends a sequence ofpoints and it is up to the producer to find the first intersection pointon its chain and send it back to the consumer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSyncNode.NodeToNode.Send.IntersectFound


***
The reply to the consumer about an intersection found.The consumer can decide weather to send more points.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSyncNode.NodeToNode.Send.IntersectNotFound


***
The reply to the consumer that no intersection was found: none of thepoints the consumer supplied are on the producer chain.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSyncNode.NodeToNode.Send.RequestNext


***
Request the next update from the producer. The response can be a rollforward, a roll back or wait.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSyncNode.NodeToNode.Send.RollBackward


***
Tell the consumer to roll back to a given point on their chain.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSyncNode.NodeToNode.Send.RollForward


***
Tell the consumer to extend their chain with the given header.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSyncSerialised.NodeToNode.Recieve.AwaitReply


***
Acknowledge the request but require the consumer to wait for the nextupdate. This means that the consumer is synced with the producer, andthe producer is waiting for its own chain state to change.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSyncSerialised.NodeToNode.Recieve.Done


***
We have to explain to the framework what our states mean, in terms ofwhich party has agency in each state.
Idle states are where it is for the client to send a message,busy states are where the server is expected to send a reply.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSyncSerialised.NodeToNode.Recieve.FindIntersect


***
Ask the producer to try to find an improved intersection point betweenthe consumer and producer's chains. The consumer sends a sequence ofpoints and it is up to the producer to find the first intersection pointon its chain and send it back to the consumer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSyncSerialised.NodeToNode.Recieve.IntersectFound


***
The reply to the consumer about an intersection found.The consumer can decide weather to send more points.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSyncSerialised.NodeToNode.Recieve.IntersectNotFound


***
The reply to the consumer that no intersection was found: none of thepoints the consumer supplied are on the producer chain.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSyncSerialised.NodeToNode.Recieve.RequestNext


***
Request the next update from the producer. The response can be a rollforward, a roll back or wait.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSyncSerialised.NodeToNode.Recieve.RollBackward


***
Tell the consumer to roll back to a given point on their chain.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSyncSerialised.NodeToNode.Recieve.RollForward


***
Tell the consumer to extend their chain with the given header.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSyncSerialised.NodeToNode.Send.AwaitReply


***
Acknowledge the request but require the consumer to wait for the nextupdate. This means that the consumer is synced with the producer, andthe producer is waiting for its own chain state to change.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSyncSerialised.NodeToNode.Send.Done


***
We have to explain to the framework what our states mean, in terms ofwhich party has agency in each state.
Idle states are where it is for the client to send a message,busy states are where the server is expected to send a reply.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSyncSerialised.NodeToNode.Send.FindIntersect


***
Ask the producer to try to find an improved intersection point betweenthe consumer and producer's chains. The consumer sends a sequence ofpoints and it is up to the producer to find the first intersection pointon its chain and send it back to the consumer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSyncSerialised.NodeToNode.Send.IntersectFound


***
The reply to the consumer about an intersection found.The consumer can decide weather to send more points.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSyncSerialised.NodeToNode.Send.IntersectNotFound


***
The reply to the consumer that no intersection was found: none of thepoints the consumer supplied are on the producer chain.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSyncSerialised.NodeToNode.Send.RequestNext


***
Request the next update from the producer. The response can be a rollforward, a roll back or wait.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSyncSerialised.NodeToNode.Send.RollBackward


***
Tell the consumer to roll back to a given point on their chain.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSyncSerialised.NodeToNode.Send.RollForward


***
Tell the consumer to extend their chain with the given header.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSyncServerBlock.ChainSyncServerEvent.Update.Update


***
A server read has occurred, either for an add block or a rollback
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSyncServerHeader.ChainSyncServerEvent.Update.Update


***
A server read has occurred, either for an add block or a rollback
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ConnectionManager.Connect


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ConnectionManager.ConnectError


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ConnectionManager.ConnectionCleanup


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ConnectionManager.ConnectionExists


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ConnectionManager.ConnectionFailure


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ConnectionManager.ConnectionHandler


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ConnectionManager.ConnectionManagerCounters


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ConnectionManager.ConnectionNotFound


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ConnectionManager.ConnectionTimeWait


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ConnectionManager.ConnectionTimeWaitDone


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ConnectionManager.ForbiddenConnection


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ConnectionManager.ForbiddenOperation


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ConnectionManager.ImpossibleConnection


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ConnectionManager.IncludeConnection


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ConnectionManager.PruneConnections


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ConnectionManager.Shutdown


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ConnectionManager.State


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ConnectionManager.TerminatedConnection


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ConnectionManager.TerminatingConnection


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ConnectionManager.UnexpectedlyFalseAssertion


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ConnectionManager.UnknownConnection


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ConnectionManager.UnregisterConnection


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ConnectionManagerTransition.ConnectionManagerTransition


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### DNSResolver.LookupAAAAError


***
AAAA lookup failed with an error.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### DNSResolver.LookupAAAAResult


***
Lookup AAAA result.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### DNSResolver.LookupAError


***
A lookup failed with an error.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### DNSResolver.LookupAResult


***
Lookup A result.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### DNSResolver.LookupException


***
A DNS lookup exception occurred.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### DNSResolver.LookupIPv4First


***
Returning IPv4 address first.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### DNSResolver.LookupIPv6First


***
Returning IPv6 address first.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### DNSSubscription.DNS.AllocateSocket


***
DNS Subscription: Allocate socket to address.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### DNSSubscription.DNS.ApplicationException


***
DNS Subscription: Application Exception occurred.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### DNSSubscription.DNS.CloseSocket


***
DNS Subscription: Closed socket to address.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### DNSSubscription.DNS.ConnectEnd


***
DNS Subscription: Connection Attempt end with destination and outcome.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### DNSSubscription.DNS.ConnectException


***
DNS Subscription: Socket Allocation Exception with destination and the exception.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### DNSSubscription.DNS.ConnectStart


***
DNS Subscription: Connection Attempt Start with destination.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### DNSSubscription.DNS.ConnectionExist


***
DNS Subscription: Connection exists to destination.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### DNSSubscription.DNS.MissingLocalAddress


***
DNS Subscription: Missing local address.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### DNSSubscription.DNS.Restart


***
DNS Subscription: Restarting Subscription after duration with desired valency and current valency.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### DNSSubscription.DNS.SkippingPeer


***
DNS Subscription: Skipping peer with address.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### DNSSubscription.DNS.SocketAllocationException


***
DNS Subscription: Connection Attempt Exception with destination and exception.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### DNSSubscription.DNS.Start


***
DNS Subscription: Starting Subscription Worker with a valency.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### DNSSubscription.DNS.SubscriptionFailed


***
DNS Subscription: Failed to start all required subscriptions.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### DNSSubscription.DNS.SubscriptionRunning


***
DNS Subscription: Required subscriptions started.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### DNSSubscription.DNS.SubscriptionWaiting


***
DNS Subscription: Waiting on address with active connections.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### DNSSubscription.DNS.SubscriptionWaitingNewConnection


***
DNS Subscription: Waiting delay time before attempting a new connection.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### DNSSubscription.DNS.TryConnectToPeer


***
DNS Subscription: Trying to connect to peer with address.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### DNSSubscription.DNS.UnsupportedRemoteAddr


***
DNS Subscription: Unsupported remote target address.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### DebugPeerSelection.DebugPeerSelection.GovernorState


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### DebugPeerSelectionResponder.DebugPeerSelection.GovernorState


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### DiffusionInit.ConfiguringLocalSocket


***
ConfiguringLocalSocket 
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### DiffusionInit.ConfiguringServerSocket


***
ConfiguringServerSocket 
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### DiffusionInit.CreateSystemdSocketForSnocketPath


***
CreateSystemdSocketForSnocketPath 
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### DiffusionInit.CreatedLocalSocket


***
CreatedLocalSocket 
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### DiffusionInit.CreatingServerSocket


***
CreatingServerSocket 
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### DiffusionInit.DiffusionErrored


***
DiffusionErrored 
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### DiffusionInit.ListeningLocalSocket


***
ListeningLocalSocket 
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### DiffusionInit.ListeningServerSocket


***
ListeningServerSocket 
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### DiffusionInit.LocalSocketUp


***
LocalSocketUp 
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### DiffusionInit.RunLocalServer


***
RunLocalServer 
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### DiffusionInit.RunServer


***
RunServer 
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### DiffusionInit.ServerSocketUp


***
ServerSocketUp 
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### DiffusionInit.UnsupportedLocalSystemdSocket


***
UnsupportedLocalSystemdSocket 
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### DiffusionInit.UnsupportedReadySocketCase


***
UnsupportedReadySocketCase 
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### DiffusionInit.UsingSystemdSocket


***
UsingSystemdSocket 
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ErrorPolicy.AcceptException


***
'accept' threw an exception.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ErrorPolicy.KeepSuspended


***
Consumer was suspended until producer will resume.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ErrorPolicy.LocalNodeError


***
caught a local exception.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ErrorPolicy.ResumeConsumer


***
Resume consumer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ErrorPolicy.ResumePeer


***
Resume a peer (both consumer and producer).
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ErrorPolicy.ResumeProducer


***
Resume producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ErrorPolicy.SuspendConsumer


***
Suspending consumer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ErrorPolicy.SuspendPeer


***
Suspending peer with a given exception.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ErrorPolicy.UnhandledApplicationException


***
An application threw an exception, which was not handled.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ErrorPolicy.UnhandledConnectionException


***
'connect' threw an exception, which was not handled by any 'ErrorPolicy'.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Forge.AdoptedBlock


***
We adopted the block we produced, we also trace the transactions  that were adopted.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Forge.BlockContext


***
We found out to which block we are going to connect the block we are about  to forge.   We record the current slot number, the block number of the block to  connect to and its point.   Note that block number of the block we will try to forge is one more than  the recorded block number.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Forge.BlockFromFuture


***
Leadership check failed: the current chain contains a block from a slot  /after/ the current slot   This can only happen if the system is under heavy load.   We record both the current slot number as well as the slot number of the  block at the tip of the chain.   See also <https://github.com/input-output-hk/ouroboros-network/issues/1462>
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Forge.DidntAdoptBlock


***
We did not adopt the block we produced, but the block was valid. We  must have adopted a block that another leader of the same slot produced  before we got the chance of adopting our own block. This is very rare,  this warrants a warning.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Forge.ForgeStateUpdateError


***
Updating the forge state failed.   For example, the KES key could not be evolved anymore.   We record the error returned by 'updateForgeState'.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Forge.ForgedBlock


***
We forged a block.
  We record the current slot number, the point of the predecessor, the block  itself, and the total size of the mempool snapshot at the time we produced  the block (which may be significantly larger than the block, due to  maximum block size)
  This will be followed by one of three messages:
  * AdoptedBlock (normally)
  * DidntAdoptBlock (rarely)
  * ForgedInvalidBlock (hopefully never -- this would indicate a bug)
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Forge.ForgedInvalidBlock


***
We forged a block that is invalid according to the ledger in the  ChainDB. This means there is an inconsistency between the mempool  validation and the ledger validation. This is a serious error!
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Forge.LedgerState


***
We obtained a ledger state for the point of the block we want to  connect to   We record both the current slot number as well as the point of the block  we attempt to connect the new block to (that we requested the ledger  state for).
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Forge.LedgerView


***
We obtained a ledger view for the current slot number   We record the current slot number.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Forge.NoLedgerState


***
Leadership check failed: we were unable to get the ledger state for the  point of the block we want to connect to   This can happen if after choosing which block to connect to the node  switched to a different fork. We expect this to happen only rather  rarely, so this certainly merits a warning; if it happens a lot, that  merits an investigation.   We record both the current slot number as well as the point of the block  we attempt to connect the new block to (that we requested the ledger  state for).
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Forge.NoLedgerView


***
Leadership check failed: we were unable to get the ledger view for the  current slot number   This will only happen if there are many missing blocks between the tip of  our chain and the current slot.   We record also the failure returned by 'forecastFor'.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Forge.NodeCannotForge


***
We did the leadership check and concluded that we should lead and forge  a block, but cannot.   This should only happen rarely and should be logged with warning severity.   Records why we cannot forge a block.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Forge.NodeIsLeader


***
We did the leadership check and concluded we /are/ the leader
  The node will soon forge; it is about to read its transactions from the  Mempool. This will be followed by ForgedBlock.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Forge.NodeNotLeader


***
We did the leadership check and concluded we are not the leader   We record the current slot number
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Forge.SlotIsImmutable


***
Leadership check failed: the tip of the ImmutableDB inhabits the  current slot   This might happen in two cases.    1. the clock moved backwards, on restart we ignored everything from the      VolatileDB since it's all in the future, and now the tip of the      ImmutableDB points to a block produced in the same slot we're trying      to produce a block in    2. k = 0 and we already adopted a block from another leader of the same      slot.   We record both the current slot number as well as the tip of the  ImmutableDB.  See also <https://github.com/input-output-hk/ouroboros-network/issues/1462>
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Forge.StartLeadershipCheck


***
Start of the leadership check.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Forge.StartLeadershipCheckPlus


***
We adopted the block we produced, we also trace the transactions  that were adopted.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ForgeStateInfo


***
kesStartPeriod 
kesEndPeriod is kesStartPeriod + tpraosMaxKESEvo
kesEvolution is the current evolution or /relative period/.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ForgeStats


***
nodeCannotForgeNum shows how many times this node could not forge.
nodeIsLeaderNum shows how many times this node was leader.
blocksForgedNum shows how many blocks did forge in this node.
slotsMissed shows how many slots were missed in this node.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Handshake.Receive.AcceptVersion


***
The remote end decides which version to use and sends chosen version.The server is allowed to modify version parameters.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Handshake.Receive.ProposeVersions


***
Propose versions together with version parameters.  It must be encoded to a sorted list..
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Handshake.Receive.Refuse


***
It refuses to run any version.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Handshake.Receive.ReplyVersions


***
`MsgReplyVersions` received as a response to 'MsgProposeVersions'.  It is not supported to explicitly send this message. It can only be received as a copy of 'MsgProposeVersions' in a simultaneous open scenario.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Handshake.Send.AcceptVersion


***
The remote end decides which version to use and sends chosen version.The server is allowed to modify version parameters.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Handshake.Send.ProposeVersions


***
Propose versions together with version parameters.  It must be encoded to a sorted list..
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Handshake.Send.Refuse


***
It refuses to run any version.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Handshake.Send.ReplyVersions


***
`MsgReplyVersions` received as a response to 'MsgProposeVersions'.  It is not supported to explicitly send this message. It can only be received as a copy of 'MsgProposeVersions' in a simultaneous open scenario.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### InboundGovernor.DemotedToColdRemote


***
All mini-protocols terminated.  The boolean is true if this connection was not used by p2p-governor, and thus the connection will be terminated.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### InboundGovernor.DemotedToWarmRemote


***
All mini-protocols terminated.  The boolean is true if this connection was not used by p2p-governor, and thus the connection will be terminated.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### InboundGovernor.InboundGovernorCounters


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### InboundGovernor.InboundGovernorError


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### InboundGovernor.MuxCleanExit


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### InboundGovernor.MuxErrored


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### InboundGovernor.NewConnection


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### InboundGovernor.PromotedToHotRemote


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### InboundGovernor.PromotedToWarmRemote


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### InboundGovernor.RemoteState


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### InboundGovernor.ResponderErrored


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### InboundGovernor.ResponderRestarted


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### InboundGovernor.ResponderStartFailure


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### InboundGovernor.ResponderStarted


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### InboundGovernor.ResponderTerminated


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### InboundGovernor.UnexpectedlyFalseAssertion


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### InboundGovernor.WaitIdleRemote


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### InboundGovernorTransition.InboundGovernorTransition


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### IpSubscription.IP.AllocateSocket


***
IP Subscription: Allocate socket to address.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### IpSubscription.IP.ApplicationException


***
IP Subscription: Application Exception occurred.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### IpSubscription.IP.CloseSocket


***
IP Subscription: Closed socket to address.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### IpSubscription.IP.ConnectEnd


***
IP Subscription: Connection Attempt end with destination and outcome.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### IpSubscription.IP.ConnectException


***
IP Subscription: Socket Allocation Exception with destination and the exception.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### IpSubscription.IP.ConnectStart


***
IP Subscription: Connection Attempt Start with destination.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### IpSubscription.IP.ConnectionExist


***
IP Subscription: Connection exists to destination.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### IpSubscription.IP.MissingLocalAddress


***
IP Subscription: Missing local address.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### IpSubscription.IP.Restart


***
IP Subscription: Restarting Subscription after duration with desired valency and current valency.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### IpSubscription.IP.SkippingPeer


***
IP Subscription: Skipping peer with address.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### IpSubscription.IP.SocketAllocationException


***
IP Subscription: Connection Attempt Exception with destination and exception.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### IpSubscription.IP.Start


***
IP Subscription: Starting Subscription Worker with a valency.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### IpSubscription.IP.SubscriptionFailed


***
IP Subscription: Failed to start all required subscriptions.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### IpSubscription.IP.SubscriptionRunning


***
IP Subscription: Required subscriptions started.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### IpSubscription.IP.SubscriptionWaiting


***
IP Subscription: Waiting on address with active connections.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### IpSubscription.IP.SubscriptionWaitingNewConnection


***
IP Subscription: Waiting delay time before attempting a new connection.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### IpSubscription.IP.TryConnectToPeer


***
IP Subscription: Trying to connect to peer with address.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### IpSubscription.IP.UnsupportedRemoteAddr


***
IP Subscription: Unsupported remote target address.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### KeepAliveClient


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LedgerPeers.DisabledLedgerPeers


***
Trace for when getting peers from the ledger is disabled, that is DontUseLedger.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LedgerPeers.FallingBackToBootstrapPeers


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LedgerPeers.FetchingNewLedgerState


***
Trace for fetching a new list of peers from the ledger. Int is the number of peers returned.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LedgerPeers.PickedPeer


***
Trace for a peer picked with accumulated and relative stake of its pool.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LedgerPeers.PickedPeers


***
Trace for the number of peers we wanted to pick and the list of peers picked.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LedgerPeers.RequestForPeers


***
RequestForPeers (NumberOfPeers 1)
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LedgerPeers.ReusingLedgerState


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LedgerPeers.TraceUseLedgerAfter


***
Trace UseLedgerAfter value.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LedgerPeers.WaitingOnRequest


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalConnectionManager.Connect


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalConnectionManager.ConnectError


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalConnectionManager.ConnectionCleanup


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalConnectionManager.ConnectionExists


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalConnectionManager.ConnectionFailure


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalConnectionManager.ConnectionHandler


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalConnectionManager.ConnectionManagerCounters


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalConnectionManager.ConnectionNotFound


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalConnectionManager.ConnectionTimeWait


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalConnectionManager.ConnectionTimeWaitDone


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalConnectionManager.ForbiddenConnection


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalConnectionManager.ForbiddenOperation


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalConnectionManager.ImpossibleConnection


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalConnectionManager.IncludeConnection


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalConnectionManager.PruneConnections


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalConnectionManager.Shutdown


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalConnectionManager.State


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalConnectionManager.TerminatedConnection


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalConnectionManager.TerminatingConnection


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalConnectionManager.UnexpectedlyFalseAssertion


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalConnectionManager.UnknownConnection


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalConnectionManager.UnregisterConnection


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalErrorPolicy.AcceptException


***
'accept' threw an exception.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### LocalErrorPolicy.KeepSuspended


***
Consumer was suspended until producer will resume.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### LocalErrorPolicy.LocalNodeError


***
caught a local exception.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### LocalErrorPolicy.ResumeConsumer


***
Resume consumer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### LocalErrorPolicy.ResumePeer


***
Resume a peer (both consumer and producer).
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### LocalErrorPolicy.ResumeProducer


***
Resume producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### LocalErrorPolicy.SuspendConsumer


***
Suspending consumer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### LocalErrorPolicy.SuspendPeer


***
Suspending peer with a given exception.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### LocalErrorPolicy.UnhandledApplicationException


***
An application threw an exception, which was not handled.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### LocalErrorPolicy.UnhandledConnectionException


***
'connect' threw an exception, which was not handled by any 'ErrorPolicy'.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### LocalHandshake.Receive.AcceptVersion


***
The remote end decides which version to use and sends chosen version.The server is allowed to modify version parameters.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalHandshake.Receive.ProposeVersions


***
Propose versions together with version parameters.  It must be encoded to a sorted list..
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalHandshake.Receive.Refuse


***
It refuses to run any version.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalHandshake.Receive.ReplyVersions


***
`MsgReplyVersions` received as a response to 'MsgProposeVersions'.  It is not supported to explicitly send this message. It can only be received as a copy of 'MsgProposeVersions' in a simultaneous open scenario.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalHandshake.Send.AcceptVersion


***
The remote end decides which version to use and sends chosen version.The server is allowed to modify version parameters.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalHandshake.Send.ProposeVersions


***
Propose versions together with version parameters.  It must be encoded to a sorted list..
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalHandshake.Send.Refuse


***
It refuses to run any version.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalHandshake.Send.ReplyVersions


***
`MsgReplyVersions` received as a response to 'MsgProposeVersions'.  It is not supported to explicitly send this message. It can only be received as a copy of 'MsgProposeVersions' in a simultaneous open scenario.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalInboundGovernor.DemotedToColdRemote


***
All mini-protocols terminated.  The boolean is true if this connection was not used by p2p-governor, and thus the connection will be terminated.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalInboundGovernor.DemotedToWarmRemote


***
All mini-protocols terminated.  The boolean is true if this connection was not used by p2p-governor, and thus the connection will be terminated.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalInboundGovernor.InboundGovernorCounters


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalInboundGovernor.InboundGovernorError


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalInboundGovernor.MuxCleanExit


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalInboundGovernor.MuxErrored


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalInboundGovernor.NewConnection


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalInboundGovernor.PromotedToHotRemote


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalInboundGovernor.PromotedToWarmRemote


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalInboundGovernor.RemoteState


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalInboundGovernor.ResponderErrored


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalInboundGovernor.ResponderRestarted


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalInboundGovernor.ResponderStartFailure


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalInboundGovernor.ResponderStarted


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalInboundGovernor.ResponderTerminated


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalInboundGovernor.UnexpectedlyFalseAssertion


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalInboundGovernor.WaitIdleRemote


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalRootPeers.LocalRootDomains


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalRootPeers.LocalRootError


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalRootPeers.LocalRootFailure


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalRootPeers.LocalRootGroups


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalRootPeers.LocalRootResult


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalRootPeers.LocalRootWaiting


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalServer.AcceptConnection


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalServer.AcceptError


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalServer.AcceptPolicy


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalServer.Error


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalServer.Started


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalServer.Stopped


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### LocalTxSubmissionServer.ReceivedTx


***
A transaction was received.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Mempool.AddedTx


***
New, valid transaction that was added to the Mempool.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Mempool.ManuallyRemovedTxs


***
Transactions that have been manually removed from the Mempool.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Mempool.RejectedTx


***
New, invalid transaction thas was rejected and thus not added to the Mempool.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Mempool.RemoveTxs


***
Previously valid transactions that are no longer valid because of changes in the ledger state. These transactions have been removed from the Mempool.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Mux.ChannelRecvEnd


***
Channel receive end.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Mux.ChannelRecvStart


***
Channel receive start.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Mux.ChannelSendEnd


***
Channel send end.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Mux.ChannelSendStart


***
Channel send start.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Mux.CleanExit


***
Miniprotocol terminated cleanly.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Mux.ExceptionExit


***
Miniprotocol terminated with exception.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Mux.HandshakeClientEnd


***
Handshake client end.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Mux.HandshakeClientError


***
Handshake client error.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Mux.HandshakeServerEnd


***
Handshake server end.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Mux.HandshakeServerError


***
Handshake server error.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Mux.HandshakeStart


***
Handshake start.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Mux.RecvDeltaQObservation


***
Bearer DeltaQ observation.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Mux.RecvDeltaQSample


***
Bearer DeltaQ sample.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Mux.RecvEnd


***
Bearer receive end.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Mux.RecvHeaderEnd


***
Bearer receive header end.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Mux.RecvHeaderStart


***
Bearer receive header start.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Mux.RecvStart


***
Bearer receive start.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Mux.SDUReadTimeoutException


***
Timed out reading SDU.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Mux.SDUWriteTimeoutException


***
Timed out writing SDU.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Mux.SendEnd


***
Bearer send end.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Mux.SendStart


***
Bearer send start.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Mux.Shutdown


***
Mux shutdown.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Mux.StartEagerly


***
Eagerly started.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Mux.StartOnDemand


***
Preparing to start.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Mux.StartedOnDemand


***
Started on demand.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Mux.State


***
State.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Mux.TCPInfo


***
TCPInfo.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Mux.Terminating


***
Terminating.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### MuxLocal.ChannelRecvEnd


***
Channel receive end.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### MuxLocal.ChannelRecvStart


***
Channel receive start.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### MuxLocal.ChannelSendEnd


***
Channel send end.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### MuxLocal.ChannelSendStart


***
Channel send start.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### MuxLocal.CleanExit


***
Miniprotocol terminated cleanly.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### MuxLocal.ExceptionExit


***
Miniprotocol terminated with exception.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### MuxLocal.HandshakeClientEnd


***
Handshake client end.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### MuxLocal.HandshakeClientError


***
Handshake client error.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### MuxLocal.HandshakeServerEnd


***
Handshake server end.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### MuxLocal.HandshakeServerError


***
Handshake server error.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### MuxLocal.HandshakeStart


***
Handshake start.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### MuxLocal.RecvDeltaQObservation


***
Bearer DeltaQ observation.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### MuxLocal.RecvDeltaQSample


***
Bearer DeltaQ sample.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### MuxLocal.RecvEnd


***
Bearer receive end.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### MuxLocal.RecvHeaderEnd


***
Bearer receive header end.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### MuxLocal.RecvHeaderStart


***
Bearer receive header start.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### MuxLocal.RecvStart


***
Bearer receive start.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### MuxLocal.SDUReadTimeoutException


***
Timed out reading SDU.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### MuxLocal.SDUWriteTimeoutException


***
Timed out writing SDU.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### MuxLocal.SendEnd


***
Bearer send end.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### MuxLocal.SendStart


***
Bearer send start.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### MuxLocal.Shutdown


***
Mux shutdown.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### MuxLocal.StartEagerly


***
Eagerly started.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### MuxLocal.StartOnDemand


***
Preparing to start.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### MuxLocal.StartedOnDemand


***
Started on demand.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### MuxLocal.State


***
State.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### MuxLocal.TCPInfo


***
TCPInfo.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### MuxLocal.Terminating


***
Terminating.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### PeerSelection.ChurnMode


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### PeerSelection.ChurnWait


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### PeerSelection.DemoteAsynchronous


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### PeerSelection.DemoteHotDone


***
target active, actual active, peer
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### PeerSelection.DemoteHotFailed


***
target active, actual active, peer, reason
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### PeerSelection.DemoteHotPeers


***
target active, actual active, selected peers
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### PeerSelection.DemoteLocalHotPeers


***
local per-group (target active, actual active), selected peers
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### PeerSelection.DemoteWarmDone


***
target established, actual established, peer
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### PeerSelection.DemoteWarmFailed


***
target established, actual established, peer, reason
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### PeerSelection.DemoteWarmPeers


***
target established, actual established, selected peers
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### PeerSelection.ForgetColdPeers


***
target known peers, actual known peers, selected peers
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### PeerSelection.GossipRequests


***
target known peers, actual known peers, peers available for gossip, peers selected for gossip
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### PeerSelection.GossipResults


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### PeerSelection.GovernorWakeup


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### PeerSelection.LocalRootPeersChanged


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### PeerSelection.PromoteColdDone


***
target active, actual active, selected peers
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### PeerSelection.PromoteColdFailed


***
target established, actual established, peer, delay until next promotion, reason
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### PeerSelection.PromoteColdLocalPeers


***
target local established, actual local established, selected peers
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### PeerSelection.PromoteColdPeers


***
target established, actual established, selected peers
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### PeerSelection.PromoteWarmAborted


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### PeerSelection.PromoteWarmDone


***
target active, actual active, peer
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### PeerSelection.PromoteWarmFailed


***
target active, actual active, peer, reason
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### PeerSelection.PromoteWarmLocalPeers


***
local per-group (target active, actual active), selected peers
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### PeerSelection.PromoteWarmPeers


***
target active, actual active, selected peers
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### PeerSelection.PublicRootsFailure


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### PeerSelection.PublicRootsRequest


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### PeerSelection.PublicRootsResults


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### PeerSelection.TargetsChanged


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### PeerSelectionActions.MonitoringError


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### PeerSelectionActions.MonitoringResult


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### PeerSelectionActions.StatusChangeFailure


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### PeerSelectionActions.StatusChanged


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### PeerSelectionCounters.PeerSelectionCounters


***
Counters for cold, warm and hot peers
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Peers


***
TODO Doc
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### PublicRootPeers.PublicRootPeers.PublicRootDomains


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### PublicRootPeers.PublicRootPeers.PublicRootFailure


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### PublicRootPeers.PublicRootPeers.PublicRootRelayAccessPoint


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### PublicRootPeers.PublicRootPeers.PublicRootResult


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ReplayBlock.LedgerReplay


***
Counts up the percent of a block replay.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Resources


***
TODO JNF
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Server.AcceptConnection


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Server.AcceptError


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Server.AcceptPolicy


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Server.Error


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Server.Started


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Server.Stopped


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Shutdown.AbnormalShutdown


***
non-isEOFerror shutdown request
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Shutdown.RequestingShutdown


***
Ringing the node shutdown doorbell
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Shutdown.ShutdownArmedAt


***
Setting up node shutdown at given slot / block.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Shutdown.ShutdownRequested


***
Node shutdown was requested.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Shutdown.ShutdownUnexpectedInput


***
Received shutdown request but found unexpected input in --shutdown-ipc FD: 
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.Byron


***
_bibSystemStartTime_: TODO JNF 
_bibSlotLength_: gives the length of a slot as time interval. 
_bibEpochLength_: gives the number of slots which forms an epoch.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.Common


***
_biConfigPath_: is the path to the config in use. 
_biProtocol_: is the name of the protocol, e.g. "Byron", "Shelley" or "Byron; Shelley". 
_biVersion_: is the version of the node software running. 
_biCommit_: is the commit revision of the software running. 
_biNodeStartTime_: gives the time this node was started.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.Network


***
_niAddresses_: IPv4 or IPv6 socket ready to accept connectionsor diffusion addresses. 
_niDiffusionMode_: shows if the node runs only initiator or bothinitiator or responder node. 
_niDnsProducers_: shows the list of domain names to subscribe to. 
_niIpProducers_: shows the list of ip subscription addresses.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.NetworkConfig


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.NetworkConfigUpdate


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.NetworkConfigUpdateError


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.P2PWarning


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.P2PWarningDevelopementNetworkProtocols


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.ShelleyBased


***
bisEra is the current era, e.g. "Shelley", "Allegra", "Mary" or "Alonzo". 
_bisSystemStartTime_: TODO JNF 
_bisSlotLength_: gives the length of a slot as time interval. 
_bisEpochLength_: gives the number of slots which forms an epoch. 
_bisSlotsPerKESPeriod_: gives the slots per KES period.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.StartupDBValidation


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.StartupInfo


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.StartupNetworkMagic


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.StartupP2PInfo


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.StartupSocketConfigError


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.StartupTime


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.WarningDevelopmentNetworkProtocols


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### StateQueryClient.Recieve.Acquire


***
The client requests that the state as of a particular recent point onthe server's chain (within K of the tip) be made available to query,and waits for confirmation or failure.
From 'NodeToClient_V8' onwards if the point is not specified, current tipwill be acquired.  For previous versions of the protocol 'point' must begiven.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### StateQueryClient.Recieve.Acquired


***
The server can confirm that it has the state at the requested point.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### StateQueryClient.Recieve.Done


***
The client can terminate the protocol.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### StateQueryClient.Recieve.Failure


***
The server can report that it cannot obtain the state for therequested point.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### StateQueryClient.Recieve.Query


***
The client can perform queries on the current acquired state.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### StateQueryClient.Recieve.ReAcquire


***
This is like 'MsgAcquire' but for when the client already has astate. By moveing to another state directly without a 'MsgRelease' itenables optimisations on the server side (e.g. moving to the state forthe immediate next block).
Note that failure to re-acquire is equivalent to 'MsgRelease',rather than keeping the exiting acquired state.
From 'NodeToClient_V8' onwards if the point is not specified, current tipwill be acquired.  For previous versions of the protocol 'point' must begiven.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### StateQueryClient.Recieve.Release


***
The client can instruct the server to release the state. This letsthe server free resources.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### StateQueryClient.Recieve.Result


***
The server must reply with the queries.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### StateQueryClient.Send.Acquire


***
The client requests that the state as of a particular recent point onthe server's chain (within K of the tip) be made available to query,and waits for confirmation or failure.
From 'NodeToClient_V8' onwards if the point is not specified, current tipwill be acquired.  For previous versions of the protocol 'point' must begiven.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### StateQueryClient.Send.Acquired


***
The server can confirm that it has the state at the requested point.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### StateQueryClient.Send.Done


***
The client can terminate the protocol.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### StateQueryClient.Send.Failure


***
The server can report that it cannot obtain the state for therequested point.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### StateQueryClient.Send.Query


***
The client can perform queries on the current acquired state.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### StateQueryClient.Send.ReAcquire


***
This is like 'MsgAcquire' but for when the client already has astate. By moveing to another state directly without a 'MsgRelease' itenables optimisations on the server side (e.g. moving to the state forthe immediate next block).
Note that failure to re-acquire is equivalent to 'MsgRelease',rather than keeping the exiting acquired state.
From 'NodeToClient_V8' onwards if the point is not specified, current tipwill be acquired.  For previous versions of the protocol 'point' must begiven.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### StateQueryClient.Send.Release


***
The client can instruct the server to release the state. This letsthe server free resources.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### StateQueryClient.Send.Result


***
The server must reply with the queries.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxInbound.TxInboundCanRequestMoreTxs


***
There are no replies in flight, but we do know some more txs we can ask for, so lets ask for them and more txids.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxInbound.TxInboundCannotRequestMoreTxs


***
There's no replies in flight, and we have no more txs we can ask for so the only remaining thing to do is to ask for more txids. Since this is the only thing to do now, we make this a blocking call.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxInbound.TxInboundTerminated


***
Server received 'MsgDone'.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxInbound.TxSubmissionCollected


***
Number of transactions just about to be inserted.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxInbound.TxSubmissionProcessed


***
Just processed transaction pass/fail breakdown.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxMonitorClient.Recieve.Acquire


***
The client requests that the state as of a particular recent point onthe server's chain (within K of the tip) be made available to query,and waits for confirmation or failure.
From 'NodeToClient_V8' onwards if the point is not specified, current tipwill be acquired.  For previous versions of the protocol 'point' must begiven.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxMonitorClient.Recieve.Acquired


***
The server can confirm that it has the state at the requested point.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxMonitorClient.Recieve.Done


***
The client can terminate the protocol.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxMonitorClient.Recieve.Failure


***
The server can report that it cannot obtain the state for therequested point.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxMonitorClient.Recieve.Query


***
The client can perform queries on the current acquired state.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxMonitorClient.Recieve.ReAcquire


***
This is like 'MsgAcquire' but for when the client already has astate. By moveing to another state directly without a 'MsgRelease' itenables optimisations on the server side (e.g. moving to the state forthe immediate next block).
Note that failure to re-acquire is equivalent to 'MsgRelease',rather than keeping the exiting acquired state.
From 'NodeToClient_V8' onwards if the point is not specified, current tipwill be acquired.  For previous versions of the protocol 'point' must begiven.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxMonitorClient.Recieve.Release


***
The client can instruct the server to release the state. This letsthe server free resources.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxMonitorClient.Recieve.Result


***
The server must reply with the queries.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxMonitorClient.Send.Acquire


***
The client requests that the state as of a particular recent point onthe server's chain (within K of the tip) be made available to query,and waits for confirmation or failure.
From 'NodeToClient_V8' onwards if the point is not specified, current tipwill be acquired.  For previous versions of the protocol 'point' must begiven.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxMonitorClient.Send.Acquired


***
The server can confirm that it has the state at the requested point.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxMonitorClient.Send.Done


***
The client can terminate the protocol.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxMonitorClient.Send.Failure


***
The server can report that it cannot obtain the state for therequested point.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxMonitorClient.Send.Query


***
The client can perform queries on the current acquired state.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxMonitorClient.Send.ReAcquire


***
This is like 'MsgAcquire' but for when the client already has astate. By moveing to another state directly without a 'MsgRelease' itenables optimisations on the server side (e.g. moving to the state forthe immediate next block).
Note that failure to re-acquire is equivalent to 'MsgRelease',rather than keeping the exiting acquired state.
From 'NodeToClient_V8' onwards if the point is not specified, current tipwill be acquired.  For previous versions of the protocol 'point' must begiven.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxMonitorClient.Send.Release


***
The client can instruct the server to release the state. This letsthe server free resources.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxMonitorClient.Send.Result


***
The server must reply with the queries.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxOutbound.ControlMessage


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxOutbound.RecvMsgRequest


***
The IDs of the transactions requested.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxOutbound.SendMsgReply


***
The transactions to be sent in the response.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission2.NodeToNode.Recieve.Done


***
Termination message, initiated by the client when the server ismaking a blocking call for more transaction identifiers.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission2.NodeToNode.Recieve.MsgHello


***
Client side hello message.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission2.NodeToNode.Recieve.ReplyTxIds


***
Reply with a list of transaction identifiers for availabletransactions, along with the size of each transaction.
The list must not be longer than the maximum number requested.
In the 'StTxIds' 'StBlocking' state the list must be non-empty whilein the 'StTxIds' 'StNonBlocking' state the list may be empty.
These transactions are added to the notional FIFO of outstandingtransaction identifiers for the protocol.
The order in which these transaction identifiers are returned must bethe order in which they are submitted to the mempool, to preservedependent transactions.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission2.NodeToNode.Recieve.ReplyTxs


***
Reply with the requested transactions, or implicitly discard.
Transactions can become invalid between the time the transactionidentifier was sent and the transaction being requested. Invalid(including committed) transactions do not need to be sent.
Any transaction identifiers requested but not provided in this replyshould be considered as if this peer had never announced them. (Notethat this is no guarantee that the transaction is invalid, it may stillbe valid and available from another peer).
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission2.NodeToNode.Recieve.RequestTxIds


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


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission2.NodeToNode.Recieve.RequestTxs


***
Request one or more transactions corresponding to the given transaction identifiers. 
While it is the responsibility of the replying peer to keep withinpipelining in-flight limits, the sender must also cooperate by keepingthe total requested across all in-flight requests within the limits.
It is an error to ask for transaction identifiers that were notpreviously announced (via 'MsgReplyTxIds').
It is an error to ask for transaction identifiers that are notoutstanding or that were already asked for.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission2.NodeToNode.Send.Done


***
Termination message, initiated by the client when the server ismaking a blocking call for more transaction identifiers.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission2.NodeToNode.Send.MsgHello


***
Client side hello message.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission2.NodeToNode.Send.ReplyTxIds


***
Reply with a list of transaction identifiers for availabletransactions, along with the size of each transaction.
The list must not be longer than the maximum number requested.
In the 'StTxIds' 'StBlocking' state the list must be non-empty whilein the 'StTxIds' 'StNonBlocking' state the list may be empty.
These transactions are added to the notional FIFO of outstandingtransaction identifiers for the protocol.
The order in which these transaction identifiers are returned must bethe order in which they are submitted to the mempool, to preservedependent transactions.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission2.NodeToNode.Send.ReplyTxs


***
Reply with the requested transactions, or implicitly discard.
Transactions can become invalid between the time the transactionidentifier was sent and the transaction being requested. Invalid(including committed) transactions do not need to be sent.
Any transaction identifiers requested but not provided in this replyshould be considered as if this peer had never announced them. (Notethat this is no guarantee that the transaction is invalid, it may stillbe valid and available from another peer).
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission2.NodeToNode.Send.RequestTxIds


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


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission2.NodeToNode.Send.RequestTxs


***
Request one or more transactions corresponding to the given transaction identifiers. 
While it is the responsibility of the replying peer to keep withinpipelining in-flight limits, the sender must also cooperate by keepingthe total requested across all in-flight requests within the limits.
It is an error to ask for transaction identifiers that were notpreviously announced (via 'MsgReplyTxIds').
It is an error to ask for transaction identifiers that are notoutstanding or that were already asked for.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmissionClient.Recieve.AcceptTx


***
The server can reply to inform the client that it has accepted thetransaction.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmissionClient.Recieve.Done


***
The client can terminate the protocol.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmissionClient.Recieve.RejectTx


***
The server can reply to inform the client that it has rejected thetransaction. A reason for the rejection is included.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmissionClient.Recieve.SubmitTx


***
The client submits a single transaction and waits a reply.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmissionClient.Send.AcceptTx


***
The server can reply to inform the client that it has accepted thetransaction.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmissionClient.Send.Done


***
The client can terminate the protocol.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmissionClient.Send.RejectTx


***
The server can reply to inform the client that it has rejected thetransaction. A reason for the rejection is included.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmissionClient.Send.SubmitTx


***
The client submits a single transaction and waits a reply.
***


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

## Metrics
### Block replay progress (%)

***
Progress in percent
***


Dispatched by: 
ReplayBlock.LedgerReplay

From current configuration:
Filtered  by config value: `Notice`

### blocksForgedNum

***
How many blocks did forge in this node?
***


Dispatched by: 
ForgeStats

From current configuration:
Filtered  by config value: `Notice`

### cardano.node.aboutToLeadSlotLast

***

***


Dispatched by: 
Forge.StartLeadershipCheck

From current configuration:
Filtered  by config value: `Info`

### cardano.node.aboutToLeadSlotLast

***

***


Dispatched by: 
Forge.StartLeadershipCheckPlus

From current configuration:
Filtered  by config value: `Info`

### cardano.node.adoptedSlotLast

***

***


Dispatched by: 
Forge.AdoptedBlock

From current configuration:
Filtered  by config value: `Info`

### cardano.node.blockContext

***

***


Dispatched by: 
Forge.BlockContext

From current configuration:
Filtered  by config value: `Info`

### cardano.node.blockFromFuture

***

***


Dispatched by: 
Forge.BlockFromFuture

From current configuration:
Filtered  by config value: `Info`

### cardano.node.blocks

***
Number of blocks in this chain fragment.
***


Dispatched by: 
ChainDB.AddBlockEvent.AddedToCurrentChain

From current configuration:
Filtered  by config value: `Info`

### cardano.node.blocks

***
Number of blocks in this chain fragment.
***


Dispatched by: 
ChainDB.AddBlockEvent.SwitchedToAFork

From current configuration:
Filtered  by config value: `Info`

### cardano.node.connectedPeers

***
Number of connected peers
***


Dispatched by: 
BlockFetchDecision

From current configuration:
Filtered  by config value: `Notice`

### cardano.node.connectionManager.duplexConns

***

***


Dispatched by: 
ConnectionManager.ConnectionManagerCounters

From current configuration:
Filtered  by config value: `Notice`

### cardano.node.connectionManager.duplexConns

***

***


Dispatched by: 
LocalConnectionManager.ConnectionManagerCounters

From current configuration:
Filtered  by config value: `Notice`

### cardano.node.connectionManager.fullDuplexConns

***

***


Dispatched by: 
ConnectionManager.ConnectionManagerCounters

From current configuration:
Filtered  by config value: `Notice`

### cardano.node.connectionManager.fullDuplexConns

***

***


Dispatched by: 
LocalConnectionManager.ConnectionManagerCounters

From current configuration:
Filtered  by config value: `Notice`

### cardano.node.connectionManager.inboundConns

***

***


Dispatched by: 
ConnectionManager.ConnectionManagerCounters

From current configuration:
Filtered  by config value: `Notice`

### cardano.node.connectionManager.inboundConns

***

***


Dispatched by: 
LocalConnectionManager.ConnectionManagerCounters

From current configuration:
Filtered  by config value: `Notice`

### cardano.node.connectionManager.outboundConns

***

***


Dispatched by: 
ConnectionManager.ConnectionManagerCounters

From current configuration:
Filtered  by config value: `Notice`

### cardano.node.connectionManager.outboundConns

***

***


Dispatched by: 
LocalConnectionManager.ConnectionManagerCounters

From current configuration:
Filtered  by config value: `Notice`

### cardano.node.connectionManager.unidirectionalConns

***

***


Dispatched by: 
ConnectionManager.ConnectionManagerCounters

From current configuration:
Filtered  by config value: `Notice`

### cardano.node.connectionManager.unidirectionalConns

***

***


Dispatched by: 
LocalConnectionManager.ConnectionManagerCounters

From current configuration:
Filtered  by config value: `Notice`

### cardano.node.couldNotForgeSlotLast

***

***


Dispatched by: 
Forge.NoLedgerState

From current configuration:
Filtered  by config value: `Info`

### cardano.node.couldNotForgeSlotLast

***

***


Dispatched by: 
Forge.NoLedgerView

From current configuration:
Filtered  by config value: `Info`

### cardano.node.currentKESPeriod

***

***


Dispatched by: 
Forge.ForgeStateUpdateError

From current configuration:
Filtered  by config value: `Info`

### cardano.node.delegMapSize

***

***


Dispatched by: 
Forge.StartLeadershipCheckPlus

From current configuration:
Filtered  by config value: `Info`

### cardano.node.density

***
The actual number of blocks created over the maximum expected number of blocks that could be created over the span of the last @k@ blocks.
***


Dispatched by: 
ChainDB.AddBlockEvent.AddedToCurrentChain

From current configuration:
Filtered  by config value: `Info`

### cardano.node.density

***
The actual number of blocks created over the maximum expected number of blocks that could be created over the span of the last @k@ blocks.
***


Dispatched by: 
ChainDB.AddBlockEvent.SwitchedToAFork

From current configuration:
Filtered  by config value: `Info`

### cardano.node.epoch

***
In which epoch is the tip of the current chain.
***


Dispatched by: 
ChainDB.AddBlockEvent.AddedToCurrentChain

From current configuration:
Filtered  by config value: `Info`

### cardano.node.epoch

***
In which epoch is the tip of the current chain.
***


Dispatched by: 
ChainDB.AddBlockEvent.SwitchedToAFork

From current configuration:
Filtered  by config value: `Info`

### cardano.node.forgedInvalidSlotLast

***

***


Dispatched by: 
Forge.ForgedInvalidBlock

From current configuration:
Filtered  by config value: `Info`

### cardano.node.forgedSlotLast

***

***


Dispatched by: 
Forge.ForgedBlock

From current configuration:
Filtered  by config value: `Info`

### cardano.node.inbound-governor.cold

***

***


Dispatched by: 
InboundGovernor.InboundGovernorCounters

From current configuration:
Filtered  by config value: `Notice`

### cardano.node.inbound-governor.cold

***

***


Dispatched by: 
LocalInboundGovernor.InboundGovernorCounters

From current configuration:
Filtered  by config value: `Notice`

### cardano.node.inbound-governor.hot

***

***


Dispatched by: 
InboundGovernor.InboundGovernorCounters

From current configuration:
Filtered  by config value: `Notice`

### cardano.node.inbound-governor.hot

***

***


Dispatched by: 
LocalInboundGovernor.InboundGovernorCounters

From current configuration:
Filtered  by config value: `Notice`

### cardano.node.inbound-governor.idle

***

***


Dispatched by: 
InboundGovernor.InboundGovernorCounters

From current configuration:
Filtered  by config value: `Notice`

### cardano.node.inbound-governor.idle

***

***


Dispatched by: 
LocalInboundGovernor.InboundGovernorCounters

From current configuration:
Filtered  by config value: `Notice`

### cardano.node.inbound-governor.warm

***

***


Dispatched by: 
InboundGovernor.InboundGovernorCounters

From current configuration:
Filtered  by config value: `Notice`

### cardano.node.inbound-governor.warm

***

***


Dispatched by: 
LocalInboundGovernor.InboundGovernorCounters

From current configuration:
Filtered  by config value: `Notice`

### cardano.node.ledgerState

***

***


Dispatched by: 
Forge.LedgerState

From current configuration:
Filtered  by config value: `Info`

### cardano.node.ledgerView

***

***


Dispatched by: 
Forge.LedgerView

From current configuration:
Filtered  by config value: `Info`

### cardano.node.mempoolBytes

***
Byte size of the mempool
***


Dispatched by: 
Mempool.AddedTx

From current configuration:
Filtered  by config value: `Info`

### cardano.node.mempoolBytes

***
Byte size of the mempool
***


Dispatched by: 
Mempool.ManuallyRemovedTxs

From current configuration:
Filtered  by config value: `Info`

### cardano.node.mempoolBytes

***
Byte size of the mempool
***


Dispatched by: 
Mempool.RejectedTx

From current configuration:
Filtered  by config value: `Info`

### cardano.node.mempoolBytes

***
Byte size of the mempool
***


Dispatched by: 
Mempool.RemoveTxs

From current configuration:
Filtered  by config value: `Info`

### cardano.node.metrics.served.header

***
A counter triggered only on header event
***


Dispatched by: 
ChainSyncServerHeader.ChainSyncServerEvent.Update.Update

From current configuration:
Filtered  by config value: `Notice`

### cardano.node.nodeCannotForge

***

***


Dispatched by: 
Forge.NodeCannotForge

From current configuration:
Filtered  by config value: `Info`

### cardano.node.nodeIsLeader

***

***


Dispatched by: 
Forge.NodeIsLeader

From current configuration:
Filtered  by config value: `Info`

### cardano.node.nodeNotLeader

***

***


Dispatched by: 
Forge.NodeNotLeader

From current configuration:
Filtered  by config value: `Info`

### cardano.node.notAdoptedSlotLast

***

***


Dispatched by: 
Forge.DidntAdoptBlock

From current configuration:
Filtered  by config value: `Info`

### cardano.node.operationalCertificateExpiryKESPeriod

***

***


Dispatched by: 
Forge.ForgeStateUpdateError

From current configuration:
Filtered  by config value: `Info`

### cardano.node.operationalCertificateStartKESPeriod

***

***


Dispatched by: 
Forge.ForgeStateUpdateError

From current configuration:
Filtered  by config value: `Info`

### cardano.node.peerSelection.cold

***
Number of cold peers
***


Dispatched by: 
PeerSelectionCounters.PeerSelectionCounters

From current configuration:
Filtered  by config value: `Notice`

### cardano.node.peerSelection.hot

***
Number of hot peers
***


Dispatched by: 
PeerSelectionCounters.PeerSelectionCounters

From current configuration:
Filtered  by config value: `Notice`

### cardano.node.peerSelection.warm

***
Number of warm peers
***


Dispatched by: 
PeerSelectionCounters.PeerSelectionCounters

From current configuration:
Filtered  by config value: `Notice`

### cardano.node.remainingKESPeriods

***

***


Dispatched by: 
Forge.ForgeStateUpdateError

From current configuration:
Filtered  by config value: `Info`

### cardano.node.served.block

***

***


Dispatched by: 
BlockFetchServer.SendBlock

From current configuration:
Filtered  by config value: `Notice`

### cardano.node.slotInEpoch

***
Relative slot number of the tip of the current chain within theepoch..
***


Dispatched by: 
ChainDB.AddBlockEvent.AddedToCurrentChain

From current configuration:
Filtered  by config value: `Info`

### cardano.node.slotInEpoch

***
Relative slot number of the tip of the current chain within theepoch..
***


Dispatched by: 
ChainDB.AddBlockEvent.SwitchedToAFork

From current configuration:
Filtered  by config value: `Info`

### cardano.node.slotIsImmutable

***

***


Dispatched by: 
Forge.SlotIsImmutable

From current configuration:
Filtered  by config value: `Info`

### cardano.node.slots

***
Number of slots in this chain fragment.
***


Dispatched by: 
ChainDB.AddBlockEvent.AddedToCurrentChain

From current configuration:
Filtered  by config value: `Info`

### cardano.node.slots

***
Number of slots in this chain fragment.
***


Dispatched by: 
ChainDB.AddBlockEvent.SwitchedToAFork

From current configuration:
Filtered  by config value: `Info`

### cardano.node.submissions.accepted

***

***


Dispatched by: 
TxInbound.TxSubmissionProcessed

From current configuration:
Filtered  by config value: `Notice`

### cardano.node.submissions.rejected

***

***


Dispatched by: 
TxInbound.TxSubmissionProcessed

From current configuration:
Filtered  by config value: `Notice`

### cardano.node.submissions.submitted

***

***


Dispatched by: 
TxInbound.TxSubmissionCollected

From current configuration:
Filtered  by config value: `Notice`

### cardano.node.txsInMempool

***
Transactions in mempool
***


Dispatched by: 
Mempool.AddedTx

From current configuration:
Filtered  by config value: `Info`

### cardano.node.txsInMempool

***
Transactions in mempool
***


Dispatched by: 
Mempool.ManuallyRemovedTxs

From current configuration:
Filtered  by config value: `Info`

### cardano.node.txsInMempool

***
Transactions in mempool
***


Dispatched by: 
Mempool.RejectedTx

From current configuration:
Filtered  by config value: `Info`

### cardano.node.txsInMempool

***
Transactions in mempool
***


Dispatched by: 
Mempool.RemoveTxs

From current configuration:
Filtered  by config value: `Info`

### cardano.node.txsProcessedNum

***

***


Dispatched by: 
Mempool.ManuallyRemovedTxs

From current configuration:
Filtered  by config value: `Info`

### cardano.node.utxoSize

***

***


Dispatched by: 
Forge.StartLeadershipCheckPlus

From current configuration:
Filtered  by config value: `Info`

### mem.resident

***
TODO JNF
***


Dispatched by: 
Resources

From current configuration:
Filtered  by config value: `Info`

### nodeCannotForgeNum

***
How many times this node could not forge?
***


Dispatched by: 
ForgeStats

From current configuration:
Filtered  by config value: `Notice`

### nodeIsLeaderNum

***
How many times this node was leader?
***


Dispatched by: 
ForgeStats

From current configuration:
Filtered  by config value: `Notice`

### peersFromNodeKernel

***
TODO Doc
***


Dispatched by: 
Peers

From current configuration:
Filtered  by config value: `Notice`

### rts.gcLiveBytes

***
TODO JNF
***


Dispatched by: 
Resources

From current configuration:
Filtered  by config value: `Info`

### rts.gcMajorNum

***
TODO JNF
***


Dispatched by: 
Resources

From current configuration:
Filtered  by config value: `Info`

### rts.gcMinorNum

***
TODO JNF
***


Dispatched by: 
Resources

From current configuration:
Filtered  by config value: `Info`

### rts.gcticks

***
TODO JNF
***


Dispatched by: 
Resources

From current configuration:
Filtered  by config value: `Info`

### rts.mutticks

***
TODO JNF
***


Dispatched by: 
Resources

From current configuration:
Filtered  by config value: `Info`

### rts.threads

***
TODO JNF
***


Dispatched by: 
Resources

From current configuration:
Filtered  by config value: `Info`

### slotsMissed

***
How many slots were missed in this node?
***


Dispatched by: 
ForgeStats

From current configuration:
Filtered  by config value: `Notice`

### stat.cputicks

***
Reports the CPU ticks, sice the process was started
***


Dispatched by: 
Resources

From current configuration:
Filtered  by config value: `Info`

## Datapoints
### NodeInfo


***
Basic information about this node collected at startup

 _niName_: Name of the node. 
 _niProtocol_: Protocol which this nodes uses. 
 _niVersion_: Software version which this node is using. 
 _niCommit_: Commit this node is built from. 
 _niStartTime_: Start time of this node. 
 _niSystemStartTime_: How long did the start of the node took.
***


### NodePeers


***
Information about peers of this node. Contains a list of _PeerInfoPP_ messages.
***


### NodeState


***
State information about this node. It is presented as a sum of the following states

 _NodeTracingOnlineConfiguring_: Tracing system came online, system configuring now. 
 _NodeOpeningDbs_: ChainDB components being opened. 
 _NodeReplays_: Replaying chain. 
 _NodeInitChainSelection_: Performing initial chain selection. 
 _NodeKernelOnline_: Node kernel online. 
 _NodeAddBlock_: Applying block. 
 _NodeStartup_: Node startup. 
 _NodeShutdown_: Node shutting down.
***


Configuration: TraceConfig {tcOptions = fromList [([],[ConfSeverity {severity = Notice},ConfDetail {detail = DNormal},ConfBackend {backends = [Stdout MachineFormat,EKGBackend,Forwarder]}]),(["AcceptPolicy"],[ConfSeverity {severity = Info}]),(["BlockFetchClient","CompletedBlockFetch"],[ConfLimiter {maxFrequency = 2.0}]),(["ChainDB"],[ConfSeverity {severity = Info}]),(["ChainDB","AddBlockEvent","AddBlockValidation","ValidCandidate"],[ConfLimiter {maxFrequency = 2.0}]),(["ChainDB","AddBlockEvent","AddedBlockToQueue"],[ConfLimiter {maxFrequency = 2.0}]),(["ChainDB","AddBlockEvent","AddedBlockToVolatileDB"],[ConfLimiter {maxFrequency = 2.0}]),(["ChainDB","CopyToImmutableDBEvent","CopiedBlockToImmutableDB"],[ConfLimiter {maxFrequency = 2.0}]),(["DNSResolver"],[ConfSeverity {severity = Info}]),(["DNSSubscription"],[ConfSeverity {severity = Info}]),(["DiffusionInit"],[ConfSeverity {severity = Info}]),(["ErrorPolicy"],[ConfSeverity {severity = Info}]),(["Forge"],[ConfSeverity {severity = Info}]),(["IpSubscription"],[ConfSeverity {severity = Info}]),(["LocalErrorPolicy"],[ConfSeverity {severity = Info}]),(["Mempool"],[ConfSeverity {severity = Info}]),(["Resources"],[ConfSeverity {severity = Info}])], tcForwarder = TraceOptionForwarder {tofConnQueueSize = 2000, tofDisconnQueueSize = 200000, tofVerbosity = Minimum}, tcNodeName = Nothing, tcPeerFrequency = Just 2000, tcResourceFrequency = Just 5000}

670 log messages.
Generated at 2022-07-01 14:37:24.670427671 +04.
