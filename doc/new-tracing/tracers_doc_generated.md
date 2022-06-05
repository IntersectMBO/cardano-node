# Cardano Trace Documentation
# Table Of Contents


## [Trace Messages](#trace-messages)
1. __AcceptPolicy__
	1. [ConnectionHardLimit](#cardanonodeacceptpolicyconnectionhardlimit)
	1. [ConnectionLimitResume](#cardanonodeacceptpolicyconnectionlimitresume)
	1. [ConnectionRateLimiting](#cardanonodeacceptpolicyconnectionratelimiting)
1. __BlockFetch__
	1. __NodeToNode__
		1. __Recieve__
			1. [BatchDone](#cardanonodeblockfetchnodetonoderecievebatchdone)
			1. [Block](#cardanonodeblockfetchnodetonoderecieveblock)
			1. [ClientDone](#cardanonodeblockfetchnodetonoderecieveclientdone)
			1. [NoBlocks](#cardanonodeblockfetchnodetonoderecievenoblocks)
			1. [RequestRange](#cardanonodeblockfetchnodetonoderecieverequestrange)
			1. [StartBatch](#cardanonodeblockfetchnodetonoderecievestartbatch)
		1. __Send__
			1. [BatchDone](#cardanonodeblockfetchnodetonodesendbatchdone)
			1. [Block](#cardanonodeblockfetchnodetonodesendblock)
			1. [ClientDone](#cardanonodeblockfetchnodetonodesendclientdone)
			1. [NoBlocks](#cardanonodeblockfetchnodetonodesendnoblocks)
			1. [RequestRange](#cardanonodeblockfetchnodetonodesendrequestrange)
			1. [StartBatch](#cardanonodeblockfetchnodetonodesendstartbatch)
1. __BlockFetchClient__
	1. [AcknowledgedFetchRequest](#cardanonodeblockfetchclientacknowledgedfetchrequest)
	1. [AddedFetchRequest](#cardanonodeblockfetchclientaddedfetchrequest)
	1. [ClientTerminating](#cardanonodeblockfetchclientclientterminating)
	1. [CompletedBlockFetch](#cardanonodeblockfetchclientcompletedblockfetch)
	1. [CompletedFetchBatch](#cardanonodeblockfetchclientcompletedfetchbatch)
	1. [RejectedFetchBatch](#cardanonodeblockfetchclientrejectedfetchbatch)
	1. [SendFetchRequest](#cardanonodeblockfetchclientsendfetchrequest)
	1. [StartedFetchBatch](#cardanonodeblockfetchclientstartedfetchbatch)
1. [BlockFetchDecision](#cardanonodeblockfetchdecision)
1. __BlockFetchSerialised__
	1. __NodeToNode__
		1. __Recieve__
			1. [BatchDone](#cardanonodeblockfetchserialisednodetonoderecievebatchdone)
			1. [Block](#cardanonodeblockfetchserialisednodetonoderecieveblock)
			1. [ClientDone](#cardanonodeblockfetchserialisednodetonoderecieveclientdone)
			1. [NoBlocks](#cardanonodeblockfetchserialisednodetonoderecievenoblocks)
			1. [RequestRange](#cardanonodeblockfetchserialisednodetonoderecieverequestrange)
			1. [StartBatch](#cardanonodeblockfetchserialisednodetonoderecievestartbatch)
		1. __Send__
			1. [BatchDone](#cardanonodeblockfetchserialisednodetonodesendbatchdone)
			1. [Block](#cardanonodeblockfetchserialisednodetonodesendblock)
			1. [ClientDone](#cardanonodeblockfetchserialisednodetonodesendclientdone)
			1. [NoBlocks](#cardanonodeblockfetchserialisednodetonodesendnoblocks)
			1. [RequestRange](#cardanonodeblockfetchserialisednodetonodesendrequestrange)
			1. [StartBatch](#cardanonodeblockfetchserialisednodetonodesendstartbatch)
1. __BlockFetchServer__
	1. [SendBlock](#cardanonodeblockfetchserversendblock)
1. __BlockchainTime__
	1. [CurrentSlotUnknown](#cardanonodeblockchaintimecurrentslotunknown)
	1. [StartTimeInTheFuture](#cardanonodeblockchaintimestarttimeinthefuture)
	1. [SystemClockMovedBack](#cardanonodeblockchaintimesystemclockmovedback)
1. __ChainDB__
	1. __AddBlockEvent__
		1. __AddBlockValidation__
			1. [CandidateContainsFutureBlocks](#cardanonodechaindbaddblockeventaddblockvalidationcandidatecontainsfutureblocks)
			1. [CandidateContainsFutureBlocksExceedingClockSkew](#cardanonodechaindbaddblockeventaddblockvalidationcandidatecontainsfutureblocksexceedingclockskew)
			1. [InvalidBlock](#cardanonodechaindbaddblockeventaddblockvalidationinvalidblock)
			1. [ValidCandidate](#cardanonodechaindbaddblockeventaddblockvalidationvalidcandidate)
		1. [AddedBlockToQueue](#cardanonodechaindbaddblockeventaddedblocktoqueue)
		1. [AddedBlockToVolatileDB](#cardanonodechaindbaddblockeventaddedblocktovolatiledb)
		1. [AddedToCurrentChain](#cardanonodechaindbaddblockeventaddedtocurrentchain)
		1. [BlockInTheFuture](#cardanonodechaindbaddblockeventblockinthefuture)
		1. [ChainSelectionForFutureBlock](#cardanonodechaindbaddblockeventchainselectionforfutureblock)
		1. [IgnoreBlockAlreadyInVolatileDB](#cardanonodechaindbaddblockeventignoreblockalreadyinvolatiledb)
		1. [IgnoreBlockOlderThanK](#cardanonodechaindbaddblockeventignoreblockolderthank)
		1. [IgnoreInvalidBlock](#cardanonodechaindbaddblockeventignoreinvalidblock)
		1. [StoreButDontChange](#cardanonodechaindbaddblockeventstorebutdontchange)
		1. [SwitchedToAFork](#cardanonodechaindbaddblockeventswitchedtoafork)
		1. [TryAddToCurrentChain](#cardanonodechaindbaddblockeventtryaddtocurrentchain)
		1. [TrySwitchToAFork](#cardanonodechaindbaddblockeventtryswitchtoafork)
	1. __TraceCopyToImmutableDBEvent__
		1. [CopiedBlockToImmutableDB](#cardanonodechaindbtracecopytoimmutabledbeventcopiedblocktoimmutabledb)
		1. [NoBlocksToCopyToImmutableDB](#cardanonodechaindbtracecopytoimmutabledbeventnoblockstocopytoimmutabledb)
	1. __TraceFollowerEvent__
		1. [FollowerNewImmIterator](#cardanonodechaindbtracefollowereventfollowernewimmiterator)
		1. [FollowerNoLongerInMem](#cardanonodechaindbtracefollowereventfollowernolongerinmem)
		1. [FollowerSwitchToMem](#cardanonodechaindbtracefollowereventfollowerswitchtomem)
		1. [NewFollower](#cardanonodechaindbtracefollowereventnewfollower)
	1. __TraceGCEvent__
		1. [PerformedGC](#cardanonodechaindbtracegceventperformedgc)
		1. [ScheduledGC](#cardanonodechaindbtracegceventscheduledgc)
	1. __TraceImmutableDBEvent__
		1. __CacheEvent__
			1. [CurrentChunkHit](#cardanonodechaindbtraceimmutabledbeventcacheeventcurrentchunkhit)
			1. [PastChunkEvict](#cardanonodechaindbtraceimmutabledbeventcacheeventpastchunkevict)
			1. [PastChunkExpired](#cardanonodechaindbtraceimmutabledbeventcacheeventpastchunkexpired)
			1. [PastChunkHit](#cardanonodechaindbtraceimmutabledbeventcacheeventpastchunkhit)
			1. [PastChunkMiss](#cardanonodechaindbtraceimmutabledbeventcacheeventpastchunkmiss)
		1. [ChunkFileDoesntFit](#cardanonodechaindbtraceimmutabledbeventchunkfiledoesntfit)
		1. __ChunkValidation__
			1. [InvalidChunkFile](#cardanonodechaindbtraceimmutabledbeventchunkvalidationinvalidchunkfile)
			1. [InvalidPrimaryIndex](#cardanonodechaindbtraceimmutabledbeventchunkvalidationinvalidprimaryindex)
			1. [InvalidSecondaryIndex](#cardanonodechaindbtraceimmutabledbeventchunkvalidationinvalidsecondaryindex)
			1. [MissingChunkFile](#cardanonodechaindbtraceimmutabledbeventchunkvalidationmissingchunkfile)
			1. [MissingPrimaryIndex](#cardanonodechaindbtraceimmutabledbeventchunkvalidationmissingprimaryindex)
			1. [MissingSecondaryIndex](#cardanonodechaindbtraceimmutabledbeventchunkvalidationmissingsecondaryindex)
			1. [RewritePrimaryIndex](#cardanonodechaindbtraceimmutabledbeventchunkvalidationrewriteprimaryindex)
			1. [RewriteSecondaryIndex](#cardanonodechaindbtraceimmutabledbeventchunkvalidationrewritesecondaryindex)
			1. [StartedValidatingChunk](#cardanonodechaindbtraceimmutabledbeventchunkvalidationstartedvalidatingchunk)
			1. [ValidatedChunk](#cardanonodechaindbtraceimmutabledbeventchunkvalidationvalidatedchunk)
		1. [DBAlreadyClosed](#cardanonodechaindbtraceimmutabledbeventdbalreadyclosed)
		1. [DBClosed](#cardanonodechaindbtraceimmutabledbeventdbclosed)
		1. [DeletingAfter](#cardanonodechaindbtraceimmutabledbeventdeletingafter)
		1. [Migrating](#cardanonodechaindbtraceimmutabledbeventmigrating)
		1. [NoValidLastLocation](#cardanonodechaindbtraceimmutabledbeventnovalidlastlocation)
		1. [ValidatedLastLocation](#cardanonodechaindbtraceimmutabledbeventvalidatedlastlocation)
	1. __TraceInitChainSelEvent__
		1. [CandidateContainsFutureBlocks](#cardanonodechaindbtraceinitchainseleventcandidatecontainsfutureblocks)
		1. [CandidateContainsFutureBlocksExceedingClockSkew](#cardanonodechaindbtraceinitchainseleventcandidatecontainsfutureblocksexceedingclockskew)
		1. [InitalChainSelected](#cardanonodechaindbtraceinitchainseleventinitalchainselected)
		1. [InvalidBlock](#cardanonodechaindbtraceinitchainseleventinvalidblock)
		1. [StartedInitChainSelection](#cardanonodechaindbtraceinitchainseleventstartedinitchainselection)
		1. [UpdateLedgerDb](#cardanonodechaindbtraceinitchainseleventupdateledgerdb)
		1. [ValidCandidate](#cardanonodechaindbtraceinitchainseleventvalidcandidate)
	1. __TraceIteratorEvent__
		1. [BlockGCedFromVolatileDB](#cardanonodechaindbtraceiteratoreventblockgcedfromvolatiledb)
		1. [BlockMissingFromVolatileDB](#cardanonodechaindbtraceiteratoreventblockmissingfromvolatiledb)
		1. [BlockWasCopiedToImmutableDB](#cardanonodechaindbtraceiteratoreventblockwascopiedtoimmutabledb)
		1. [StreamFromBoth](#cardanonodechaindbtraceiteratoreventstreamfromboth)
		1. [StreamFromImmutableDB](#cardanonodechaindbtraceiteratoreventstreamfromimmutabledb)
		1. [StreamFromVolatileDB](#cardanonodechaindbtraceiteratoreventstreamfromvolatiledb)
		1. [SwitchBackToVolatileDB](#cardanonodechaindbtraceiteratoreventswitchbacktovolatiledb)
		1. [UnknownRangeRequested](#cardanonodechaindbtraceiteratoreventunknownrangerequested)
	1. __TraceLedgerEvent__
		1. [DeletedSnapshot](#cardanonodechaindbtraceledgereventdeletedsnapshot)
		1. [InvalidSnapshot](#cardanonodechaindbtraceledgereventinvalidsnapshot)
		1. [TookSnapshot](#cardanonodechaindbtraceledgereventtooksnapshot)
	1. __TraceLedgerReplayEvent__
		1. [ReplayFromGenesis](#cardanonodechaindbtraceledgerreplayeventreplayfromgenesis)
		1. [ReplayFromSnapshot](#cardanonodechaindbtraceledgerreplayeventreplayfromsnapshot)
		1. [ReplayedBlock](#cardanonodechaindbtraceledgerreplayeventreplayedblock)
	1. __TraceOpenEvent__
		1. [ClosedDB](#cardanonodechaindbtraceopeneventcloseddb)
		1. [OpenedDB](#cardanonodechaindbtraceopeneventopeneddb)
		1. [OpenedImmutableDB](#cardanonodechaindbtraceopeneventopenedimmutabledb)
		1. [OpenedLgrDB](#cardanonodechaindbtraceopeneventopenedlgrdb)
		1. [OpenedVolatileDB](#cardanonodechaindbtraceopeneventopenedvolatiledb)
		1. [StartedOpeningDB](#cardanonodechaindbtraceopeneventstartedopeningdb)
		1. [StartedOpeningImmutableDB](#cardanonodechaindbtraceopeneventstartedopeningimmutabledb)
		1. [StartedOpeningLgrDB](#cardanonodechaindbtraceopeneventstartedopeninglgrdb)
		1. [StartedOpeningVolatileDB](#cardanonodechaindbtraceopeneventstartedopeningvolatiledb)
	1. __TraceVolatileDBEvent__
		1. [BlockAlreadyHere](#cardanonodechaindbtracevolatiledbeventblockalreadyhere)
		1. [DBAlreadyClosed](#cardanonodechaindbtracevolatiledbeventdbalreadyclosed)
		1. [InvalidFileNames](#cardanonodechaindbtracevolatiledbeventinvalidfilenames)
		1. [Truncate](#cardanonodechaindbtracevolatiledbeventtruncate)
1. __ChainSync__
	1. __NodeToClient__
		1. __Recieve__
			1. [AwaitReply](#cardanonodechainsyncnodetoclientrecieveawaitreply)
			1. [Done](#cardanonodechainsyncnodetoclientrecievedone)
			1. [FindIntersect](#cardanonodechainsyncnodetoclientrecievefindintersect)
			1. [IntersectFound](#cardanonodechainsyncnodetoclientrecieveintersectfound)
			1. [IntersectNotFound](#cardanonodechainsyncnodetoclientrecieveintersectnotfound)
			1. [RequestNext](#cardanonodechainsyncnodetoclientrecieverequestnext)
			1. [RollBackward](#cardanonodechainsyncnodetoclientrecieverollbackward)
			1. [RollForward](#cardanonodechainsyncnodetoclientrecieverollforward)
		1. __Send__
			1. [AwaitReply](#cardanonodechainsyncnodetoclientsendawaitreply)
			1. [Done](#cardanonodechainsyncnodetoclientsenddone)
			1. [FindIntersect](#cardanonodechainsyncnodetoclientsendfindintersect)
			1. [IntersectFound](#cardanonodechainsyncnodetoclientsendintersectfound)
			1. [IntersectNotFound](#cardanonodechainsyncnodetoclientsendintersectnotfound)
			1. [RequestNext](#cardanonodechainsyncnodetoclientsendrequestnext)
			1. [RollBackward](#cardanonodechainsyncnodetoclientsendrollbackward)
			1. [RollForward](#cardanonodechainsyncnodetoclientsendrollforward)
1. __ChainSyncClient__
	1. __ChainSyncClientEvent__
		1. [DownloadedHeader](#cardanonodechainsyncclientchainsyncclienteventdownloadedheader)
		1. [Exception](#cardanonodechainsyncclientchainsyncclienteventexception)
		1. [FoundIntersection](#cardanonodechainsyncclientchainsyncclienteventfoundintersection)
		1. [RolledBack](#cardanonodechainsyncclientchainsyncclienteventrolledback)
		1. [Termination](#cardanonodechainsyncclientchainsyncclienteventtermination)
1. __ChainSyncNode__
	1. __NodeToNode__
		1. __Recieve__
			1. [AwaitReply](#cardanonodechainsyncnodenodetonoderecieveawaitreply)
			1. [Done](#cardanonodechainsyncnodenodetonoderecievedone)
			1. [FindIntersect](#cardanonodechainsyncnodenodetonoderecievefindintersect)
			1. [IntersectFound](#cardanonodechainsyncnodenodetonoderecieveintersectfound)
			1. [IntersectNotFound](#cardanonodechainsyncnodenodetonoderecieveintersectnotfound)
			1. [RequestNext](#cardanonodechainsyncnodenodetonoderecieverequestnext)
			1. [RollBackward](#cardanonodechainsyncnodenodetonoderecieverollbackward)
			1. [RollForward](#cardanonodechainsyncnodenodetonoderecieverollforward)
		1. __Send__
			1. [AwaitReply](#cardanonodechainsyncnodenodetonodesendawaitreply)
			1. [Done](#cardanonodechainsyncnodenodetonodesenddone)
			1. [FindIntersect](#cardanonodechainsyncnodenodetonodesendfindintersect)
			1. [IntersectFound](#cardanonodechainsyncnodenodetonodesendintersectfound)
			1. [IntersectNotFound](#cardanonodechainsyncnodenodetonodesendintersectnotfound)
			1. [RequestNext](#cardanonodechainsyncnodenodetonodesendrequestnext)
			1. [RollBackward](#cardanonodechainsyncnodenodetonodesendrollbackward)
			1. [RollForward](#cardanonodechainsyncnodenodetonodesendrollforward)
1. __ChainSyncSerialised__
	1. __NodeToNode__
		1. __Recieve__
			1. [AwaitReply](#cardanonodechainsyncserialisednodetonoderecieveawaitreply)
			1. [Done](#cardanonodechainsyncserialisednodetonoderecievedone)
			1. [FindIntersect](#cardanonodechainsyncserialisednodetonoderecievefindintersect)
			1. [IntersectFound](#cardanonodechainsyncserialisednodetonoderecieveintersectfound)
			1. [IntersectNotFound](#cardanonodechainsyncserialisednodetonoderecieveintersectnotfound)
			1. [RequestNext](#cardanonodechainsyncserialisednodetonoderecieverequestnext)
			1. [RollBackward](#cardanonodechainsyncserialisednodetonoderecieverollbackward)
			1. [RollForward](#cardanonodechainsyncserialisednodetonoderecieverollforward)
		1. __Send__
			1. [AwaitReply](#cardanonodechainsyncserialisednodetonodesendawaitreply)
			1. [Done](#cardanonodechainsyncserialisednodetonodesenddone)
			1. [FindIntersect](#cardanonodechainsyncserialisednodetonodesendfindintersect)
			1. [IntersectFound](#cardanonodechainsyncserialisednodetonodesendintersectfound)
			1. [IntersectNotFound](#cardanonodechainsyncserialisednodetonodesendintersectnotfound)
			1. [RequestNext](#cardanonodechainsyncserialisednodetonodesendrequestnext)
			1. [RollBackward](#cardanonodechainsyncserialisednodetonodesendrollbackward)
			1. [RollForward](#cardanonodechainsyncserialisednodetonodesendrollforward)
1. __ChainSyncServerBlock__
	1. __ChainSyncServerEvent__
		1. __ServerRead__
			1. [RollBackward](#cardanonodechainsyncserverblockchainsyncservereventserverreadrollbackward)
			1. [RollForward](#cardanonodechainsyncserverblockchainsyncservereventserverreadrollforward)
			1. [ServerRead](#cardanonodechainsyncserverblockchainsyncservereventserverreadserverread)
			1. [ServerReadBlocked](#cardanonodechainsyncserverblockchainsyncservereventserverreadserverreadblocked)
1. __ChainSyncServerHeader__
	1. __ChainSyncServerEvent__
		1. __ServerRead__
			1. [RollBackward](#cardanonodechainsyncserverheaderchainsyncservereventserverreadrollbackward)
			1. [RollForward](#cardanonodechainsyncserverheaderchainsyncservereventserverreadrollforward)
			1. [ServerRead](#cardanonodechainsyncserverheaderchainsyncservereventserverreadserverread)
			1. [ServerReadBlocked](#cardanonodechainsyncserverheaderchainsyncservereventserverreadserverreadblocked)
1. __ConnectionManager__
	1. [Connect](#cardanonodeconnectionmanagerconnect)
	1. [ConnectError](#cardanonodeconnectionmanagerconnecterror)
	1. [ConnectionCleanup](#cardanonodeconnectionmanagerconnectioncleanup)
	1. [ConnectionExists](#cardanonodeconnectionmanagerconnectionexists)
	1. [ConnectionFailure](#cardanonodeconnectionmanagerconnectionfailure)
	1. [ConnectionHandler](#cardanonodeconnectionmanagerconnectionhandler)
	1. [ConnectionManagerCounters](#cardanonodeconnectionmanagerconnectionmanagercounters)
	1. [ConnectionNotFound](#cardanonodeconnectionmanagerconnectionnotfound)
	1. [ConnectionTimeWait](#cardanonodeconnectionmanagerconnectiontimewait)
	1. [ConnectionTimeWaitDone](#cardanonodeconnectionmanagerconnectiontimewaitdone)
	1. [ForbiddenConnection](#cardanonodeconnectionmanagerforbiddenconnection)
	1. [ForbiddenOperation](#cardanonodeconnectionmanagerforbiddenoperation)
	1. [ImpossibleConnection](#cardanonodeconnectionmanagerimpossibleconnection)
	1. [IncludeConnection](#cardanonodeconnectionmanagerincludeconnection)
	1. [PruneConnections](#cardanonodeconnectionmanagerpruneconnections)
	1. [Shutdown](#cardanonodeconnectionmanagershutdown)
	1. [State](#cardanonodeconnectionmanagerstate)
	1. [TerminatedConnection](#cardanonodeconnectionmanagerterminatedconnection)
	1. [TerminatingConnection](#cardanonodeconnectionmanagerterminatingconnection)
	1. [UnexpectedlyFalseAssertion](#cardanonodeconnectionmanagerunexpectedlyfalseassertion)
	1. [UnknownConnection](#cardanonodeconnectionmanagerunknownconnection)
	1. [UnregisterConnection](#cardanonodeconnectionmanagerunregisterconnection)
1. __ConnectionManagerTransition__
	1. [ConnectionManagerTransition](#cardanonodeconnectionmanagertransitionconnectionmanagertransition)
1. __DNSResolver__
	1. [LookupAAAAError](#cardanonodednsresolverlookupaaaaerror)
	1. [LookupAAAAResult](#cardanonodednsresolverlookupaaaaresult)
	1. [LookupAError](#cardanonodednsresolverlookupaerror)
	1. [LookupAResult](#cardanonodednsresolverlookuparesult)
	1. [LookupException](#cardanonodednsresolverlookupexception)
	1. [LookupIPv4First](#cardanonodednsresolverlookupipv4first)
	1. [LookupIPv6First](#cardanonodednsresolverlookupipv6first)
1. __DNSSubscription__
	1. __DNS__
		1. [AllocateSocket](#cardanonodednssubscriptiondnsallocatesocket)
		1. [ApplicationException](#cardanonodednssubscriptiondnsapplicationexception)
		1. [CloseSocket](#cardanonodednssubscriptiondnsclosesocket)
		1. [ConnectEnd](#cardanonodednssubscriptiondnsconnectend)
		1. [ConnectException](#cardanonodednssubscriptiondnsconnectexception)
		1. [ConnectStart](#cardanonodednssubscriptiondnsconnectstart)
		1. [ConnectionExist](#cardanonodednssubscriptiondnsconnectionexist)
		1. [MissingLocalAddress](#cardanonodednssubscriptiondnsmissinglocaladdress)
		1. [Restart](#cardanonodednssubscriptiondnsrestart)
		1. [SkippingPeer](#cardanonodednssubscriptiondnsskippingpeer)
		1. [SocketAllocationException](#cardanonodednssubscriptiondnssocketallocationexception)
		1. [Start](#cardanonodednssubscriptiondnsstart)
		1. [SubscriptionFailed](#cardanonodednssubscriptiondnssubscriptionfailed)
		1. [SubscriptionRunning](#cardanonodednssubscriptiondnssubscriptionrunning)
		1. [SubscriptionWaiting](#cardanonodednssubscriptiondnssubscriptionwaiting)
		1. [SubscriptionWaitingNewConnection](#cardanonodednssubscriptiondnssubscriptionwaitingnewconnection)
		1. [TryConnectToPeer](#cardanonodednssubscriptiondnstryconnecttopeer)
		1. [UnsupportedRemoteAddr](#cardanonodednssubscriptiondnsunsupportedremoteaddr)
1. __DebugPeerSelection__
	1. __DebugPeerSelection__
		1. [GovernorState](#cardanonodedebugpeerselectiondebugpeerselectiongovernorstate)
1. __DebugPeerSelectionResponder__
	1. __DebugPeerSelection__
		1. [GovernorState](#cardanonodedebugpeerselectionresponderdebugpeerselectiongovernorstate)
1. __DiffusionInit__
	1. [ConfiguringLocalSocket](#cardanonodediffusioninitconfiguringlocalsocket)
	1. [ConfiguringServerSocket](#cardanonodediffusioninitconfiguringserversocket)
	1. [CreateSystemdSocketForSnocketPath](#cardanonodediffusioninitcreatesystemdsocketforsnocketpath)
	1. [CreatedLocalSocket](#cardanonodediffusioninitcreatedlocalsocket)
	1. [CreatingServerSocket](#cardanonodediffusioninitcreatingserversocket)
	1. [DiffusionErrored](#cardanonodediffusioninitdiffusionerrored)
	1. [ListeningLocalSocket](#cardanonodediffusioninitlisteninglocalsocket)
	1. [ListeningServerSocket](#cardanonodediffusioninitlisteningserversocket)
	1. [LocalSocketUp](#cardanonodediffusioninitlocalsocketup)
	1. [RunLocalServer](#cardanonodediffusioninitrunlocalserver)
	1. [RunServer](#cardanonodediffusioninitrunserver)
	1. [ServerSocketUp](#cardanonodediffusioninitserversocketup)
	1. [UnsupportedLocalSystemdSocket](#cardanonodediffusioninitunsupportedlocalsystemdsocket)
	1. [UnsupportedReadySocketCase](#cardanonodediffusioninitunsupportedreadysocketcase)
	1. [UsingSystemdSocket](#cardanonodediffusioninitusingsystemdsocket)
1. __ErrorPolicy__
	1. [AcceptException](#cardanonodeerrorpolicyacceptexception)
	1. [KeepSuspended](#cardanonodeerrorpolicykeepsuspended)
	1. [LocalNodeError](#cardanonodeerrorpolicylocalnodeerror)
	1. [ResumeConsumer](#cardanonodeerrorpolicyresumeconsumer)
	1. [ResumePeer](#cardanonodeerrorpolicyresumepeer)
	1. [ResumeProducer](#cardanonodeerrorpolicyresumeproducer)
	1. [SuspendConsumer](#cardanonodeerrorpolicysuspendconsumer)
	1. [SuspendPeer](#cardanonodeerrorpolicysuspendpeer)
	1. [UnhandledApplicationException](#cardanonodeerrorpolicyunhandledapplicationexception)
	1. [UnhandledConnectionException](#cardanonodeerrorpolicyunhandledconnectionexception)
1. __Forge__
	1. [AdoptedBlock](#cardanonodeforgeadoptedblock)
	1. [BlockContext](#cardanonodeforgeblockcontext)
	1. [BlockFromFuture](#cardanonodeforgeblockfromfuture)
	1. [DidntAdoptBlock](#cardanonodeforgedidntadoptblock)
	1. [ForgeStateUpdateError](#cardanonodeforgeforgestateupdateerror)
	1. [ForgedBlock](#cardanonodeforgeforgedblock)
	1. [ForgedInvalidBlock](#cardanonodeforgeforgedinvalidblock)
	1. [LedgerState](#cardanonodeforgeledgerstate)
	1. [LedgerView](#cardanonodeforgeledgerview)
	1. [NoLedgerState](#cardanonodeforgenoledgerstate)
	1. [NoLedgerView](#cardanonodeforgenoledgerview)
	1. [NodeCannotForge](#cardanonodeforgenodecannotforge)
	1. [NodeIsLeader](#cardanonodeforgenodeisleader)
	1. [NodeNotLeader](#cardanonodeforgenodenotleader)
	1. [SlotIsImmutable](#cardanonodeforgeslotisimmutable)
	1. [StartLeadershipCheck](#cardanonodeforgestartleadershipcheck)
	1. [StartLeadershipCheckPlus](#cardanonodeforgestartleadershipcheckplus)
1. [ForgeStateInfo](#cardanonodeforgestateinfo)
1. [ForgeStats](#cardanonodeforgestats)
1. __Handshake__
	1. __Receive__
		1. [AcceptVersion](#cardanonodehandshakereceiveacceptversion)
		1. [ProposeVersions](#cardanonodehandshakereceiveproposeversions)
		1. [Refuse](#cardanonodehandshakereceiverefuse)
		1. [ReplyVersions](#cardanonodehandshakereceivereplyversions)
	1. __Send__
		1. [AcceptVersion](#cardanonodehandshakesendacceptversion)
		1. [ProposeVersions](#cardanonodehandshakesendproposeversions)
		1. [Refuse](#cardanonodehandshakesendrefuse)
		1. [ReplyVersions](#cardanonodehandshakesendreplyversions)
1. __InboundGovernor__
	1. [DemotedToColdRemote](#cardanonodeinboundgovernordemotedtocoldremote)
	1. [DemotedToWarmRemote](#cardanonodeinboundgovernordemotedtowarmremote)
	1. [InboundGovernorCounters](#cardanonodeinboundgovernorinboundgovernorcounters)
	1. [InboundGovernorError](#cardanonodeinboundgovernorinboundgovernorerror)
	1. [MuxCleanExit](#cardanonodeinboundgovernormuxcleanexit)
	1. [MuxErrored](#cardanonodeinboundgovernormuxerrored)
	1. [NewConnection](#cardanonodeinboundgovernornewconnection)
	1. [PromotedToHotRemote](#cardanonodeinboundgovernorpromotedtohotremote)
	1. [PromotedToWarmRemote](#cardanonodeinboundgovernorpromotedtowarmremote)
	1. [RemoteState](#cardanonodeinboundgovernorremotestate)
	1. [ResponderErrored](#cardanonodeinboundgovernorrespondererrored)
	1. [ResponderRestarted](#cardanonodeinboundgovernorresponderrestarted)
	1. [ResponderStartFailure](#cardanonodeinboundgovernorresponderstartfailure)
	1. [ResponderStarted](#cardanonodeinboundgovernorresponderstarted)
	1. [ResponderTerminated](#cardanonodeinboundgovernorresponderterminated)
	1. [UnexpectedlyFalseAssertion](#cardanonodeinboundgovernorunexpectedlyfalseassertion)
	1. [WaitIdleRemote](#cardanonodeinboundgovernorwaitidleremote)
1. __InboundGovernorTransition__
	1. [InboundGovernorTransition](#cardanonodeinboundgovernortransitioninboundgovernortransition)
1. __IpSubscription__
	1. __IP__
		1. [AllocateSocket](#cardanonodeipsubscriptionipallocatesocket)
		1. [ApplicationException](#cardanonodeipsubscriptionipapplicationexception)
		1. [CloseSocket](#cardanonodeipsubscriptionipclosesocket)
		1. [ConnectEnd](#cardanonodeipsubscriptionipconnectend)
		1. [ConnectException](#cardanonodeipsubscriptionipconnectexception)
		1. [ConnectStart](#cardanonodeipsubscriptionipconnectstart)
		1. [ConnectionExist](#cardanonodeipsubscriptionipconnectionexist)
		1. [MissingLocalAddress](#cardanonodeipsubscriptionipmissinglocaladdress)
		1. [Restart](#cardanonodeipsubscriptioniprestart)
		1. [SkippingPeer](#cardanonodeipsubscriptionipskippingpeer)
		1. [SocketAllocationException](#cardanonodeipsubscriptionipsocketallocationexception)
		1. [Start](#cardanonodeipsubscriptionipstart)
		1. [SubscriptionFailed](#cardanonodeipsubscriptionipsubscriptionfailed)
		1. [SubscriptionRunning](#cardanonodeipsubscriptionipsubscriptionrunning)
		1. [SubscriptionWaiting](#cardanonodeipsubscriptionipsubscriptionwaiting)
		1. [SubscriptionWaitingNewConnection](#cardanonodeipsubscriptionipsubscriptionwaitingnewconnection)
		1. [TryConnectToPeer](#cardanonodeipsubscriptioniptryconnecttopeer)
		1. [UnsupportedRemoteAddr](#cardanonodeipsubscriptionipunsupportedremoteaddr)
1. [KeepAliveClient](#cardanonodekeepaliveclient)
1. __LedgerPeers__
	1. [DisabledLedgerPeers](#cardanonodeledgerpeersdisabledledgerpeers)
	1. [FallingBackToBootstrapPeers](#cardanonodeledgerpeersfallingbacktobootstrappeers)
	1. [FetchingNewLedgerState](#cardanonodeledgerpeersfetchingnewledgerstate)
	1. [PickedPeer](#cardanonodeledgerpeerspickedpeer)
	1. [PickedPeers](#cardanonodeledgerpeerspickedpeers)
	1. [RequestForPeers](#cardanonodeledgerpeersrequestforpeers)
	1. [ReusingLedgerState](#cardanonodeledgerpeersreusingledgerstate)
	1. [TraceUseLedgerAfter](#cardanonodeledgerpeerstraceuseledgerafter)
	1. [WaitingOnRequest](#cardanonodeledgerpeerswaitingonrequest)
1. __LocalConnectionManager__
	1. [Connect](#cardanonodelocalconnectionmanagerconnect)
	1. [ConnectError](#cardanonodelocalconnectionmanagerconnecterror)
	1. [ConnectionCleanup](#cardanonodelocalconnectionmanagerconnectioncleanup)
	1. [ConnectionExists](#cardanonodelocalconnectionmanagerconnectionexists)
	1. [ConnectionFailure](#cardanonodelocalconnectionmanagerconnectionfailure)
	1. [ConnectionHandler](#cardanonodelocalconnectionmanagerconnectionhandler)
	1. [ConnectionManagerCounters](#cardanonodelocalconnectionmanagerconnectionmanagercounters)
	1. [ConnectionNotFound](#cardanonodelocalconnectionmanagerconnectionnotfound)
	1. [ConnectionTimeWait](#cardanonodelocalconnectionmanagerconnectiontimewait)
	1. [ConnectionTimeWaitDone](#cardanonodelocalconnectionmanagerconnectiontimewaitdone)
	1. [ForbiddenConnection](#cardanonodelocalconnectionmanagerforbiddenconnection)
	1. [ForbiddenOperation](#cardanonodelocalconnectionmanagerforbiddenoperation)
	1. [ImpossibleConnection](#cardanonodelocalconnectionmanagerimpossibleconnection)
	1. [IncludeConnection](#cardanonodelocalconnectionmanagerincludeconnection)
	1. [PruneConnections](#cardanonodelocalconnectionmanagerpruneconnections)
	1. [Shutdown](#cardanonodelocalconnectionmanagershutdown)
	1. [State](#cardanonodelocalconnectionmanagerstate)
	1. [TerminatedConnection](#cardanonodelocalconnectionmanagerterminatedconnection)
	1. [TerminatingConnection](#cardanonodelocalconnectionmanagerterminatingconnection)
	1. [UnexpectedlyFalseAssertion](#cardanonodelocalconnectionmanagerunexpectedlyfalseassertion)
	1. [UnknownConnection](#cardanonodelocalconnectionmanagerunknownconnection)
	1. [UnregisterConnection](#cardanonodelocalconnectionmanagerunregisterconnection)
1. __LocalErrorPolicy__
	1. [AcceptException](#cardanonodelocalerrorpolicyacceptexception)
	1. [KeepSuspended](#cardanonodelocalerrorpolicykeepsuspended)
	1. [LocalNodeError](#cardanonodelocalerrorpolicylocalnodeerror)
	1. [ResumeConsumer](#cardanonodelocalerrorpolicyresumeconsumer)
	1. [ResumePeer](#cardanonodelocalerrorpolicyresumepeer)
	1. [ResumeProducer](#cardanonodelocalerrorpolicyresumeproducer)
	1. [SuspendConsumer](#cardanonodelocalerrorpolicysuspendconsumer)
	1. [SuspendPeer](#cardanonodelocalerrorpolicysuspendpeer)
	1. [UnhandledApplicationException](#cardanonodelocalerrorpolicyunhandledapplicationexception)
	1. [UnhandledConnectionException](#cardanonodelocalerrorpolicyunhandledconnectionexception)
1. __LocalHandshake__
	1. __Receive__
		1. [AcceptVersion](#cardanonodelocalhandshakereceiveacceptversion)
		1. [ProposeVersions](#cardanonodelocalhandshakereceiveproposeversions)
		1. [Refuse](#cardanonodelocalhandshakereceiverefuse)
		1. [ReplyVersions](#cardanonodelocalhandshakereceivereplyversions)
	1. __Send__
		1. [AcceptVersion](#cardanonodelocalhandshakesendacceptversion)
		1. [ProposeVersions](#cardanonodelocalhandshakesendproposeversions)
		1. [Refuse](#cardanonodelocalhandshakesendrefuse)
		1. [ReplyVersions](#cardanonodelocalhandshakesendreplyversions)
1. __LocalInboundGovernor__
	1. [DemotedToColdRemote](#cardanonodelocalinboundgovernordemotedtocoldremote)
	1. [DemotedToWarmRemote](#cardanonodelocalinboundgovernordemotedtowarmremote)
	1. [InboundGovernorCounters](#cardanonodelocalinboundgovernorinboundgovernorcounters)
	1. [InboundGovernorError](#cardanonodelocalinboundgovernorinboundgovernorerror)
	1. [MuxCleanExit](#cardanonodelocalinboundgovernormuxcleanexit)
	1. [MuxErrored](#cardanonodelocalinboundgovernormuxerrored)
	1. [NewConnection](#cardanonodelocalinboundgovernornewconnection)
	1. [PromotedToHotRemote](#cardanonodelocalinboundgovernorpromotedtohotremote)
	1. [PromotedToWarmRemote](#cardanonodelocalinboundgovernorpromotedtowarmremote)
	1. [RemoteState](#cardanonodelocalinboundgovernorremotestate)
	1. [ResponderErrored](#cardanonodelocalinboundgovernorrespondererrored)
	1. [ResponderRestarted](#cardanonodelocalinboundgovernorresponderrestarted)
	1. [ResponderStartFailure](#cardanonodelocalinboundgovernorresponderstartfailure)
	1. [ResponderStarted](#cardanonodelocalinboundgovernorresponderstarted)
	1. [ResponderTerminated](#cardanonodelocalinboundgovernorresponderterminated)
	1. [UnexpectedlyFalseAssertion](#cardanonodelocalinboundgovernorunexpectedlyfalseassertion)
	1. [WaitIdleRemote](#cardanonodelocalinboundgovernorwaitidleremote)
1. __LocalRootPeers__
	1. [LocalRootDomains](#cardanonodelocalrootpeerslocalrootdomains)
	1. [LocalRootError](#cardanonodelocalrootpeerslocalrooterror)
	1. [LocalRootFailure](#cardanonodelocalrootpeerslocalrootfailure)
	1. [LocalRootGroups](#cardanonodelocalrootpeerslocalrootgroups)
	1. [LocalRootResult](#cardanonodelocalrootpeerslocalrootresult)
	1. [LocalRootWaiting](#cardanonodelocalrootpeerslocalrootwaiting)
1. __LocalServer__
	1. [AcceptConnection](#cardanonodelocalserveracceptconnection)
	1. [AcceptError](#cardanonodelocalserveraccepterror)
	1. [AcceptPolicy](#cardanonodelocalserveracceptpolicy)
	1. [Error](#cardanonodelocalservererror)
	1. [Started](#cardanonodelocalserverstarted)
	1. [Stopped](#cardanonodelocalserverstopped)
1. __LocalTxSubmissionServer__
	1. [ReceivedTx](#cardanonodelocaltxsubmissionserverreceivedtx)
1. __Mempool__
	1. [AddedTx](#cardanonodemempooladdedtx)
	1. [ManuallyRemovedTxs](#cardanonodemempoolmanuallyremovedtxs)
	1. [RejectedTx](#cardanonodemempoolrejectedtx)
	1. [RemoveTxs](#cardanonodemempoolremovetxs)
1. __Mux__
	1. [ChannelRecvEnd](#cardanonodemuxchannelrecvend)
	1. [ChannelRecvStart](#cardanonodemuxchannelrecvstart)
	1. [ChannelSendEnd](#cardanonodemuxchannelsendend)
	1. [ChannelSendStart](#cardanonodemuxchannelsendstart)
	1. [CleanExit](#cardanonodemuxcleanexit)
	1. [ExceptionExit](#cardanonodemuxexceptionexit)
	1. [HandshakeClientEnd](#cardanonodemuxhandshakeclientend)
	1. [HandshakeClientError](#cardanonodemuxhandshakeclienterror)
	1. [HandshakeServerEnd](#cardanonodemuxhandshakeserverend)
	1. [HandshakeServerError](#cardanonodemuxhandshakeservererror)
	1. [HandshakeStart](#cardanonodemuxhandshakestart)
	1. [RecvDeltaQObservation](#cardanonodemuxrecvdeltaqobservation)
	1. [RecvDeltaQSample](#cardanonodemuxrecvdeltaqsample)
	1. [RecvEnd](#cardanonodemuxrecvend)
	1. [RecvHeaderEnd](#cardanonodemuxrecvheaderend)
	1. [RecvHeaderStart](#cardanonodemuxrecvheaderstart)
	1. [RecvStart](#cardanonodemuxrecvstart)
	1. [SDUReadTimeoutException](#cardanonodemuxsdureadtimeoutexception)
	1. [SDUWriteTimeoutException](#cardanonodemuxsduwritetimeoutexception)
	1. [SendEnd](#cardanonodemuxsendend)
	1. [SendStart](#cardanonodemuxsendstart)
	1. [Shutdown](#cardanonodemuxshutdown)
	1. [StartEagerly](#cardanonodemuxstarteagerly)
	1. [StartOnDemand](#cardanonodemuxstartondemand)
	1. [StartedOnDemand](#cardanonodemuxstartedondemand)
	1. [State](#cardanonodemuxstate)
	1. [TCPInfo](#cardanonodemuxtcpinfo)
	1. [Terminating](#cardanonodemuxterminating)
1. __MuxLocal__
	1. [ChannelRecvEnd](#cardanonodemuxlocalchannelrecvend)
	1. [ChannelRecvStart](#cardanonodemuxlocalchannelrecvstart)
	1. [ChannelSendEnd](#cardanonodemuxlocalchannelsendend)
	1. [ChannelSendStart](#cardanonodemuxlocalchannelsendstart)
	1. [CleanExit](#cardanonodemuxlocalcleanexit)
	1. [ExceptionExit](#cardanonodemuxlocalexceptionexit)
	1. [HandshakeClientEnd](#cardanonodemuxlocalhandshakeclientend)
	1. [HandshakeClientError](#cardanonodemuxlocalhandshakeclienterror)
	1. [HandshakeServerEnd](#cardanonodemuxlocalhandshakeserverend)
	1. [HandshakeServerError](#cardanonodemuxlocalhandshakeservererror)
	1. [HandshakeStart](#cardanonodemuxlocalhandshakestart)
	1. [RecvDeltaQObservation](#cardanonodemuxlocalrecvdeltaqobservation)
	1. [RecvDeltaQSample](#cardanonodemuxlocalrecvdeltaqsample)
	1. [RecvEnd](#cardanonodemuxlocalrecvend)
	1. [RecvHeaderEnd](#cardanonodemuxlocalrecvheaderend)
	1. [RecvHeaderStart](#cardanonodemuxlocalrecvheaderstart)
	1. [RecvStart](#cardanonodemuxlocalrecvstart)
	1. [SDUReadTimeoutException](#cardanonodemuxlocalsdureadtimeoutexception)
	1. [SDUWriteTimeoutException](#cardanonodemuxlocalsduwritetimeoutexception)
	1. [SendEnd](#cardanonodemuxlocalsendend)
	1. [SendStart](#cardanonodemuxlocalsendstart)
	1. [Shutdown](#cardanonodemuxlocalshutdown)
	1. [StartEagerly](#cardanonodemuxlocalstarteagerly)
	1. [StartOnDemand](#cardanonodemuxlocalstartondemand)
	1. [StartedOnDemand](#cardanonodemuxlocalstartedondemand)
	1. [State](#cardanonodemuxlocalstate)
	1. [TCPInfo](#cardanonodemuxlocaltcpinfo)
	1. [Terminating](#cardanonodemuxlocalterminating)
1. __PeerSelection__
	1. [ChurnMode](#cardanonodepeerselectionchurnmode)
	1. [ChurnWait](#cardanonodepeerselectionchurnwait)
	1. [DemoteAsynchronous](#cardanonodepeerselectiondemoteasynchronous)
	1. [DemoteHotDone](#cardanonodepeerselectiondemotehotdone)
	1. [DemoteHotFailed](#cardanonodepeerselectiondemotehotfailed)
	1. [DemoteHotPeers](#cardanonodepeerselectiondemotehotpeers)
	1. [DemoteLocalHotPeers](#cardanonodepeerselectiondemotelocalhotpeers)
	1. [DemoteWarmDone](#cardanonodepeerselectiondemotewarmdone)
	1. [DemoteWarmFailed](#cardanonodepeerselectiondemotewarmfailed)
	1. [DemoteWarmPeers](#cardanonodepeerselectiondemotewarmpeers)
	1. [ForgetColdPeers](#cardanonodepeerselectionforgetcoldpeers)
	1. [GossipRequests](#cardanonodepeerselectiongossiprequests)
	1. [GossipResults](#cardanonodepeerselectiongossipresults)
	1. [GovernorWakeup](#cardanonodepeerselectiongovernorwakeup)
	1. [LocalRootPeersChanged](#cardanonodepeerselectionlocalrootpeerschanged)
	1. [PromoteColdDone](#cardanonodepeerselectionpromotecolddone)
	1. [PromoteColdFailed](#cardanonodepeerselectionpromotecoldfailed)
	1. [PromoteColdLocalPeers](#cardanonodepeerselectionpromotecoldlocalpeers)
	1. [PromoteColdPeers](#cardanonodepeerselectionpromotecoldpeers)
	1. [PromoteWarmAborted](#cardanonodepeerselectionpromotewarmaborted)
	1. [PromoteWarmDone](#cardanonodepeerselectionpromotewarmdone)
	1. [PromoteWarmFailed](#cardanonodepeerselectionpromotewarmfailed)
	1. [PromoteWarmLocalPeers](#cardanonodepeerselectionpromotewarmlocalpeers)
	1. [PromoteWarmPeers](#cardanonodepeerselectionpromotewarmpeers)
	1. [PublicRootsFailure](#cardanonodepeerselectionpublicrootsfailure)
	1. [PublicRootsRequest](#cardanonodepeerselectionpublicrootsrequest)
	1. [PublicRootsResults](#cardanonodepeerselectionpublicrootsresults)
	1. [TargetsChanged](#cardanonodepeerselectiontargetschanged)
1. __PeerSelectionActions__
	1. [MonitoringError](#cardanonodepeerselectionactionsmonitoringerror)
	1. [MonitoringResult](#cardanonodepeerselectionactionsmonitoringresult)
	1. [StatusChangeFailure](#cardanonodepeerselectionactionsstatuschangefailure)
	1. [StatusChanged](#cardanonodepeerselectionactionsstatuschanged)
1. __PeerSelectionCounters__
	1. [PeerSelectionCounters](#cardanonodepeerselectioncounterspeerselectioncounters)
1. [Peers](#cardanonodepeers)
1. __PublicRootPeers__
	1. __PublicRootPeers__
		1. [PublicRootDomains](#cardanonodepublicrootpeerspublicrootpeerspublicrootdomains)
		1. [PublicRootFailure](#cardanonodepublicrootpeerspublicrootpeerspublicrootfailure)
		1. [PublicRootRelayAccessPoint](#cardanonodepublicrootpeerspublicrootpeerspublicrootrelayaccesspoint)
		1. [PublicRootResult](#cardanonodepublicrootpeerspublicrootpeerspublicrootresult)
1. __ReplayBlock__
	1. [LedgerReplay](#cardanonodereplayblockledgerreplay)
1. [Resources](#cardanonoderesources)
1. __Server__
	1. [AcceptConnection](#cardanonodeserveracceptconnection)
	1. [AcceptError](#cardanonodeserveraccepterror)
	1. [AcceptPolicy](#cardanonodeserveracceptpolicy)
	1. [Error](#cardanonodeservererror)
	1. [Started](#cardanonodeserverstarted)
	1. [Stopped](#cardanonodeserverstopped)
1. __Shutdown__
	1. [AbnormalShutdown](#cardanonodeshutdownabnormalshutdown)
	1. [RequestingShutdown](#cardanonodeshutdownrequestingshutdown)
	1. [ShutdownArmedAtSlotBlock](#cardanonodeshutdownshutdownarmedatslotblock)
	1. [ShutdownRequested](#cardanonodeshutdownshutdownrequested)
	1. [ShutdownUnexpectedInput](#cardanonodeshutdownshutdownunexpectedinput)
1. __Startup__
	1. [Byron](#cardanonodestartupbyron)
	1. [Common](#cardanonodestartupcommon)
	1. [Network](#cardanonodestartupnetwork)
	1. [NetworkConfig](#cardanonodestartupnetworkconfig)
	1. [NetworkConfigUpdate](#cardanonodestartupnetworkconfigupdate)
	1. [NetworkConfigUpdateError](#cardanonodestartupnetworkconfigupdateerror)
	1. [P2PWarning](#cardanonodestartupp2pwarning)
	1. [P2PWarningDevelopementNetworkProtocols](#cardanonodestartupp2pwarningdevelopementnetworkprotocols)
	1. [ShelleyBased](#cardanonodestartupshelleybased)
	1. [StartupDBValidation](#cardanonodestartupstartupdbvalidation)
	1. [StartupInfo](#cardanonodestartupstartupinfo)
	1. [StartupNetworkMagic](#cardanonodestartupstartupnetworkmagic)
	1. [StartupP2PInfo](#cardanonodestartupstartupp2pinfo)
	1. [StartupSocketConfigError](#cardanonodestartupstartupsocketconfigerror)
	1. [StartupTime](#cardanonodestartupstartuptime)
	1. [WarningDevelopmentNetworkProtocols](#cardanonodestartupwarningdevelopmentnetworkprotocols)
1. __StateQueryClient__
	1. __Recieve__
		1. [Acquire](#cardanonodestatequeryclientrecieveacquire)
		1. [Acquired](#cardanonodestatequeryclientrecieveacquired)
		1. [Done](#cardanonodestatequeryclientrecievedone)
		1. [Failure](#cardanonodestatequeryclientrecievefailure)
		1. [Query](#cardanonodestatequeryclientrecievequery)
		1. [ReAcquire](#cardanonodestatequeryclientrecievereacquire)
		1. [Release](#cardanonodestatequeryclientrecieverelease)
		1. [Result](#cardanonodestatequeryclientrecieveresult)
	1. __Send__
		1. [Acquire](#cardanonodestatequeryclientsendacquire)
		1. [Acquired](#cardanonodestatequeryclientsendacquired)
		1. [Done](#cardanonodestatequeryclientsenddone)
		1. [Failure](#cardanonodestatequeryclientsendfailure)
		1. [Query](#cardanonodestatequeryclientsendquery)
		1. [ReAcquire](#cardanonodestatequeryclientsendreacquire)
		1. [Release](#cardanonodestatequeryclientsendrelease)
		1. [Result](#cardanonodestatequeryclientsendresult)
1. __TxInbound__
	1. [TxInboundCanRequestMoreTxs](#cardanonodetxinboundtxinboundcanrequestmoretxs)
	1. [TxInboundCannotRequestMoreTxs](#cardanonodetxinboundtxinboundcannotrequestmoretxs)
	1. [TxInboundTerminated](#cardanonodetxinboundtxinboundterminated)
	1. [TxSubmissionCollected](#cardanonodetxinboundtxsubmissioncollected)
	1. [TxSubmissionProcessed](#cardanonodetxinboundtxsubmissionprocessed)
1. __TxMonitorClient__
	1. __Recieve__
		1. [Acquire](#cardanonodetxmonitorclientrecieveacquire)
		1. [Acquired](#cardanonodetxmonitorclientrecieveacquired)
		1. [Done](#cardanonodetxmonitorclientrecievedone)
		1. [Failure](#cardanonodetxmonitorclientrecievefailure)
		1. [Query](#cardanonodetxmonitorclientrecievequery)
		1. [ReAcquire](#cardanonodetxmonitorclientrecievereacquire)
		1. [Release](#cardanonodetxmonitorclientrecieverelease)
		1. [Result](#cardanonodetxmonitorclientrecieveresult)
	1. __Send__
		1. [Acquire](#cardanonodetxmonitorclientsendacquire)
		1. [Acquired](#cardanonodetxmonitorclientsendacquired)
		1. [Done](#cardanonodetxmonitorclientsenddone)
		1. [Failure](#cardanonodetxmonitorclientsendfailure)
		1. [Query](#cardanonodetxmonitorclientsendquery)
		1. [ReAcquire](#cardanonodetxmonitorclientsendreacquire)
		1. [Release](#cardanonodetxmonitorclientsendrelease)
		1. [Result](#cardanonodetxmonitorclientsendresult)
1. __TxOutbound__
	1. [ControlMessage](#cardanonodetxoutboundcontrolmessage)
	1. [RecvMsgRequest](#cardanonodetxoutboundrecvmsgrequest)
	1. [SendMsgReply](#cardanonodetxoutboundsendmsgreply)
1. __TxSubmission__
	1. __NodeToNode__
		1. __Recieve__
			1. [Done](#cardanonodetxsubmissionnodetonoderecievedone)
			1. [ReplyTxIds](#cardanonodetxsubmissionnodetonoderecievereplytxids)
			1. [ReplyTxs](#cardanonodetxsubmissionnodetonoderecievereplytxs)
			1. [RequestTxIds](#cardanonodetxsubmissionnodetonoderecieverequesttxids)
			1. [RequestTxs](#cardanonodetxsubmissionnodetonoderecieverequesttxs)
		1. __Send__
			1. [Done](#cardanonodetxsubmissionnodetonodesenddone)
			1. [ReplyTxIds](#cardanonodetxsubmissionnodetonodesendreplytxids)
			1. [ReplyTxs](#cardanonodetxsubmissionnodetonodesendreplytxs)
			1. [RequestTxIds](#cardanonodetxsubmissionnodetonodesendrequesttxids)
			1. [RequestTxs](#cardanonodetxsubmissionnodetonodesendrequesttxs)
1. __TxSubmission2__
	1. __NodeToNode__
		1. __Recieve__
			1. [Done](#cardanonodetxsubmission2nodetonoderecievedone)
			1. [MsgHello](#cardanonodetxsubmission2nodetonoderecievemsghello)
			1. [ReplyTxIds](#cardanonodetxsubmission2nodetonoderecievereplytxids)
			1. [ReplyTxs](#cardanonodetxsubmission2nodetonoderecievereplytxs)
			1. [RequestTxIds](#cardanonodetxsubmission2nodetonoderecieverequesttxids)
			1. [RequestTxs](#cardanonodetxsubmission2nodetonoderecieverequesttxs)
		1. __Send__
			1. [Done](#cardanonodetxsubmission2nodetonodesenddone)
			1. [MsgHello](#cardanonodetxsubmission2nodetonodesendmsghello)
			1. [ReplyTxIds](#cardanonodetxsubmission2nodetonodesendreplytxids)
			1. [ReplyTxs](#cardanonodetxsubmission2nodetonodesendreplytxs)
			1. [RequestTxIds](#cardanonodetxsubmission2nodetonodesendrequesttxids)
			1. [RequestTxs](#cardanonodetxsubmission2nodetonodesendrequesttxs)
1. __TxSubmissionClient__
	1. __Recieve__
		1. [AcceptTx](#cardanonodetxsubmissionclientrecieveaccepttx)
		1. [Done](#cardanonodetxsubmissionclientrecievedone)
		1. [RejectTx](#cardanonodetxsubmissionclientrecieverejecttx)
		1. [SubmitTx](#cardanonodetxsubmissionclientrecievesubmittx)
	1. __Send__
		1. [AcceptTx](#cardanonodetxsubmissionclientsendaccepttx)
		1. [Done](#cardanonodetxsubmissionclientsenddone)
		1. [RejectTx](#cardanonodetxsubmissionclientsendrejecttx)
		1. [SubmitTx](#cardanonodetxsubmissionclientsendsubmittx)

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
				1. [header](#cardanonodemetricsservedheader)
				1. [header](#cardanonodemetricsservedheader)
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

## Trace Messages
### Cardano.Node.AcceptPolicy.ConnectionHardLimit


***
Hard rate limit reached, waiting until the number of connections drops below n.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.AcceptPolicy.ConnectionLimitResume


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.AcceptPolicy.ConnectionRateLimiting


***
Rate limiting accepting connections, delaying next accept for given time, currently serving n connections.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.BlockFetch.NodeToNode.Recieve.BatchDone


***
End of block streaming.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.BlockFetch.NodeToNode.Recieve.Block


***
Stream a single block.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.BlockFetch.NodeToNode.Recieve.ClientDone


***
Client termination message.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.BlockFetch.NodeToNode.Recieve.NoBlocks


***
Respond that there are no blocks.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.BlockFetch.NodeToNode.Recieve.RequestRange


***
Request range of blocks.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.BlockFetch.NodeToNode.Recieve.StartBatch


***
Start block streaming.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.BlockFetch.NodeToNode.Send.BatchDone


***
End of block streaming.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.BlockFetch.NodeToNode.Send.Block


***
Stream a single block.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.BlockFetch.NodeToNode.Send.ClientDone


***
Client termination message.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.BlockFetch.NodeToNode.Send.NoBlocks


***
Respond that there are no blocks.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.BlockFetch.NodeToNode.Send.RequestRange


***
Request range of blocks.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.BlockFetch.NodeToNode.Send.StartBatch


***
Start block streaming.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.BlockFetchClient.AcknowledgedFetchRequest


***
Mark the point when the fetch client picks up the request added by the block fetch decision thread. Note that this event can happen fewer times than the 'AddedFetchRequest' due to fetch request merging.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.BlockFetchClient.AddedFetchRequest


***
The block fetch decision thread has added a new fetch instruction consisting of one or more individual request ranges.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.BlockFetchClient.ClientTerminating


***
The client is terminating.  Log the number of outstanding requests.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.BlockFetchClient.CompletedBlockFetch


***
Mark the successful end of receiving a streaming batch of blocks.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`
Limiters: Limiter `["Node","BlockFetchClient","CompletedBlockFetch"]` with frequency `2.0`

### Cardano.Node.BlockFetchClient.CompletedFetchBatch


***
Mark the successful end of receiving a streaming batch of blocks
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.BlockFetchClient.RejectedFetchBatch


***
If the other peer rejects our request then we have this event instead of 'StartedFetchBatch' and 'CompletedFetchBatch'.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.BlockFetchClient.SendFetchRequest


***
Mark the point when fetch request for a fragment is actually sent over the wire.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.BlockFetchClient.StartedFetchBatch


***
Mark the start of receiving a streaming batch of blocks. This will be followed by one or more 'CompletedBlockFetch' and a final 'CompletedFetchBatch'
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.BlockFetchDecision


***
Throughout the decision making process we accumulate reasons to decline to fetch any blocks. This message carries the intermediate and final results.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.BlockFetchSerialised.NodeToNode.Recieve.BatchDone


***
End of block streaming.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.BlockFetchSerialised.NodeToNode.Recieve.Block


***
Stream a single block.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.BlockFetchSerialised.NodeToNode.Recieve.ClientDone


***
Client termination message.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.BlockFetchSerialised.NodeToNode.Recieve.NoBlocks


***
Respond that there are no blocks.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.BlockFetchSerialised.NodeToNode.Recieve.RequestRange


***
Request range of blocks.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.BlockFetchSerialised.NodeToNode.Recieve.StartBatch


***
Start block streaming.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.BlockFetchSerialised.NodeToNode.Send.BatchDone


***
End of block streaming.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.BlockFetchSerialised.NodeToNode.Send.Block


***
Stream a single block.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.BlockFetchSerialised.NodeToNode.Send.ClientDone


***
Client termination message.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.BlockFetchSerialised.NodeToNode.Send.NoBlocks


***
Respond that there are no blocks.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.BlockFetchSerialised.NodeToNode.Send.RequestRange


***
Request range of blocks.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.BlockFetchSerialised.NodeToNode.Send.StartBatch


***
Start block streaming.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.BlockFetchServer.SendBlock


***
The server sent a block to the peer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.BlockchainTime.CurrentSlotUnknown


***
Current slot is not yet known
 This happens when the tip of our current chain is so far in the past that we cannot translate the current wallclock to a slot number, typically during syncing. Until the current slot number is known, we cannot produce blocks. Seeing this message during syncing therefore is normal and to be expected.
 We record the current time (the time we tried to translate to a 'SlotNo') as well as the 'PastHorizonException', which provides detail on the bounds between which we /can/ do conversions. The distance between the current time and the upper bound should rapidly decrease with consecutive 'CurrentSlotUnknown' messages during syncing.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.BlockchainTime.StartTimeInTheFuture


***
The start time of the blockchain time is in the future
 We have to block (for 'NominalDiffTime') until that time comes.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.BlockchainTime.SystemClockMovedBack


***
The system clock moved back an acceptable time span, e.g., because of an NTP sync.
 The system clock moved back such that the new current slot would be smaller than the previous one. If this is within the configured limit, we trace this warning but *do not change the current slot*. The current slot never decreases, but the current slot may stay the same longer than expected.
 When the system clock moved back more than the configured limit, we shut down with a fatal exception.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainDB.AddBlockEvent.AddBlockValidation.CandidateContainsFutureBlocks


***
An event traced during validating performed while adding a block. Candidate contains headers from the future which do no exceed the clock skew.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.AddBlockEvent.AddBlockValidation.CandidateContainsFutureBlocksExceedingClockSkew


***
An event traced during validating performed while adding a block. Candidate contains headers from the future which exceed the clock skew.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.AddBlockEvent.AddBlockValidation.InvalidBlock


***
An event traced during validating performed while adding a block. A point was found to be invalid.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.AddBlockEvent.AddBlockValidation.ValidCandidate


***
An event traced during validating performed while adding a block. A candidate chain was valid.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`
Limiters: Limiter `["Node","ChainDB","AddBlockEvent","AddBlockValidation","ValidCandidate"]` with frequency `2.0`

### Cardano.Node.ChainDB.AddBlockEvent.AddedBlockToQueue


***
The block was added to the queue and will be added to the ChainDB by the background thread. The size of the queue is included..
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`
Limiters: Limiter `["Node","ChainDB","AddBlockEvent","AddedBlockToQueue"]` with frequency `2.0`

### Cardano.Node.ChainDB.AddBlockEvent.AddedBlockToVolatileDB


***
A block was added to the Volatile DB
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`
Limiters: Limiter `["Node","ChainDB","AddBlockEvent","AddedBlockToVolatileDB"]` with frequency `2.0`

### Cardano.Node.ChainDB.AddBlockEvent.AddedToCurrentChain


***
The new block fits onto the current chain (first fragment) and we have successfully used it to extend our (new) current chain (second fragment).
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.AddBlockEvent.BlockInTheFuture


***
The block is from the future, i.e., its slot number is greater than the current slot (the second argument).
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.AddBlockEvent.ChainSelectionForFutureBlock


***
Run chain selection for a block that was previously from the future. This is done for all blocks from the future each time a new block is added.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.AddBlockEvent.IgnoreBlockAlreadyInVolatileDB


***
A block that is already in the Volatile DB was ignored.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.AddBlockEvent.IgnoreBlockOlderThanK


***
A block with a 'BlockNo' more than @k@ back than the current tip was ignored.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.AddBlockEvent.IgnoreInvalidBlock


***
A block that is already in the Volatile DB was ignored.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.AddBlockEvent.StoreButDontChange


***
The block fits onto some fork, we'll try to switch to that fork (if it is preferable to our chain).
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.AddBlockEvent.SwitchedToAFork


***
The new block fits onto some fork and we have switched to that fork (second fragment), as it is preferable to our (previous) current chain (first fragment).
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.AddBlockEvent.TryAddToCurrentChain


***
The block fits onto the current chain, we'll try to use it to extend our chain.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.AddBlockEvent.TrySwitchToAFork


***
The block fits onto some fork, we'll try to switch to that fork (if it is preferable to our chain)
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceCopyToImmutableDBEvent.CopiedBlockToImmutableDB


***
A block was successfully copied to the ImmDB.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceCopyToImmutableDBEvent.NoBlocksToCopyToImmutableDB


***
There are no block to copy to the ImmDB.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceFollowerEvent.FollowerNewImmIterator


***
The follower is in the 'FollowerInImmutableDB' state but the iterator is exhausted while the ImmDB has grown, so we open a new iterator to stream these blocks too.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceFollowerEvent.FollowerNoLongerInMem


***
The follower was in the 'FollowerInImmutableDB' state and is switched to the 'FollowerInMem' state.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceFollowerEvent.FollowerSwitchToMem


***
The follower was in the 'FollowerInImmutableDB' state and is switched to the 'FollowerInMem' state.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceFollowerEvent.NewFollower


***
A new follower was created.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceGCEvent.PerformedGC


***
There are no block to copy to the ImmDB.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceGCEvent.ScheduledGC


***
There are no block to copy to the ImmDB.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceImmutableDBEvent.CacheEvent.CurrentChunkHit


***
Current chunk found in the cache.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceImmutableDBEvent.CacheEvent.PastChunkEvict


***
The least recently used past chunk was evicted because the cache was full.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceImmutableDBEvent.CacheEvent.PastChunkExpired


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceImmutableDBEvent.CacheEvent.PastChunkHit


***
Past chunk found in the cache
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceImmutableDBEvent.CacheEvent.PastChunkMiss


***
Past chunk was not found in the cache
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceImmutableDBEvent.ChunkFileDoesntFit


***
The hash of the last block in the previous epoch doesn't match the previous hash of the first block in the current epoch
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceImmutableDBEvent.ChunkValidation.InvalidChunkFile


***
Chunk file is invalid
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceImmutableDBEvent.ChunkValidation.InvalidPrimaryIndex


***
The primary index is invalid.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceImmutableDBEvent.ChunkValidation.InvalidSecondaryIndex


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceImmutableDBEvent.ChunkValidation.MissingChunkFile


***
Chunk file is missing
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceImmutableDBEvent.ChunkValidation.MissingPrimaryIndex


***
The primary index is missing.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceImmutableDBEvent.ChunkValidation.MissingSecondaryIndex


***
The secondary index is missing.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceImmutableDBEvent.ChunkValidation.RewritePrimaryIndex


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceImmutableDBEvent.ChunkValidation.RewriteSecondaryIndex


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceImmutableDBEvent.ChunkValidation.StartedValidatingChunk


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceImmutableDBEvent.ChunkValidation.ValidatedChunk


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceImmutableDBEvent.DBAlreadyClosed


***
The immutable DB is already closed
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceImmutableDBEvent.DBClosed


***
Closing the immutable DB
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceImmutableDBEvent.DeletingAfter


***
Delete after
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceImmutableDBEvent.Migrating


***
Performing a migration of the on-disk files.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceImmutableDBEvent.NoValidLastLocation


***
No valid last location was found
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceImmutableDBEvent.ValidatedLastLocation


***
The last location was validatet
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceInitChainSelEvent.CandidateContainsFutureBlocks


***
Candidate contains headers from the future which do not exceed the clock skew.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceInitChainSelEvent.CandidateContainsFutureBlocksExceedingClockSkew


***
Candidate contains headers from the future which exceed the clock skew, making them invalid.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceInitChainSelEvent.InitalChainSelected


***
InitalChainSelected
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceInitChainSelEvent.InvalidBlock


***
A point was found to be invalid.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceInitChainSelEvent.StartedInitChainSelection


***
StartedInitChainSelection
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceInitChainSelEvent.UpdateLedgerDb


***
UpdateLedgerDb
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceInitChainSelEvent.ValidCandidate


***
A candidate chain was valid.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceIteratorEvent.BlockGCedFromVolatileDB


***
A block is no longer in the VolatileDB and isn't in the ImmDB either; it wasn't part of the current chain.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceIteratorEvent.BlockMissingFromVolatileDB


***
A block is no longer in the VolatileDB because it has been garbage collected. It might now be in the ImmDB if it was part of the current chain.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceIteratorEvent.BlockWasCopiedToImmutableDB


***
A block that has been garbage collected from the VolatileDB is now found and streamed from the ImmDB.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceIteratorEvent.StreamFromBoth


***
Stream from both the VolatileDB and the ImmDB.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceIteratorEvent.StreamFromImmutableDB


***
Stream only from the ImmDB.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceIteratorEvent.StreamFromVolatileDB


***
Stream only from the VolatileDB.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceIteratorEvent.SwitchBackToVolatileDB


***
We have streamed one or more blocks from the ImmDB that were part of the VolatileDB when initialising the iterator. Now, we have to look back in the VolatileDB again because the ImmDB doesn't have the next block we're looking for.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceIteratorEvent.UnknownRangeRequested


***
An unknown range was requested, see 'UnknownRange'.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceLedgerEvent.DeletedSnapshot


***
An old or invalid on-disk snapshot was deleted.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceLedgerEvent.InvalidSnapshot


***
An on disk snapshot was skipped because it was invalid.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceLedgerEvent.TookSnapshot


***
A snapshot was written to disk.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceLedgerReplayEvent.ReplayFromGenesis


***
There were no LedgerDB snapshots on disk, so we're replaying all blocks starting from Genesis against the initial ledger. The @replayTo@ parameter corresponds to the block at the tip of the ImmDB, i.e., the last block to replay.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceLedgerReplayEvent.ReplayFromSnapshot


***
There was a LedgerDB snapshot on disk corresponding to the given tip. We're replaying more recent blocks against it. The @replayTo@ parameter corresponds to the block at the tip of the ImmDB, i.e., the last block to replay.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceLedgerReplayEvent.ReplayedBlock


***
We replayed the given block (reference) on the genesis snapshot during the initialisation of the LedgerDB.
 The @blockInfo@ parameter corresponds replayed block and the @replayTo@ parameter corresponds to the block at the tip of the ImmDB, i.e., the last block to replay.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceOpenEvent.ClosedDB


***
The ChainDB was closed.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceOpenEvent.OpenedDB


***
The ChainDB was opened.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceOpenEvent.OpenedImmutableDB


***
The ImmDB was opened.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceOpenEvent.OpenedLgrDB


***
The LedgerDB was opened.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceOpenEvent.OpenedVolatileDB


***
The VolatileDB was opened.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceOpenEvent.StartedOpeningDB


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceOpenEvent.StartedOpeningImmutableDB


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceOpenEvent.StartedOpeningLgrDB


***
The LedgerDB was opened.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceOpenEvent.StartedOpeningVolatileDB


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceVolatileDBEvent.BlockAlreadyHere


***
A block was found to be already in the DB.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceVolatileDBEvent.DBAlreadyClosed


***
When closing the DB it was found itis closed already.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceVolatileDBEvent.InvalidFileNames


***
Reports a list of invalid file paths.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainDB.TraceVolatileDBEvent.Truncate


***
Truncates a file up to offset because of the error.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainSync.NodeToClient.Recieve.AwaitReply


***
Acknowledge the request but require the consumer to wait for the nextupdate. This means that the consumer is synced with the producer, andthe producer is waiting for its own chain state to change.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSync.NodeToClient.Recieve.Done


***
We have to explain to the framework what our states mean, in terms ofwhich party has agency in each state.
Idle states are where it is for the client to send a message,busy states are where the server is expected to send a reply.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSync.NodeToClient.Recieve.FindIntersect


***
Ask the producer to try to find an improved intersection point betweenthe consumer and producer's chains. The consumer sends a sequence ofpoints and it is up to the producer to find the first intersection pointon its chain and send it back to the consumer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSync.NodeToClient.Recieve.IntersectFound


***
The reply to the consumer about an intersection found.The consumer can decide weather to send more points.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSync.NodeToClient.Recieve.IntersectNotFound


***
The reply to the consumer that no intersection was found: none of thepoints the consumer supplied are on the producer chain.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSync.NodeToClient.Recieve.RequestNext


***
Request the next update from the producer. The response can be a rollforward, a roll back or wait.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSync.NodeToClient.Recieve.RollBackward


***
Tell the consumer to roll back to a given point on their chain.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSync.NodeToClient.Recieve.RollForward


***
Tell the consumer to extend their chain with the given header.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSync.NodeToClient.Send.AwaitReply


***
Acknowledge the request but require the consumer to wait for the nextupdate. This means that the consumer is synced with the producer, andthe producer is waiting for its own chain state to change.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSync.NodeToClient.Send.Done


***
We have to explain to the framework what our states mean, in terms ofwhich party has agency in each state.
Idle states are where it is for the client to send a message,busy states are where the server is expected to send a reply.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSync.NodeToClient.Send.FindIntersect


***
Ask the producer to try to find an improved intersection point betweenthe consumer and producer's chains. The consumer sends a sequence ofpoints and it is up to the producer to find the first intersection pointon its chain and send it back to the consumer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSync.NodeToClient.Send.IntersectFound


***
The reply to the consumer about an intersection found.The consumer can decide weather to send more points.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSync.NodeToClient.Send.IntersectNotFound


***
The reply to the consumer that no intersection was found: none of thepoints the consumer supplied are on the producer chain.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSync.NodeToClient.Send.RequestNext


***
Request the next update from the producer. The response can be a rollforward, a roll back or wait.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSync.NodeToClient.Send.RollBackward


***
Tell the consumer to roll back to a given point on their chain.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSync.NodeToClient.Send.RollForward


***
Tell the consumer to extend their chain with the given header.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSyncClient.ChainSyncClientEvent.DownloadedHeader


***
While following a candidate chain, we rolled forward by downloading a header.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSyncClient.ChainSyncClientEvent.Exception


***
An exception was thrown by the Chain Sync Client.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSyncClient.ChainSyncClientEvent.FoundIntersection


***
We found an intersection between our chain fragment and the candidate's chain.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSyncClient.ChainSyncClientEvent.RolledBack


***
While following a candidate chain, we rolled back to the given point.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSyncClient.ChainSyncClientEvent.Termination


***
The client has terminated.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSyncNode.NodeToNode.Recieve.AwaitReply


***
Acknowledge the request but require the consumer to wait for the nextupdate. This means that the consumer is synced with the producer, andthe producer is waiting for its own chain state to change.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSyncNode.NodeToNode.Recieve.Done


***
We have to explain to the framework what our states mean, in terms ofwhich party has agency in each state.
Idle states are where it is for the client to send a message,busy states are where the server is expected to send a reply.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSyncNode.NodeToNode.Recieve.FindIntersect


***
Ask the producer to try to find an improved intersection point betweenthe consumer and producer's chains. The consumer sends a sequence ofpoints and it is up to the producer to find the first intersection pointon its chain and send it back to the consumer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSyncNode.NodeToNode.Recieve.IntersectFound


***
The reply to the consumer about an intersection found.The consumer can decide weather to send more points.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSyncNode.NodeToNode.Recieve.IntersectNotFound


***
The reply to the consumer that no intersection was found: none of thepoints the consumer supplied are on the producer chain.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSyncNode.NodeToNode.Recieve.RequestNext


***
Request the next update from the producer. The response can be a rollforward, a roll back or wait.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSyncNode.NodeToNode.Recieve.RollBackward


***
Tell the consumer to roll back to a given point on their chain.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSyncNode.NodeToNode.Recieve.RollForward


***
Tell the consumer to extend their chain with the given header.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSyncNode.NodeToNode.Send.AwaitReply


***
Acknowledge the request but require the consumer to wait for the nextupdate. This means that the consumer is synced with the producer, andthe producer is waiting for its own chain state to change.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSyncNode.NodeToNode.Send.Done


***
We have to explain to the framework what our states mean, in terms ofwhich party has agency in each state.
Idle states are where it is for the client to send a message,busy states are where the server is expected to send a reply.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSyncNode.NodeToNode.Send.FindIntersect


***
Ask the producer to try to find an improved intersection point betweenthe consumer and producer's chains. The consumer sends a sequence ofpoints and it is up to the producer to find the first intersection pointon its chain and send it back to the consumer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSyncNode.NodeToNode.Send.IntersectFound


***
The reply to the consumer about an intersection found.The consumer can decide weather to send more points.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSyncNode.NodeToNode.Send.IntersectNotFound


***
The reply to the consumer that no intersection was found: none of thepoints the consumer supplied are on the producer chain.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSyncNode.NodeToNode.Send.RequestNext


***
Request the next update from the producer. The response can be a rollforward, a roll back or wait.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSyncNode.NodeToNode.Send.RollBackward


***
Tell the consumer to roll back to a given point on their chain.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSyncNode.NodeToNode.Send.RollForward


***
Tell the consumer to extend their chain with the given header.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSyncSerialised.NodeToNode.Recieve.AwaitReply


***
Acknowledge the request but require the consumer to wait for the nextupdate. This means that the consumer is synced with the producer, andthe producer is waiting for its own chain state to change.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSyncSerialised.NodeToNode.Recieve.Done


***
We have to explain to the framework what our states mean, in terms ofwhich party has agency in each state.
Idle states are where it is for the client to send a message,busy states are where the server is expected to send a reply.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSyncSerialised.NodeToNode.Recieve.FindIntersect


***
Ask the producer to try to find an improved intersection point betweenthe consumer and producer's chains. The consumer sends a sequence ofpoints and it is up to the producer to find the first intersection pointon its chain and send it back to the consumer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSyncSerialised.NodeToNode.Recieve.IntersectFound


***
The reply to the consumer about an intersection found.The consumer can decide weather to send more points.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSyncSerialised.NodeToNode.Recieve.IntersectNotFound


***
The reply to the consumer that no intersection was found: none of thepoints the consumer supplied are on the producer chain.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSyncSerialised.NodeToNode.Recieve.RequestNext


***
Request the next update from the producer. The response can be a rollforward, a roll back or wait.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSyncSerialised.NodeToNode.Recieve.RollBackward


***
Tell the consumer to roll back to a given point on their chain.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSyncSerialised.NodeToNode.Recieve.RollForward


***
Tell the consumer to extend their chain with the given header.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSyncSerialised.NodeToNode.Send.AwaitReply


***
Acknowledge the request but require the consumer to wait for the nextupdate. This means that the consumer is synced with the producer, andthe producer is waiting for its own chain state to change.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSyncSerialised.NodeToNode.Send.Done


***
We have to explain to the framework what our states mean, in terms ofwhich party has agency in each state.
Idle states are where it is for the client to send a message,busy states are where the server is expected to send a reply.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSyncSerialised.NodeToNode.Send.FindIntersect


***
Ask the producer to try to find an improved intersection point betweenthe consumer and producer's chains. The consumer sends a sequence ofpoints and it is up to the producer to find the first intersection pointon its chain and send it back to the consumer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSyncSerialised.NodeToNode.Send.IntersectFound


***
The reply to the consumer about an intersection found.The consumer can decide weather to send more points.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSyncSerialised.NodeToNode.Send.IntersectNotFound


***
The reply to the consumer that no intersection was found: none of thepoints the consumer supplied are on the producer chain.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSyncSerialised.NodeToNode.Send.RequestNext


***
Request the next update from the producer. The response can be a rollforward, a roll back or wait.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSyncSerialised.NodeToNode.Send.RollBackward


***
Tell the consumer to roll back to a given point on their chain.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSyncSerialised.NodeToNode.Send.RollForward


***
Tell the consumer to extend their chain with the given header.
The message also tells the consumer about the head point of the producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSyncServerBlock.ChainSyncServerEvent.ServerRead.RollBackward


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSyncServerBlock.ChainSyncServerEvent.ServerRead.RollForward


***
Roll forward to the given point.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSyncServerBlock.ChainSyncServerEvent.ServerRead.ServerRead


***
A server read has occurred, either for an add block or a rollback
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSyncServerBlock.ChainSyncServerEvent.ServerRead.ServerReadBlocked


***
A server read has blocked, either for an add block or a rollback
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ChainSyncServerHeader.ChainSyncServerEvent.ServerRead.RollBackward


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainSyncServerHeader.ChainSyncServerEvent.ServerRead.RollForward


***
Roll forward to the given point.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainSyncServerHeader.ChainSyncServerEvent.ServerRead.ServerRead


***
A server read has occurred, either for an add block or a rollback
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ChainSyncServerHeader.ChainSyncServerEvent.ServerRead.ServerReadBlocked


***
A server read has blocked, either for an add block or a rollback
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ConnectionManager.Connect


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ConnectionManager.ConnectError


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ConnectionManager.ConnectionCleanup


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ConnectionManager.ConnectionExists


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ConnectionManager.ConnectionFailure


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ConnectionManager.ConnectionHandler


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ConnectionManager.ConnectionManagerCounters


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ConnectionManager.ConnectionNotFound


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ConnectionManager.ConnectionTimeWait


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ConnectionManager.ConnectionTimeWaitDone


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ConnectionManager.ForbiddenConnection


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ConnectionManager.ForbiddenOperation


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ConnectionManager.ImpossibleConnection


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ConnectionManager.IncludeConnection


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ConnectionManager.PruneConnections


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ConnectionManager.Shutdown


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ConnectionManager.State


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ConnectionManager.TerminatedConnection


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ConnectionManager.TerminatingConnection


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ConnectionManager.UnexpectedlyFalseAssertion


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ConnectionManager.UnknownConnection


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ConnectionManager.UnregisterConnection


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ConnectionManagerTransition.ConnectionManagerTransition


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.DNSResolver.LookupAAAAError


***
AAAA lookup failed with an error.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.DNSResolver.LookupAAAAResult


***
Lookup AAAA result.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.DNSResolver.LookupAError


***
A lookup failed with an error.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.DNSResolver.LookupAResult


***
Lookup A result.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.DNSResolver.LookupException


***
A DNS lookup exception occurred.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.DNSResolver.LookupIPv4First


***
Returning IPv4 address first.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.DNSResolver.LookupIPv6First


***
Returning IPv6 address first.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.DNSSubscription.DNS.AllocateSocket


***
DNS Subscription: Allocate socket to address.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.DNSSubscription.DNS.ApplicationException


***
DNS Subscription: Application Exception occurred.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.DNSSubscription.DNS.CloseSocket


***
DNS Subscription: Closed socket to address.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.DNSSubscription.DNS.ConnectEnd


***
DNS Subscription: Connection Attempt end with destination and outcome.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.DNSSubscription.DNS.ConnectException


***
DNS Subscription: Socket Allocation Exception with destination and the exception.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.DNSSubscription.DNS.ConnectStart


***
DNS Subscription: Connection Attempt Start with destination.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.DNSSubscription.DNS.ConnectionExist


***
DNS Subscription: Connection exists to destination.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.DNSSubscription.DNS.MissingLocalAddress


***
DNS Subscription: Missing local address.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.DNSSubscription.DNS.Restart


***
DNS Subscription: Restarting Subscription after duration with desired valency and current valency.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.DNSSubscription.DNS.SkippingPeer


***
DNS Subscription: Skipping peer with address.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.DNSSubscription.DNS.SocketAllocationException


***
DNS Subscription: Connection Attempt Exception with destination and exception.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.DNSSubscription.DNS.Start


***
DNS Subscription: Starting Subscription Worker with a valency.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.DNSSubscription.DNS.SubscriptionFailed


***
DNS Subscription: Failed to start all required subscriptions.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.DNSSubscription.DNS.SubscriptionRunning


***
DNS Subscription: Required subscriptions started.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.DNSSubscription.DNS.SubscriptionWaiting


***
DNS Subscription: Waiting on address with active connections.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.DNSSubscription.DNS.SubscriptionWaitingNewConnection


***
DNS Subscription: Waiting delay time before attempting a new connection.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.DNSSubscription.DNS.TryConnectToPeer


***
DNS Subscription: Trying to connect to peer with address.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.DNSSubscription.DNS.UnsupportedRemoteAddr


***
DNS Subscription: Unsupported remote target address.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.DebugPeerSelection.DebugPeerSelection.GovernorState


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.DebugPeerSelectionResponder.DebugPeerSelection.GovernorState


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.DiffusionInit.ConfiguringLocalSocket


***
ConfiguringLocalSocket 
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.DiffusionInit.ConfiguringServerSocket


***
ConfiguringServerSocket 
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.DiffusionInit.CreateSystemdSocketForSnocketPath


***
CreateSystemdSocketForSnocketPath 
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.DiffusionInit.CreatedLocalSocket


***
CreatedLocalSocket 
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.DiffusionInit.CreatingServerSocket


***
CreatingServerSocket 
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.DiffusionInit.DiffusionErrored


***
DiffusionErrored 
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.DiffusionInit.ListeningLocalSocket


***
ListeningLocalSocket 
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.DiffusionInit.ListeningServerSocket


***
ListeningServerSocket 
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.DiffusionInit.LocalSocketUp


***
LocalSocketUp 
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.DiffusionInit.RunLocalServer


***
RunLocalServer 
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.DiffusionInit.RunServer


***
RunServer 
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.DiffusionInit.ServerSocketUp


***
ServerSocketUp 
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.DiffusionInit.UnsupportedLocalSystemdSocket


***
UnsupportedLocalSystemdSocket 
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.DiffusionInit.UnsupportedReadySocketCase


***
UnsupportedReadySocketCase 
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.DiffusionInit.UsingSystemdSocket


***
UsingSystemdSocket 
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ErrorPolicy.AcceptException


***
'accept' threw an exception.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ErrorPolicy.KeepSuspended


***
Consumer was suspended until producer will resume.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ErrorPolicy.LocalNodeError


***
caught a local exception.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ErrorPolicy.ResumeConsumer


***
Resume consumer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ErrorPolicy.ResumePeer


***
Resume a peer (both consumer and producer).
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ErrorPolicy.ResumeProducer


***
Resume producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ErrorPolicy.SuspendConsumer


***
Suspending consumer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ErrorPolicy.SuspendPeer


***
Suspending peer with a given exception.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ErrorPolicy.UnhandledApplicationException


***
An application threw an exception, which was not handled.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ErrorPolicy.UnhandledConnectionException


***
'connect' threw an exception, which was not handled by any 'ErrorPolicy'.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.Forge.AdoptedBlock


***
We adopted the block we produced, we also trace the transactions  that were adopted.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.Forge.BlockContext


***
We found out to which block we are going to connect the block we are about  to forge.   We record the current slot number, the block number of the block to  connect to and its point.   Note that block number of the block we will try to forge is one more than  the recorded block number.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.Forge.BlockFromFuture


***
Leadership check failed: the current chain contains a block from a slot  /after/ the current slot   This can only happen if the system is under heavy load.   We record both the current slot number as well as the slot number of the  block at the tip of the chain.   See also <https://github.com/input-output-hk/ouroboros-network/issues/1462>
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.Forge.DidntAdoptBlock


***
We did not adopt the block we produced, but the block was valid. We  must have adopted a block that another leader of the same slot produced  before we got the chance of adopting our own block. This is very rare,  this warrants a warning.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.Forge.ForgeStateUpdateError


***
Updating the forge state failed.   For example, the KES key could not be evolved anymore.   We record the error returned by 'updateForgeState'.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.Forge.ForgedBlock


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
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.Forge.ForgedInvalidBlock


***
We forged a block that is invalid according to the ledger in the  ChainDB. This means there is an inconsistency between the mempool  validation and the ledger validation. This is a serious error!
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.Forge.LedgerState


***
We obtained a ledger state for the point of the block we want to  connect to   We record both the current slot number as well as the point of the block  we attempt to connect the new block to (that we requested the ledger  state for).
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.Forge.LedgerView


***
We obtained a ledger view for the current slot number   We record the current slot number.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.Forge.NoLedgerState


***
Leadership check failed: we were unable to get the ledger state for the  point of the block we want to connect to   This can happen if after choosing which block to connect to the node  switched to a different fork. We expect this to happen only rather  rarely, so this certainly merits a warning; if it happens a lot, that  merits an investigation.   We record both the current slot number as well as the point of the block  we attempt to connect the new block to (that we requested the ledger  state for).
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.Forge.NoLedgerView


***
Leadership check failed: we were unable to get the ledger view for the  current slot number   This will only happen if there are many missing blocks between the tip of  our chain and the current slot.   We record also the failure returned by 'forecastFor'.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.Forge.NodeCannotForge


***
We did the leadership check and concluded that we should lead and forge  a block, but cannot.   This should only happen rarely and should be logged with warning severity.   Records why we cannot forge a block.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.Forge.NodeIsLeader


***
We did the leadership check and concluded we /are/ the leader
  The node will soon forge; it is about to read its transactions from the  Mempool. This will be followed by ForgedBlock.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.Forge.NodeNotLeader


***
We did the leadership check and concluded we are not the leader   We record the current slot number
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.Forge.SlotIsImmutable


***
Leadership check failed: the tip of the ImmutableDB inhabits the  current slot   This might happen in two cases.    1. the clock moved backwards, on restart we ignored everything from the      VolatileDB since it's all in the future, and now the tip of the      ImmutableDB points to a block produced in the same slot we're trying      to produce a block in    2. k = 0 and we already adopted a block from another leader of the same      slot.   We record both the current slot number as well as the tip of the  ImmutableDB.  See also <https://github.com/input-output-hk/ouroboros-network/issues/1462>
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.Forge.StartLeadershipCheck


***
Start of the leadership check.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.Forge.StartLeadershipCheckPlus


***
We adopted the block we produced, we also trace the transactions  that were adopted.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.ForgeStateInfo


***
kesStartPeriod 
kesEndPeriod is kesStartPeriod + tpraosMaxKESEvo
kesEvolution is the current evolution or /relative period/.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ForgeStats


***
nodeCannotForgeNum shows how many times this node could not forge.
nodeIsLeaderNum shows how many times this node was leader.
blocksForgedNum shows how many blocks did forge in this node.
slotsMissed shows how many slots were missed in this node.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.Handshake.Receive.AcceptVersion


***
The remote end decides which version to use and sends chosen version.The server is allowed to modify version parameters.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Handshake.Receive.ProposeVersions


***
Propose versions together with version parameters.  It must be encoded to a sorted list..
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Handshake.Receive.Refuse


***
It refuses to run any version.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Handshake.Receive.ReplyVersions


***
`MsgReplyVersions` received as a response to 'MsgProposeVersions'.  It is not supported to explicitly send this message. It can only be received as a copy of 'MsgProposeVersions' in a simultaneous open scenario.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Handshake.Send.AcceptVersion


***
The remote end decides which version to use and sends chosen version.The server is allowed to modify version parameters.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Handshake.Send.ProposeVersions


***
Propose versions together with version parameters.  It must be encoded to a sorted list..
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Handshake.Send.Refuse


***
It refuses to run any version.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Handshake.Send.ReplyVersions


***
`MsgReplyVersions` received as a response to 'MsgProposeVersions'.  It is not supported to explicitly send this message. It can only be received as a copy of 'MsgProposeVersions' in a simultaneous open scenario.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.InboundGovernor.DemotedToColdRemote


***
All mini-protocols terminated.  The boolean is true if this connection was not used by p2p-governor, and thus the connection will be terminated.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.InboundGovernor.DemotedToWarmRemote


***
All mini-protocols terminated.  The boolean is true if this connection was not used by p2p-governor, and thus the connection will be terminated.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.InboundGovernor.InboundGovernorCounters


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.InboundGovernor.InboundGovernorError


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.InboundGovernor.MuxCleanExit


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.InboundGovernor.MuxErrored


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.InboundGovernor.NewConnection


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.InboundGovernor.PromotedToHotRemote


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.InboundGovernor.PromotedToWarmRemote


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.InboundGovernor.RemoteState


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.InboundGovernor.ResponderErrored


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.InboundGovernor.ResponderRestarted


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.InboundGovernor.ResponderStartFailure


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.InboundGovernor.ResponderStarted


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.InboundGovernor.ResponderTerminated


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.InboundGovernor.UnexpectedlyFalseAssertion


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.InboundGovernor.WaitIdleRemote


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.InboundGovernorTransition.InboundGovernorTransition


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.IpSubscription.IP.AllocateSocket


***
IP Subscription: Allocate socket to address.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.IpSubscription.IP.ApplicationException


***
IP Subscription: Application Exception occurred.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.IpSubscription.IP.CloseSocket


***
IP Subscription: Closed socket to address.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.IpSubscription.IP.ConnectEnd


***
IP Subscription: Connection Attempt end with destination and outcome.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.IpSubscription.IP.ConnectException


***
IP Subscription: Socket Allocation Exception with destination and the exception.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.IpSubscription.IP.ConnectStart


***
IP Subscription: Connection Attempt Start with destination.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.IpSubscription.IP.ConnectionExist


***
IP Subscription: Connection exists to destination.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.IpSubscription.IP.MissingLocalAddress


***
IP Subscription: Missing local address.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.IpSubscription.IP.Restart


***
IP Subscription: Restarting Subscription after duration with desired valency and current valency.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.IpSubscription.IP.SkippingPeer


***
IP Subscription: Skipping peer with address.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.IpSubscription.IP.SocketAllocationException


***
IP Subscription: Connection Attempt Exception with destination and exception.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.IpSubscription.IP.Start


***
IP Subscription: Starting Subscription Worker with a valency.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.IpSubscription.IP.SubscriptionFailed


***
IP Subscription: Failed to start all required subscriptions.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.IpSubscription.IP.SubscriptionRunning


***
IP Subscription: Required subscriptions started.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.IpSubscription.IP.SubscriptionWaiting


***
IP Subscription: Waiting on address with active connections.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.IpSubscription.IP.SubscriptionWaitingNewConnection


***
IP Subscription: Waiting delay time before attempting a new connection.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.IpSubscription.IP.TryConnectToPeer


***
IP Subscription: Trying to connect to peer with address.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.IpSubscription.IP.UnsupportedRemoteAddr


***
IP Subscription: Unsupported remote target address.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.KeepAliveClient


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LedgerPeers.DisabledLedgerPeers


***
Trace for when getting peers from the ledger is disabled, that is DontUseLedger.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LedgerPeers.FallingBackToBootstrapPeers


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LedgerPeers.FetchingNewLedgerState


***
Trace for fetching a new list of peers from the ledger. Int is the number of peers returned.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LedgerPeers.PickedPeer


***
Trace for a peer picked with accumulated and relative stake of its pool.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LedgerPeers.PickedPeers


***
Trace for the number of peers we wanted to pick and the list of peers picked.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LedgerPeers.RequestForPeers


***
RequestForPeers (NumberOfPeers 1)
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LedgerPeers.ReusingLedgerState


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LedgerPeers.TraceUseLedgerAfter


***
Trace UseLedgerAfter value.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LedgerPeers.WaitingOnRequest


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalConnectionManager.Connect


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalConnectionManager.ConnectError


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalConnectionManager.ConnectionCleanup


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalConnectionManager.ConnectionExists


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalConnectionManager.ConnectionFailure


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalConnectionManager.ConnectionHandler


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalConnectionManager.ConnectionManagerCounters


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.LocalConnectionManager.ConnectionNotFound


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalConnectionManager.ConnectionTimeWait


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalConnectionManager.ConnectionTimeWaitDone


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalConnectionManager.ForbiddenConnection


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalConnectionManager.ForbiddenOperation


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalConnectionManager.ImpossibleConnection


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalConnectionManager.IncludeConnection


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalConnectionManager.PruneConnections


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalConnectionManager.Shutdown


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalConnectionManager.State


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalConnectionManager.TerminatedConnection


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalConnectionManager.TerminatingConnection


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalConnectionManager.UnexpectedlyFalseAssertion


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalConnectionManager.UnknownConnection


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalConnectionManager.UnregisterConnection


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalErrorPolicy.AcceptException


***
'accept' threw an exception.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.LocalErrorPolicy.KeepSuspended


***
Consumer was suspended until producer will resume.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.LocalErrorPolicy.LocalNodeError


***
caught a local exception.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.LocalErrorPolicy.ResumeConsumer


***
Resume consumer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.LocalErrorPolicy.ResumePeer


***
Resume a peer (both consumer and producer).
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.LocalErrorPolicy.ResumeProducer


***
Resume producer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.LocalErrorPolicy.SuspendConsumer


***
Suspending consumer.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.LocalErrorPolicy.SuspendPeer


***
Suspending peer with a given exception.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.LocalErrorPolicy.UnhandledApplicationException


***
An application threw an exception, which was not handled.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.LocalErrorPolicy.UnhandledConnectionException


***
'connect' threw an exception, which was not handled by any 'ErrorPolicy'.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.LocalHandshake.Receive.AcceptVersion


***
The remote end decides which version to use and sends chosen version.The server is allowed to modify version parameters.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalHandshake.Receive.ProposeVersions


***
Propose versions together with version parameters.  It must be encoded to a sorted list..
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalHandshake.Receive.Refuse


***
It refuses to run any version.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalHandshake.Receive.ReplyVersions


***
`MsgReplyVersions` received as a response to 'MsgProposeVersions'.  It is not supported to explicitly send this message. It can only be received as a copy of 'MsgProposeVersions' in a simultaneous open scenario.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalHandshake.Send.AcceptVersion


***
The remote end decides which version to use and sends chosen version.The server is allowed to modify version parameters.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalHandshake.Send.ProposeVersions


***
Propose versions together with version parameters.  It must be encoded to a sorted list..
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalHandshake.Send.Refuse


***
It refuses to run any version.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalHandshake.Send.ReplyVersions


***
`MsgReplyVersions` received as a response to 'MsgProposeVersions'.  It is not supported to explicitly send this message. It can only be received as a copy of 'MsgProposeVersions' in a simultaneous open scenario.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalInboundGovernor.DemotedToColdRemote


***
All mini-protocols terminated.  The boolean is true if this connection was not used by p2p-governor, and thus the connection will be terminated.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalInboundGovernor.DemotedToWarmRemote


***
All mini-protocols terminated.  The boolean is true if this connection was not used by p2p-governor, and thus the connection will be terminated.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalInboundGovernor.InboundGovernorCounters


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.LocalInboundGovernor.InboundGovernorError


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalInboundGovernor.MuxCleanExit


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalInboundGovernor.MuxErrored


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalInboundGovernor.NewConnection


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalInboundGovernor.PromotedToHotRemote


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalInboundGovernor.PromotedToWarmRemote


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalInboundGovernor.RemoteState


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalInboundGovernor.ResponderErrored


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalInboundGovernor.ResponderRestarted


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalInboundGovernor.ResponderStartFailure


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalInboundGovernor.ResponderStarted


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalInboundGovernor.ResponderTerminated


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalInboundGovernor.UnexpectedlyFalseAssertion


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalInboundGovernor.WaitIdleRemote


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalRootPeers.LocalRootDomains


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalRootPeers.LocalRootError


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalRootPeers.LocalRootFailure


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalRootPeers.LocalRootGroups


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalRootPeers.LocalRootResult


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalRootPeers.LocalRootWaiting


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalServer.AcceptConnection


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalServer.AcceptError


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalServer.AcceptPolicy


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalServer.Error


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalServer.Started


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalServer.Stopped


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.LocalTxSubmissionServer.ReceivedTx


***
A transaction was received.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Mempool.AddedTx


***
New, valid transaction that was added to the Mempool.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.Mempool.ManuallyRemovedTxs


***
Transactions that have been manually removed from the Mempool.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.Mempool.RejectedTx


***
New, invalid transaction thas was rejected and thus not added to the Mempool.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.Mempool.RemoveTxs


***
Previously valid transactions that are no longer valid because of changes in the ledger state. These transactions have been removed from the Mempool.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.Mux.ChannelRecvEnd


***
Channel receive end.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Mux.ChannelRecvStart


***
Channel receive start.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Mux.ChannelSendEnd


***
Channel send end.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Mux.ChannelSendStart


***
Channel send start.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Mux.CleanExit


***
Miniprotocol terminated cleanly.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Mux.ExceptionExit


***
Miniprotocol terminated with exception.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Mux.HandshakeClientEnd


***
Handshake client end.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Mux.HandshakeClientError


***
Handshake client error.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Mux.HandshakeServerEnd


***
Handshake server end.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Mux.HandshakeServerError


***
Handshake server error.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Mux.HandshakeStart


***
Handshake start.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Mux.RecvDeltaQObservation


***
Bearer DeltaQ observation.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Mux.RecvDeltaQSample


***
Bearer DeltaQ sample.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Mux.RecvEnd


***
Bearer receive end.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Mux.RecvHeaderEnd


***
Bearer receive header end.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Mux.RecvHeaderStart


***
Bearer receive header start.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Mux.RecvStart


***
Bearer receive start.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Mux.SDUReadTimeoutException


***
Timed out reading SDU.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Mux.SDUWriteTimeoutException


***
Timed out writing SDU.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Mux.SendEnd


***
Bearer send end.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Mux.SendStart


***
Bearer send start.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Mux.Shutdown


***
Mux shutdown.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Mux.StartEagerly


***
Eagerly started.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Mux.StartOnDemand


***
Preparing to start.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Mux.StartedOnDemand


***
Started on demand.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Mux.State


***
State.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Mux.TCPInfo


***
TCPInfo.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Mux.Terminating


***
Terminating.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.MuxLocal.ChannelRecvEnd


***
Channel receive end.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.MuxLocal.ChannelRecvStart


***
Channel receive start.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.MuxLocal.ChannelSendEnd


***
Channel send end.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.MuxLocal.ChannelSendStart


***
Channel send start.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.MuxLocal.CleanExit


***
Miniprotocol terminated cleanly.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.MuxLocal.ExceptionExit


***
Miniprotocol terminated with exception.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.MuxLocal.HandshakeClientEnd


***
Handshake client end.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.MuxLocal.HandshakeClientError


***
Handshake client error.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.MuxLocal.HandshakeServerEnd


***
Handshake server end.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.MuxLocal.HandshakeServerError


***
Handshake server error.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.MuxLocal.HandshakeStart


***
Handshake start.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.MuxLocal.RecvDeltaQObservation


***
Bearer DeltaQ observation.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.MuxLocal.RecvDeltaQSample


***
Bearer DeltaQ sample.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.MuxLocal.RecvEnd


***
Bearer receive end.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.MuxLocal.RecvHeaderEnd


***
Bearer receive header end.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.MuxLocal.RecvHeaderStart


***
Bearer receive header start.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.MuxLocal.RecvStart


***
Bearer receive start.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.MuxLocal.SDUReadTimeoutException


***
Timed out reading SDU.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.MuxLocal.SDUWriteTimeoutException


***
Timed out writing SDU.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.MuxLocal.SendEnd


***
Bearer send end.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.MuxLocal.SendStart


***
Bearer send start.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.MuxLocal.Shutdown


***
Mux shutdown.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.MuxLocal.StartEagerly


***
Eagerly started.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.MuxLocal.StartOnDemand


***
Preparing to start.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.MuxLocal.StartedOnDemand


***
Started on demand.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.MuxLocal.State


***
State.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.MuxLocal.TCPInfo


***
TCPInfo.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.MuxLocal.Terminating


***
Terminating.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.PeerSelection.ChurnMode


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.PeerSelection.ChurnWait


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.PeerSelection.DemoteAsynchronous


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.PeerSelection.DemoteHotDone


***
target active, actual active, peer
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.PeerSelection.DemoteHotFailed


***
target active, actual active, peer, reason
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.PeerSelection.DemoteHotPeers


***
target active, actual active, selected peers
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.PeerSelection.DemoteLocalHotPeers


***
local per-group (target active, actual active), selected peers
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.PeerSelection.DemoteWarmDone


***
target established, actual established, peer
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.PeerSelection.DemoteWarmFailed


***
target established, actual established, peer, reason
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.PeerSelection.DemoteWarmPeers


***
target established, actual established, selected peers
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.PeerSelection.ForgetColdPeers


***
target known peers, actual known peers, selected peers
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.PeerSelection.GossipRequests


***
target known peers, actual known peers, peers available for gossip, peers selected for gossip
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.PeerSelection.GossipResults


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.PeerSelection.GovernorWakeup


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.PeerSelection.LocalRootPeersChanged


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.PeerSelection.PromoteColdDone


***
target active, actual active, selected peers
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.PeerSelection.PromoteColdFailed


***
target established, actual established, peer, delay until next promotion, reason
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.PeerSelection.PromoteColdLocalPeers


***
target local established, actual local established, selected peers
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.PeerSelection.PromoteColdPeers


***
target established, actual established, selected peers
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.PeerSelection.PromoteWarmAborted


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.PeerSelection.PromoteWarmDone


***
target active, actual active, peer
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.PeerSelection.PromoteWarmFailed


***
target active, actual active, peer, reason
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.PeerSelection.PromoteWarmLocalPeers


***
local per-group (target active, actual active), selected peers
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.PeerSelection.PromoteWarmPeers


***
target active, actual active, selected peers
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.PeerSelection.PublicRootsFailure


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.PeerSelection.PublicRootsRequest


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.PeerSelection.PublicRootsResults


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.PeerSelection.TargetsChanged


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.PeerSelectionActions.MonitoringError


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.PeerSelectionActions.MonitoringResult


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.PeerSelectionActions.StatusChangeFailure


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.PeerSelectionActions.StatusChanged


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.PeerSelectionCounters.PeerSelectionCounters


***
Counters for cold, warm and hot peers
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.Peers


***
TODO Doc
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.PublicRootPeers.PublicRootPeers.PublicRootDomains


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.PublicRootPeers.PublicRootPeers.PublicRootFailure


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.PublicRootPeers.PublicRootPeers.PublicRootRelayAccessPoint


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.PublicRootPeers.PublicRootPeers.PublicRootResult


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.ReplayBlock.LedgerReplay


***
Counts up the percent of a block replay.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.Resources


***
TODO JNF
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.Server.AcceptConnection


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Server.AcceptError


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Server.AcceptPolicy


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Server.Error


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Server.Started


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Server.Stopped


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Shutdown.AbnormalShutdown


***
non-isEOFerror shutdown request
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Shutdown.RequestingShutdown


***
Ringing the node shutdown doorbell
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Shutdown.ShutdownArmedAtSlotBlock


***
Setting up node shutdown at given slot / block.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Shutdown.ShutdownRequested


***
Node shutdown was requested.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Shutdown.ShutdownUnexpectedInput


***
Received shutdown request but found unexpected input in --shutdown-ipc FD: 
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Startup.Byron


***
_bibSystemStartTime_: TODO JNF 
_bibSlotLength_: gives the length of a slot as time interval. 
_bibEpochLength_: gives the number of slots which forms an epoch.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Startup.Common


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
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Startup.Network


***
_niAddresses_: IPv4 or IPv6 socket ready to accept connectionsor diffusion addresses. 
_niDiffusionMode_: shows if the node runs only initiator or bothinitiator or responder node. 
_niDnsProducers_: shows the list of domain names to subscribe to. 
_niIpProducers_: shows the list of ip subscription addresses.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Startup.NetworkConfig


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Startup.NetworkConfigUpdate


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Startup.NetworkConfigUpdateError


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Startup.P2PWarning


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Startup.P2PWarningDevelopementNetworkProtocols


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Startup.ShelleyBased


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
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Startup.StartupDBValidation


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Startup.StartupInfo


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Startup.StartupNetworkMagic


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Startup.StartupP2PInfo


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Startup.StartupSocketConfigError


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Startup.StartupTime


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.Startup.WarningDevelopmentNetworkProtocols


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.StateQueryClient.Recieve.Acquire


***
The client requests that the state as of a particular recent point onthe server's chain (within K of the tip) be made available to query,and waits for confirmation or failure.
From 'NodeToClient_V8' onwards if the point is not specified, current tipwill be acquired.  For previous versions of the protocol 'point' must begiven.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.StateQueryClient.Recieve.Acquired


***
The server can confirm that it has the state at the requested point.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.StateQueryClient.Recieve.Done


***
The client can terminate the protocol.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.StateQueryClient.Recieve.Failure


***
The server can report that it cannot obtain the state for therequested point.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.StateQueryClient.Recieve.Query


***
The client can perform queries on the current acquired state.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.StateQueryClient.Recieve.ReAcquire


***
This is like 'MsgAcquire' but for when the client already has astate. By moveing to another state directly without a 'MsgRelease' itenables optimisations on the server side (e.g. moving to the state forthe immediate next block).
Note that failure to re-acquire is equivalent to 'MsgRelease',rather than keeping the exiting acquired state.
From 'NodeToClient_V8' onwards if the point is not specified, current tipwill be acquired.  For previous versions of the protocol 'point' must begiven.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.StateQueryClient.Recieve.Release


***
The client can instruct the server to release the state. This letsthe server free resources.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.StateQueryClient.Recieve.Result


***
The server must reply with the queries.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.StateQueryClient.Send.Acquire


***
The client requests that the state as of a particular recent point onthe server's chain (within K of the tip) be made available to query,and waits for confirmation or failure.
From 'NodeToClient_V8' onwards if the point is not specified, current tipwill be acquired.  For previous versions of the protocol 'point' must begiven.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.StateQueryClient.Send.Acquired


***
The server can confirm that it has the state at the requested point.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.StateQueryClient.Send.Done


***
The client can terminate the protocol.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.StateQueryClient.Send.Failure


***
The server can report that it cannot obtain the state for therequested point.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.StateQueryClient.Send.Query


***
The client can perform queries on the current acquired state.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.StateQueryClient.Send.ReAcquire


***
This is like 'MsgAcquire' but for when the client already has astate. By moveing to another state directly without a 'MsgRelease' itenables optimisations on the server side (e.g. moving to the state forthe immediate next block).
Note that failure to re-acquire is equivalent to 'MsgRelease',rather than keeping the exiting acquired state.
From 'NodeToClient_V8' onwards if the point is not specified, current tipwill be acquired.  For previous versions of the protocol 'point' must begiven.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.StateQueryClient.Send.Release


***
The client can instruct the server to release the state. This letsthe server free resources.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.StateQueryClient.Send.Result


***
The server must reply with the queries.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxInbound.TxInboundCanRequestMoreTxs


***
There are no replies in flight, but we do know some more txs we can ask for, so lets ask for them and more txids.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxInbound.TxInboundCannotRequestMoreTxs


***
There's no replies in flight, and we have no more txs we can ask for so the only remaining thing to do is to ask for more txids. Since this is the only thing to do now, we make this a blocking call.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxInbound.TxInboundTerminated


***
Server received 'MsgDone'.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxInbound.TxSubmissionCollected


***
Number of transactions just about to be inserted.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.TxInbound.TxSubmissionProcessed


***
Just processed transaction pass/fail breakdown.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Info`

### Cardano.Node.TxMonitorClient.Recieve.Acquire


***
The client requests that the state as of a particular recent point onthe server's chain (within K of the tip) be made available to query,and waits for confirmation or failure.
From 'NodeToClient_V8' onwards if the point is not specified, current tipwill be acquired.  For previous versions of the protocol 'point' must begiven.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxMonitorClient.Recieve.Acquired


***
The server can confirm that it has the state at the requested point.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxMonitorClient.Recieve.Done


***
The client can terminate the protocol.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxMonitorClient.Recieve.Failure


***
The server can report that it cannot obtain the state for therequested point.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxMonitorClient.Recieve.Query


***
The client can perform queries on the current acquired state.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxMonitorClient.Recieve.ReAcquire


***
This is like 'MsgAcquire' but for when the client already has astate. By moveing to another state directly without a 'MsgRelease' itenables optimisations on the server side (e.g. moving to the state forthe immediate next block).
Note that failure to re-acquire is equivalent to 'MsgRelease',rather than keeping the exiting acquired state.
From 'NodeToClient_V8' onwards if the point is not specified, current tipwill be acquired.  For previous versions of the protocol 'point' must begiven.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxMonitorClient.Recieve.Release


***
The client can instruct the server to release the state. This letsthe server free resources.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxMonitorClient.Recieve.Result


***
The server must reply with the queries.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxMonitorClient.Send.Acquire


***
The client requests that the state as of a particular recent point onthe server's chain (within K of the tip) be made available to query,and waits for confirmation or failure.
From 'NodeToClient_V8' onwards if the point is not specified, current tipwill be acquired.  For previous versions of the protocol 'point' must begiven.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxMonitorClient.Send.Acquired


***
The server can confirm that it has the state at the requested point.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxMonitorClient.Send.Done


***
The client can terminate the protocol.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxMonitorClient.Send.Failure


***
The server can report that it cannot obtain the state for therequested point.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxMonitorClient.Send.Query


***
The client can perform queries on the current acquired state.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxMonitorClient.Send.ReAcquire


***
This is like 'MsgAcquire' but for when the client already has astate. By moveing to another state directly without a 'MsgRelease' itenables optimisations on the server side (e.g. moving to the state forthe immediate next block).
Note that failure to re-acquire is equivalent to 'MsgRelease',rather than keeping the exiting acquired state.
From 'NodeToClient_V8' onwards if the point is not specified, current tipwill be acquired.  For previous versions of the protocol 'point' must begiven.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxMonitorClient.Send.Release


***
The client can instruct the server to release the state. This letsthe server free resources.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxMonitorClient.Send.Result


***
The server must reply with the queries.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxOutbound.ControlMessage


***

***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxOutbound.RecvMsgRequest


***
The IDs of the transactions requested.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxOutbound.SendMsgReply


***
The transactions to be sent in the response.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxSubmission.NodeToNode.Recieve.Done


***
Termination message, initiated by the client when the server is making a blocking call for more transaction identifiers.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxSubmission.NodeToNode.Recieve.ReplyTxIds


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
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxSubmission.NodeToNode.Recieve.ReplyTxs


***
Reply with the requested transactions, or implicitly discard.
Transactions can become invalid between the time the transaction identifier was sent and the transaction being requested. Invalid (including committed) transactions do not need to be sent.
Any transaction identifiers requested but not provided in this reply should be considered as if this peer had never announced them. (Note that this is no guarantee that the transaction is invalid, it may still be valid and available from another peer).
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

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


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxSubmission.NodeToNode.Recieve.RequestTxs


***
Request one or more transactions corresponding to the given transaction identifiers. 
While it is the responsibility of the replying peer to keep within pipelining in-flight limits, the sender must also cooperate by keeping the total requested across all in-flight requests within the limits. 
It is an error to ask for transaction identifiers that were not previously announced (via 'MsgReplyTxIds'). 
It is an error to ask for transaction identifiers that are not outstanding or that were already asked for.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxSubmission.NodeToNode.Send.Done


***
Termination message, initiated by the client when the server is making a blocking call for more transaction identifiers.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxSubmission.NodeToNode.Send.ReplyTxIds


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
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxSubmission.NodeToNode.Send.ReplyTxs


***
Reply with the requested transactions, or implicitly discard.
Transactions can become invalid between the time the transaction identifier was sent and the transaction being requested. Invalid (including committed) transactions do not need to be sent.
Any transaction identifiers requested but not provided in this reply should be considered as if this peer had never announced them. (Note that this is no guarantee that the transaction is invalid, it may still be valid and available from another peer).
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

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


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxSubmission.NodeToNode.Send.RequestTxs


***
Request one or more transactions corresponding to the given transaction identifiers. 
While it is the responsibility of the replying peer to keep within pipelining in-flight limits, the sender must also cooperate by keeping the total requested across all in-flight requests within the limits. 
It is an error to ask for transaction identifiers that were not previously announced (via 'MsgReplyTxIds'). 
It is an error to ask for transaction identifiers that are not outstanding or that were already asked for.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxSubmission2.NodeToNode.Recieve.Done


***
Termination message, initiated by the client when the server ismaking a blocking call for more transaction identifiers.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxSubmission2.NodeToNode.Recieve.MsgHello


***
Client side hello message.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxSubmission2.NodeToNode.Recieve.ReplyTxIds


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
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxSubmission2.NodeToNode.Recieve.ReplyTxs


***
Reply with the requested transactions, or implicitly discard.
Transactions can become invalid between the time the transactionidentifier was sent and the transaction being requested. Invalid(including committed) transactions do not need to be sent.
Any transaction identifiers requested but not provided in this replyshould be considered as if this peer had never announced them. (Notethat this is no guarantee that the transaction is invalid, it may stillbe valid and available from another peer).
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

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


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxSubmission2.NodeToNode.Recieve.RequestTxs


***
Request one or more transactions corresponding to the given transaction identifiers. 
While it is the responsibility of the replying peer to keep withinpipelining in-flight limits, the sender must also cooperate by keepingthe total requested across all in-flight requests within the limits.
It is an error to ask for transaction identifiers that were notpreviously announced (via 'MsgReplyTxIds').
It is an error to ask for transaction identifiers that are notoutstanding or that were already asked for.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxSubmission2.NodeToNode.Send.Done


***
Termination message, initiated by the client when the server ismaking a blocking call for more transaction identifiers.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxSubmission2.NodeToNode.Send.MsgHello


***
Client side hello message.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxSubmission2.NodeToNode.Send.ReplyTxIds


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
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxSubmission2.NodeToNode.Send.ReplyTxs


***
Reply with the requested transactions, or implicitly discard.
Transactions can become invalid between the time the transactionidentifier was sent and the transaction being requested. Invalid(including committed) transactions do not need to be sent.
Any transaction identifiers requested but not provided in this replyshould be considered as if this peer had never announced them. (Notethat this is no guarantee that the transaction is invalid, it may stillbe valid and available from another peer).
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

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


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxSubmission2.NodeToNode.Send.RequestTxs


***
Request one or more transactions corresponding to the given transaction identifiers. 
While it is the responsibility of the replying peer to keep withinpipelining in-flight limits, the sender must also cooperate by keepingthe total requested across all in-flight requests within the limits.
It is an error to ask for transaction identifiers that were notpreviously announced (via 'MsgReplyTxIds').
It is an error to ask for transaction identifiers that are notoutstanding or that were already asked for.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxSubmissionClient.Recieve.AcceptTx


***
The server can reply to inform the client that it has accepted thetransaction.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxSubmissionClient.Recieve.Done


***
The client can terminate the protocol.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxSubmissionClient.Recieve.RejectTx


***
The server can reply to inform the client that it has rejected thetransaction. A reason for the rejection is included.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxSubmissionClient.Recieve.SubmitTx


***
The client submits a single transaction and waits a reply.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxSubmissionClient.Send.AcceptTx


***
The server can reply to inform the client that it has accepted thetransaction.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxSubmissionClient.Send.Done


***
The client can terminate the protocol.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxSubmissionClient.Send.RejectTx


***
The server can reply to inform the client that it has rejected thetransaction. A reason for the rejection is included.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

### Cardano.Node.TxSubmissionClient.Send.SubmitTx


***
The client submits a single transaction and waits a reply.
***


From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`Forwarder`,
			`EKGBackend`
Filtered  by config value: `Notice`

## Metrics
### Block replay progress (%)

***
Progress in percent
***


Dispatched by: 
Cardano.Node.ReplayBlock.LedgerReplay

From current configuration:
Filtered  by config value: `Info`

### blocksForgedNum

***
How many blocks did forge in this node?
***


Dispatched by: 
Cardano.Node.ForgeStats

From current configuration:
Filtered  by config value: `Info`

### cardano.node.aboutToLeadSlotLast

***

***


Dispatched by: 
Cardano.Node.Forge.StartLeadershipCheck

From current configuration:
Filtered  by config value: `Info`

### cardano.node.aboutToLeadSlotLast

***

***


Dispatched by: 
Cardano.Node.Forge.StartLeadershipCheckPlus

From current configuration:
Filtered  by config value: `Info`

### cardano.node.adoptedSlotLast

***

***


Dispatched by: 
Cardano.Node.Forge.AdoptedBlock

From current configuration:
Filtered  by config value: `Info`

### cardano.node.blockContext

***

***


Dispatched by: 
Cardano.Node.Forge.BlockContext

From current configuration:
Filtered  by config value: `Info`

### cardano.node.blockFromFuture

***

***


Dispatched by: 
Cardano.Node.Forge.BlockFromFuture

From current configuration:
Filtered  by config value: `Info`

### cardano.node.blocks

***
Number of blocks in this chain fragment.
***


Dispatched by: 
Cardano.Node.ChainDB.AddBlockEvent.AddedToCurrentChain

From current configuration:
Filtered  by config value: `Info`

### cardano.node.blocks

***
Number of blocks in this chain fragment.
***


Dispatched by: 
Cardano.Node.ChainDB.AddBlockEvent.SwitchedToAFork

From current configuration:
Filtered  by config value: `Info`

### cardano.node.connectedPeers

***
Number of connected peers
***


Dispatched by: 
Cardano.Node.BlockFetchDecision

From current configuration:
Filtered  by config value: `Info`

### cardano.node.connectionManager.duplexConns

***

***


Dispatched by: 
Cardano.Node.ConnectionManager.ConnectionManagerCounters

From current configuration:
Filtered  by config value: `Info`

### cardano.node.connectionManager.duplexConns

***

***


Dispatched by: 
Cardano.Node.LocalConnectionManager.ConnectionManagerCounters

From current configuration:
Filtered  by config value: `Info`

### cardano.node.connectionManager.fullDuplexConns

***

***


Dispatched by: 
Cardano.Node.ConnectionManager.ConnectionManagerCounters

From current configuration:
Filtered  by config value: `Info`

### cardano.node.connectionManager.fullDuplexConns

***

***


Dispatched by: 
Cardano.Node.LocalConnectionManager.ConnectionManagerCounters

From current configuration:
Filtered  by config value: `Info`

### cardano.node.connectionManager.inboundConns

***

***


Dispatched by: 
Cardano.Node.ConnectionManager.ConnectionManagerCounters

From current configuration:
Filtered  by config value: `Info`

### cardano.node.connectionManager.inboundConns

***

***


Dispatched by: 
Cardano.Node.LocalConnectionManager.ConnectionManagerCounters

From current configuration:
Filtered  by config value: `Info`

### cardano.node.connectionManager.outboundConns

***

***


Dispatched by: 
Cardano.Node.ConnectionManager.ConnectionManagerCounters

From current configuration:
Filtered  by config value: `Info`

### cardano.node.connectionManager.outboundConns

***

***


Dispatched by: 
Cardano.Node.LocalConnectionManager.ConnectionManagerCounters

From current configuration:
Filtered  by config value: `Info`

### cardano.node.connectionManager.unidirectionalConns

***

***


Dispatched by: 
Cardano.Node.ConnectionManager.ConnectionManagerCounters

From current configuration:
Filtered  by config value: `Info`

### cardano.node.connectionManager.unidirectionalConns

***

***


Dispatched by: 
Cardano.Node.LocalConnectionManager.ConnectionManagerCounters

From current configuration:
Filtered  by config value: `Info`

### cardano.node.couldNotForgeSlotLast

***

***


Dispatched by: 
Cardano.Node.Forge.NoLedgerState

From current configuration:
Filtered  by config value: `Info`

### cardano.node.couldNotForgeSlotLast

***

***


Dispatched by: 
Cardano.Node.Forge.NoLedgerView

From current configuration:
Filtered  by config value: `Info`

### cardano.node.currentKESPeriod

***

***


Dispatched by: 
Cardano.Node.Forge.ForgeStateUpdateError

From current configuration:
Filtered  by config value: `Info`

### cardano.node.delegMapSize

***

***


Dispatched by: 
Cardano.Node.Forge.StartLeadershipCheckPlus

From current configuration:
Filtered  by config value: `Info`

### cardano.node.density

***
The actual number of blocks created over the maximum expected number of blocks that could be created over the span of the last @k@ blocks.
***


Dispatched by: 
Cardano.Node.ChainDB.AddBlockEvent.AddedToCurrentChain

From current configuration:
Filtered  by config value: `Info`

### cardano.node.density

***
The actual number of blocks created over the maximum expected number of blocks that could be created over the span of the last @k@ blocks.
***


Dispatched by: 
Cardano.Node.ChainDB.AddBlockEvent.SwitchedToAFork

From current configuration:
Filtered  by config value: `Info`

### cardano.node.epoch

***
In which epoch is the tip of the current chain.
***


Dispatched by: 
Cardano.Node.ChainDB.AddBlockEvent.AddedToCurrentChain

From current configuration:
Filtered  by config value: `Info`

### cardano.node.epoch

***
In which epoch is the tip of the current chain.
***


Dispatched by: 
Cardano.Node.ChainDB.AddBlockEvent.SwitchedToAFork

From current configuration:
Filtered  by config value: `Info`

### cardano.node.forgedInvalidSlotLast

***

***


Dispatched by: 
Cardano.Node.Forge.ForgedInvalidBlock

From current configuration:
Filtered  by config value: `Info`

### cardano.node.forgedSlotLast

***

***


Dispatched by: 
Cardano.Node.Forge.ForgedBlock

From current configuration:
Filtered  by config value: `Info`

### cardano.node.inbound-governor.cold

***

***


Dispatched by: 
Cardano.Node.InboundGovernor.InboundGovernorCounters

From current configuration:
Filtered  by config value: `Info`

### cardano.node.inbound-governor.cold

***

***


Dispatched by: 
Cardano.Node.LocalInboundGovernor.InboundGovernorCounters

From current configuration:
Filtered  by config value: `Info`

### cardano.node.inbound-governor.hot

***

***


Dispatched by: 
Cardano.Node.InboundGovernor.InboundGovernorCounters

From current configuration:
Filtered  by config value: `Info`

### cardano.node.inbound-governor.hot

***

***


Dispatched by: 
Cardano.Node.LocalInboundGovernor.InboundGovernorCounters

From current configuration:
Filtered  by config value: `Info`

### cardano.node.inbound-governor.idle

***

***


Dispatched by: 
Cardano.Node.InboundGovernor.InboundGovernorCounters

From current configuration:
Filtered  by config value: `Info`

### cardano.node.inbound-governor.idle

***

***


Dispatched by: 
Cardano.Node.LocalInboundGovernor.InboundGovernorCounters

From current configuration:
Filtered  by config value: `Info`

### cardano.node.inbound-governor.warm

***

***


Dispatched by: 
Cardano.Node.InboundGovernor.InboundGovernorCounters

From current configuration:
Filtered  by config value: `Info`

### cardano.node.inbound-governor.warm

***

***


Dispatched by: 
Cardano.Node.LocalInboundGovernor.InboundGovernorCounters

From current configuration:
Filtered  by config value: `Info`

### cardano.node.ledgerState

***

***


Dispatched by: 
Cardano.Node.Forge.LedgerState

From current configuration:
Filtered  by config value: `Info`

### cardano.node.ledgerView

***

***


Dispatched by: 
Cardano.Node.Forge.LedgerView

From current configuration:
Filtered  by config value: `Info`

### cardano.node.mempoolBytes

***
Byte size of the mempool
***


Dispatched by: 
Cardano.Node.Mempool.AddedTx

From current configuration:
Filtered  by config value: `Info`

### cardano.node.mempoolBytes

***
Byte size of the mempool
***


Dispatched by: 
Cardano.Node.Mempool.ManuallyRemovedTxs

From current configuration:
Filtered  by config value: `Info`

### cardano.node.mempoolBytes

***
Byte size of the mempool
***


Dispatched by: 
Cardano.Node.Mempool.RejectedTx

From current configuration:
Filtered  by config value: `Info`

### cardano.node.mempoolBytes

***
Byte size of the mempool
***


Dispatched by: 
Cardano.Node.Mempool.RemoveTxs

From current configuration:
Filtered  by config value: `Info`

### cardano.node.metrics.served.header

***
A counter triggered ony on header event
***


Dispatched by: 
Cardano.Node.ChainSyncServerHeader.ChainSyncServerEvent.ServerRead.RollBackward

From current configuration:
Filtered  by config value: `Info`

### cardano.node.metrics.served.header

***
A counter triggered ony on header event
***


Dispatched by: 
Cardano.Node.ChainSyncServerHeader.ChainSyncServerEvent.ServerRead.RollForward

From current configuration:
Filtered  by config value: `Info`

### cardano.node.metrics.served.header

***
A counter triggered ony on header event
***


Dispatched by: 
Cardano.Node.ChainSyncServerHeader.ChainSyncServerEvent.ServerRead.ServerRead

From current configuration:
Filtered  by config value: `Info`

### cardano.node.metrics.served.header

***
A counter triggered ony on header event
***


Dispatched by: 
Cardano.Node.ChainSyncServerHeader.ChainSyncServerEvent.ServerRead.ServerReadBlocked

From current configuration:
Filtered  by config value: `Info`

### cardano.node.nodeCannotForge

***

***


Dispatched by: 
Cardano.Node.Forge.NodeCannotForge

From current configuration:
Filtered  by config value: `Info`

### cardano.node.nodeIsLeader

***

***


Dispatched by: 
Cardano.Node.Forge.NodeIsLeader

From current configuration:
Filtered  by config value: `Info`

### cardano.node.nodeNotLeader

***

***


Dispatched by: 
Cardano.Node.Forge.NodeNotLeader

From current configuration:
Filtered  by config value: `Info`

### cardano.node.notAdoptedSlotLast

***

***


Dispatched by: 
Cardano.Node.Forge.DidntAdoptBlock

From current configuration:
Filtered  by config value: `Info`

### cardano.node.operationalCertificateExpiryKESPeriod

***

***


Dispatched by: 
Cardano.Node.Forge.ForgeStateUpdateError

From current configuration:
Filtered  by config value: `Info`

### cardano.node.operationalCertificateStartKESPeriod

***

***


Dispatched by: 
Cardano.Node.Forge.ForgeStateUpdateError

From current configuration:
Filtered  by config value: `Info`

### cardano.node.peerSelection.cold

***
Number of cold peers
***


Dispatched by: 
Cardano.Node.PeerSelectionCounters.PeerSelectionCounters

From current configuration:
Filtered  by config value: `Info`

### cardano.node.peerSelection.hot

***
Number of hot peers
***


Dispatched by: 
Cardano.Node.PeerSelectionCounters.PeerSelectionCounters

From current configuration:
Filtered  by config value: `Info`

### cardano.node.peerSelection.warm

***
Number of warm peers
***


Dispatched by: 
Cardano.Node.PeerSelectionCounters.PeerSelectionCounters

From current configuration:
Filtered  by config value: `Info`

### cardano.node.remainingKESPeriods

***

***


Dispatched by: 
Cardano.Node.Forge.ForgeStateUpdateError

From current configuration:
Filtered  by config value: `Info`

### cardano.node.served.block

***

***


Dispatched by: 
Cardano.Node.BlockFetchServer.SendBlock

From current configuration:
Filtered  by config value: `Info`

### cardano.node.slotInEpoch

***
Relative slot number of the tip of the current chain within theepoch..
***


Dispatched by: 
Cardano.Node.ChainDB.AddBlockEvent.AddedToCurrentChain

From current configuration:
Filtered  by config value: `Info`

### cardano.node.slotInEpoch

***
Relative slot number of the tip of the current chain within theepoch..
***


Dispatched by: 
Cardano.Node.ChainDB.AddBlockEvent.SwitchedToAFork

From current configuration:
Filtered  by config value: `Info`

### cardano.node.slotIsImmutable

***

***


Dispatched by: 
Cardano.Node.Forge.SlotIsImmutable

From current configuration:
Filtered  by config value: `Info`

### cardano.node.slots

***
Number of slots in this chain fragment.
***


Dispatched by: 
Cardano.Node.ChainDB.AddBlockEvent.AddedToCurrentChain

From current configuration:
Filtered  by config value: `Info`

### cardano.node.slots

***
Number of slots in this chain fragment.
***


Dispatched by: 
Cardano.Node.ChainDB.AddBlockEvent.SwitchedToAFork

From current configuration:
Filtered  by config value: `Info`

### cardano.node.submissions.accepted

***

***


Dispatched by: 
Cardano.Node.TxInbound.TxSubmissionProcessed

From current configuration:
Filtered  by config value: `Info`

### cardano.node.submissions.rejected

***

***


Dispatched by: 
Cardano.Node.TxInbound.TxSubmissionProcessed

From current configuration:
Filtered  by config value: `Info`

### cardano.node.submissions.submitted

***

***


Dispatched by: 
Cardano.Node.TxInbound.TxSubmissionCollected

From current configuration:
Filtered  by config value: `Info`

### cardano.node.txsInMempool

***
Transactions in mempool
***


Dispatched by: 
Cardano.Node.Mempool.AddedTx

From current configuration:
Filtered  by config value: `Info`

### cardano.node.txsInMempool

***
Transactions in mempool
***


Dispatched by: 
Cardano.Node.Mempool.ManuallyRemovedTxs

From current configuration:
Filtered  by config value: `Info`

### cardano.node.txsInMempool

***
Transactions in mempool
***


Dispatched by: 
Cardano.Node.Mempool.RejectedTx

From current configuration:
Filtered  by config value: `Info`

### cardano.node.txsInMempool

***
Transactions in mempool
***


Dispatched by: 
Cardano.Node.Mempool.RemoveTxs

From current configuration:
Filtered  by config value: `Info`

### cardano.node.txsProcessedNum

***

***


Dispatched by: 
Cardano.Node.Mempool.ManuallyRemovedTxs

From current configuration:
Filtered  by config value: `Info`

### cardano.node.utxoSize

***

***


Dispatched by: 
Cardano.Node.Forge.StartLeadershipCheckPlus

From current configuration:
Filtered  by config value: `Info`

### mem.resident

***
TODO JNF
***


Dispatched by: 
Cardano.Node.Resources

From current configuration:
Filtered  by config value: `Info`

### nodeCannotForgeNum

***
How many times this node could not forge?
***


Dispatched by: 
Cardano.Node.ForgeStats

From current configuration:
Filtered  by config value: `Info`

### nodeIsLeaderNum

***
How many times this node was leader?
***


Dispatched by: 
Cardano.Node.ForgeStats

From current configuration:
Filtered  by config value: `Info`

### peersFromNodeKernel

***
TODO Doc
***


Dispatched by: 
Cardano.Node.Peers

From current configuration:
Filtered  by config value: `Info`

### rts.gcLiveBytes

***
TODO JNF
***


Dispatched by: 
Cardano.Node.Resources

From current configuration:
Filtered  by config value: `Info`

### rts.gcMajorNum

***
TODO JNF
***


Dispatched by: 
Cardano.Node.Resources

From current configuration:
Filtered  by config value: `Info`

### rts.gcMinorNum

***
TODO JNF
***


Dispatched by: 
Cardano.Node.Resources

From current configuration:
Filtered  by config value: `Info`

### rts.gcticks

***
TODO JNF
***


Dispatched by: 
Cardano.Node.Resources

From current configuration:
Filtered  by config value: `Info`

### rts.mutticks

***
TODO JNF
***


Dispatched by: 
Cardano.Node.Resources

From current configuration:
Filtered  by config value: `Info`

### rts.threads

***
TODO JNF
***


Dispatched by: 
Cardano.Node.Resources

From current configuration:
Filtered  by config value: `Info`

### slotsMissed

***
How many slots were missed in this node?
***


Dispatched by: 
Cardano.Node.ForgeStats

From current configuration:
Filtered  by config value: `Info`

### stat.cputicks

***
Reports the CPU ticks, sice the process was started
***


Dispatched by: 
Cardano.Node.Resources

From current configuration:
Filtered  by config value: `Info`

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


Configuration: TraceConfig {tcOptions = fromList [(["Node"],[ConfSeverity {severity = Notice},ConfDetail {detail = DNormal},ConfBackend {backends = [Stdout MachineFormat,EKGBackend,Forwarder]}]),(["Node","AcceptPolicy"],[ConfSeverity {severity = Info}]),(["Node","BlockFetchClient","CompletedBlockFetch"],[ConfLimiter {maxFrequency = 2.0}]),(["Node","BlockFetchDecision"],[ConfSeverity {severity = Info}]),(["Node","BlockFetchServer","SendBlock"],[ConfSeverity {severity = Info}]),(["Node","ChainDB"],[ConfSeverity {severity = Info}]),(["Node","ChainDB","AddBlockEvent","AddBlockValidation","ValidCandidate"],[ConfLimiter {maxFrequency = 2.0}]),(["Node","ChainDB","AddBlockEvent","AddedBlockToQueue"],[ConfLimiter {maxFrequency = 2.0}]),(["Node","ChainDB","AddBlockEvent","AddedBlockToVolatileDB"],[ConfLimiter {maxFrequency = 2.0}]),(["Node","ChainDB","AddBlockEvent","AddedToCurrentChain"],[ConfSeverity {severity = Info}]),(["Node","ChainDB","AddBlockEvent","SwitchedToAFork"],[ConfSeverity {severity = Info}]),(["Node","ChainDB","CopyToImmutableDBEvent","CopiedBlockToImmutableDB"],[ConfLimiter {maxFrequency = 2.0}]),(["Node","ChainSyncServerHeader","ChainSyncServerEvent","ServerRead","RollBackward"],[ConfSeverity {severity = Info}]),(["Node","ChainSyncServerHeader","ChainSyncServerEvent","ServerRead","RollForward"],[ConfSeverity {severity = Info}]),(["Node","ChainSyncServerHeader","ChainSyncServerEvent","ServerRead","ServerRead"],[ConfSeverity {severity = Info}]),(["Node","ChainSyncServerHeader","ChainSyncServerEvent","ServerRead","ServerReadBlocked"],[ConfSeverity {severity = Info}]),(["Node","ConnectionManager","ConnectionManagerCounters"],[ConfSeverity {severity = Info}]),(["Node","DNSResolver"],[ConfSeverity {severity = Info}]),(["Node","DNSSubscription"],[ConfSeverity {severity = Info}]),(["Node","DiffusionInit"],[ConfSeverity {severity = Info}]),(["Node","ErrorPolicy"],[ConfSeverity {severity = Info}]),(["Node","Forge"],[ConfSeverity {severity = Info}]),(["Node","Forge","AdoptedBlock"],[ConfSeverity {severity = Info}]),(["Node","Forge","BlockContext"],[ConfSeverity {severity = Info}]),(["Node","Forge","BlockFromFuture"],[ConfSeverity {severity = Info}]),(["Node","Forge","DidntAdoptBlock"],[ConfSeverity {severity = Info}]),(["Node","Forge","ForgeStateUpdateError"],[ConfSeverity {severity = Info}]),(["Node","Forge","ForgedBlock"],[ConfSeverity {severity = Info}]),(["Node","Forge","ForgedInvalidBlock"],[ConfSeverity {severity = Info}]),(["Node","Forge","LedgerState"],[ConfSeverity {severity = Info}]),(["Node","Forge","LedgerView"],[ConfSeverity {severity = Info}]),(["Node","Forge","NoLedgerState"],[ConfSeverity {severity = Info}]),(["Node","Forge","NoLedgerView"],[ConfSeverity {severity = Info}]),(["Node","Forge","NodeCannotForge"],[ConfSeverity {severity = Info}]),(["Node","Forge","NodeIsLeader"],[ConfSeverity {severity = Info}]),(["Node","Forge","NodeNotLeader"],[ConfSeverity {severity = Info}]),(["Node","Forge","SlotIsImmutable"],[ConfSeverity {severity = Info}]),(["Node","Forge","StartLeadershipCheck"],[ConfSeverity {severity = Info}]),(["Node","Forge","StartLeadershipCheckPlus"],[ConfSeverity {severity = Info}]),(["Node","ForgeStats"],[ConfSeverity {severity = Info}]),(["Node","InboundGovernor","InboundGovernorCounters"],[ConfSeverity {severity = Info}]),(["Node","IpSubscription"],[ConfSeverity {severity = Info}]),(["Node","LocalConnectionManager","ConnectionManagerCounters"],[ConfSeverity {severity = Info}]),(["Node","LocalErrorPolicy"],[ConfSeverity {severity = Info}]),(["Node","LocalInboundGovernor","InboundGovernorCounters"],[ConfSeverity {severity = Info}]),(["Node","Mempool"],[ConfSeverity {severity = Info}]),(["Node","Mempool","AddedTx"],[ConfSeverity {severity = Info}]),(["Node","Mempool","ManuallyRemovedTxs"],[ConfSeverity {severity = Info}]),(["Node","Mempool","RejectedTx"],[ConfSeverity {severity = Info}]),(["Node","Mempool","RemoveTxs"],[ConfSeverity {severity = Info}]),(["Node","PeerSelectionCounters","PeerSelectionCounters"],[ConfSeverity {severity = Info}]),(["Node","Peers"],[ConfSeverity {severity = Info}]),(["Node","ReplayBlock","LedgerReplay"],[ConfSeverity {severity = Info}]),(["Node","Resources"],[ConfSeverity {severity = Info}]),(["Node","TxInbound","TxSubmissionCollected"],[ConfSeverity {severity = Info}]),(["Node","TxInbound","TxSubmissionProcessed"],[ConfSeverity {severity = Info}])], tcForwarder = TraceOptionForwarder {tofAddress = LocalSocket "/tmp/forwarder.sock", tofMode = Initiator, tofConnQueueSize = 2000, tofDisconnQueueSize = 200000, tofVerbosity = Minimum}, tcNodeName = Nothing, tcPeerFreqency = Just 2000, tcResourceFreqency = Just 5000}

682 log messages.
Generated at 2022-05-18 15:22:36.737189067 CEST.
