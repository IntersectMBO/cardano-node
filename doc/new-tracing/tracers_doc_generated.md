# Cardano Trace Documentation
# Table Of Contents


## [Trace Messages](#trace-messages)
1. __BlockFetch__
	1. __ClientEvent__
		1. [AcknowledgedFetchRequest](#blockfetchclienteventacknowledgedfetchrequest)
		1. [AddedFetchRequest](#blockfetchclienteventaddedfetchrequest)
		1. [ClientTerminating](#blockfetchclienteventclientterminating)
		1. [CompletedBlockFetch](#blockfetchclienteventcompletedblockfetch)
		1. [CompletedFetchBatch](#blockfetchclienteventcompletedfetchbatch)
		1. [RejectedFetchBatch](#blockfetchclienteventrejectedfetchbatch)
		1. [SendFetchRequest](#blockfetchclienteventsendfetchrequest)
		1. [StartedFetchBatch](#blockfetchclienteventstartedfetchbatch)
	1. [Decision](#blockfetchdecision)
	1. __Remote__
		1. __Receive__
			1. [BatchDone](#blockfetchremotereceivebatchdone)
			1. [Block](#blockfetchremotereceiveblock)
			1. [ClientDone](#blockfetchremotereceiveclientdone)
			1. [NoBlocks](#blockfetchremotereceivenoblocks)
			1. [RequestRange](#blockfetchremotereceiverequestrange)
			1. [StartBatch](#blockfetchremotereceivestartbatch)
		1. __Send__
			1. [BatchDone](#blockfetchremotesendbatchdone)
			1. [Block](#blockfetchremotesendblock)
			1. [ClientDone](#blockfetchremotesendclientdone)
			1. [NoBlocks](#blockfetchremotesendnoblocks)
			1. [RequestRange](#blockfetchremotesendrequestrange)
			1. [StartBatch](#blockfetchremotesendstartbatch)
		1. __Serialised__
			1. __Receive__
				1. [BatchDone](#blockfetchremoteserialisedreceivebatchdone)
				1. [Block](#blockfetchremoteserialisedreceiveblock)
				1. [ClientDone](#blockfetchremoteserialisedreceiveclientdone)
				1. [NoBlocks](#blockfetchremoteserialisedreceivenoblocks)
				1. [RequestRange](#blockfetchremoteserialisedreceiverequestrange)
				1. [StartBatch](#blockfetchremoteserialisedreceivestartbatch)
			1. __Send__
				1. [BatchDone](#blockfetchremoteserialisedsendbatchdone)
				1. [Block](#blockfetchremoteserialisedsendblock)
				1. [ClientDone](#blockfetchremoteserialisedsendclientdone)
				1. [NoBlocks](#blockfetchremoteserialisedsendnoblocks)
				1. [RequestRange](#blockfetchremoteserialisedsendrequestrange)
				1. [StartBatch](#blockfetchremoteserialisedsendstartbatch)
	1. __ServerBlock__
		1. [SendBlock](#blockfetchserverblocksendblock)
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
	1. __ReplayBlock__
		1. [LedgerReplay](#chaindbreplayblockledgerreplay)
	1. __VolatileDBEvent__
		1. [BlockAlreadyHere](#chaindbvolatiledbeventblockalreadyhere)
		1. [DBAlreadyClosed](#chaindbvolatiledbeventdbalreadyclosed)
		1. [InvalidFileNames](#chaindbvolatiledbeventinvalidfilenames)
		1. [Truncate](#chaindbvolatiledbeventtruncate)
1. __ChainSync__
	1. __ClientEvent__
		1. [DownloadedHeader](#chainsyncclienteventdownloadedheader)
		1. [Exception](#chainsyncclienteventexception)
		1. [FoundIntersection](#chainsyncclienteventfoundintersection)
		1. [RolledBack](#chainsyncclienteventrolledback)
		1. [Termination](#chainsyncclienteventtermination)
	1. __Local__
		1. __Receive__
			1. [AwaitReply](#chainsynclocalreceiveawaitreply)
			1. [Done](#chainsynclocalreceivedone)
			1. [FindIntersect](#chainsynclocalreceivefindintersect)
			1. [IntersectFound](#chainsynclocalreceiveintersectfound)
			1. [IntersectNotFound](#chainsynclocalreceiveintersectnotfound)
			1. [RequestNext](#chainsynclocalreceiverequestnext)
			1. [RollBackward](#chainsynclocalreceiverollbackward)
			1. [RollForward](#chainsynclocalreceiverollforward)
		1. __Send__
			1. [AwaitReply](#chainsynclocalsendawaitreply)
			1. [Done](#chainsynclocalsenddone)
			1. [FindIntersect](#chainsynclocalsendfindintersect)
			1. [IntersectFound](#chainsynclocalsendintersectfound)
			1. [IntersectNotFound](#chainsynclocalsendintersectnotfound)
			1. [RequestNext](#chainsynclocalsendrequestnext)
			1. [RollBackward](#chainsynclocalsendrollbackward)
			1. [RollForward](#chainsynclocalsendrollforward)
	1. __Remote__
		1. __Receive__
			1. [AwaitReply](#chainsyncremotereceiveawaitreply)
			1. [Done](#chainsyncremotereceivedone)
			1. [FindIntersect](#chainsyncremotereceivefindintersect)
			1. [IntersectFound](#chainsyncremotereceiveintersectfound)
			1. [IntersectNotFound](#chainsyncremotereceiveintersectnotfound)
			1. [RequestNext](#chainsyncremotereceiverequestnext)
			1. [RollBackward](#chainsyncremotereceiverollbackward)
			1. [RollForward](#chainsyncremotereceiverollforward)
		1. __Send__
			1. [AwaitReply](#chainsyncremotesendawaitreply)
			1. [Done](#chainsyncremotesenddone)
			1. [FindIntersect](#chainsyncremotesendfindintersect)
			1. [IntersectFound](#chainsyncremotesendintersectfound)
			1. [IntersectNotFound](#chainsyncremotesendintersectnotfound)
			1. [RequestNext](#chainsyncremotesendrequestnext)
			1. [RollBackward](#chainsyncremotesendrollbackward)
			1. [RollForward](#chainsyncremotesendrollforward)
		1. __Serialised__
			1. __Receive__
				1. [AwaitReply](#chainsyncremoteserialisedreceiveawaitreply)
				1. [Done](#chainsyncremoteserialisedreceivedone)
				1. [FindIntersect](#chainsyncremoteserialisedreceivefindintersect)
				1. [IntersectFound](#chainsyncremoteserialisedreceiveintersectfound)
				1. [IntersectNotFound](#chainsyncremoteserialisedreceiveintersectnotfound)
				1. [RequestNext](#chainsyncremoteserialisedreceiverequestnext)
				1. [RollBackward](#chainsyncremoteserialisedreceiverollbackward)
				1. [RollForward](#chainsyncremoteserialisedreceiverollforward)
			1. __Send__
				1. [AwaitReply](#chainsyncremoteserialisedsendawaitreply)
				1. [Done](#chainsyncremoteserialisedsenddone)
				1. [FindIntersect](#chainsyncremoteserialisedsendfindintersect)
				1. [IntersectFound](#chainsyncremoteserialisedsendintersectfound)
				1. [IntersectNotFound](#chainsyncremoteserialisedsendintersectnotfound)
				1. [RequestNext](#chainsyncremoteserialisedsendrequestnext)
				1. [RollBackward](#chainsyncremoteserialisedsendrollbackward)
				1. [RollForward](#chainsyncremoteserialisedsendrollforward)
	1. __ServerBlock__
		1. [Update](#chainsyncserverblockupdate)
	1. __ServerHeader__
		1. [Update](#chainsyncserverheaderupdate)
1. __Forge__
	1. [KESInfo](#forgekesinfo)
	1. __StateInfo__
		1. [AdoptedBlock](#forgestateinfoadoptedblock)
		1. [BlockContext](#forgestateinfoblockcontext)
		1. [BlockFromFuture](#forgestateinfoblockfromfuture)
		1. [DidntAdoptBlock](#forgestateinfodidntadoptblock)
		1. [ForgeStateUpdateError](#forgestateinfoforgestateupdateerror)
		1. [ForgedBlock](#forgestateinfoforgedblock)
		1. [ForgedInvalidBlock](#forgestateinfoforgedinvalidblock)
		1. [LedgerState](#forgestateinfoledgerstate)
		1. [LedgerView](#forgestateinfoledgerview)
		1. [NoLedgerState](#forgestateinfonoledgerstate)
		1. [NoLedgerView](#forgestateinfonoledgerview)
		1. [NodeCannotForge](#forgestateinfonodecannotforge)
		1. [NodeIsLeader](#forgestateinfonodeisleader)
		1. [NodeNotLeader](#forgestateinfonodenotleader)
		1. [SlotIsImmutable](#forgestateinfoslotisimmutable)
		1. [StartLeadershipCheck](#forgestateinfostartleadershipcheck)
		1. [StartLeadershipCheckPlus](#forgestateinfostartleadershipcheckplus)
	1. [Stats](#forgestats)
1. __Mempool__
	1. [AddedTx](#mempooladdedtx)
	1. [ManuallyRemovedTxs](#mempoolmanuallyremovedtxs)
	1. [RejectedTx](#mempoolrejectedtx)
	1. [RemoveTxs](#mempoolremovetxs)
1. __Net__
	1. __AcceptPolicy__
		1. [ConnectionHardLimit](#netacceptpolicyconnectionhardlimit)
		1. [ConnectionLimitResume](#netacceptpolicyconnectionlimitresume)
		1. [ConnectionRateLimiting](#netacceptpolicyconnectionratelimiting)
	1. __ConnectionManager__
		1. __Local__
			1. [Connect](#netconnectionmanagerlocalconnect)
			1. [ConnectError](#netconnectionmanagerlocalconnecterror)
			1. [ConnectionCleanup](#netconnectionmanagerlocalconnectioncleanup)
			1. [ConnectionExists](#netconnectionmanagerlocalconnectionexists)
			1. [ConnectionFailure](#netconnectionmanagerlocalconnectionfailure)
			1. [ConnectionHandler](#netconnectionmanagerlocalconnectionhandler)
			1. [ConnectionManagerCounters](#netconnectionmanagerlocalconnectionmanagercounters)
			1. [ConnectionNotFound](#netconnectionmanagerlocalconnectionnotfound)
			1. [ConnectionTimeWait](#netconnectionmanagerlocalconnectiontimewait)
			1. [ConnectionTimeWaitDone](#netconnectionmanagerlocalconnectiontimewaitdone)
			1. [ForbiddenConnection](#netconnectionmanagerlocalforbiddenconnection)
			1. [ForbiddenOperation](#netconnectionmanagerlocalforbiddenoperation)
			1. [ImpossibleConnection](#netconnectionmanagerlocalimpossibleconnection)
			1. [IncludeConnection](#netconnectionmanagerlocalincludeconnection)
			1. [PruneConnections](#netconnectionmanagerlocalpruneconnections)
			1. [Shutdown](#netconnectionmanagerlocalshutdown)
			1. [State](#netconnectionmanagerlocalstate)
			1. [TerminatedConnection](#netconnectionmanagerlocalterminatedconnection)
			1. [TerminatingConnection](#netconnectionmanagerlocalterminatingconnection)
			1. [UnexpectedlyFalseAssertion](#netconnectionmanagerlocalunexpectedlyfalseassertion)
			1. [UnregisterConnection](#netconnectionmanagerlocalunregisterconnection)
		1. __Remote__
			1. [Connect](#netconnectionmanagerremoteconnect)
			1. [ConnectError](#netconnectionmanagerremoteconnecterror)
			1. [ConnectionCleanup](#netconnectionmanagerremoteconnectioncleanup)
			1. [ConnectionExists](#netconnectionmanagerremoteconnectionexists)
			1. [ConnectionFailure](#netconnectionmanagerremoteconnectionfailure)
			1. [ConnectionHandler](#netconnectionmanagerremoteconnectionhandler)
			1. [ConnectionManagerCounters](#netconnectionmanagerremoteconnectionmanagercounters)
			1. [ConnectionNotFound](#netconnectionmanagerremoteconnectionnotfound)
			1. [ConnectionTimeWait](#netconnectionmanagerremoteconnectiontimewait)
			1. [ConnectionTimeWaitDone](#netconnectionmanagerremoteconnectiontimewaitdone)
			1. [ForbiddenConnection](#netconnectionmanagerremoteforbiddenconnection)
			1. [ForbiddenOperation](#netconnectionmanagerremoteforbiddenoperation)
			1. [ImpossibleConnection](#netconnectionmanagerremoteimpossibleconnection)
			1. [IncludeConnection](#netconnectionmanagerremoteincludeconnection)
			1. [PruneConnections](#netconnectionmanagerremotepruneconnections)
			1. [Shutdown](#netconnectionmanagerremoteshutdown)
			1. [State](#netconnectionmanagerremotestate)
			1. [TerminatedConnection](#netconnectionmanagerremoteterminatedconnection)
			1. [TerminatingConnection](#netconnectionmanagerremoteterminatingconnection)
			1. __Transition__
				1. [ConnectionManagerTransition](#netconnectionmanagerremotetransitionconnectionmanagertransition)
			1. [UnexpectedlyFalseAssertion](#netconnectionmanagerremoteunexpectedlyfalseassertion)
			1. [UnregisterConnection](#netconnectionmanagerremoteunregisterconnection)
	1. __DNSResolver__
		1. [LookupAAAAError](#netdnsresolverlookupaaaaerror)
		1. [LookupAAAAResult](#netdnsresolverlookupaaaaresult)
		1. [LookupAError](#netdnsresolverlookupaerror)
		1. [LookupAResult](#netdnsresolverlookuparesult)
		1. [LookupException](#netdnsresolverlookupexception)
		1. [LookupIPv4First](#netdnsresolverlookupipv4first)
		1. [LookupIPv6First](#netdnsresolverlookupipv6first)
	1. __ErrorPolicy__
		1. __Local__
			1. [AcceptException](#neterrorpolicylocalacceptexception)
			1. [KeepSuspended](#neterrorpolicylocalkeepsuspended)
			1. [LocalNodeError](#neterrorpolicylocallocalnodeerror)
			1. [ResumeConsumer](#neterrorpolicylocalresumeconsumer)
			1. [ResumePeer](#neterrorpolicylocalresumepeer)
			1. [ResumeProducer](#neterrorpolicylocalresumeproducer)
			1. [SuspendConsumer](#neterrorpolicylocalsuspendconsumer)
			1. [SuspendPeer](#neterrorpolicylocalsuspendpeer)
			1. [UnhandledApplicationException](#neterrorpolicylocalunhandledapplicationexception)
			1. [UnhandledConnectionException](#neterrorpolicylocalunhandledconnectionexception)
		1. __Remote__
			1. [AcceptException](#neterrorpolicyremoteacceptexception)
			1. [KeepSuspended](#neterrorpolicyremotekeepsuspended)
			1. [LocalNodeError](#neterrorpolicyremotelocalnodeerror)
			1. [ResumeConsumer](#neterrorpolicyremoteresumeconsumer)
			1. [ResumePeer](#neterrorpolicyremoteresumepeer)
			1. [ResumeProducer](#neterrorpolicyremoteresumeproducer)
			1. [SuspendConsumer](#neterrorpolicyremotesuspendconsumer)
			1. [SuspendPeer](#neterrorpolicyremotesuspendpeer)
			1. [UnhandledApplicationException](#neterrorpolicyremoteunhandledapplicationexception)
			1. [UnhandledConnectionException](#neterrorpolicyremoteunhandledconnectionexception)
	1. __Handshake__
		1. __Local__
			1. __Receive__
				1. [AcceptVersion](#nethandshakelocalreceiveacceptversion)
				1. [ProposeVersions](#nethandshakelocalreceiveproposeversions)
				1. [Refuse](#nethandshakelocalreceiverefuse)
				1. [ReplyVersions](#nethandshakelocalreceivereplyversions)
			1. __Send__
				1. [AcceptVersion](#nethandshakelocalsendacceptversion)
				1. [ProposeVersions](#nethandshakelocalsendproposeversions)
				1. [Refuse](#nethandshakelocalsendrefuse)
				1. [ReplyVersions](#nethandshakelocalsendreplyversions)
		1. __Remote__
			1. __Receive__
				1. [AcceptVersion](#nethandshakeremotereceiveacceptversion)
				1. [ProposeVersions](#nethandshakeremotereceiveproposeversions)
				1. [Refuse](#nethandshakeremotereceiverefuse)
				1. [ReplyVersions](#nethandshakeremotereceivereplyversions)
			1. __Send__
				1. [AcceptVersion](#nethandshakeremotesendacceptversion)
				1. [ProposeVersions](#nethandshakeremotesendproposeversions)
				1. [Refuse](#nethandshakeremotesendrefuse)
				1. [ReplyVersions](#nethandshakeremotesendreplyversions)
	1. __InboundGovernor__
		1. __Local__
			1. [DemotedToColdRemote](#netinboundgovernorlocaldemotedtocoldremote)
			1. [DemotedToWarmRemote](#netinboundgovernorlocaldemotedtowarmremote)
			1. [InboundGovernorCounters](#netinboundgovernorlocalinboundgovernorcounters)
			1. [InboundGovernorError](#netinboundgovernorlocalinboundgovernorerror)
			1. [MuxCleanExit](#netinboundgovernorlocalmuxcleanexit)
			1. [MuxErrored](#netinboundgovernorlocalmuxerrored)
			1. [NewConnection](#netinboundgovernorlocalnewconnection)
			1. [PromotedToHotRemote](#netinboundgovernorlocalpromotedtohotremote)
			1. [PromotedToWarmRemote](#netinboundgovernorlocalpromotedtowarmremote)
			1. [RemoteState](#netinboundgovernorlocalremotestate)
			1. [ResponderErrored](#netinboundgovernorlocalrespondererrored)
			1. [ResponderRestarted](#netinboundgovernorlocalresponderrestarted)
			1. [ResponderStartFailure](#netinboundgovernorlocalresponderstartfailure)
			1. [ResponderStarted](#netinboundgovernorlocalresponderstarted)
			1. [ResponderTerminated](#netinboundgovernorlocalresponderterminated)
			1. [UnexpectedlyFalseAssertion](#netinboundgovernorlocalunexpectedlyfalseassertion)
			1. [WaitIdleRemote](#netinboundgovernorlocalwaitidleremote)
		1. __Remote__
			1. [DemotedToColdRemote](#netinboundgovernorremotedemotedtocoldremote)
			1. [DemotedToWarmRemote](#netinboundgovernorremotedemotedtowarmremote)
			1. [InboundGovernorCounters](#netinboundgovernorremoteinboundgovernorcounters)
			1. [InboundGovernorError](#netinboundgovernorremoteinboundgovernorerror)
			1. [MuxCleanExit](#netinboundgovernorremotemuxcleanexit)
			1. [MuxErrored](#netinboundgovernorremotemuxerrored)
			1. [NewConnection](#netinboundgovernorremotenewconnection)
			1. [PromotedToHotRemote](#netinboundgovernorremotepromotedtohotremote)
			1. [PromotedToWarmRemote](#netinboundgovernorremotepromotedtowarmremote)
			1. [RemoteState](#netinboundgovernorremoteremotestate)
			1. [ResponderErrored](#netinboundgovernorremoterespondererrored)
			1. [ResponderRestarted](#netinboundgovernorremoteresponderrestarted)
			1. [ResponderStartFailure](#netinboundgovernorremoteresponderstartfailure)
			1. [ResponderStarted](#netinboundgovernorremoteresponderstarted)
			1. [ResponderTerminated](#netinboundgovernorremoteresponderterminated)
			1. __Transition__
				1. [InboundGovernorTransition](#netinboundgovernorremotetransitioninboundgovernortransition)
			1. [UnexpectedlyFalseAssertion](#netinboundgovernorremoteunexpectedlyfalseassertion)
			1. [WaitIdleRemote](#netinboundgovernorremotewaitidleremote)
	1. [KeepAliveClient](#netkeepaliveclient)
	1. __Mux__
		1. __Local__
			1. [ChannelRecvEnd](#netmuxlocalchannelrecvend)
			1. [ChannelRecvStart](#netmuxlocalchannelrecvstart)
			1. [ChannelSendEnd](#netmuxlocalchannelsendend)
			1. [ChannelSendStart](#netmuxlocalchannelsendstart)
			1. [CleanExit](#netmuxlocalcleanexit)
			1. [ExceptionExit](#netmuxlocalexceptionexit)
			1. [HandshakeClientEnd](#netmuxlocalhandshakeclientend)
			1. [HandshakeClientError](#netmuxlocalhandshakeclienterror)
			1. [HandshakeServerEnd](#netmuxlocalhandshakeserverend)
			1. [HandshakeServerError](#netmuxlocalhandshakeservererror)
			1. [HandshakeStart](#netmuxlocalhandshakestart)
			1. [RecvDeltaQObservation](#netmuxlocalrecvdeltaqobservation)
			1. [RecvDeltaQSample](#netmuxlocalrecvdeltaqsample)
			1. [RecvEnd](#netmuxlocalrecvend)
			1. [RecvHeaderEnd](#netmuxlocalrecvheaderend)
			1. [RecvHeaderStart](#netmuxlocalrecvheaderstart)
			1. [RecvStart](#netmuxlocalrecvstart)
			1. [SDUReadTimeoutException](#netmuxlocalsdureadtimeoutexception)
			1. [SDUWriteTimeoutException](#netmuxlocalsduwritetimeoutexception)
			1. [SendEnd](#netmuxlocalsendend)
			1. [SendStart](#netmuxlocalsendstart)
			1. [Shutdown](#netmuxlocalshutdown)
			1. [StartEagerly](#netmuxlocalstarteagerly)
			1. [StartOnDemand](#netmuxlocalstartondemand)
			1. [StartedOnDemand](#netmuxlocalstartedondemand)
			1. [State](#netmuxlocalstate)
			1. [TCPInfo](#netmuxlocaltcpinfo)
			1. [Terminating](#netmuxlocalterminating)
		1. __Remote__
			1. [ChannelRecvEnd](#netmuxremotechannelrecvend)
			1. [ChannelRecvStart](#netmuxremotechannelrecvstart)
			1. [ChannelSendEnd](#netmuxremotechannelsendend)
			1. [ChannelSendStart](#netmuxremotechannelsendstart)
			1. [CleanExit](#netmuxremotecleanexit)
			1. [ExceptionExit](#netmuxremoteexceptionexit)
			1. [HandshakeClientEnd](#netmuxremotehandshakeclientend)
			1. [HandshakeClientError](#netmuxremotehandshakeclienterror)
			1. [HandshakeServerEnd](#netmuxremotehandshakeserverend)
			1. [HandshakeServerError](#netmuxremotehandshakeservererror)
			1. [HandshakeStart](#netmuxremotehandshakestart)
			1. [RecvDeltaQObservation](#netmuxremoterecvdeltaqobservation)
			1. [RecvDeltaQSample](#netmuxremoterecvdeltaqsample)
			1. [RecvEnd](#netmuxremoterecvend)
			1. [RecvHeaderEnd](#netmuxremoterecvheaderend)
			1. [RecvHeaderStart](#netmuxremoterecvheaderstart)
			1. [RecvStart](#netmuxremoterecvstart)
			1. [SDUReadTimeoutException](#netmuxremotesdureadtimeoutexception)
			1. [SDUWriteTimeoutException](#netmuxremotesduwritetimeoutexception)
			1. [SendEnd](#netmuxremotesendend)
			1. [SendStart](#netmuxremotesendstart)
			1. [Shutdown](#netmuxremoteshutdown)
			1. [StartEagerly](#netmuxremotestarteagerly)
			1. [StartOnDemand](#netmuxremotestartondemand)
			1. [StartedOnDemand](#netmuxremotestartedondemand)
			1. [State](#netmuxremotestate)
			1. [TCPInfo](#netmuxremotetcpinfo)
			1. [Terminating](#netmuxremoteterminating)
	1. __PeerSelection__
		1. __Actions__
			1. [MonitoringError](#netpeerselectionactionsmonitoringerror)
			1. [MonitoringResult](#netpeerselectionactionsmonitoringresult)
			1. [StatusChangeFailure](#netpeerselectionactionsstatuschangefailure)
			1. [StatusChanged](#netpeerselectionactionsstatuschanged)
		1. [Counters](#netpeerselectioncounters)
		1. __Initiator__
			1. [GovernorState](#netpeerselectioninitiatorgovernorstate)
		1. __Responder__
			1. [GovernorState](#netpeerselectionrespondergovernorstate)
		1. __Selection__
			1. [ChurnMode](#netpeerselectionselectionchurnmode)
			1. [ChurnWait](#netpeerselectionselectionchurnwait)
			1. [DemoteAsynchronous](#netpeerselectionselectiondemoteasynchronous)
			1. [DemoteHotDone](#netpeerselectionselectiondemotehotdone)
			1. [DemoteHotFailed](#netpeerselectionselectiondemotehotfailed)
			1. [DemoteHotPeers](#netpeerselectionselectiondemotehotpeers)
			1. [DemoteLocalHotPeers](#netpeerselectionselectiondemotelocalhotpeers)
			1. [DemoteWarmDone](#netpeerselectionselectiondemotewarmdone)
			1. [DemoteWarmFailed](#netpeerselectionselectiondemotewarmfailed)
			1. [DemoteWarmPeers](#netpeerselectionselectiondemotewarmpeers)
			1. [ForgetColdPeers](#netpeerselectionselectionforgetcoldpeers)
			1. [GossipRequests](#netpeerselectionselectiongossiprequests)
			1. [GossipResults](#netpeerselectionselectiongossipresults)
			1. [GovernorWakeup](#netpeerselectionselectiongovernorwakeup)
			1. [LocalRootPeersChanged](#netpeerselectionselectionlocalrootpeerschanged)
			1. [PromoteColdDone](#netpeerselectionselectionpromotecolddone)
			1. [PromoteColdFailed](#netpeerselectionselectionpromotecoldfailed)
			1. [PromoteColdLocalPeers](#netpeerselectionselectionpromotecoldlocalpeers)
			1. [PromoteColdPeers](#netpeerselectionselectionpromotecoldpeers)
			1. [PromoteWarmAborted](#netpeerselectionselectionpromotewarmaborted)
			1. [PromoteWarmDone](#netpeerselectionselectionpromotewarmdone)
			1. [PromoteWarmFailed](#netpeerselectionselectionpromotewarmfailed)
			1. [PromoteWarmLocalPeers](#netpeerselectionselectionpromotewarmlocalpeers)
			1. [PromoteWarmPeers](#netpeerselectionselectionpromotewarmpeers)
			1. [PublicRootsFailure](#netpeerselectionselectionpublicrootsfailure)
			1. [PublicRootsRequest](#netpeerselectionselectionpublicrootsrequest)
			1. [PublicRootsResults](#netpeerselectionselectionpublicrootsresults)
			1. [TargetsChanged](#netpeerselectionselectiontargetschanged)
	1. __Peers__
		1. __Ledger__
			1. [DisabledLedgerPeers](#netpeersledgerdisabledledgerpeers)
			1. [FallingBackToBootstrapPeers](#netpeersledgerfallingbacktobootstrappeers)
			1. [FetchingNewLedgerState](#netpeersledgerfetchingnewledgerstate)
			1. [PickedPeer](#netpeersledgerpickedpeer)
			1. [PickedPeers](#netpeersledgerpickedpeers)
			1. [RequestForPeers](#netpeersledgerrequestforpeers)
			1. [ReusingLedgerState](#netpeersledgerreusingledgerstate)
			1. [TraceUseLedgerAfter](#netpeersledgertraceuseledgerafter)
			1. [WaitingOnRequest](#netpeersledgerwaitingonrequest)
		1. [List](#netpeerslist)
		1. __LocalRoot__
			1. [LocalRootDomains](#netpeerslocalrootlocalrootdomains)
			1. [LocalRootError](#netpeerslocalrootlocalrooterror)
			1. [LocalRootFailure](#netpeerslocalrootlocalrootfailure)
			1. [LocalRootGroups](#netpeerslocalrootlocalrootgroups)
			1. [LocalRootResult](#netpeerslocalrootlocalrootresult)
			1. [LocalRootWaiting](#netpeerslocalrootlocalrootwaiting)
		1. __PublicRoot__
			1. [PublicRootDomains](#netpeerspublicrootpublicrootdomains)
			1. [PublicRootFailure](#netpeerspublicrootpublicrootfailure)
			1. [PublicRootRelayAccessPoint](#netpeerspublicrootpublicrootrelayaccesspoint)
			1. [PublicRootResult](#netpeerspublicrootpublicrootresult)
	1. __Server__
		1. __Local__
			1. [AcceptConnection](#netserverlocalacceptconnection)
			1. [AcceptError](#netserverlocalaccepterror)
			1. [AcceptPolicy](#netserverlocalacceptpolicy)
			1. [Error](#netserverlocalerror)
			1. [Started](#netserverlocalstarted)
			1. [Stopped](#netserverlocalstopped)
		1. __Remote__
			1. [AcceptConnection](#netserverremoteacceptconnection)
			1. [AcceptError](#netserverremoteaccepterror)
			1. [AcceptPolicy](#netserverremoteacceptpolicy)
			1. [Error](#netserverremoteerror)
			1. [Started](#netserverremotestarted)
			1. [Stopped](#netserverremotestopped)
	1. __Subscription__
		1. __DNS__
			1. [AllocateSocket](#netsubscriptiondnsallocatesocket)
			1. [ApplicationException](#netsubscriptiondnsapplicationexception)
			1. [CloseSocket](#netsubscriptiondnsclosesocket)
			1. [ConnectEnd](#netsubscriptiondnsconnectend)
			1. [ConnectException](#netsubscriptiondnsconnectexception)
			1. [ConnectStart](#netsubscriptiondnsconnectstart)
			1. [ConnectionExist](#netsubscriptiondnsconnectionexist)
			1. [MissingLocalAddress](#netsubscriptiondnsmissinglocaladdress)
			1. [Restart](#netsubscriptiondnsrestart)
			1. [SkippingPeer](#netsubscriptiondnsskippingpeer)
			1. [SocketAllocationException](#netsubscriptiondnssocketallocationexception)
			1. [Start](#netsubscriptiondnsstart)
			1. [SubscriptionFailed](#netsubscriptiondnssubscriptionfailed)
			1. [SubscriptionRunning](#netsubscriptiondnssubscriptionrunning)
			1. [SubscriptionWaiting](#netsubscriptiondnssubscriptionwaiting)
			1. [SubscriptionWaitingNewConnection](#netsubscriptiondnssubscriptionwaitingnewconnection)
			1. [TryConnectToPeer](#netsubscriptiondnstryconnecttopeer)
			1. [UnsupportedRemoteAddr](#netsubscriptiondnsunsupportedremoteaddr)
		1. __IP__
			1. [AllocateSocket](#netsubscriptionipallocatesocket)
			1. [ApplicationException](#netsubscriptionipapplicationexception)
			1. [CloseSocket](#netsubscriptionipclosesocket)
			1. [ConnectEnd](#netsubscriptionipconnectend)
			1. [ConnectException](#netsubscriptionipconnectexception)
			1. [ConnectStart](#netsubscriptionipconnectstart)
			1. [ConnectionExist](#netsubscriptionipconnectionexist)
			1. [MissingLocalAddress](#netsubscriptionipmissinglocaladdress)
			1. [Restart](#netsubscriptioniprestart)
			1. [SkippingPeer](#netsubscriptionipskippingpeer)
			1. [SocketAllocationException](#netsubscriptionipsocketallocationexception)
			1. [Start](#netsubscriptionipstart)
			1. [SubscriptionFailed](#netsubscriptionipsubscriptionfailed)
			1. [SubscriptionRunning](#netsubscriptionipsubscriptionrunning)
			1. [SubscriptionWaiting](#netsubscriptionipsubscriptionwaiting)
			1. [SubscriptionWaitingNewConnection](#netsubscriptionipsubscriptionwaitingnewconnection)
			1. [TryConnectToPeer](#netsubscriptioniptryconnecttopeer)
			1. [UnsupportedRemoteAddr](#netsubscriptionipunsupportedremoteaddr)
1. __NodeState__
	1. [NodeAddBlock](#nodestatenodeaddblock)
	1. [NodeInitChainSelection](#nodestatenodeinitchainselection)
	1. [NodeKernelOnline](#nodestatenodekernelonline)
	1. [NodeOpeningDbs](#nodestatenodeopeningdbs)
	1. [NodeReplays](#nodestatenodereplays)
	1. [NodeShutdown](#nodestatenodeshutdown)
	1. [NodeStartup](#nodestatenodestartup)
	1. [NodeTracingOnlineConfiguring](#nodestatenodetracingonlineconfiguring)
1. [Resources](#resources)
1. __Shutdown__
	1. [Abnormal](#shutdownabnormal)
	1. [ArmedAt](#shutdownarmedat)
	1. [Requested](#shutdownrequested)
	1. [Requesting](#shutdownrequesting)
	1. [UnexpectedInput](#shutdownunexpectedinput)
1. __Startup__
	1. [Byron](#startupbyron)
	1. [Common](#startupcommon)
	1. [DBValidation](#startupdbvalidation)
	1. __DiffusionInit__
		1. [ConfiguringLocalSocket](#startupdiffusioninitconfiguringlocalsocket)
		1. [ConfiguringServerSocket](#startupdiffusioninitconfiguringserversocket)
		1. [CreateSystemdSocketForSnocketPath](#startupdiffusioninitcreatesystemdsocketforsnocketpath)
		1. [CreatedLocalSocket](#startupdiffusioninitcreatedlocalsocket)
		1. [CreatingServerSocket](#startupdiffusioninitcreatingserversocket)
		1. [DiffusionErrored](#startupdiffusioninitdiffusionerrored)
		1. [ListeningLocalSocket](#startupdiffusioninitlisteninglocalsocket)
		1. [ListeningServerSocket](#startupdiffusioninitlisteningserversocket)
		1. [LocalSocketUp](#startupdiffusioninitlocalsocketup)
		1. [RunLocalServer](#startupdiffusioninitrunlocalserver)
		1. [RunServer](#startupdiffusioninitrunserver)
		1. [ServerSocketUp](#startupdiffusioninitserversocketup)
		1. [UnsupportedLocalSystemdSocket](#startupdiffusioninitunsupportedlocalsystemdsocket)
		1. [UnsupportedReadySocketCase](#startupdiffusioninitunsupportedreadysocketcase)
		1. [UsingSystemdSocket](#startupdiffusioninitusingsystemdsocket)
	1. [Info](#startupinfo)
	1. [Network](#startupnetwork)
	1. [NetworkConfig](#startupnetworkconfig)
	1. [NetworkConfigUpdate](#startupnetworkconfigupdate)
	1. [NetworkConfigUpdateError](#startupnetworkconfigupdateerror)
	1. [NetworkMagic](#startupnetworkmagic)
	1. [P2PInfo](#startupp2pinfo)
	1. [P2PWarning](#startupp2pwarning)
	1. [P2PWarningDevelopementNetworkProtocols](#startupp2pwarningdevelopementnetworkprotocols)
	1. [ShelleyBased](#startupshelleybased)
	1. [SocketConfigError](#startupsocketconfigerror)
	1. [Time](#startuptime)
	1. [WarningDevelopmentNetworkProtocols](#startupwarningdevelopmentnetworkprotocols)
1. __StateQueryServer__
	1. __Receive__
		1. [Acquire](#statequeryserverreceiveacquire)
		1. [Acquired](#statequeryserverreceiveacquired)
		1. [Done](#statequeryserverreceivedone)
		1. [Failure](#statequeryserverreceivefailure)
		1. [Query](#statequeryserverreceivequery)
		1. [ReAcquire](#statequeryserverreceivereacquire)
		1. [Release](#statequeryserverreceiverelease)
		1. [Result](#statequeryserverreceiveresult)
	1. __Send__
		1. [Acquire](#statequeryserversendacquire)
		1. [Acquired](#statequeryserversendacquired)
		1. [Done](#statequeryserversenddone)
		1. [Failure](#statequeryserversendfailure)
		1. [Query](#statequeryserversendquery)
		1. [ReAcquire](#statequeryserversendreacquire)
		1. [Release](#statequeryserversendrelease)
		1. [Result](#statequeryserversendresult)
1. __TxSubmission__
	1. __Local__
		1. __Receive__
			1. [AcceptTx](#txsubmissionlocalreceiveaccepttx)
			1. [Done](#txsubmissionlocalreceivedone)
			1. [RejectTx](#txsubmissionlocalreceiverejecttx)
			1. [SubmitTx](#txsubmissionlocalreceivesubmittx)
		1. __Send__
			1. [AcceptTx](#txsubmissionlocalsendaccepttx)
			1. [Done](#txsubmissionlocalsenddone)
			1. [RejectTx](#txsubmissionlocalsendrejecttx)
			1. [SubmitTx](#txsubmissionlocalsendsubmittx)
	1. __LocalServer__
		1. [ReceivedTx](#txsubmissionlocalserverreceivedtx)
	1. __MonitorClient__
		1. __Receive__
			1. [Acquire](#txsubmissionmonitorclientreceiveacquire)
			1. [Acquired](#txsubmissionmonitorclientreceiveacquired)
			1. [Done](#txsubmissionmonitorclientreceivedone)
			1. [Failure](#txsubmissionmonitorclientreceivefailure)
			1. [Query](#txsubmissionmonitorclientreceivequery)
			1. [ReAcquire](#txsubmissionmonitorclientreceivereacquire)
			1. [Release](#txsubmissionmonitorclientreceiverelease)
			1. [Result](#txsubmissionmonitorclientreceiveresult)
		1. __Send__
			1. [Acquire](#txsubmissionmonitorclientsendacquire)
			1. [Acquired](#txsubmissionmonitorclientsendacquired)
			1. [Done](#txsubmissionmonitorclientsenddone)
			1. [Failure](#txsubmissionmonitorclientsendfailure)
			1. [Query](#txsubmissionmonitorclientsendquery)
			1. [ReAcquire](#txsubmissionmonitorclientsendreacquire)
			1. [Release](#txsubmissionmonitorclientsendrelease)
			1. [Result](#txsubmissionmonitorclientsendresult)
	1. __Remote__
		1. __Receive__
			1. [Done](#txsubmissionremotereceivedone)
			1. [MsgHello](#txsubmissionremotereceivemsghello)
			1. [ReplyTxIds](#txsubmissionremotereceivereplytxids)
			1. [ReplyTxs](#txsubmissionremotereceivereplytxs)
			1. [RequestTxIds](#txsubmissionremotereceiverequesttxids)
			1. [RequestTxs](#txsubmissionremotereceiverequesttxs)
		1. __Send__
			1. [Done](#txsubmissionremotesenddone)
			1. [MsgHello](#txsubmissionremotesendmsghello)
			1. [ReplyTxIds](#txsubmissionremotesendreplytxids)
			1. [ReplyTxs](#txsubmissionremotesendreplytxs)
			1. [RequestTxIds](#txsubmissionremotesendrequesttxids)
			1. [RequestTxs](#txsubmissionremotesendrequesttxs)
	1. __TxInbound__
		1. [CanRequestMoreTxs](#txsubmissiontxinboundcanrequestmoretxs)
		1. [CannotRequestMoreTxs](#txsubmissiontxinboundcannotrequestmoretxs)
		1. [Collected](#txsubmissiontxinboundcollected)
		1. [Processed](#txsubmissiontxinboundprocessed)
		1. [Terminated](#txsubmissiontxinboundterminated)
	1. __TxOutbound__
		1. [ControlMessage](#txsubmissiontxoutboundcontrolmessage)
		1. [RecvMsgRequest](#txsubmissiontxoutboundrecvmsgrequest)
		1. [SendMsgReply](#txsubmissiontxoutboundsendmsgreply)

## [Metrics](#metrics)
1. __BlockFetch__
	1. [BlocksServed](#blockfetchblocksserved)
	1. [ConnectedPeers](#blockfetchconnectedpeers)
1. __ChainDB__
	1. [BlockReplayProgress](#chaindbblockreplayprogress)
	1. [Blocks](#chaindbblocks)
	1. [Density](#chaindbdensity)
	1. [Epoch](#chaindbepoch)
	1. [SlotInEpoch](#chaindbslotinepoch)
	1. [Slots](#chaindbslots)
1. __ChainSync__
	1. [HeadersServed](#chainsyncheadersserved)
1. __Forge__
	1. [AboutToLeadSlotLast](#forgeabouttoleadslotlast)
	1. [AdoptedOwnBlockSlotLast](#forgeadoptedownblockslotlast)
	1. [BlockContext](#forgeblockcontext)
	1. [BlockFromFuture](#forgeblockfromfuture)
	1. [BlocksForgedNum](#forgeblocksforgednum)
	1. [CouldNotForgeSlotLast](#forgecouldnotforgeslotlast)
	1. [CurrentKESPeriod](#forgecurrentkesperiod)
	1. [DelegMapSize](#forgedelegmapsize)
	1. [ForgedInvalidSlotLast](#forgeforgedinvalidslotlast)
	1. [ForgedSlotLast](#forgeforgedslotlast)
	1. [LastSlot](#forgelastslot)
	1. [LedgerState](#forgeledgerstate)
	1. [LedgerView](#forgeledgerview)
	1. [NodeCannotForge](#forgenodecannotforge)
	1. [NodeCannotForgeNum](#forgenodecannotforgenum)
	1. [NodeIsLeader](#forgenodeisleader)
	1. [NodeIsLeaderNum](#forgenodeisleadernum)
	1. [NodeNotLeader](#forgenodenotleader)
	1. [NotAdoptedSlotLast](#forgenotadoptedslotlast)
	1. [OperationalCertificateExpiryKESPeriod](#forgeoperationalcertificateexpirykesperiod)
	1. [OperationalCertificateStartKESPeriod](#forgeoperationalcertificatestartkesperiod)
	1. [RemainingKESPeriods](#forgeremainingkesperiods)
	1. [SlotIsImmutable](#forgeslotisimmutable)
	1. [SlotsMissed](#forgeslotsmissed)
	1. [UtxoSize](#forgeutxosize)
1. __Mempool__
	1. [MempoolBytes](#mempoolmempoolbytes)
	1. [TxsInMempool](#mempooltxsinmempool)
	1. [TxsProcessedNum](#mempooltxsprocessednum)
1. __Net__
	1. __ConnectionManager__
		1. [DuplexConns](#netconnectionmanagerduplexconns)
		1. [DuplexConns](#netconnectionmanagerduplexconns)
		1. [FullDuplexConns](#netconnectionmanagerfullduplexconns)
		1. [FullDuplexConns](#netconnectionmanagerfullduplexconns)
		1. [InboundConns](#netconnectionmanagerinboundconns)
		1. [InboundConns](#netconnectionmanagerinboundconns)
		1. [OutboundConns](#netconnectionmanageroutboundconns)
		1. [OutboundConns](#netconnectionmanageroutboundconns)
		1. [UnidirectionalConns](#netconnectionmanagerunidirectionalconns)
		1. [UnidirectionalConns](#netconnectionmanagerunidirectionalconns)
	1. __InboundGovernor__
		1. [Cold](#netinboundgovernorcold)
		1. [Hot](#netinboundgovernorhot)
		1. [Idle](#netinboundgovernoridle)
		1. [Warm](#netinboundgovernorwarm)
	1. __LocalInboundGovernor__
		1. [Cold](#netlocalinboundgovernorcold)
		1. [Hot](#netlocalinboundgovernorhot)
		1. [Idle](#netlocalinboundgovernoridle)
		1. [Warm](#netlocalinboundgovernorwarm)
	1. __PeerSelection__
		1. [Cold](#netpeerselectioncold)
		1. [Hot](#netpeerselectionhot)
		1. [Warm](#netpeerselectionwarm)
	1. [PeersFromNodeKernel](#netpeersfromnodekernel)
1. __Resources__
	1. __Mem__
		1. [Resident](#resourcesmemresident)
	1. __RTS__
		1. [GcLiveBytes](#resourcesrtsgclivebytes)
		1. [GcMajorNum](#resourcesrtsgcmajornum)
		1. [GcMinorNum](#resourcesrtsgcminornum)
		1. [Gcticks](#resourcesrtsgcticks)
		1. [Mutticks](#resourcesrtsmutticks)
		1. [Threads](#resourcesrtsthreads)
	1. __Stat__
		1. [Cputicks](#resourcesstatcputicks)
1. __TxSubmission__
	1. [Accepted](#txsubmissionaccepted)
	1. [Rejected](#txsubmissionrejected)
	1. [Submitted](#txsubmissionsubmitted)

## [Datapoints](#datapoints)
1. [NodeInfo](#nodeinfo)

## Trace Messages
### BlockFetch.ClientEvent.AcknowledgedFetchRequest


> Mark the point when the fetch client picks up the request added by the block fetch decision thread. Note that this event can happen fewer times than the 'AddedFetchRequest' due to fetch request merging.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetch.ClientEvent.AddedFetchRequest


> The block fetch decision thread has added a new fetch instruction consisting of one or more individual request ranges.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetch.ClientEvent.ClientTerminating


> The client is terminating.  Log the number of outstanding requests.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetch.ClientEvent.CompletedBlockFetch


> Mark the successful end of receiving a streaming batch of blocks.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetch.ClientEvent.CompletedFetchBatch


> Mark the successful end of receiving a streaming batch of blocks


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetch.ClientEvent.RejectedFetchBatch


> If the other peer rejects our request then we have this event instead of 'StartedFetchBatch' and 'CompletedFetchBatch'.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetch.ClientEvent.SendFetchRequest


> Mark the point when fetch request for a fragment is actually sent over the wire.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetch.ClientEvent.StartedFetchBatch


> Mark the start of receiving a streaming batch of blocks. This will be followed by one or more 'CompletedBlockFetch' and a final 'CompletedFetchBatch'


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetch.Decision


> Throughout the decision making process we accumulate reasons to decline to fetch any blocks. This message carries the intermediate and final results.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetch.Remote.Receive.BatchDone


> End of block streaming.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetch.Remote.Receive.Block


> Stream a single block.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetch.Remote.Receive.ClientDone


> Client termination message.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetch.Remote.Receive.NoBlocks


> Respond that there are no blocks.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetch.Remote.Receive.RequestRange


> Request range of blocks.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetch.Remote.Receive.StartBatch


> Start block streaming.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetch.Remote.Send.BatchDone


> End of block streaming.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetch.Remote.Send.Block


> Stream a single block.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetch.Remote.Send.ClientDone


> Client termination message.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetch.Remote.Send.NoBlocks


> Respond that there are no blocks.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetch.Remote.Send.RequestRange


> Request range of blocks.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetch.Remote.Send.StartBatch


> Start block streaming.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetch.Remote.Serialised.Receive.BatchDone


> End of block streaming.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetch.Remote.Serialised.Receive.Block


> Stream a single block.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetch.Remote.Serialised.Receive.ClientDone


> Client termination message.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetch.Remote.Serialised.Receive.NoBlocks


> Respond that there are no blocks.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetch.Remote.Serialised.Receive.RequestRange


> Request range of blocks.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetch.Remote.Serialised.Receive.StartBatch


> Start block streaming.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetch.Remote.Serialised.Send.BatchDone


> End of block streaming.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetch.Remote.Serialised.Send.Block


> Stream a single block.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetch.Remote.Serialised.Send.ClientDone


> Client termination message.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetch.Remote.Serialised.Send.NoBlocks


> Respond that there are no blocks.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetch.Remote.Serialised.Send.RequestRange


> Request range of blocks.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetch.Remote.Serialised.Send.StartBatch


> Start block streaming.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockFetch.ServerBlock.SendBlock


> The server sent a block to the peer.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockchainTime.CurrentSlotUnknown


> Current slot is not yet known
>  This happens when the tip of our current chain is so far in the past that we cannot translate the current wallclock to a slot number, typically during syncing. Until the current slot number is known, we cannot produce blocks. Seeing this message during syncing therefore is normal and to be expected.
>  We record the current time (the time we tried to translate to a 'SlotNo') as well as the 'PastHorizonException', which provides detail on the bounds between which we /can/ do conversions. The distance between the current time and the upper bound should rapidly decrease with consecutive 'CurrentSlotUnknown' messages during syncing.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockchainTime.StartTimeInTheFuture


> The start time of the blockchain time is in the future
>  We have to block (for 'NominalDiffTime') until that time comes.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### BlockchainTime.SystemClockMovedBack


> The system clock moved back an acceptable time span, e.g., because of an NTP sync.
>  The system clock moved back such that the new current slot would be smaller than the previous one. If this is within the configured limit, we trace this warning but *do not change the current slot*. The current slot never decreases, but the current slot may stay the same longer than expected.
>  When the system clock moved back more than the configured limit, we shut down with a fatal exception.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainDB.AddBlockEvent.AddBlockValidation.CandidateContainsFutureBlocks


> An event traced during validating performed while adding a block. Candidate contains headers from the future which do no exceed the clock skew.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.AddBlockEvent.AddBlockValidation.CandidateContainsFutureBlocksExceedingClockSkew


> An event traced during validating performed while adding a block. Candidate contains headers from the future which exceed the clock skew.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.AddBlockEvent.AddBlockValidation.InvalidBlock


> An event traced during validating performed while adding a block. A point was found to be invalid.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.AddBlockEvent.AddBlockValidation.UpdateLedgerDb




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.AddBlockEvent.AddBlockValidation.ValidCandidate


> An event traced during validating performed while adding a block. A candidate chain was valid.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`
Limiters: Limiter `ChainDB.AddBlockEvent.AddBlockValidation.ValidCandidate` with frequency `2.0`

### ChainDB.AddBlockEvent.AddedBlockToQueue


> The block was added to the queue and will be added to the ChainDB by the background thread. The size of the queue is included..


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`
Limiters: Limiter `ChainDB.AddBlockEvent.AddedBlockToQueue` with frequency `2.0`

### ChainDB.AddBlockEvent.AddedBlockToVolatileDB


> A block was added to the Volatile DB


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`
Limiters: Limiter `ChainDB.AddBlockEvent.AddedBlockToVolatileDB` with frequency `2.0`

### ChainDB.AddBlockEvent.AddedToCurrentChain


> The new block fits onto the current chain (first fragment) and we have successfully used it to extend our (new) current chain (second fragment).


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.AddBlockEvent.BlockInTheFuture


> The block is from the future, i.e., its slot number is greater than the current slot (the second argument).


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.AddBlockEvent.ChainSelectionForFutureBlock


> Run chain selection for a block that was previously from the future. This is done for all blocks from the future each time a new block is added.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.AddBlockEvent.IgnoreBlockAlreadyInVolatileDB


> A block that is already in the Volatile DB was ignored.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.AddBlockEvent.IgnoreBlockOlderThanK


> A block with a 'BlockNo' more than @k@ back than the current tip was ignored.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.AddBlockEvent.IgnoreInvalidBlock


> A block that is already in the Volatile DB was ignored.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.AddBlockEvent.PipeliningEvent.OutdatedTentativeHeader


> An event traced during block selection when the tentative header got cleared on chain selection.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.AddBlockEvent.PipeliningEvent.SetTentativeHeader


> An event traced during block selection when the tentative header (in the context of diffusion pipelining) is set.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.AddBlockEvent.PipeliningEvent.TrapTentativeHeader


> An event traced during block selection when the body of the tentative header turned out to be invalid.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.AddBlockEvent.PoppedBlockFromQueue




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.AddBlockEvent.StoreButDontChange


> The block fits onto some fork, we'll try to switch to that fork (if it is preferable to our chain).


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.AddBlockEvent.SwitchedToAFork


> The new block fits onto some fork and we have switched to that fork (second fragment), as it is preferable to our (previous) current chain (first fragment).


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.AddBlockEvent.TryAddToCurrentChain


> The block fits onto the current chain, we'll try to use it to extend our chain.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.AddBlockEvent.TrySwitchToAFork


> The block fits onto some fork, we'll try to switch to that fork (if it is preferable to our chain)


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.CopyToImmutableDBEvent.CopiedBlockToImmutableDB


> A block was successfully copied to the ImmDB.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`
Limiters: Limiter `ChainDB.CopyToImmutableDBEvent.CopiedBlockToImmutableDB` with frequency `2.0`

### ChainDB.CopyToImmutableDBEvent.NoBlocksToCopyToImmutableDB


> There are no block to copy to the ImmDB.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.FollowerEvent.FollowerNewImmIterator


> The follower is in the 'FollowerInImmutableDB' state but the iterator is exhausted while the ImmDB has grown, so we open a new iterator to stream these blocks too.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.FollowerEvent.FollowerNoLongerInMem


> The follower was in the 'FollowerInImmutableDB' state and is switched to the 'FollowerInMem' state.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.FollowerEvent.FollowerSwitchToMem


> The follower was in the 'FollowerInImmutableDB' state and is switched to the 'FollowerInMem' state.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.FollowerEvent.NewFollower


> A new follower was created.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.GCEvent.PerformedGC


> There are no block to copy to the ImmDB.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.GCEvent.ScheduledGC


> There are no block to copy to the ImmDB.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.ImmutableDBEvent.CacheEvent.CurrentChunkHit


> Current chunk found in the cache.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.ImmutableDBEvent.CacheEvent.PastChunkEvict


> The least recently used past chunk was evicted because the cache was full.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.ImmutableDBEvent.CacheEvent.PastChunkExpired




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.ImmutableDBEvent.CacheEvent.PastChunkHit


> Past chunk found in the cache


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.ImmutableDBEvent.CacheEvent.PastChunkMiss


> Past chunk was not found in the cache


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.ImmutableDBEvent.ChunkFileDoesntFit


> The hash of the last block in the previous epoch doesn't match the previous hash of the first block in the current epoch


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.ImmutableDBEvent.ChunkValidation.InvalidChunkFile


> Chunk file is invalid


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.ImmutableDBEvent.ChunkValidation.InvalidPrimaryIndex


> The primary index is invalid.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.ImmutableDBEvent.ChunkValidation.InvalidSecondaryIndex




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.ImmutableDBEvent.ChunkValidation.MissingChunkFile


> Chunk file is missing


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.ImmutableDBEvent.ChunkValidation.MissingPrimaryIndex


> The primary index is missing.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.ImmutableDBEvent.ChunkValidation.MissingSecondaryIndex


> The secondary index is missing.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.ImmutableDBEvent.ChunkValidation.RewritePrimaryIndex




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.ImmutableDBEvent.ChunkValidation.RewriteSecondaryIndex




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.ImmutableDBEvent.ChunkValidation.StartedValidatingChunk




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.ImmutableDBEvent.ChunkValidation.ValidatedChunk




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.ImmutableDBEvent.DBAlreadyClosed


> The immutable DB is already closed


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.ImmutableDBEvent.DBClosed


> Closing the immutable DB


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.ImmutableDBEvent.DeletingAfter


> Delete after


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.ImmutableDBEvent.Migrating


> Performing a migration of the on-disk files.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.ImmutableDBEvent.NoValidLastLocation


> No valid last location was found


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.ImmutableDBEvent.ValidatedLastLocation


> The last location was validatet


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.InitChainSelEvent.CandidateContainsFutureBlocks


> Candidate contains headers from the future which do not exceed the clock skew.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.InitChainSelEvent.CandidateContainsFutureBlocksExceedingClockSkew


> Candidate contains headers from the future which exceed the clock skew, making them invalid.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.InitChainSelEvent.InitalChainSelected


> InitalChainSelected


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.InitChainSelEvent.InvalidBlock


> A point was found to be invalid.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.InitChainSelEvent.StartedInitChainSelection


> StartedInitChainSelection


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.InitChainSelEvent.UpdateLedgerDb


> UpdateLedgerDb


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.InitChainSelEvent.ValidCandidate


> A candidate chain was valid.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.IteratorEvent.BlockGCedFromVolatileDB


> A block is no longer in the VolatileDB and isn't in the ImmDB either; it wasn't part of the current chain.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.IteratorEvent.BlockMissingFromVolatileDB


> A block is no longer in the VolatileDB because it has been garbage collected. It might now be in the ImmDB if it was part of the current chain.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.IteratorEvent.BlockWasCopiedToImmutableDB


> A block that has been garbage collected from the VolatileDB is now found and streamed from the ImmDB.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.IteratorEvent.StreamFromBoth


> Stream from both the VolatileDB and the ImmDB.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.IteratorEvent.StreamFromImmutableDB


> Stream only from the ImmDB.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.IteratorEvent.StreamFromVolatileDB


> Stream only from the VolatileDB.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.IteratorEvent.SwitchBackToVolatileDB


> We have streamed one or more blocks from the ImmDB that were part of the VolatileDB when initialising the iterator. Now, we have to look back in the VolatileDB again because the ImmDB doesn't have the next block we're looking for.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.IteratorEvent.UnknownRangeRequested


> An unknown range was requested, see 'UnknownRange'.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.LedgerEvent.DeletedSnapshot


> An old or invalid on-disk snapshot was deleted.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.LedgerEvent.InvalidSnapshot


> An on disk snapshot was skipped because it was invalid.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.LedgerEvent.TookSnapshot


> A snapshot was written to disk.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.LedgerReplayEvent.ReplayFromGenesis


> There were no LedgerDB snapshots on disk, so we're replaying all blocks starting from Genesis against the initial ledger. The @replayTo@ parameter corresponds to the block at the tip of the ImmDB, i.e., the last block to replay.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.LedgerReplayEvent.ReplayFromSnapshot


> There was a LedgerDB snapshot on disk corresponding to the given tip. We're replaying more recent blocks against it. The @replayTo@ parameter corresponds to the block at the tip of the ImmDB, i.e., the last block to replay.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.LedgerReplayEvent.ReplayedBlock


> We replayed the given block (reference) on the genesis snapshot during the initialisation of the LedgerDB.
>  The @blockInfo@ parameter corresponds replayed block and the @replayTo@ parameter corresponds to the block at the tip of the ImmDB, i.e., the last block to replay.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.OpenEvent.ClosedDB


> The ChainDB was closed.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.OpenEvent.OpenedDB


> The ChainDB was opened.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.OpenEvent.OpenedImmutableDB


> The ImmDB was opened.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.OpenEvent.OpenedLgrDB


> The LedgerDB was opened.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.OpenEvent.OpenedVolatileDB


> The VolatileDB was opened.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.OpenEvent.StartedOpeningDB




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.OpenEvent.StartedOpeningImmutableDB




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.OpenEvent.StartedOpeningLgrDB


> The LedgerDB was opened.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.OpenEvent.StartedOpeningVolatileDB




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.ReplayBlock.LedgerReplay


> Counts up the percent of a block replay.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.VolatileDBEvent.BlockAlreadyHere


> A block was found to be already in the DB.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.VolatileDBEvent.DBAlreadyClosed


> When closing the DB it was found itis closed already.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.VolatileDBEvent.InvalidFileNames


> Reports a list of invalid file paths.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainDB.VolatileDBEvent.Truncate


> Truncates a file up to offset because of the error.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### ChainSync.ClientEvent.DownloadedHeader


> While following a candidate chain, we rolled forward by downloading a header.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.ClientEvent.Exception


> An exception was thrown by the Chain Sync Client.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.ClientEvent.FoundIntersection


> We found an intersection between our chain fragment and the candidate's chain.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.ClientEvent.RolledBack


> While following a candidate chain, we rolled back to the given point.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.ClientEvent.Termination


> The client has terminated.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Local.Receive.AwaitReply


> Acknowledge the request but require the consumer to wait for the next update. This means that the consumer is synced with the producer, and the producer is waiting for its own chain state to change.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Local.Receive.Done


> We have to explain to the framework what our states mean, in terms of which party has agency in each state. 
>  Idle states are where it is for the client to send a message, busy states are where the server is expected to send a reply.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Local.Receive.FindIntersect


> Ask the producer to try to find an improved intersection point between the consumer and producer's chains. The consumer sends a sequence of points and it is up to the producer to find the first intersection point on its chain and send it back to the consumer.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Local.Receive.IntersectFound


> The reply to the consumer about an intersection found. The consumer can decide weather to send more points. 
>  The message also tells the consumer about the head point of the producer.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Local.Receive.IntersectNotFound


> The reply to the consumer that no intersection was found: none of the points the consumer supplied are on the producer chain. 
>  The message also tells the consumer about the head point of the producer.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Local.Receive.RequestNext


> Request the next update from the producer. The response can be a roll forward, a roll back or wait.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Local.Receive.RollBackward


> Tell the consumer to roll back to a given point on their chain. 
>  The message also tells the consumer about the head point of the producer.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Local.Receive.RollForward


> Tell the consumer to extend their chain with the given header. 
>  The message also tells the consumer about the head point of the producer.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Local.Send.AwaitReply


> Acknowledge the request but require the consumer to wait for the next update. This means that the consumer is synced with the producer, and the producer is waiting for its own chain state to change.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Local.Send.Done


> We have to explain to the framework what our states mean, in terms of which party has agency in each state. 
>  Idle states are where it is for the client to send a message, busy states are where the server is expected to send a reply.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Local.Send.FindIntersect


> Ask the producer to try to find an improved intersection point between the consumer and producer's chains. The consumer sends a sequence of points and it is up to the producer to find the first intersection point on its chain and send it back to the consumer.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Local.Send.IntersectFound


> The reply to the consumer about an intersection found. The consumer can decide weather to send more points. 
>  The message also tells the consumer about the head point of the producer.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Local.Send.IntersectNotFound


> The reply to the consumer that no intersection was found: none of the points the consumer supplied are on the producer chain. 
>  The message also tells the consumer about the head point of the producer.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Local.Send.RequestNext


> Request the next update from the producer. The response can be a roll forward, a roll back or wait.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Local.Send.RollBackward


> Tell the consumer to roll back to a given point on their chain. 
>  The message also tells the consumer about the head point of the producer.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Local.Send.RollForward


> Tell the consumer to extend their chain with the given header. 
>  The message also tells the consumer about the head point of the producer.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Remote.Receive.AwaitReply


> Acknowledge the request but require the consumer to wait for the next update. This means that the consumer is synced with the producer, and the producer is waiting for its own chain state to change.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Remote.Receive.Done


> We have to explain to the framework what our states mean, in terms of which party has agency in each state. 
>  Idle states are where it is for the client to send a message, busy states are where the server is expected to send a reply.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Remote.Receive.FindIntersect


> Ask the producer to try to find an improved intersection point between the consumer and producer's chains. The consumer sends a sequence of points and it is up to the producer to find the first intersection point on its chain and send it back to the consumer.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Remote.Receive.IntersectFound


> The reply to the consumer about an intersection found. The consumer can decide weather to send more points. 
>  The message also tells the consumer about the head point of the producer.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Remote.Receive.IntersectNotFound


> The reply to the consumer that no intersection was found: none of the points the consumer supplied are on the producer chain. 
>  The message also tells the consumer about the head point of the producer.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Remote.Receive.RequestNext


> Request the next update from the producer. The response can be a roll forward, a roll back or wait.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Remote.Receive.RollBackward


> Tell the consumer to roll back to a given point on their chain. 
>  The message also tells the consumer about the head point of the producer.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Remote.Receive.RollForward


> Tell the consumer to extend their chain with the given header. 
>  The message also tells the consumer about the head point of the producer.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Remote.Send.AwaitReply


> Acknowledge the request but require the consumer to wait for the next update. This means that the consumer is synced with the producer, and the producer is waiting for its own chain state to change.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Remote.Send.Done


> We have to explain to the framework what our states mean, in terms of which party has agency in each state. 
>  Idle states are where it is for the client to send a message, busy states are where the server is expected to send a reply.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Remote.Send.FindIntersect


> Ask the producer to try to find an improved intersection point between the consumer and producer's chains. The consumer sends a sequence of points and it is up to the producer to find the first intersection point on its chain and send it back to the consumer.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Remote.Send.IntersectFound


> The reply to the consumer about an intersection found. The consumer can decide weather to send more points. 
>  The message also tells the consumer about the head point of the producer.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Remote.Send.IntersectNotFound


> The reply to the consumer that no intersection was found: none of the points the consumer supplied are on the producer chain. 
>  The message also tells the consumer about the head point of the producer.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Remote.Send.RequestNext


> Request the next update from the producer. The response can be a roll forward, a roll back or wait.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Remote.Send.RollBackward


> Tell the consumer to roll back to a given point on their chain. 
>  The message also tells the consumer about the head point of the producer.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Remote.Send.RollForward


> Tell the consumer to extend their chain with the given header. 
>  The message also tells the consumer about the head point of the producer.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Remote.Serialised.Receive.AwaitReply


> Acknowledge the request but require the consumer to wait for the next update. This means that the consumer is synced with the producer, and the producer is waiting for its own chain state to change.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Remote.Serialised.Receive.Done


> We have to explain to the framework what our states mean, in terms of which party has agency in each state. 
>  Idle states are where it is for the client to send a message, busy states are where the server is expected to send a reply.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Remote.Serialised.Receive.FindIntersect


> Ask the producer to try to find an improved intersection point between the consumer and producer's chains. The consumer sends a sequence of points and it is up to the producer to find the first intersection point on its chain and send it back to the consumer.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Remote.Serialised.Receive.IntersectFound


> The reply to the consumer about an intersection found. The consumer can decide weather to send more points. 
>  The message also tells the consumer about the head point of the producer.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Remote.Serialised.Receive.IntersectNotFound


> The reply to the consumer that no intersection was found: none of the points the consumer supplied are on the producer chain. 
>  The message also tells the consumer about the head point of the producer.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Remote.Serialised.Receive.RequestNext


> Request the next update from the producer. The response can be a roll forward, a roll back or wait.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Remote.Serialised.Receive.RollBackward


> Tell the consumer to roll back to a given point on their chain. 
>  The message also tells the consumer about the head point of the producer.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Remote.Serialised.Receive.RollForward


> Tell the consumer to extend their chain with the given header. 
>  The message also tells the consumer about the head point of the producer.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Remote.Serialised.Send.AwaitReply


> Acknowledge the request but require the consumer to wait for the next update. This means that the consumer is synced with the producer, and the producer is waiting for its own chain state to change.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Remote.Serialised.Send.Done


> We have to explain to the framework what our states mean, in terms of which party has agency in each state. 
>  Idle states are where it is for the client to send a message, busy states are where the server is expected to send a reply.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Remote.Serialised.Send.FindIntersect


> Ask the producer to try to find an improved intersection point between the consumer and producer's chains. The consumer sends a sequence of points and it is up to the producer to find the first intersection point on its chain and send it back to the consumer.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Remote.Serialised.Send.IntersectFound


> The reply to the consumer about an intersection found. The consumer can decide weather to send more points. 
>  The message also tells the consumer about the head point of the producer.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Remote.Serialised.Send.IntersectNotFound


> The reply to the consumer that no intersection was found: none of the points the consumer supplied are on the producer chain. 
>  The message also tells the consumer about the head point of the producer.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Remote.Serialised.Send.RequestNext


> Request the next update from the producer. The response can be a roll forward, a roll back or wait.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Remote.Serialised.Send.RollBackward


> Tell the consumer to roll back to a given point on their chain. 
>  The message also tells the consumer about the head point of the producer.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.Remote.Serialised.Send.RollForward


> Tell the consumer to extend their chain with the given header. 
>  The message also tells the consumer about the head point of the producer.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.ServerBlock.Update


> A server read has occurred, either for an add block or a rollback


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### ChainSync.ServerHeader.Update


> A server read has occurred, either for an add block or a rollback


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Forge.KESInfo


> kesStartPeriod 
> kesEndPeriod is kesStartPeriod + tpraosMaxKESEvo
> kesEvolution is the current evolution or /relative period/.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Forge.StateInfo.AdoptedBlock


> We adopted the block we produced, we also trace the transactions  that were adopted.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Forge.StateInfo.BlockContext


> We found out to which block we are going to connect the block we are about  to forge.   We record the current slot number, the block number of the block to  connect to and its point.   Note that block number of the block we will try to forge is one more than  the recorded block number.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Forge.StateInfo.BlockFromFuture


> Leadership check failed: the current chain contains a block from a slot  /after/ the current slot   This can only happen if the system is under heavy load.   We record both the current slot number as well as the slot number of the  block at the tip of the chain.   See also <https://github.com/input-output-hk/ouroboros-network/issues/1462>


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Forge.StateInfo.DidntAdoptBlock


> We did not adopt the block we produced, but the block was valid. We  must have adopted a block that another leader of the same slot produced  before we got the chance of adopting our own block. This is very rare,  this warrants a warning.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Forge.StateInfo.ForgeStateUpdateError


> Updating the forge state failed.   For example, the KES key could not be evolved anymore.   We record the error returned by 'updateForgeState'.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Forge.StateInfo.ForgedBlock


> We forged a block.
>   We record the current slot number, the point of the predecessor, the block  itself, and the total size of the mempool snapshot at the time we produced  the block (which may be significantly larger than the block, due to  maximum block size)
>   This will be followed by one of three messages:
>   * AdoptedBlock (normally)
>   * DidntAdoptBlock (rarely)
>   * ForgedInvalidBlock (hopefully never -- this would indicate a bug)


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Forge.StateInfo.ForgedInvalidBlock


> We forged a block that is invalid according to the ledger in the  ChainDB. This means there is an inconsistency between the mempool  validation and the ledger validation. This is a serious error!


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Forge.StateInfo.LedgerState


> We obtained a ledger state for the point of the block we want to  connect to   We record both the current slot number as well as the point of the block  we attempt to connect the new block to (that we requested the ledger  state for).


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Forge.StateInfo.LedgerView


> We obtained a ledger view for the current slot number   We record the current slot number.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Forge.StateInfo.NoLedgerState


> Leadership check failed: we were unable to get the ledger state for the  point of the block we want to connect to   This can happen if after choosing which block to connect to the node  switched to a different fork. We expect this to happen only rather  rarely, so this certainly merits a warning; if it happens a lot, that  merits an investigation.   We record both the current slot number as well as the point of the block  we attempt to connect the new block to (that we requested the ledger  state for).


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Forge.StateInfo.NoLedgerView


> Leadership check failed: we were unable to get the ledger view for the  current slot number   This will only happen if there are many missing blocks between the tip of  our chain and the current slot.   We record also the failure returned by 'forecastFor'.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Forge.StateInfo.NodeCannotForge


> We did the leadership check and concluded that we should lead and forge  a block, but cannot.   This should only happen rarely and should be logged with warning severity.   Records why we cannot forge a block.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Forge.StateInfo.NodeIsLeader


> We did the leadership check and concluded we /are/ the leader
>   The node will soon forge; it is about to read its transactions from the  Mempool. This will be followed by ForgedBlock.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Forge.StateInfo.NodeNotLeader


> We did the leadership check and concluded we are not the leader   We record the current slot number


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Forge.StateInfo.SlotIsImmutable


> Leadership check failed: the tip of the ImmutableDB inhabits the  current slot   This might happen in two cases.    1. the clock moved backwards, on restart we ignored everything from the      VolatileDB since it's all in the future, and now the tip of the      ImmutableDB points to a block produced in the same slot we're trying      to produce a block in    2. k = 0 and we already adopted a block from another leader of the same      slot.   We record both the current slot number as well as the tip of the  ImmutableDB.  See also <https://github.com/input-output-hk/ouroboros-network/issues/1462>


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Forge.StateInfo.StartLeadershipCheck


> Start of the leadership check.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Forge.StateInfo.StartLeadershipCheckPlus


> We adopted the block we produced, we also trace the transactions  that were adopted.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Forge.Stats


> nodeCannotForgeNum shows how many times this node could not forge.
> nodeIsLeaderNum shows how many times this node was leader.
> blocksForgedNum shows how many blocks did forge in this node.
> slotsMissed shows how many slots were missed in this node.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Mempool.AddedTx


> New, valid transaction that was added to the Mempool.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Mempool.ManuallyRemovedTxs


> Transactions that have been manually removed from the Mempool.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Mempool.RejectedTx


> New, invalid transaction thas was rejected and thus not added to the Mempool.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Mempool.RemoveTxs


> Previously valid transactions that are no longer valid because of changes in the ledger state. These transactions have been removed from the Mempool.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Net.AcceptPolicy.ConnectionHardLimit


> Hard rate limit reached, waiting until the number of connections drops below n.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.AcceptPolicy.ConnectionLimitResume




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.AcceptPolicy.ConnectionRateLimiting


> Rate limiting accepting connections, delaying next accept for given time, currently serving n connections.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ConnectionManager.Local.Connect




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ConnectionManager.Local.ConnectError




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ConnectionManager.Local.ConnectionCleanup




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ConnectionManager.Local.ConnectionExists




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ConnectionManager.Local.ConnectionFailure




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ConnectionManager.Local.ConnectionHandler




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ConnectionManager.Local.ConnectionManagerCounters




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ConnectionManager.Local.ConnectionNotFound




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ConnectionManager.Local.ConnectionTimeWait




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ConnectionManager.Local.ConnectionTimeWaitDone




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ConnectionManager.Local.ForbiddenConnection




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ConnectionManager.Local.ForbiddenOperation




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ConnectionManager.Local.ImpossibleConnection




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ConnectionManager.Local.IncludeConnection




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ConnectionManager.Local.PruneConnections




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ConnectionManager.Local.Shutdown




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ConnectionManager.Local.State




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ConnectionManager.Local.TerminatedConnection




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ConnectionManager.Local.TerminatingConnection




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ConnectionManager.Local.UnexpectedlyFalseAssertion




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ConnectionManager.Local.UnregisterConnection




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ConnectionManager.Remote.Connect




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ConnectionManager.Remote.ConnectError




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ConnectionManager.Remote.ConnectionCleanup




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ConnectionManager.Remote.ConnectionExists




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ConnectionManager.Remote.ConnectionFailure




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ConnectionManager.Remote.ConnectionHandler




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ConnectionManager.Remote.ConnectionManagerCounters




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ConnectionManager.Remote.ConnectionNotFound




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ConnectionManager.Remote.ConnectionTimeWait




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ConnectionManager.Remote.ConnectionTimeWaitDone




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ConnectionManager.Remote.ForbiddenConnection




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ConnectionManager.Remote.ForbiddenOperation




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ConnectionManager.Remote.ImpossibleConnection




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ConnectionManager.Remote.IncludeConnection




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ConnectionManager.Remote.PruneConnections




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ConnectionManager.Remote.Shutdown




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ConnectionManager.Remote.State




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ConnectionManager.Remote.TerminatedConnection




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ConnectionManager.Remote.TerminatingConnection




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ConnectionManager.Remote.Transition.ConnectionManagerTransition




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ConnectionManager.Remote.UnexpectedlyFalseAssertion




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ConnectionManager.Remote.UnregisterConnection




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.DNSResolver.LookupAAAAError


> AAAA lookup failed with an error.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.DNSResolver.LookupAAAAResult


> Lookup AAAA result.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.DNSResolver.LookupAError


> A lookup failed with an error.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.DNSResolver.LookupAResult


> Lookup A result.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.DNSResolver.LookupException


> A DNS lookup exception occurred.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.DNSResolver.LookupIPv4First


> Returning IPv4 address first.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.DNSResolver.LookupIPv6First


> Returning IPv6 address first.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ErrorPolicy.Local.AcceptException


> 'accept' threw an exception.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ErrorPolicy.Local.KeepSuspended


> Consumer was suspended until producer will resume.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ErrorPolicy.Local.LocalNodeError


> caught a local exception.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ErrorPolicy.Local.ResumeConsumer


> Resume consumer.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ErrorPolicy.Local.ResumePeer


> Resume a peer (both consumer and producer).


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ErrorPolicy.Local.ResumeProducer


> Resume producer.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ErrorPolicy.Local.SuspendConsumer


> Suspending consumer.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ErrorPolicy.Local.SuspendPeer


> Suspending peer with a given exception.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ErrorPolicy.Local.UnhandledApplicationException


> An application threw an exception, which was not handled.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ErrorPolicy.Local.UnhandledConnectionException


> 'connect' threw an exception, which was not handled by any 'ErrorPolicy'.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ErrorPolicy.Remote.AcceptException


> 'accept' threw an exception.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ErrorPolicy.Remote.KeepSuspended


> Consumer was suspended until producer will resume.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ErrorPolicy.Remote.LocalNodeError


> caught a local exception.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ErrorPolicy.Remote.ResumeConsumer


> Resume consumer.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ErrorPolicy.Remote.ResumePeer


> Resume a peer (both consumer and producer).


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ErrorPolicy.Remote.ResumeProducer


> Resume producer.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ErrorPolicy.Remote.SuspendConsumer


> Suspending consumer.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ErrorPolicy.Remote.SuspendPeer


> Suspending peer with a given exception.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ErrorPolicy.Remote.UnhandledApplicationException


> An application threw an exception, which was not handled.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.ErrorPolicy.Remote.UnhandledConnectionException


> 'connect' threw an exception, which was not handled by any 'ErrorPolicy'.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Handshake.Local.Receive.AcceptVersion


> The remote end decides which version to use and sends chosen version.The server is allowed to modify version parameters.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Handshake.Local.Receive.ProposeVersions


> Propose versions together with version parameters.  It must be encoded to a sorted list..


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Handshake.Local.Receive.Refuse


> It refuses to run any version.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Handshake.Local.Receive.ReplyVersions


> `MsgReplyVersions` received as a response to 'MsgProposeVersions'.  It is not supported to explicitly send this message. It can only be received as a copy of 'MsgProposeVersions' in a simultaneous open scenario.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Handshake.Local.Send.AcceptVersion


> The remote end decides which version to use and sends chosen version.The server is allowed to modify version parameters.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Handshake.Local.Send.ProposeVersions


> Propose versions together with version parameters.  It must be encoded to a sorted list..


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Handshake.Local.Send.Refuse


> It refuses to run any version.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Handshake.Local.Send.ReplyVersions


> `MsgReplyVersions` received as a response to 'MsgProposeVersions'.  It is not supported to explicitly send this message. It can only be received as a copy of 'MsgProposeVersions' in a simultaneous open scenario.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Handshake.Remote.Receive.AcceptVersion


> The remote end decides which version to use and sends chosen version.The server is allowed to modify version parameters.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Handshake.Remote.Receive.ProposeVersions


> Propose versions together with version parameters.  It must be encoded to a sorted list..


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Handshake.Remote.Receive.Refuse


> It refuses to run any version.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Handshake.Remote.Receive.ReplyVersions


> `MsgReplyVersions` received as a response to 'MsgProposeVersions'.  It is not supported to explicitly send this message. It can only be received as a copy of 'MsgProposeVersions' in a simultaneous open scenario.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Handshake.Remote.Send.AcceptVersion


> The remote end decides which version to use and sends chosen version.The server is allowed to modify version parameters.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Handshake.Remote.Send.ProposeVersions


> Propose versions together with version parameters.  It must be encoded to a sorted list..


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Handshake.Remote.Send.Refuse


> It refuses to run any version.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Handshake.Remote.Send.ReplyVersions


> `MsgReplyVersions` received as a response to 'MsgProposeVersions'.  It is not supported to explicitly send this message. It can only be received as a copy of 'MsgProposeVersions' in a simultaneous open scenario.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.InboundGovernor.Local.DemotedToColdRemote


> All mini-protocols terminated.  The boolean is true if this connection was not used by p2p-governor, and thus the connection will be terminated.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.InboundGovernor.Local.DemotedToWarmRemote


> All mini-protocols terminated.  The boolean is true if this connection was not used by p2p-governor, and thus the connection will be terminated.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.InboundGovernor.Local.InboundGovernorCounters




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.InboundGovernor.Local.InboundGovernorError




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.InboundGovernor.Local.MuxCleanExit




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.InboundGovernor.Local.MuxErrored




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.InboundGovernor.Local.NewConnection




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.InboundGovernor.Local.PromotedToHotRemote




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.InboundGovernor.Local.PromotedToWarmRemote




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.InboundGovernor.Local.RemoteState




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.InboundGovernor.Local.ResponderErrored




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.InboundGovernor.Local.ResponderRestarted




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.InboundGovernor.Local.ResponderStartFailure




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.InboundGovernor.Local.ResponderStarted




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.InboundGovernor.Local.ResponderTerminated




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.InboundGovernor.Local.UnexpectedlyFalseAssertion




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.InboundGovernor.Local.WaitIdleRemote




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.InboundGovernor.Remote.DemotedToColdRemote


> All mini-protocols terminated.  The boolean is true if this connection was not used by p2p-governor, and thus the connection will be terminated.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.InboundGovernor.Remote.DemotedToWarmRemote


> All mini-protocols terminated.  The boolean is true if this connection was not used by p2p-governor, and thus the connection will be terminated.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.InboundGovernor.Remote.InboundGovernorCounters




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.InboundGovernor.Remote.InboundGovernorError




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.InboundGovernor.Remote.MuxCleanExit




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.InboundGovernor.Remote.MuxErrored




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.InboundGovernor.Remote.NewConnection




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.InboundGovernor.Remote.PromotedToHotRemote




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.InboundGovernor.Remote.PromotedToWarmRemote




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.InboundGovernor.Remote.RemoteState




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.InboundGovernor.Remote.ResponderErrored




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.InboundGovernor.Remote.ResponderRestarted




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.InboundGovernor.Remote.ResponderStartFailure




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.InboundGovernor.Remote.ResponderStarted




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.InboundGovernor.Remote.ResponderTerminated




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.InboundGovernor.Remote.Transition.InboundGovernorTransition




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.InboundGovernor.Remote.UnexpectedlyFalseAssertion




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.InboundGovernor.Remote.WaitIdleRemote




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.KeepAliveClient




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Local.ChannelRecvEnd


> Channel receive end.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Local.ChannelRecvStart


> Channel receive start.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Local.ChannelSendEnd


> Channel send end.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Local.ChannelSendStart


> Channel send start.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Local.CleanExit


> Miniprotocol terminated cleanly.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Local.ExceptionExit


> Miniprotocol terminated with exception.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Local.HandshakeClientEnd


> Handshake client end.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Local.HandshakeClientError


> Handshake client error.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Local.HandshakeServerEnd


> Handshake server end.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Local.HandshakeServerError


> Handshake server error.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Local.HandshakeStart


> Handshake start.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Local.RecvDeltaQObservation


> Bearer DeltaQ observation.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Local.RecvDeltaQSample


> Bearer DeltaQ sample.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Local.RecvEnd


> Bearer receive end.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Local.RecvHeaderEnd


> Bearer receive header end.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Local.RecvHeaderStart


> Bearer receive header start.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Local.RecvStart


> Bearer receive start.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Local.SDUReadTimeoutException


> Timed out reading SDU.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Local.SDUWriteTimeoutException


> Timed out writing SDU.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Local.SendEnd


> Bearer send end.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Local.SendStart


> Bearer send start.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Local.Shutdown


> Mux shutdown.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Local.StartEagerly


> Eagerly started.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Local.StartOnDemand


> Preparing to start.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Local.StartedOnDemand


> Started on demand.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Local.State


> State.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Local.TCPInfo


> TCPInfo.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Local.Terminating


> Terminating.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Remote.ChannelRecvEnd


> Channel receive end.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Remote.ChannelRecvStart


> Channel receive start.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Remote.ChannelSendEnd


> Channel send end.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Remote.ChannelSendStart


> Channel send start.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Remote.CleanExit


> Miniprotocol terminated cleanly.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Remote.ExceptionExit


> Miniprotocol terminated with exception.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Remote.HandshakeClientEnd


> Handshake client end.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Remote.HandshakeClientError


> Handshake client error.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Remote.HandshakeServerEnd


> Handshake server end.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Remote.HandshakeServerError


> Handshake server error.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Remote.HandshakeStart


> Handshake start.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Remote.RecvDeltaQObservation


> Bearer DeltaQ observation.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Remote.RecvDeltaQSample


> Bearer DeltaQ sample.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Remote.RecvEnd


> Bearer receive end.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Remote.RecvHeaderEnd


> Bearer receive header end.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Remote.RecvHeaderStart


> Bearer receive header start.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Remote.RecvStart


> Bearer receive start.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Remote.SDUReadTimeoutException


> Timed out reading SDU.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Remote.SDUWriteTimeoutException


> Timed out writing SDU.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Remote.SendEnd


> Bearer send end.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Remote.SendStart


> Bearer send start.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Remote.Shutdown


> Mux shutdown.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Remote.StartEagerly


> Eagerly started.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Remote.StartOnDemand


> Preparing to start.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Remote.StartedOnDemand


> Started on demand.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Remote.State


> State.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Remote.TCPInfo


> TCPInfo.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Mux.Remote.Terminating


> Terminating.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.PeerSelection.Actions.MonitoringError




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.PeerSelection.Actions.MonitoringResult




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.PeerSelection.Actions.StatusChangeFailure




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.PeerSelection.Actions.StatusChanged




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.PeerSelection.Counters


> Counters for cold, warm and hot peers


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.PeerSelection.Initiator.GovernorState




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.PeerSelection.Responder.GovernorState




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.PeerSelection.Selection.ChurnMode




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.PeerSelection.Selection.ChurnWait




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.PeerSelection.Selection.DemoteAsynchronous




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.PeerSelection.Selection.DemoteHotDone


> target active, actual active, peer


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.PeerSelection.Selection.DemoteHotFailed


> target active, actual active, peer, reason


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.PeerSelection.Selection.DemoteHotPeers


> target active, actual active, selected peers


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.PeerSelection.Selection.DemoteLocalHotPeers


> local per-group (target active, actual active), selected peers


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.PeerSelection.Selection.DemoteWarmDone


> target established, actual established, peer


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.PeerSelection.Selection.DemoteWarmFailed


> target established, actual established, peer, reason


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.PeerSelection.Selection.DemoteWarmPeers


> target established, actual established, selected peers


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.PeerSelection.Selection.ForgetColdPeers


> target known peers, actual known peers, selected peers


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.PeerSelection.Selection.GossipRequests


> target known peers, actual known peers, peers available for gossip, peers selected for gossip


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.PeerSelection.Selection.GossipResults




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.PeerSelection.Selection.GovernorWakeup




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.PeerSelection.Selection.LocalRootPeersChanged




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.PeerSelection.Selection.PromoteColdDone


> target active, actual active, selected peers


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.PeerSelection.Selection.PromoteColdFailed


> target established, actual established, peer, delay until next promotion, reason


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.PeerSelection.Selection.PromoteColdLocalPeers


> target local established, actual local established, selected peers


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.PeerSelection.Selection.PromoteColdPeers


> target established, actual established, selected peers


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.PeerSelection.Selection.PromoteWarmAborted




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.PeerSelection.Selection.PromoteWarmDone


> target active, actual active, peer


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.PeerSelection.Selection.PromoteWarmFailed


> target active, actual active, peer, reason


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.PeerSelection.Selection.PromoteWarmLocalPeers


> local per-group (target active, actual active), selected peers


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.PeerSelection.Selection.PromoteWarmPeers


> target active, actual active, selected peers


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.PeerSelection.Selection.PublicRootsFailure




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.PeerSelection.Selection.PublicRootsRequest




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.PeerSelection.Selection.PublicRootsResults




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.PeerSelection.Selection.TargetsChanged




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Peers.Ledger.DisabledLedgerPeers


> Trace for when getting peers from the ledger is disabled, that is DontUseLedger.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Peers.Ledger.FallingBackToBootstrapPeers




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Peers.Ledger.FetchingNewLedgerState


> Trace for fetching a new list of peers from the ledger. Int is the number of peers returned.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Peers.Ledger.PickedPeer


> Trace for a peer picked with accumulated and relative stake of its pool.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Peers.Ledger.PickedPeers


> Trace for the number of peers we wanted to pick and the list of peers picked.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Peers.Ledger.RequestForPeers


> RequestForPeers (NumberOfPeers 1)


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Peers.Ledger.ReusingLedgerState




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Peers.Ledger.TraceUseLedgerAfter


> Trace UseLedgerAfter value.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Peers.Ledger.WaitingOnRequest




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Peers.List




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Peers.LocalRoot.LocalRootDomains




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Peers.LocalRoot.LocalRootError




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Peers.LocalRoot.LocalRootFailure




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Peers.LocalRoot.LocalRootGroups




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Peers.LocalRoot.LocalRootResult




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Peers.LocalRoot.LocalRootWaiting




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Peers.PublicRoot.PublicRootDomains




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Peers.PublicRoot.PublicRootFailure




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Peers.PublicRoot.PublicRootRelayAccessPoint




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Peers.PublicRoot.PublicRootResult




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Server.Local.AcceptConnection




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Server.Local.AcceptError




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Server.Local.AcceptPolicy




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Server.Local.Error




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Server.Local.Started




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Server.Local.Stopped




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Server.Remote.AcceptConnection




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Server.Remote.AcceptError




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Server.Remote.AcceptPolicy




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Server.Remote.Error




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Server.Remote.Started




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Server.Remote.Stopped




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Subscription.DNS.AllocateSocket


> DNS Subscription: Allocate socket to address.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Subscription.DNS.ApplicationException


> DNS Subscription: Application Exception occurred.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Subscription.DNS.CloseSocket


> DNS Subscription: Closed socket to address.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Subscription.DNS.ConnectEnd


> DNS Subscription: Connection Attempt end with destination and outcome.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Subscription.DNS.ConnectException


> DNS Subscription: Socket Allocation Exception with destination and the exception.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Subscription.DNS.ConnectStart


> DNS Subscription: Connection Attempt Start with destination.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Subscription.DNS.ConnectionExist


> DNS Subscription: Connection exists to destination.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Subscription.DNS.MissingLocalAddress


> DNS Subscription: Missing local address.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Subscription.DNS.Restart


> DNS Subscription: Restarting Subscription after duration with desired valency and current valency.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Subscription.DNS.SkippingPeer


> DNS Subscription: Skipping peer with address.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Subscription.DNS.SocketAllocationException


> DNS Subscription: Connection Attempt Exception with destination and exception.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Subscription.DNS.Start


> DNS Subscription: Starting Subscription Worker with a valency.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Subscription.DNS.SubscriptionFailed


> DNS Subscription: Failed to start all required subscriptions.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Subscription.DNS.SubscriptionRunning


> DNS Subscription: Required subscriptions started.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Subscription.DNS.SubscriptionWaiting


> DNS Subscription: Waiting on address with active connections.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Subscription.DNS.SubscriptionWaitingNewConnection


> DNS Subscription: Waiting delay time before attempting a new connection.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Subscription.DNS.TryConnectToPeer


> DNS Subscription: Trying to connect to peer with address.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Subscription.DNS.UnsupportedRemoteAddr


> DNS Subscription: Unsupported remote target address.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Subscription.IP.AllocateSocket


> IP Subscription: Allocate socket to address.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Subscription.IP.ApplicationException


> IP Subscription: Application Exception occurred.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Subscription.IP.CloseSocket


> IP Subscription: Closed socket to address.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Subscription.IP.ConnectEnd


> IP Subscription: Connection Attempt end with destination and outcome.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Subscription.IP.ConnectException


> IP Subscription: Socket Allocation Exception with destination and the exception.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Subscription.IP.ConnectStart


> IP Subscription: Connection Attempt Start with destination.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Subscription.IP.ConnectionExist


> IP Subscription: Connection exists to destination.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Subscription.IP.MissingLocalAddress


> IP Subscription: Missing local address.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Subscription.IP.Restart


> IP Subscription: Restarting Subscription after duration with desired valency and current valency.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Subscription.IP.SkippingPeer


> IP Subscription: Skipping peer with address.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Subscription.IP.SocketAllocationException


> IP Subscription: Connection Attempt Exception with destination and exception.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Subscription.IP.Start


> IP Subscription: Starting Subscription Worker with a valency.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Subscription.IP.SubscriptionFailed


> IP Subscription: Failed to start all required subscriptions.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Subscription.IP.SubscriptionRunning


> IP Subscription: Required subscriptions started.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Subscription.IP.SubscriptionWaiting


> IP Subscription: Waiting on address with active connections.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Subscription.IP.SubscriptionWaitingNewConnection


> IP Subscription: Waiting delay time before attempting a new connection.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Subscription.IP.TryConnectToPeer


> IP Subscription: Trying to connect to peer with address.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Net.Subscription.IP.UnsupportedRemoteAddr


> IP Subscription: Unsupported remote target address.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### NodeState.NodeAddBlock


> Applying block


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### NodeState.NodeInitChainSelection


> Performing initial chain selection


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### NodeState.NodeKernelOnline


> Node kernel online


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### NodeState.NodeOpeningDbs


> ChainDB components being opened


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### NodeState.NodeReplays


> Replaying chain


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### NodeState.NodeShutdown


> Node shutting down


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### NodeState.NodeStartup


> Node startup


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### NodeState.NodeTracingOnlineConfiguring


> Tracing system came online, system configuring now


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Resources




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Info`

### Shutdown.Abnormal


> non-isEOFerror shutdown request


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Shutdown.ArmedAt


> Setting up node shutdown at given slot / block.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Shutdown.Requested


> Node shutdown was requested.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Shutdown.Requesting


> Ringing the node shutdown doorbell


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Shutdown.UnexpectedInput


> Received shutdown request but found unexpected input in --shutdown-ipc FD: 


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.Byron


> _bibSystemStartTime_: TODO JNF 
> _bibSlotLength_: gives the length of a slot as time interval. 
> _bibEpochLength_: gives the number of slots which forms an epoch.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.Common


> _biConfigPath_: is the path to the config in use. 
> _biProtocol_: is the name of the protocol, e.g. "Byron", "Shelley" or "Byron; Shelley". 
> _biVersion_: is the version of the node software running. 
> _biCommit_: is the commit revision of the software running. 
> _biNodeStartTime_: gives the time this node was started.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.DBValidation




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.DiffusionInit.ConfiguringLocalSocket


> ConfiguringLocalSocket 


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.DiffusionInit.ConfiguringServerSocket


> ConfiguringServerSocket 


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.DiffusionInit.CreateSystemdSocketForSnocketPath


> CreateSystemdSocketForSnocketPath 


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.DiffusionInit.CreatedLocalSocket


> CreatedLocalSocket 


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.DiffusionInit.CreatingServerSocket


> CreatingServerSocket 


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.DiffusionInit.DiffusionErrored


> DiffusionErrored 


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.DiffusionInit.ListeningLocalSocket


> ListeningLocalSocket 


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.DiffusionInit.ListeningServerSocket


> ListeningServerSocket 


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.DiffusionInit.LocalSocketUp


> LocalSocketUp 


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.DiffusionInit.RunLocalServer


> RunLocalServer 


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.DiffusionInit.RunServer


> RunServer 


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.DiffusionInit.ServerSocketUp


> ServerSocketUp 


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.DiffusionInit.UnsupportedLocalSystemdSocket


> UnsupportedLocalSystemdSocket 


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.DiffusionInit.UnsupportedReadySocketCase


> UnsupportedReadySocketCase 


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.DiffusionInit.UsingSystemdSocket


> UsingSystemdSocket 


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.Info




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.Network


> _niAddresses_: IPv4 or IPv6 socket ready to accept connectionsor diffusion addresses. 
> _niDiffusionMode_: shows if the node runs only initiator or bothinitiator or responder node. 
> _niDnsProducers_: shows the list of domain names to subscribe to. 
> _niIpProducers_: shows the list of ip subscription addresses.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.NetworkConfig




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.NetworkConfigUpdate




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.NetworkConfigUpdateError




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.NetworkMagic




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.P2PInfo




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.P2PWarning




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.P2PWarningDevelopementNetworkProtocols




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.ShelleyBased


> bisEra is the current era, e.g. "Shelley", "Allegra", "Mary" or "Alonzo". 
> _bisSystemStartTime_: TODO JNF 
> _bisSlotLength_: gives the length of a slot as time interval. 
> _bisEpochLength_: gives the number of slots which forms an epoch. 
> _bisSlotsPerKESPeriod_: gives the slots per KES period.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.SocketConfigError




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.Time




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### Startup.WarningDevelopmentNetworkProtocols




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### StateQueryServer.Receive.Acquire


> The client requests that the state as of a particular recent point on the server's chain (within K of the tip) be made available to query, and waits for confirmation or failure. 
>  From 'NodeToClient_V8' onwards if the point is not specified, current tip will be acquired.  For previous versions of the protocol 'point' must be given.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### StateQueryServer.Receive.Acquired


> The server can confirm that it has the state at the requested point.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### StateQueryServer.Receive.Done


> The client can terminate the protocol.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### StateQueryServer.Receive.Failure


> The server can report that it cannot obtain the state for the requested point.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### StateQueryServer.Receive.Query


> The client can perform queries on the current acquired state.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### StateQueryServer.Receive.ReAcquire


> This is like 'MsgAcquire' but for when the client already has a state. By moveing to another state directly without a 'MsgRelease' it enables optimisations on the server side (e.g. moving to the state for the immediate next block). 
>  Note that failure to re-acquire is equivalent to 'MsgRelease', rather than keeping the exiting acquired state. 
>  From 'NodeToClient_V8' onwards if the point is not specified, current tip will be acquired.  For previous versions of the protocol 'point' must be given.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### StateQueryServer.Receive.Release


> The client can instruct the server to release the state. This lets the server free resources.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### StateQueryServer.Receive.Result


> The server must reply with the queries.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### StateQueryServer.Send.Acquire


> The client requests that the state as of a particular recent point on the server's chain (within K of the tip) be made available to query, and waits for confirmation or failure. 
>  From 'NodeToClient_V8' onwards if the point is not specified, current tip will be acquired.  For previous versions of the protocol 'point' must be given.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### StateQueryServer.Send.Acquired


> The server can confirm that it has the state at the requested point.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### StateQueryServer.Send.Done


> The client can terminate the protocol.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### StateQueryServer.Send.Failure


> The server can report that it cannot obtain the state for the requested point.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### StateQueryServer.Send.Query


> The client can perform queries on the current acquired state.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### StateQueryServer.Send.ReAcquire


> This is like 'MsgAcquire' but for when the client already has a state. By moveing to another state directly without a 'MsgRelease' it enables optimisations on the server side (e.g. moving to the state for the immediate next block). 
>  Note that failure to re-acquire is equivalent to 'MsgRelease', rather than keeping the exiting acquired state. 
>  From 'NodeToClient_V8' onwards if the point is not specified, current tip will be acquired.  For previous versions of the protocol 'point' must be given.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### StateQueryServer.Send.Release


> The client can instruct the server to release the state. This lets the server free resources.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### StateQueryServer.Send.Result


> The server must reply with the queries.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission.Local.Receive.AcceptTx


> The server can reply to inform the client that it has accepted the transaction.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission.Local.Receive.Done


> The client can terminate the protocol.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission.Local.Receive.RejectTx


> The server can reply to inform the client that it has rejected the transaction. A reason for the rejection is included.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission.Local.Receive.SubmitTx


> The client submits a single transaction and waits a reply.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission.Local.Send.AcceptTx


> The server can reply to inform the client that it has accepted the transaction.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission.Local.Send.Done


> The client can terminate the protocol.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission.Local.Send.RejectTx


> The server can reply to inform the client that it has rejected the transaction. A reason for the rejection is included.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission.Local.Send.SubmitTx


> The client submits a single transaction and waits a reply.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission.LocalServer.ReceivedTx


> A transaction was received.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission.MonitorClient.Receive.Acquire


> The client requests that the state as of a particular recent point on the server's chain (within K of the tip) be made available to query, and waits for confirmation or failure. 
>  From 'NodeToClient_V8' onwards if the point is not specified, current tip will be acquired.  For previous versions of the protocol 'point' must be given.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission.MonitorClient.Receive.Acquired


> The server can confirm that it has the state at the requested point.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission.MonitorClient.Receive.Done


> The client can terminate the protocol.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission.MonitorClient.Receive.Failure


> The server can report that it cannot obtain the state for the requested point.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission.MonitorClient.Receive.Query


> The client can perform queries on the current acquired state.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission.MonitorClient.Receive.ReAcquire


> This is like 'MsgAcquire' but for when the client already has a state. By moveing to another state directly without a 'MsgRelease' it enables optimisations on the server side (e.g. moving to the state for the immediate next block). 
>  Note that failure to re-acquire is equivalent to 'MsgRelease', rather than keeping the exiting acquired state. 
>  From 'NodeToClient_V8' onwards if the point is not specified, current tip will be acquired.  For previous versions of the protocol 'point' must be given.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission.MonitorClient.Receive.Release


> The client can instruct the server to release the state. This lets the server free resources.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission.MonitorClient.Receive.Result


> The server must reply with the queries.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission.MonitorClient.Send.Acquire


> The client requests that the state as of a particular recent point on the server's chain (within K of the tip) be made available to query, and waits for confirmation or failure. 
>  From 'NodeToClient_V8' onwards if the point is not specified, current tip will be acquired.  For previous versions of the protocol 'point' must be given.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission.MonitorClient.Send.Acquired


> The server can confirm that it has the state at the requested point.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission.MonitorClient.Send.Done


> The client can terminate the protocol.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission.MonitorClient.Send.Failure


> The server can report that it cannot obtain the state for the requested point.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission.MonitorClient.Send.Query


> The client can perform queries on the current acquired state.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission.MonitorClient.Send.ReAcquire


> This is like 'MsgAcquire' but for when the client already has a state. By moveing to another state directly without a 'MsgRelease' it enables optimisations on the server side (e.g. moving to the state for the immediate next block). 
>  Note that failure to re-acquire is equivalent to 'MsgRelease', rather than keeping the exiting acquired state. 
>  From 'NodeToClient_V8' onwards if the point is not specified, current tip will be acquired.  For previous versions of the protocol 'point' must be given.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission.MonitorClient.Send.Release


> The client can instruct the server to release the state. This lets the server free resources.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission.MonitorClient.Send.Result


> The server must reply with the queries.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission.Remote.Receive.Done


> Termination message, initiated by the client when the server is making a blocking call for more transaction identifiers.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission.Remote.Receive.MsgHello


> Client side hello message.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission.Remote.Receive.ReplyTxIds


> Reply with a list of transaction identifiers for available transactions, along with the size of each transaction. 
>  The list must not be longer than the maximum number requested. 
>  In the 'StTxIds' 'StBlocking' state the list must be non-empty while in the 'StTxIds' 'StNonBlocking' state the list may be empty. 
>  These transactions are added to the notional FIFO of outstanding transaction identifiers for the protocol. 
>  The order in which these transaction identifiers are returned must be the order in which they are submitted to the mempool, to preserve dependent transactions.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission.Remote.Receive.ReplyTxs


> Reply with the requested transactions, or implicitly discard. 
>  Transactions can become invalid between the time the transaction identifier was sent and the transaction being requested. Invalid (including committed) transactions do not need to be sent. 
>  Any transaction identifiers requested but not provided in this reply should be considered as if this peer had never announced them. (Note that this is no guarantee that the transaction is invalid, it may still be valid and available from another peer).


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission.Remote.Receive.RequestTxIds


> Request a non-empty list of transaction identifiers from the client, and confirm a number of outstanding transaction identifiers. 
> With 'TokBlocking' this is a a blocking operation: the response will always have at least one transaction identifier, and it does not expect a prompt response: there is no timeout. This covers the case when there is nothing else to do but wait. For example this covers leaf nodes that rarely, if ever, create and submit a transaction. 
> With 'TokNonBlocking' this is a non-blocking operation: the response may be an empty list and this does expect a prompt response. This covers high throughput use cases where we wish to pipeline, by interleaving requests for additional transaction identifiers with requests for transactions, which requires these requests not block. 
> The request gives the maximum number of transaction identifiers that can be accepted in the response. This must be greater than zero in the 'TokBlocking' case. In the 'TokNonBlocking' case either the numbers acknowledged or the number requested must be non-zero. In either case, the number requested must not put the total outstanding over the fixed protocol limit. 
> The request also gives the number of outstanding transaction identifiers that can now be acknowledged. The actual transactions to acknowledge are known to the peer based on the FIFO order in which they were provided. 
> There is no choice about when to use the blocking case versus the non-blocking case, it depends on whether there are any remaining unacknowledged transactions (after taking into account the ones acknowledged in this message): 
> * The blocking case must be used when there are zero remaining   unacknowledged transactions. 
> * The non-blocking case must be used when there are non-zero remaining   unacknowledged transactions.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission.Remote.Receive.RequestTxs


> Request one or more transactions corresponding to the given  transaction identifiers.  
>  While it is the responsibility of the replying peer to keep within pipelining in-flight limits, the sender must also cooperate by keeping the total requested across all in-flight requests within the limits. 
>  It is an error to ask for transaction identifiers that were not previously announced (via 'MsgReplyTxIds'). 
>  It is an error to ask for transaction identifiers that are not outstanding or that were already asked for.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission.Remote.Send.Done


> Termination message, initiated by the client when the server is making a blocking call for more transaction identifiers.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission.Remote.Send.MsgHello


> Client side hello message.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission.Remote.Send.ReplyTxIds


> Reply with a list of transaction identifiers for available transactions, along with the size of each transaction. 
>  The list must not be longer than the maximum number requested. 
>  In the 'StTxIds' 'StBlocking' state the list must be non-empty while in the 'StTxIds' 'StNonBlocking' state the list may be empty. 
>  These transactions are added to the notional FIFO of outstanding transaction identifiers for the protocol. 
>  The order in which these transaction identifiers are returned must be the order in which they are submitted to the mempool, to preserve dependent transactions.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission.Remote.Send.ReplyTxs


> Reply with the requested transactions, or implicitly discard. 
>  Transactions can become invalid between the time the transaction identifier was sent and the transaction being requested. Invalid (including committed) transactions do not need to be sent. 
>  Any transaction identifiers requested but not provided in this reply should be considered as if this peer had never announced them. (Note that this is no guarantee that the transaction is invalid, it may still be valid and available from another peer).


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission.Remote.Send.RequestTxIds


> Request a non-empty list of transaction identifiers from the client, and confirm a number of outstanding transaction identifiers. 
> With 'TokBlocking' this is a a blocking operation: the response will always have at least one transaction identifier, and it does not expect a prompt response: there is no timeout. This covers the case when there is nothing else to do but wait. For example this covers leaf nodes that rarely, if ever, create and submit a transaction. 
> With 'TokNonBlocking' this is a non-blocking operation: the response may be an empty list and this does expect a prompt response. This covers high throughput use cases where we wish to pipeline, by interleaving requests for additional transaction identifiers with requests for transactions, which requires these requests not block. 
> The request gives the maximum number of transaction identifiers that can be accepted in the response. This must be greater than zero in the 'TokBlocking' case. In the 'TokNonBlocking' case either the numbers acknowledged or the number requested must be non-zero. In either case, the number requested must not put the total outstanding over the fixed protocol limit. 
> The request also gives the number of outstanding transaction identifiers that can now be acknowledged. The actual transactions to acknowledge are known to the peer based on the FIFO order in which they were provided. 
> There is no choice about when to use the blocking case versus the non-blocking case, it depends on whether there are any remaining unacknowledged transactions (after taking into account the ones acknowledged in this message): 
> * The blocking case must be used when there are zero remaining   unacknowledged transactions. 
> * The non-blocking case must be used when there are non-zero remaining   unacknowledged transactions.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission.Remote.Send.RequestTxs


> Request one or more transactions corresponding to the given  transaction identifiers.  
>  While it is the responsibility of the replying peer to keep within pipelining in-flight limits, the sender must also cooperate by keeping the total requested across all in-flight requests within the limits. 
>  It is an error to ask for transaction identifiers that were not previously announced (via 'MsgReplyTxIds'). 
>  It is an error to ask for transaction identifiers that are not outstanding or that were already asked for.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission.TxInbound.CanRequestMoreTxs


> There are no replies in flight, but we do know some more txs we can ask for, so lets ask for them and more txids.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission.TxInbound.CannotRequestMoreTxs


> There's no replies in flight, and we have no more txs we can ask for so the only remaining thing to do is to ask for more txids. Since this is the only thing to do now, we make this a blocking call.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission.TxInbound.Collected


> Number of transactions just about to be inserted.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission.TxInbound.Processed


> Just processed transaction pass/fail breakdown.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission.TxInbound.Terminated


> Server received 'MsgDone'.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission.TxOutbound.ControlMessage




From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission.TxOutbound.RecvMsgRequest


> The IDs of the transactions requested.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

### TxSubmission.TxOutbound.SendMsgReply


> The transactions to be sent in the response.


From current configuration:
Details:   `DNormal`
Backends:
			`EKGBackend`,
			`Stdout MachineFormat`,
			`Forwarder`
Filtered  by config value: `Notice`

## Metrics
### BlockFetch.BlocksServed



Dispatched by: 
BlockFetch.ServerBlock.SendBlock

### BlockFetch.ConnectedPeers

> Number of connected peers


Dispatched by: 
BlockFetch.Decision

### ChainDB.BlockReplayProgress

> Progress in percent


Dispatched by: 
ChainDB.ReplayBlock.LedgerReplay

### ChainDB.Blocks

> Number of blocks in this chain fragment.


Dispatched by: 
ChainDB.AddBlockEvent.AddedToCurrentChain
ChainDB.AddBlockEvent.SwitchedToAFork

### ChainDB.Density

> The actual number of blocks created over the maximum expected number of blocks that could be created over the span of the last @k@ blocks.


Dispatched by: 
ChainDB.AddBlockEvent.AddedToCurrentChain
ChainDB.AddBlockEvent.SwitchedToAFork

### ChainDB.Epoch

> In which epoch is the tip of the current chain.


Dispatched by: 
ChainDB.AddBlockEvent.AddedToCurrentChain
ChainDB.AddBlockEvent.SwitchedToAFork

### ChainDB.SlotInEpoch

> Relative slot number of the tip of the current chain within theepoch..


Dispatched by: 
ChainDB.AddBlockEvent.AddedToCurrentChain
ChainDB.AddBlockEvent.SwitchedToAFork

### ChainDB.Slots

> Number of slots in this chain fragment.


Dispatched by: 
ChainDB.AddBlockEvent.AddedToCurrentChain
ChainDB.AddBlockEvent.SwitchedToAFork

### ChainSync.HeadersServed

> A counter triggered only on header event


Dispatched by: 
ChainSync.ServerHeader.Update

### Forge.AboutToLeadSlotLast



Dispatched by: 
Forge.StateInfo.StartLeadershipCheck
Forge.StateInfo.StartLeadershipCheckPlus

### Forge.AdoptedOwnBlockSlotLast



Dispatched by: 
Forge.StateInfo.AdoptedBlock

### Forge.BlockContext



Dispatched by: 
Forge.StateInfo.BlockContext

### Forge.BlockFromFuture



Dispatched by: 
Forge.StateInfo.BlockFromFuture

### Forge.BlocksForgedNum

> How many blocks did forge in this node?


Dispatched by: 
Forge.Stats

### Forge.CouldNotForgeSlotLast



Dispatched by: 
Forge.StateInfo.NoLedgerState
Forge.StateInfo.NoLedgerView

### Forge.CurrentKESPeriod



Dispatched by: 
Forge.StateInfo.ForgeStateUpdateError

### Forge.DelegMapSize



Dispatched by: 
Forge.StateInfo.StartLeadershipCheckPlus

### Forge.ForgedInvalidSlotLast



Dispatched by: 
Forge.StateInfo.ForgedInvalidBlock

### Forge.ForgedSlotLast



Dispatched by: 
Forge.StateInfo.ForgedBlock

### Forge.LastSlot



Dispatched by: 
Forge.Stats

### Forge.LedgerState



Dispatched by: 
Forge.StateInfo.LedgerState

### Forge.LedgerView



Dispatched by: 
Forge.StateInfo.LedgerView

### Forge.NodeCannotForge



Dispatched by: 
Forge.StateInfo.NodeCannotForge

### Forge.NodeCannotForgeNum

> How many times this node could not forge?


Dispatched by: 
Forge.Stats

### Forge.NodeIsLeader



Dispatched by: 
Forge.StateInfo.NodeIsLeader

### Forge.NodeIsLeaderNum

> How many times this node was leader?


Dispatched by: 
Forge.Stats

### Forge.NodeNotLeader



Dispatched by: 
Forge.StateInfo.NodeNotLeader

### Forge.NotAdoptedSlotLast



Dispatched by: 
Forge.StateInfo.DidntAdoptBlock

### Forge.OperationalCertificateExpiryKESPeriod



Dispatched by: 
Forge.StateInfo.ForgeStateUpdateError

### Forge.OperationalCertificateStartKESPeriod



Dispatched by: 
Forge.StateInfo.ForgeStateUpdateError

### Forge.RemainingKESPeriods



Dispatched by: 
Forge.StateInfo.ForgeStateUpdateError

### Forge.SlotIsImmutable



Dispatched by: 
Forge.StateInfo.SlotIsImmutable

### Forge.SlotsMissed

> How many slots were missed in this node?


Dispatched by: 
Forge.Stats

### Forge.UtxoSize



Dispatched by: 
Forge.StateInfo.StartLeadershipCheckPlus

### Mempool.MempoolBytes

> Byte size of the mempool


Dispatched by: 
Mempool.AddedTx
Mempool.ManuallyRemovedTxs
Mempool.RejectedTx
Mempool.RemoveTxs

### Mempool.TxsInMempool

> Transactions in mempool


Dispatched by: 
Mempool.AddedTx
Mempool.ManuallyRemovedTxs
Mempool.RejectedTx
Mempool.RemoveTxs

### Mempool.TxsProcessedNum



Dispatched by: 
Mempool.ManuallyRemovedTxs

### Net.ConnectionManager.DuplexConns



Dispatched by: 
Net.ConnectionManager.Remote.ConnectionManagerCounters

### Net.ConnectionManager.DuplexConns



Dispatched by: 
Net.ConnectionManager.Local.ConnectionManagerCounters

### Net.ConnectionManager.FullDuplexConns



Dispatched by: 
Net.ConnectionManager.Remote.ConnectionManagerCounters

### Net.ConnectionManager.FullDuplexConns



Dispatched by: 
Net.ConnectionManager.Local.ConnectionManagerCounters

### Net.ConnectionManager.InboundConns



Dispatched by: 
Net.ConnectionManager.Remote.ConnectionManagerCounters

### Net.ConnectionManager.InboundConns



Dispatched by: 
Net.ConnectionManager.Local.ConnectionManagerCounters

### Net.ConnectionManager.OutboundConns



Dispatched by: 
Net.ConnectionManager.Remote.ConnectionManagerCounters

### Net.ConnectionManager.OutboundConns



Dispatched by: 
Net.ConnectionManager.Local.ConnectionManagerCounters

### Net.ConnectionManager.UnidirectionalConns



Dispatched by: 
Net.ConnectionManager.Remote.ConnectionManagerCounters

### Net.ConnectionManager.UnidirectionalConns



Dispatched by: 
Net.ConnectionManager.Local.ConnectionManagerCounters

### Net.InboundGovernor.Cold



Dispatched by: 
Net.InboundGovernor.Remote.InboundGovernorCounters

### Net.InboundGovernor.Hot



Dispatched by: 
Net.InboundGovernor.Remote.InboundGovernorCounters

### Net.InboundGovernor.Idle



Dispatched by: 
Net.InboundGovernor.Remote.InboundGovernorCounters

### Net.InboundGovernor.Warm



Dispatched by: 
Net.InboundGovernor.Remote.InboundGovernorCounters

### Net.LocalInboundGovernor.Cold



Dispatched by: 
Net.InboundGovernor.Local.InboundGovernorCounters

### Net.LocalInboundGovernor.Hot



Dispatched by: 
Net.InboundGovernor.Local.InboundGovernorCounters

### Net.LocalInboundGovernor.Idle



Dispatched by: 
Net.InboundGovernor.Local.InboundGovernorCounters

### Net.LocalInboundGovernor.Warm



Dispatched by: 
Net.InboundGovernor.Local.InboundGovernorCounters

### Net.PeerSelection.Cold

> Number of cold peers


Dispatched by: 
Net.PeerSelection.Counters

### Net.PeerSelection.Hot

> Number of hot peers


Dispatched by: 
Net.PeerSelection.Counters

### Net.PeerSelection.Warm

> Number of warm peers


Dispatched by: 
Net.PeerSelection.Counters

### Net.PeersFromNodeKernel



Dispatched by: 
Net.Peers.List

### Resources.Mem.Resident



Dispatched by: 
Resources

### Resources.RTS.GcLiveBytes



Dispatched by: 
Resources

### Resources.RTS.GcMajorNum



Dispatched by: 
Resources

### Resources.RTS.GcMinorNum



Dispatched by: 
Resources

### Resources.RTS.Gcticks



Dispatched by: 
Resources

### Resources.RTS.Mutticks



Dispatched by: 
Resources

### Resources.RTS.Threads



Dispatched by: 
Resources

### Resources.Stat.Cputicks

> Reports the CPU ticks, sice the process was started


Dispatched by: 
Resources

### TxSubmission.Accepted



Dispatched by: 
TxSubmission.TxInbound.Processed

### TxSubmission.Rejected



Dispatched by: 
TxSubmission.TxInbound.Processed

### TxSubmission.Submitted



Dispatched by: 
TxSubmission.TxInbound.Collected

## Datapoints
### NodeInfo


> Basic information about this node collected at startup
> 
>  _niName_: Name of the node. 
>  _niProtocol_: Protocol which this nodes uses. 
>  _niVersion_: Software version which this node is using. 
>  _niStartTime_: Start time of this node. 
>  _niSystemStartTime_: How long did the start of the node took.


Configuration: TraceConfig {tcOptions = fromList [([],[ConfSeverity {severity = Notice},ConfDetail {detail = DNormal},ConfBackend {backends = [Stdout MachineFormat,EKGBackend,Forwarder]}]),(["AcceptPolicy"],[ConfSeverity {severity = Info}]),(["BlockFetchClient","CompletedBlockFetch"],[ConfLimiter {maxFrequency = 2.0}]),(["ChainDB"],[ConfSeverity {severity = Info}]),(["ChainDB","AddBlockEvent","AddBlockValidation","ValidCandidate"],[ConfLimiter {maxFrequency = 2.0}]),(["ChainDB","AddBlockEvent","AddedBlockToQueue"],[ConfLimiter {maxFrequency = 2.0}]),(["ChainDB","AddBlockEvent","AddedBlockToVolatileDB"],[ConfLimiter {maxFrequency = 2.0}]),(["ChainDB","CopyToImmutableDBEvent","CopiedBlockToImmutableDB"],[ConfLimiter {maxFrequency = 2.0}]),(["DNSResolver"],[ConfSeverity {severity = Info}]),(["DNSSubscription"],[ConfSeverity {severity = Info}]),(["DiffusionInit"],[ConfSeverity {severity = Info}]),(["ErrorPolicy"],[ConfSeverity {severity = Info}]),(["Forge"],[ConfSeverity {severity = Info}]),(["IpSubscription"],[ConfSeverity {severity = Info}]),(["LocalErrorPolicy"],[ConfSeverity {severity = Info}]),(["Mempool"],[ConfSeverity {severity = Info}]),(["Resources"],[ConfSeverity {severity = Info}])], tcForwarder = TraceOptionForwarder {tofConnQueueSize = 2000, tofDisconnQueueSize = 200000, tofVerbosity = Minimum}, tcNodeName = Nothing, tcPeerFrequency = Just 2000, tcResourceFrequency = Just 5000}

662 log messages.
Generated at 2022-07-06 17:02:52.510663705 CEST.