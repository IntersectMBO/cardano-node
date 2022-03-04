# Cardano Trace Documentation
# Table Of Contents


## [Trace Messages](#trace-messages)
1. [Cardano.Node.AcceptPolicy.ConnectionRateLimiting](#cardanonodeacceptpolicyconnectionratelimiting)
1. [Cardano.Node.AcceptPolicy.ConnectionHardLimit](#cardanonodeacceptpolicyconnectionhardlimit)
1. [Cardano.Node.BlockFetch.NodeToNode.Send.BatchDone](#cardanonodeblockfetchnodetonodesendbatchdone)
1. [Cardano.Node.BlockFetch.NodeToNode.Send.Block](#cardanonodeblockfetchnodetonodesendblock)
1. [Cardano.Node.BlockFetch.NodeToNode.Send.ClientDone](#cardanonodeblockfetchnodetonodesendclientdone)
1. [Cardano.Node.BlockFetch.NodeToNode.Send.NoBlocks](#cardanonodeblockfetchnodetonodesendnoblocks)
1. [Cardano.Node.BlockFetch.NodeToNode.Send.RequestRange](#cardanonodeblockfetchnodetonodesendrequestrange)
1. [Cardano.Node.BlockFetch.NodeToNode.Send.StartBatch](#cardanonodeblockfetchnodetonodesendstartbatch)
1. [Cardano.Node.BlockFetchClient.AcknowledgedFetchRequest](#cardanonodeblockfetchclientacknowledgedfetchrequest)
1. [Cardano.Node.BlockFetchClient.AddedFetchRequest](#cardanonodeblockfetchclientaddedfetchrequest)
1. [Cardano.Node.BlockFetchClient.ClientTerminating](#cardanonodeblockfetchclientclientterminating)
1. [Cardano.Node.BlockFetchClient.CompletedFetchBatch](#cardanonodeblockfetchclientcompletedfetchbatch)
1. [Cardano.Node.BlockFetchClient.RejectedFetchBatch](#cardanonodeblockfetchclientrejectedfetchbatch)
1. [Cardano.Node.BlockFetchClient.StartedFetchBatch](#cardanonodeblockfetchclientstartedfetchbatch)
1. [Cardano.Node.BlockFetchDecision](#cardanonodeblockfetchdecision)
1. [Cardano.Node.BlockFetchSerialised.NodeToNode.Send.BatchDone](#cardanonodeblockfetchserialisednodetonodesendbatchdone)
1. [Cardano.Node.BlockFetchSerialised.NodeToNode.Send.Block](#cardanonodeblockfetchserialisednodetonodesendblock)
1. [Cardano.Node.BlockFetchSerialised.NodeToNode.Send.ClientDone](#cardanonodeblockfetchserialisednodetonodesendclientdone)
1. [Cardano.Node.BlockFetchSerialised.NodeToNode.Send.NoBlocks](#cardanonodeblockfetchserialisednodetonodesendnoblocks)
1. [Cardano.Node.BlockFetchSerialised.NodeToNode.Send.RequestRange](#cardanonodeblockfetchserialisednodetonodesendrequestrange)
1. [Cardano.Node.BlockFetchSerialised.NodeToNode.Send.StartBatch](#cardanonodeblockfetchserialisednodetonodesendstartbatch)
1. [Cardano.Node.BlockFetchServer.SendBlock](#cardanonodeblockfetchserversendblock)
1. [Cardano.Node.BlockchainTime.CurrentSlotUnknown](#cardanonodeblockchaintimecurrentslotunknown)
1. [Cardano.Node.BlockchainTime.StartTimeInTheFuture](#cardanonodeblockchaintimestarttimeinthefuture)
1. [Cardano.Node.BlockchainTime.SystemClockMovedBack](#cardanonodeblockchaintimesystemclockmovedback)
1. [Cardano.Node.ChainDB.AddBlockEvent.AddBlockValidation.CandidateContainsFutureBlocks](#cardanonodechaindbaddblockeventaddblockvalidationcandidatecontainsfutureblocks)
1. [Cardano.Node.ChainDB.AddBlockEvent.AddBlockValidation.CandidateContainsFutureBlocksExceedingClockSkew](#cardanonodechaindbaddblockeventaddblockvalidationcandidatecontainsfutureblocksexceedingclockskew)
1. [Cardano.Node.ChainDB.AddBlockEvent.AddBlockValidation.InvalidBlock](#cardanonodechaindbaddblockeventaddblockvalidationinvalidblock)
1. [Cardano.Node.ChainDB.AddBlockEvent.AddBlockValidation.ValidCandidate](#cardanonodechaindbaddblockeventaddblockvalidationvalidcandidate)
1. [Cardano.Node.ChainDB.AddBlockEvent.AddedBlockToQueue](#cardanonodechaindbaddblockeventaddedblocktoqueue)
1. [Cardano.Node.ChainDB.AddBlockEvent.AddedBlockToVolatileDB](#cardanonodechaindbaddblockeventaddedblocktovolatiledb)
1. [Cardano.Node.ChainDB.AddBlockEvent.AddedToCurrentChain](#cardanonodechaindbaddblockeventaddedtocurrentchain)
1. [Cardano.Node.ChainDB.AddBlockEvent.BlockInTheFuture](#cardanonodechaindbaddblockeventblockinthefuture)
1. [Cardano.Node.ChainDB.AddBlockEvent.ChainSelectionForFutureBlock](#cardanonodechaindbaddblockeventchainselectionforfutureblock)
1. [Cardano.Node.ChainDB.AddBlockEvent.IgnoreBlockAlreadyInVolatileDB](#cardanonodechaindbaddblockeventignoreblockalreadyinvolatiledb)
1. [Cardano.Node.ChainDB.AddBlockEvent.IgnoreBlockAlreadyInVolatileDB](#cardanonodechaindbaddblockeventignoreblockalreadyinvolatiledb)
1. [Cardano.Node.ChainDB.AddBlockEvent.IgnoreBlockOlderThanK](#cardanonodechaindbaddblockeventignoreblockolderthank)
1. [Cardano.Node.ChainDB.AddBlockEvent.StoreButDontChange](#cardanonodechaindbaddblockeventstorebutdontchange)
1. [Cardano.Node.ChainDB.AddBlockEvent.SwitchedToAFork](#cardanonodechaindbaddblockeventswitchedtoafork)
1. [Cardano.Node.ChainDB.AddBlockEvent.TryAddToCurrentChain](#cardanonodechaindbaddblockeventtryaddtocurrentchain)
1. [Cardano.Node.ChainDB.AddBlockEvent.TrySwitchToAFork](#cardanonodechaindbaddblockeventtryswitchtoafork)
1. [Cardano.Node.ChainDB.CopyToImmutableDBEvent.CopiedBlockToImmutableDB](#cardanonodechaindbcopytoimmutabledbeventcopiedblocktoimmutabledb)
1. [Cardano.Node.ChainDB.CopyToImmutableDBEvent.NoBlocksToCopyToImmutableDB](#cardanonodechaindbcopytoimmutabledbeventnoblockstocopytoimmutabledb)
1. [Cardano.Node.ChainDB.FollowerEvent.FollowerNewImmIterator](#cardanonodechaindbfollowereventfollowernewimmiterator)
1. [Cardano.Node.ChainDB.FollowerEvent.FollowerNoLongerInMem](#cardanonodechaindbfollowereventfollowernolongerinmem)
1. [Cardano.Node.ChainDB.FollowerEvent.FollowerSwitchToMem](#cardanonodechaindbfollowereventfollowerswitchtomem)
1. [Cardano.Node.ChainDB.FollowerEvent.NewFollower](#cardanonodechaindbfollowereventnewfollower)
1. [Cardano.Node.ChainDB.GCEvent.PerformedGC](#cardanonodechaindbgceventperformedgc)
1. [Cardano.Node.ChainDB.GCEvent.ScheduledGC](#cardanonodechaindbgceventscheduledgc)
1. [Cardano.Node.ChainDB.ImmDbEvent.CacheEvent.CurrentChunkHit](#cardanonodechaindbimmdbeventcacheeventcurrentchunkhit)
1. [Cardano.Node.ChainDB.ImmDbEvent.CacheEvent.PastChunkEvict](#cardanonodechaindbimmdbeventcacheeventpastchunkevict)
1. [Cardano.Node.ChainDB.ImmDbEvent.CacheEvent.PastChunkExpired](#cardanonodechaindbimmdbeventcacheeventpastchunkexpired)
1. [Cardano.Node.ChainDB.ImmDbEvent.CacheEvent.PastChunkHit](#cardanonodechaindbimmdbeventcacheeventpastchunkhit)
1. [Cardano.Node.ChainDB.ImmDbEvent.CacheEvent.PastChunkMiss](#cardanonodechaindbimmdbeventcacheeventpastchunkmiss)
1. [Cardano.Node.ChainDB.ImmDbEvent.ChunkFileDoesntFit](#cardanonodechaindbimmdbeventchunkfiledoesntfit)
1. [Cardano.Node.ChainDB.ImmDbEvent.ChunkValidation.InvalidChunkFile](#cardanonodechaindbimmdbeventchunkvalidationinvalidchunkfile)
1. [Cardano.Node.ChainDB.ImmDbEvent.ChunkValidation.InvalidPrimaryIndex](#cardanonodechaindbimmdbeventchunkvalidationinvalidprimaryindex)
1. [Cardano.Node.ChainDB.ImmDbEvent.ChunkValidation.InvalidSecondaryIndex](#cardanonodechaindbimmdbeventchunkvalidationinvalidsecondaryindex)
1. [Cardano.Node.ChainDB.ImmDbEvent.ChunkValidation.MissingChunkFile](#cardanonodechaindbimmdbeventchunkvalidationmissingchunkfile)
1. [Cardano.Node.ChainDB.ImmDbEvent.ChunkValidation.MissingPrimaryIndex](#cardanonodechaindbimmdbeventchunkvalidationmissingprimaryindex)
1. [Cardano.Node.ChainDB.ImmDbEvent.ChunkValidation.MissingSecondaryIndex](#cardanonodechaindbimmdbeventchunkvalidationmissingsecondaryindex)
1. [Cardano.Node.ChainDB.ImmDbEvent.DBAlreadyClosed](#cardanonodechaindbimmdbeventdbalreadyclosed)
1. [Cardano.Node.ChainDB.ImmDbEvent.DBClosed](#cardanonodechaindbimmdbeventdbclosed)
1. [Cardano.Node.ChainDB.ImmDbEvent.DeletingAfter](#cardanonodechaindbimmdbeventdeletingafter)
1. [Cardano.Node.ChainDB.ImmDbEvent.Migrating](#cardanonodechaindbimmdbeventmigrating)
1. [Cardano.Node.ChainDB.ImmDbEvent.NoValidLastLocation](#cardanonodechaindbimmdbeventnovalidlastlocation)
1. [Cardano.Node.ChainDB.ImmDbEvent.ValidatedLastLocation](#cardanonodechaindbimmdbeventvalidatedlastlocation)
1. [Cardano.Node.ChainDB.InitChainSelEvent.CandidateContainsFutureBlocks](#cardanonodechaindbinitchainseleventcandidatecontainsfutureblocks)
1. [Cardano.Node.ChainDB.InitChainSelEvent.CandidateContainsFutureBlocksExceedingClockSkew](#cardanonodechaindbinitchainseleventcandidatecontainsfutureblocksexceedingclockskew)
1. [Cardano.Node.ChainDB.InitChainSelEvent.InvalidBlock](#cardanonodechaindbinitchainseleventinvalidblock)
1. [Cardano.Node.ChainDB.InitChainSelEvent.ValidCandidate](#cardanonodechaindbinitchainseleventvalidcandidate)
1. [Cardano.Node.ChainDB.IteratorEvent.BlockGCedFromVolatileDB](#cardanonodechaindbiteratoreventblockgcedfromvolatiledb)
1. [Cardano.Node.ChainDB.IteratorEvent.BlockMissingFromVolatileDB](#cardanonodechaindbiteratoreventblockmissingfromvolatiledb)
1. [Cardano.Node.ChainDB.IteratorEvent.BlockWasCopiedToImmutableDB](#cardanonodechaindbiteratoreventblockwascopiedtoimmutabledb)
1. [Cardano.Node.ChainDB.IteratorEvent.StreamFromBoth](#cardanonodechaindbiteratoreventstreamfromboth)
1. [Cardano.Node.ChainDB.IteratorEvent.StreamFromImmutableDB](#cardanonodechaindbiteratoreventstreamfromimmutabledb)
1. [Cardano.Node.ChainDB.IteratorEvent.StreamFromVolatileDB](#cardanonodechaindbiteratoreventstreamfromvolatiledb)
1. [Cardano.Node.ChainDB.IteratorEvent.SwitchBackToVolatileDB](#cardanonodechaindbiteratoreventswitchbacktovolatiledb)
1. [Cardano.Node.ChainDB.IteratorEvent.UnknownRangeRequested](#cardanonodechaindbiteratoreventunknownrangerequested)
1. [Cardano.Node.ChainDB.LedgerEvent.DeletedSnapshot](#cardanonodechaindbledgereventdeletedsnapshot)
1. [Cardano.Node.ChainDB.LedgerEvent.InvalidSnapshot](#cardanonodechaindbledgereventinvalidsnapshot)
1. [Cardano.Node.ChainDB.LedgerEvent.ReplayFromGenesis](#cardanonodechaindbledgereventreplayfromgenesis)
1. [Cardano.Node.ChainDB.LedgerEvent.ReplayFromSnapshot](#cardanonodechaindbledgereventreplayfromsnapshot)
1. [Cardano.Node.ChainDB.LedgerEvent.ReplayedBlock](#cardanonodechaindbledgereventreplayedblock)
1. [Cardano.Node.ChainDB.LedgerEvent.TookSnapshot](#cardanonodechaindbledgereventtooksnapshot)
1. [Cardano.Node.ChainDB.OpenEvent.ClosedDB](#cardanonodechaindbopeneventcloseddb)
1. [Cardano.Node.ChainDB.OpenEvent.OpenedDB](#cardanonodechaindbopeneventopeneddb)
1. [Cardano.Node.ChainDB.OpenEvent.OpenedImmutableDB](#cardanonodechaindbopeneventopenedimmutabledb)
1. [Cardano.Node.ChainDB.OpenEvent.OpenedLgrDB](#cardanonodechaindbopeneventopenedlgrdb)
1. [Cardano.Node.ChainDB.OpenEvent.OpenedVolatileDB](#cardanonodechaindbopeneventopenedvolatiledb)
1. [Cardano.Node.ChainDB.VolatileDbEvent.BlockAlreadyHere](#cardanonodechaindbvolatiledbeventblockalreadyhere)
1. [Cardano.Node.ChainDB.VolatileDbEvent.DBAlreadyClosed](#cardanonodechaindbvolatiledbeventdbalreadyclosed)
1. [Cardano.Node.ChainDB.VolatileDbEvent.InvalidFileNames](#cardanonodechaindbvolatiledbeventinvalidfilenames)
1. [Cardano.Node.ChainDB.VolatileDbEvent.Truncate](#cardanonodechaindbvolatiledbeventtruncate)
1. [Cardano.Node.ChainSyncClient.ChainSyncClientEvent.DownloadedHeader](#cardanonodechainsyncclientchainsyncclientevent.downloadedheader)
1. [Cardano.Node.ChainSyncClient.ChainSyncClientEvent.Exception](#cardanonodechainsyncclientchainsyncclientevent.exception)
1. [Cardano.Node.ChainSyncClient.ChainSyncClientEvent.FoundIntersection](#cardanonodechainsyncclientchainsyncclientevent.foundintersection)
1. [Cardano.Node.ChainSyncClient.ChainSyncClientEvent.RolledBack](#cardanonodechainsyncclientchainsyncclientevent.rolledback)
1. [Cardano.Node.ChainSyncClient.ChainSyncClientEvent.Termination](#cardanonodechainsyncclientchainsyncclientevent.termination)
1. [Cardano.Node.ChainSyncClient.NodeToClient.Send.AwaitReply](#cardanonodechainsyncclientnodetoclientsendawaitreply)
1. [Cardano.Node.ChainSyncClient.NodeToClient.Send.AwaitReply](#cardanonodechainsyncclientnodetoclientsendawaitreply)
1. [Cardano.Node.ChainSyncClient.NodeToClient.Send.Done](#cardanonodechainsyncclientnodetoclientsenddone)
1. [Cardano.Node.ChainSyncClient.NodeToClient.Send.FindIntersect](#cardanonodechainsyncclientnodetoclientsendfindintersect)
1. [Cardano.Node.ChainSyncClient.NodeToClient.Send.IntersectFound](#cardanonodechainsyncclientnodetoclientsendintersectfound)
1. [Cardano.Node.ChainSyncClient.NodeToClient.Send.IntersectNotFound](#cardanonodechainsyncclientnodetoclientsendintersectnotfound)
1. [Cardano.Node.ChainSyncClient.NodeToClient.Send.RequestNext](#cardanonodechainsyncclientnodetoclientsendrequestnext)
1. [Cardano.Node.ChainSyncClient.NodeToClient.Send.RollBackward](#cardanonodechainsyncclientnodetoclientsendrollbackward)
1. [Cardano.Node.ChainSyncClient.NodeToClient.Send.RollForward](#cardanonodechainsyncclientnodetoclientsendrollforward)
1. [Cardano.Node.ChainSyncNode.NodeToNode.Send.AwaitReply](#cardanonodechainsyncnodenodetonodesendawaitreply)
1. [Cardano.Node.ChainSyncNode.NodeToNode.Send.AwaitReply](#cardanonodechainsyncnodenodetonodesendawaitreply)
1. [Cardano.Node.ChainSyncNode.NodeToNode.Send.Done](#cardanonodechainsyncnodenodetonodesenddone)
1. [Cardano.Node.ChainSyncNode.NodeToNode.Send.FindIntersect](#cardanonodechainsyncnodenodetonodesendfindintersect)
1. [Cardano.Node.ChainSyncNode.NodeToNode.Send.IntersectFound](#cardanonodechainsyncnodenodetonodesendintersectfound)
1. [Cardano.Node.ChainSyncNode.NodeToNode.Send.IntersectNotFound](#cardanonodechainsyncnodenodetonodesendintersectnotfound)
1. [Cardano.Node.ChainSyncNode.NodeToNode.Send.RequestNext](#cardanonodechainsyncnodenodetonodesendrequestnext)
1. [Cardano.Node.ChainSyncNode.NodeToNode.Send.RollBackward](#cardanonodechainsyncnodenodetonodesendrollbackward)
1. [Cardano.Node.ChainSyncNode.NodeToNode.Send.RollForward](#cardanonodechainsyncnodenodetonodesendrollforward)
1. [Cardano.Node.ChainSyncSerialised.NodeToNode.Send.AwaitReply](#cardanonodechainsyncserialisednodetonodesendawaitreply)
1. [Cardano.Node.ChainSyncSerialised.NodeToNode.Send.AwaitReply](#cardanonodechainsyncserialisednodetonodesendawaitreply)
1. [Cardano.Node.ChainSyncSerialised.NodeToNode.Send.Done](#cardanonodechainsyncserialisednodetonodesenddone)
1. [Cardano.Node.ChainSyncSerialised.NodeToNode.Send.FindIntersect](#cardanonodechainsyncserialisednodetonodesendfindintersect)
1. [Cardano.Node.ChainSyncSerialised.NodeToNode.Send.IntersectFound](#cardanonodechainsyncserialisednodetonodesendintersectfound)
1. [Cardano.Node.ChainSyncSerialised.NodeToNode.Send.IntersectNotFound](#cardanonodechainsyncserialisednodetonodesendintersectnotfound)
1. [Cardano.Node.ChainSyncSerialised.NodeToNode.Send.RequestNext](#cardanonodechainsyncserialisednodetonodesendrequestnext)
1. [Cardano.Node.ChainSyncSerialised.NodeToNode.Send.RollBackward](#cardanonodechainsyncserialisednodetonodesendrollbackward)
1. [Cardano.Node.ChainSyncSerialised.NodeToNode.Send.RollForward](#cardanonodechainsyncserialisednodetonodesendrollforward)
1. [Cardano.Node.ChainSyncServerBlock.ChainSyncServerEvent.RollBackward](#cardanonodechainsyncserverblockchainsyncserverevent.rollbackward)
1. [Cardano.Node.ChainSyncServerBlock.ChainSyncServerEvent.RollForward](#cardanonodechainsyncserverblockchainsyncserverevent.rollforward)
1. [Cardano.Node.ChainSyncServerBlock.ChainSyncServerEvent.ServerRead](#cardanonodechainsyncserverblockchainsyncserverevent.serverread)
1. [Cardano.Node.ChainSyncServerBlock.ChainSyncServerEvent.ServerReadBlocked](#cardanonodechainsyncserverblockchainsyncserverevent.serverreadblocked)
1. [Cardano.Node.ChainSyncServerHeader.ChainSyncServerEvent.RollBackward](#cardanonodechainsyncserverheaderchainsyncserverevent.rollbackward)
1. [Cardano.Node.ChainSyncServerHeader.ChainSyncServerEvent.RollForward](#cardanonodechainsyncserverheaderchainsyncserverevent.rollforward)
1. [Cardano.Node.ChainSyncServerHeader.ChainSyncServerEvent.ServerRead](#cardanonodechainsyncserverheaderchainsyncserverevent.serverread)
1. [Cardano.Node.ChainSyncServerHeader.ChainSyncServerEvent.ServerReadBlocked](#cardanonodechainsyncserverheaderchainsyncserverevent.serverreadblocked)
1. [Cardano.Node.ConnectionManager.Connect](#cardanonodeconnectionmanagerconnect)
1. [Cardano.Node.ConnectionManager.ConnectError](#cardanonodeconnectionmanagerconnecterror)
1. [Cardano.Node.ConnectionManager.ConnectionCleanup](#cardanonodeconnectionmanagerconnectioncleanup)
1. [Cardano.Node.ConnectionManager.ConnectionExists](#cardanonodeconnectionmanagerconnectionexists)
1. [Cardano.Node.ConnectionManager.ConnectionFailure](#cardanonodeconnectionmanagerconnectionfailure)
1. [Cardano.Node.ConnectionManager.ConnectionHandler](#cardanonodeconnectionmanagerconnectionhandler)
1. [Cardano.Node.ConnectionManager.ConnectionManagerCounters](#cardanonodeconnectionmanagerconnectionmanagercounters)
1. [Cardano.Node.ConnectionManager.ConnectionNotFound](#cardanonodeconnectionmanagerconnectionnotfound)
1. [Cardano.Node.ConnectionManager.ConnectionTimeWait](#cardanonodeconnectionmanagerconnectiontimewait)
1. [Cardano.Node.ConnectionManager.ConnectionTimeWaitDone](#cardanonodeconnectionmanagerconnectiontimewaitdone)
1. [Cardano.Node.ConnectionManager.ForbiddenConnection](#cardanonodeconnectionmanagerforbiddenconnection)
1. [Cardano.Node.ConnectionManager.ForbiddenOperation](#cardanonodeconnectionmanagerforbiddenoperation)
1. [Cardano.Node.ConnectionManager.ImpossibleConnection](#cardanonodeconnectionmanagerimpossibleconnection)
1. [Cardano.Node.ConnectionManager.IncludeConnection](#cardanonodeconnectionmanagerincludeconnection)
1. [Cardano.Node.ConnectionManager.PruneConnections](#cardanonodeconnectionmanagerpruneconnections)
1. [Cardano.Node.ConnectionManager.Shutdown](#cardanonodeconnectionmanagershutdown)
1. [Cardano.Node.ConnectionManager.State](#cardanonodeconnectionmanagerstate)
1. [Cardano.Node.ConnectionManager.TerminatedConnection](#cardanonodeconnectionmanagerterminatedconnection)
1. [Cardano.Node.ConnectionManager.TerminatingConnection](#cardanonodeconnectionmanagerterminatingconnection)
1. [Cardano.Node.ConnectionManager.UnexpectedlyFalseAssertion](#cardanonodeconnectionmanagerunexpectedlyfalseassertion)
1. [Cardano.Node.ConnectionManager.UnregisterConnection](#cardanonodeconnectionmanagerunregisterconnection)
1. [Cardano.Node.DNSResolver.LookupAAAAError](#cardanonodednsresolverlookupaaaaerror)
1. [Cardano.Node.DNSResolver.LookupAAAAResult](#cardanonodednsresolverlookupaaaaresult)
1. [Cardano.Node.DNSResolver.LookupAError](#cardanonodednsresolverlookupaerror)
1. [Cardano.Node.DNSResolver.LookupAResult](#cardanonodednsresolverlookuparesult)
1. [Cardano.Node.DNSResolver.LookupException](#cardanonodednsresolverlookupexception)
1. [Cardano.Node.DNSResolver.LookupIPv4First](#cardanonodednsresolverlookupipv4first)
1. [Cardano.Node.DNSResolver.LookupIPv6First](#cardanonodednsresolverlookupipv6first)
1. [Cardano.Node.DNSResolver.LookupIPv6First](#cardanonodednsresolverlookupipv6first)
1. [Cardano.Node.DNSSubscription.DNS.AllocateSocket](#cardanonodednssubscriptiondnsallocatesocket)
1. [Cardano.Node.DNSSubscription.DNS.ApplicationException](#cardanonodednssubscriptiondnsapplicationexception)
1. [Cardano.Node.DNSSubscription.DNS.CloseSocket](#cardanonodednssubscriptiondnsclosesocket)
1. [Cardano.Node.DNSSubscription.DNS.ConnectEnd](#cardanonodednssubscriptiondnsconnectend)
1. [Cardano.Node.DNSSubscription.DNS.ConnectException](#cardanonodednssubscriptiondnsconnectexception)
1. [Cardano.Node.DNSSubscription.DNS.ConnectStart](#cardanonodednssubscriptiondnsconnectstart)
1. [Cardano.Node.DNSSubscription.DNS.ConnectionExist](#cardanonodednssubscriptiondnsconnectionexist)
1. [Cardano.Node.DNSSubscription.DNS.MissingLocalAddress](#cardanonodednssubscriptiondnsmissinglocaladdress)
1. [Cardano.Node.DNSSubscription.DNS.Restart](#cardanonodednssubscriptiondnsrestart)
1. [Cardano.Node.DNSSubscription.DNS.SkippingPeer](#cardanonodednssubscriptiondnsskippingpeer)
1. [Cardano.Node.DNSSubscription.DNS.SocketAllocationException](#cardanonodednssubscriptiondnssocketallocationexception)
1. [Cardano.Node.DNSSubscription.DNS.Start](#cardanonodednssubscriptiondnsstart)
1. [Cardano.Node.DNSSubscription.DNS.SubscriptionFailed](#cardanonodednssubscriptiondnssubscriptionfailed)
1. [Cardano.Node.DNSSubscription.DNS.SubscriptionRunning](#cardanonodednssubscriptiondnssubscriptionrunning)
1. [Cardano.Node.DNSSubscription.DNS.SubscriptionWaiting](#cardanonodednssubscriptiondnssubscriptionwaiting)
1. [Cardano.Node.DNSSubscription.DNS.SubscriptionWaitingNewConnection](#cardanonodednssubscriptiondnssubscriptionwaitingnewconnection)
1. [Cardano.Node.DNSSubscription.DNS.TryConnectToPeer](#cardanonodednssubscriptiondnstryconnecttopeer)
1. [Cardano.Node.DNSSubscription.DNS.UnsupportedRemoteAddr](#cardanonodednssubscriptiondnsunsupportedremoteaddr)
1. [Cardano.Node.DebugPeerSelection.GovernorState](#cardanonodedebugpeerselectiongovernorstate)
1. [Cardano.Node.DebugPeerSelectionResponder.GovernorState](#cardanonodedebugpeerselectionrespondergovernorstate)
1. [Cardano.Node.DiffusionInit.ConfiguringLocalSocket](#cardanonodediffusioninitconfiguringlocalsocket)
1. [Cardano.Node.DiffusionInit.ConfiguringServerSocket](#cardanonodediffusioninitconfiguringserversocket)
1. [Cardano.Node.DiffusionInit.CreateSystemdSocketForSnocketPath](#cardanonodediffusioninitcreatesystemdsocketforsnocketpath)
1. [Cardano.Node.DiffusionInit.CreatedLocalSocket](#cardanonodediffusioninitcreatedlocalsocket)
1. [Cardano.Node.DiffusionInit.CreatingServerSocket](#cardanonodediffusioninitcreatingserversocket)
1. [Cardano.Node.DiffusionInit.DiffusionErrored](#cardanonodediffusioninitdiffusionerrored)
1. [Cardano.Node.DiffusionInit.ListeningLocalSocket](#cardanonodediffusioninitlisteninglocalsocket)
1. [Cardano.Node.DiffusionInit.ListeningServerSocket](#cardanonodediffusioninitlisteningserversocket)
1. [Cardano.Node.DiffusionInit.LocalSocketUp](#cardanonodediffusioninitlocalsocketup)
1. [Cardano.Node.DiffusionInit.RunLocalServer](#cardanonodediffusioninitrunlocalserver)
1. [Cardano.Node.DiffusionInit.RunServer](#cardanonodediffusioninitrunserver)
1. [Cardano.Node.DiffusionInit.ServerSocketUp](#cardanonodediffusioninitserversocketup)
1. [Cardano.Node.DiffusionInit.UnsupportedLocalSystemdSocket](#cardanonodediffusioninitunsupportedlocalsystemdsocket)
1. [Cardano.Node.DiffusionInit.UnsupportedReadySocketCase](#cardanonodediffusioninitunsupportedreadysocketcase)
1. [Cardano.Node.DiffusionInit.UsingSystemdSocket](#cardanonodediffusioninitusingsystemdsocket)
1. [Cardano.Node.ErrorPolicy.AcceptException](#cardanonodeerrorpolicyacceptexception)
1. [Cardano.Node.ErrorPolicy.KeepSuspended](#cardanonodeerrorpolicykeepsuspended)
1. [Cardano.Node.ErrorPolicy.LocalNodeError](#cardanonodeerrorpolicylocalnodeerror)
1. [Cardano.Node.ErrorPolicy.ResumeConsumer](#cardanonodeerrorpolicyresumeconsumer)
1. [Cardano.Node.ErrorPolicy.ResumePeer](#cardanonodeerrorpolicyresumepeer)
1. [Cardano.Node.ErrorPolicy.ResumeProducer](#cardanonodeerrorpolicyresumeproducer)
1. [Cardano.Node.ErrorPolicy.SuspendConsumer](#cardanonodeerrorpolicysuspendconsumer)
1. [Cardano.Node.ErrorPolicy.SuspendPeer](#cardanonodeerrorpolicysuspendpeer)
1. [Cardano.Node.ErrorPolicy.UnhandledApplicationException](#cardanonodeerrorpolicyunhandledapplicationexception)
1. [Cardano.Node.ErrorPolicy.UnhandledConnectionException](#cardanonodeerrorpolicyunhandledconnectionexception)
1. [Cardano.Node.Forge.AdoptedBlock](#cardanonodeforgeadoptedblock)
1. [Cardano.Node.Forge.BlockContext](#cardanonodeforgeblockcontext)
1. [Cardano.Node.Forge.BlockFromFuture](#cardanonodeforgeblockfromfuture)
1. [Cardano.Node.Forge.DidntAdoptBlock](#cardanonodeforgedidntadoptblock)
1. [Cardano.Node.Forge.ForgeStateUpdateError](#cardanonodeforgeforgestateupdateerror)
1. [Cardano.Node.Forge.ForgedBlock](#cardanonodeforgeforgedblock)
1. [Cardano.Node.Forge.ForgedInvalidBlock](#cardanonodeforgeforgedinvalidblock)
1. [Cardano.Node.Forge.LedgerState](#cardanonodeforgeledgerstate)
1. [Cardano.Node.Forge.LedgerView](#cardanonodeforgeledgerview)
1. [Cardano.Node.Forge.NoLedgerState](#cardanonodeforgenoledgerstate)
1. [Cardano.Node.Forge.NoLedgerView](#cardanonodeforgenoledgerview)
1. [Cardano.Node.Forge.NodeCannotForge](#cardanonodeforgenodecannotforge)
1. [Cardano.Node.Forge.NodeIsLeader](#cardanonodeforgenodeisleader)
1. [Cardano.Node.Forge.NodeNotLeader](#cardanonodeforgenodenotleader)
1. [Cardano.Node.Forge.SlotIsImmutable](#cardanonodeforgeslotisimmutable)
1. [Cardano.Node.Forge.StartLeadershipCheck](#cardanonodeforgestartleadershipcheck)
1. [Cardano.Node.Forge.StartLeadershipCheckPlus](#cardanonodeforgestartleadershipcheckplus)
1. [Cardano.Node.ForgeStateInfo](#cardanonodeforgestateinfo)
1. [Cardano.Node.Handshake.AcceptVersion](#cardanonodehandshakeacceptversion)
1. [Cardano.Node.Handshake.ProposeVersions](#cardanonodehandshakeproposeversions)
1. [Cardano.Node.Handshake.Refuse](#cardanonodehandshakerefuse)
1. [Cardano.Node.InboundGovernor.DemotedToColdRemote](#cardanonodeinboundgovernordemotedtocoldremote)
1. [Cardano.Node.InboundGovernor.DemotedToWarmRemote](#cardanonodeinboundgovernordemotedtowarmremote)
1. [Cardano.Node.InboundGovernor.InboundGovernorCounters](#cardanonodeinboundgovernorinboundgovernorcounters)
1. [Cardano.Node.InboundGovernor.MuxCleanExit](#cardanonodeinboundgovernormuxcleanexit)
1. [Cardano.Node.InboundGovernor.MuxErrored](#cardanonodeinboundgovernormuxerrored)
1. [Cardano.Node.InboundGovernor.NewConnection](#cardanonodeinboundgovernornewconnection)
1. [Cardano.Node.InboundGovernor.PromotedToHotRemote](#cardanonodeinboundgovernorpromotedtohotremote)
1. [Cardano.Node.InboundGovernor.PromotedToWarmRemote](#cardanonodeinboundgovernorpromotedtowarmremote)
1. [Cardano.Node.InboundGovernor.RemoteState](#cardanonodeinboundgovernorremotestate)
1. [Cardano.Node.InboundGovernor.ResponderErrored](#cardanonodeinboundgovernorrespondererrored)
1. [Cardano.Node.InboundGovernor.ResponderRestarted](#cardanonodeinboundgovernorresponderrestarted)
1. [Cardano.Node.InboundGovernor.ResponderStartFailure](#cardanonodeinboundgovernorresponderstartfailure)
1. [Cardano.Node.InboundGovernor.ResponderStarted](#cardanonodeinboundgovernorresponderstarted)
1. [Cardano.Node.InboundGovernor.ResponderTerminated](#cardanonodeinboundgovernorresponderterminated)
1. [Cardano.Node.InboundGovernor.WaitIdleRemote](#cardanonodeinboundgovernorwaitidleremote)
1. [Cardano.Node.IpSubscription.IP.AllocateSocket](#cardanonodeipsubscriptionipallocatesocket)
1. [Cardano.Node.IpSubscription.IP.ApplicationException](#cardanonodeipsubscriptionipapplicationexception)
1. [Cardano.Node.IpSubscription.IP.CloseSocket](#cardanonodeipsubscriptionipclosesocket)
1. [Cardano.Node.IpSubscription.IP.ConnectEnd](#cardanonodeipsubscriptionipconnectend)
1. [Cardano.Node.IpSubscription.IP.ConnectException](#cardanonodeipsubscriptionipconnectexception)
1. [Cardano.Node.IpSubscription.IP.ConnectStart](#cardanonodeipsubscriptionipconnectstart)
1. [Cardano.Node.IpSubscription.IP.ConnectionExist](#cardanonodeipsubscriptionipconnectionexist)
1. [Cardano.Node.IpSubscription.IP.MissingLocalAddress](#cardanonodeipsubscriptionipmissinglocaladdress)
1. [Cardano.Node.IpSubscription.IP.Restart](#cardanonodeipsubscriptioniprestart)
1. [Cardano.Node.IpSubscription.IP.SkippingPeer](#cardanonodeipsubscriptionipskippingpeer)
1. [Cardano.Node.IpSubscription.IP.SocketAllocationException](#cardanonodeipsubscriptionipsocketallocationexception)
1. [Cardano.Node.IpSubscription.IP.Start](#cardanonodeipsubscriptionipstart)
1. [Cardano.Node.IpSubscription.IP.SubscriptionFailed](#cardanonodeipsubscriptionipsubscriptionfailed)
1. [Cardano.Node.IpSubscription.IP.SubscriptionRunning](#cardanonodeipsubscriptionipsubscriptionrunning)
1. [Cardano.Node.IpSubscription.IP.SubscriptionWaiting](#cardanonodeipsubscriptionipsubscriptionwaiting)
1. [Cardano.Node.IpSubscription.IP.SubscriptionWaitingNewConnection](#cardanonodeipsubscriptionipsubscriptionwaitingnewconnection)
1. [Cardano.Node.IpSubscription.IP.TryConnectToPeer](#cardanonodeipsubscriptioniptryconnecttopeer)
1. [Cardano.Node.IpSubscription.IP.UnsupportedRemoteAddr](#cardanonodeipsubscriptionipunsupportedremoteaddr)
1. [Cardano.Node.KeepAliveClient.KeepAliveClient](#cardanonodekeepaliveclientkeepaliveclient)
1. [Cardano.Node.LedgerPeers.DisabledLedgerPeers](#cardanonodeledgerpeersdisabledledgerpeers)
1. [Cardano.Node.LedgerPeers.DisabledLedgerPeers](#cardanonodeledgerpeersdisabledledgerpeers)
1. [Cardano.Node.LedgerPeers.FallingBackToBootstrapPeers](#cardanonodeledgerpeersfallingbacktobootstrappeers)
1. [Cardano.Node.LedgerPeers.FetchingNewLedgerState](#cardanonodeledgerpeersfetchingnewledgerstate)
1. [Cardano.Node.LedgerPeers.PickedPeer](#cardanonodeledgerpeerspickedpeer)
1. [Cardano.Node.LedgerPeers.PickedPeers](#cardanonodeledgerpeerspickedpeers)
1. [Cardano.Node.LedgerPeers.ReusingLedgerState](#cardanonodeledgerpeersreusingledgerstate)
1. [Cardano.Node.LedgerPeers.TraceUseLedgerAfter](#cardanonodeledgerpeerstraceuseledgerafter)
1. [Cardano.Node.LedgerPeers.WaitingOnRequest](#cardanonodeledgerpeerswaitingonrequest)
1. [Cardano.Node.LedgerPeers.WaitingOnRequest](#cardanonodeledgerpeerswaitingonrequest)
1. [Cardano.Node.LocalConnectionManager.Connect](#cardanonodelocalconnectionmanagerconnect)
1. [Cardano.Node.LocalConnectionManager.ConnectError](#cardanonodelocalconnectionmanagerconnecterror)
1. [Cardano.Node.LocalConnectionManager.ConnectionCleanup](#cardanonodelocalconnectionmanagerconnectioncleanup)
1. [Cardano.Node.LocalConnectionManager.ConnectionExists](#cardanonodelocalconnectionmanagerconnectionexists)
1. [Cardano.Node.LocalConnectionManager.ConnectionFailure](#cardanonodelocalconnectionmanagerconnectionfailure)
1. [Cardano.Node.LocalConnectionManager.ConnectionHandler](#cardanonodelocalconnectionmanagerconnectionhandler)
1. [Cardano.Node.LocalConnectionManager.ConnectionManagerCounters](#cardanonodelocalconnectionmanagerconnectionmanagercounters)
1. [Cardano.Node.LocalConnectionManager.ConnectionNotFound](#cardanonodelocalconnectionmanagerconnectionnotfound)
1. [Cardano.Node.LocalConnectionManager.ConnectionTimeWait](#cardanonodelocalconnectionmanagerconnectiontimewait)
1. [Cardano.Node.LocalConnectionManager.ConnectionTimeWaitDone](#cardanonodelocalconnectionmanagerconnectiontimewaitdone)
1. [Cardano.Node.LocalConnectionManager.ForbiddenConnection](#cardanonodelocalconnectionmanagerforbiddenconnection)
1. [Cardano.Node.LocalConnectionManager.ForbiddenOperation](#cardanonodelocalconnectionmanagerforbiddenoperation)
1. [Cardano.Node.LocalConnectionManager.ImpossibleConnection](#cardanonodelocalconnectionmanagerimpossibleconnection)
1. [Cardano.Node.LocalConnectionManager.IncludeConnection](#cardanonodelocalconnectionmanagerincludeconnection)
1. [Cardano.Node.LocalConnectionManager.PruneConnections](#cardanonodelocalconnectionmanagerpruneconnections)
1. [Cardano.Node.LocalConnectionManager.Shutdown](#cardanonodelocalconnectionmanagershutdown)
1. [Cardano.Node.LocalConnectionManager.State](#cardanonodelocalconnectionmanagerstate)
1. [Cardano.Node.LocalConnectionManager.TerminatedConnection](#cardanonodelocalconnectionmanagerterminatedconnection)
1. [Cardano.Node.LocalConnectionManager.TerminatingConnection](#cardanonodelocalconnectionmanagerterminatingconnection)
1. [Cardano.Node.LocalConnectionManager.UnexpectedlyFalseAssertion](#cardanonodelocalconnectionmanagerunexpectedlyfalseassertion)
1. [Cardano.Node.LocalConnectionManager.UnregisterConnection](#cardanonodelocalconnectionmanagerunregisterconnection)
1. [Cardano.Node.LocalErrorPolicy.AcceptException](#cardanonodelocalerrorpolicyacceptexception)
1. [Cardano.Node.LocalErrorPolicy.KeepSuspended](#cardanonodelocalerrorpolicykeepsuspended)
1. [Cardano.Node.LocalErrorPolicy.LocalNodeError](#cardanonodelocalerrorpolicylocalnodeerror)
1. [Cardano.Node.LocalErrorPolicy.ResumeConsumer](#cardanonodelocalerrorpolicyresumeconsumer)
1. [Cardano.Node.LocalErrorPolicy.ResumePeer](#cardanonodelocalerrorpolicyresumepeer)
1. [Cardano.Node.LocalErrorPolicy.ResumeProducer](#cardanonodelocalerrorpolicyresumeproducer)
1. [Cardano.Node.LocalErrorPolicy.SuspendConsumer](#cardanonodelocalerrorpolicysuspendconsumer)
1. [Cardano.Node.LocalErrorPolicy.SuspendPeer](#cardanonodelocalerrorpolicysuspendpeer)
1. [Cardano.Node.LocalErrorPolicy.UnhandledApplicationException](#cardanonodelocalerrorpolicyunhandledapplicationexception)
1. [Cardano.Node.LocalErrorPolicy.UnhandledConnectionException](#cardanonodelocalerrorpolicyunhandledconnectionexception)
1. [Cardano.Node.LocalHandshake.AcceptVersion](#cardanonodelocalhandshakeacceptversion)
1. [Cardano.Node.LocalHandshake.ProposeVersions](#cardanonodelocalhandshakeproposeversions)
1. [Cardano.Node.LocalHandshake.Refuse](#cardanonodelocalhandshakerefuse)
1. [Cardano.Node.LocalInboundGovernor.DemotedToColdRemote](#cardanonodelocalinboundgovernordemotedtocoldremote)
1. [Cardano.Node.LocalInboundGovernor.DemotedToWarmRemote](#cardanonodelocalinboundgovernordemotedtowarmremote)
1. [Cardano.Node.LocalInboundGovernor.InboundGovernorCounters](#cardanonodelocalinboundgovernorinboundgovernorcounters)
1. [Cardano.Node.LocalInboundGovernor.MuxCleanExit](#cardanonodelocalinboundgovernormuxcleanexit)
1. [Cardano.Node.LocalInboundGovernor.MuxErrored](#cardanonodelocalinboundgovernormuxerrored)
1. [Cardano.Node.LocalInboundGovernor.NewConnection](#cardanonodelocalinboundgovernornewconnection)
1. [Cardano.Node.LocalInboundGovernor.PromotedToHotRemote](#cardanonodelocalinboundgovernorpromotedtohotremote)
1. [Cardano.Node.LocalInboundGovernor.PromotedToWarmRemote](#cardanonodelocalinboundgovernorpromotedtowarmremote)
1. [Cardano.Node.LocalInboundGovernor.RemoteState](#cardanonodelocalinboundgovernorremotestate)
1. [Cardano.Node.LocalInboundGovernor.ResponderErrored](#cardanonodelocalinboundgovernorrespondererrored)
1. [Cardano.Node.LocalInboundGovernor.ResponderRestarted](#cardanonodelocalinboundgovernorresponderrestarted)
1. [Cardano.Node.LocalInboundGovernor.ResponderStartFailure](#cardanonodelocalinboundgovernorresponderstartfailure)
1. [Cardano.Node.LocalInboundGovernor.ResponderStarted](#cardanonodelocalinboundgovernorresponderstarted)
1. [Cardano.Node.LocalInboundGovernor.ResponderTerminated](#cardanonodelocalinboundgovernorresponderterminated)
1. [Cardano.Node.LocalInboundGovernor.WaitIdleRemote](#cardanonodelocalinboundgovernorwaitidleremote)
1. [Cardano.Node.LocalRootPeers.LocalRootDomains](#cardanonodelocalrootpeerslocalrootdomains)
1. [Cardano.Node.LocalRootPeers.LocalRootError](#cardanonodelocalrootpeerslocalrooterror)
1. [Cardano.Node.LocalRootPeers.LocalRootFailure](#cardanonodelocalrootpeerslocalrootfailure)
1. [Cardano.Node.LocalRootPeers.LocalRootGroups](#cardanonodelocalrootpeerslocalrootgroups)
1. [Cardano.Node.LocalRootPeers.LocalRootResult](#cardanonodelocalrootpeerslocalrootresult)
1. [Cardano.Node.LocalRootPeers.LocalRootWaiting](#cardanonodelocalrootpeerslocalrootwaiting)
1. [Cardano.Node.LocalServer.AcceptConnection](#cardanonodelocalserveracceptconnection)
1. [Cardano.Node.LocalServer.AcceptError](#cardanonodelocalserveraccepterror)
1. [Cardano.Node.LocalServer.AcceptPolicy](#cardanonodelocalserveracceptpolicy)
1. [Cardano.Node.LocalServer.Error](#cardanonodelocalservererror)
1. [Cardano.Node.LocalServer.Started](#cardanonodelocalserverstarted)
1. [Cardano.Node.LocalServer.Stopped](#cardanonodelocalserverstopped)
1. [Cardano.Node.LocalTxSubmissionServer.ReceivedTx](#cardanonodelocaltxsubmissionserverreceivedtx)
1. [Cardano.Node.Mempool.AddedTx](#cardanonodemempooladdedtx)
1. [Cardano.Node.Mempool.ManuallyRemovedTxs](#cardanonodemempoolmanuallyremovedtxs)
1. [Cardano.Node.Mempool.RejectedTx](#cardanonodemempoolrejectedtx)
1. [Cardano.Node.Mempool.RemoveTxs](#cardanonodemempoolremovetxs)
1. [Cardano.Node.Mux.ChannelRecvEnd](#cardanonodemuxchannelrecvend)
1. [Cardano.Node.Mux.ChannelRecvStart](#cardanonodemuxchannelrecvstart)
1. [Cardano.Node.Mux.ChannelSendEnd](#cardanonodemuxchannelsendend)
1. [Cardano.Node.Mux.ChannelSendStart](#cardanonodemuxchannelsendstart)
1. [Cardano.Node.Mux.CleanExit](#cardanonodemuxcleanexit)
1. [Cardano.Node.Mux.ExceptionExit](#cardanonodemuxexceptionexit)
1. [Cardano.Node.Mux.HandshakeClientEnd](#cardanonodemuxhandshakeclientend)
1. [Cardano.Node.Mux.HandshakeClientError](#cardanonodemuxhandshakeclienterror)
1. [Cardano.Node.Mux.HandshakeServerError](#cardanonodemuxhandshakeservererror)
1. [Cardano.Node.Mux.HandshakeStart ](#cardanonodemuxhandshakestart )
1. [Cardano.Node.Mux.RecvDeltaQObservation](#cardanonodemuxrecvdeltaqobservation)
1. [Cardano.Node.Mux.RecvDeltaQSample](#cardanonodemuxrecvdeltaqsample)
1. [Cardano.Node.Mux.RecvEnd](#cardanonodemuxrecvend)
1. [Cardano.Node.Mux.RecvHeaderEnd](#cardanonodemuxrecvheaderend)
1. [Cardano.Node.Mux.RecvHeaderStart](#cardanonodemuxrecvheaderstart)
1. [Cardano.Node.Mux.RecvStart](#cardanonodemuxrecvstart)
1. [Cardano.Node.Mux.SDUReadTimeoutException](#cardanonodemuxsdureadtimeoutexception)
1. [Cardano.Node.Mux.SDUWriteTimeoutException](#cardanonodemuxsduwritetimeoutexception)
1. [Cardano.Node.Mux.SendEnd](#cardanonodemuxsendend)
1. [Cardano.Node.Mux.SendStart](#cardanonodemuxsendstart)
1. [Cardano.Node.Mux.Shutdown](#cardanonodemuxshutdown)
1. [Cardano.Node.Mux.StartEagerly](#cardanonodemuxstarteagerly)
1. [Cardano.Node.Mux.StartOnDemand](#cardanonodemuxstartondemand)
1. [Cardano.Node.Mux.StartedOnDemand](#cardanonodemuxstartedondemand)
1. [Cardano.Node.Mux.State](#cardanonodemuxstate)
1. [Cardano.Node.Mux.Terminating](#cardanonodemuxterminating)
1. [Cardano.Node.MuxLocal.ChannelRecvEnd](#cardanonodemuxlocalchannelrecvend)
1. [Cardano.Node.MuxLocal.ChannelRecvStart](#cardanonodemuxlocalchannelrecvstart)
1. [Cardano.Node.MuxLocal.ChannelSendEnd](#cardanonodemuxlocalchannelsendend)
1. [Cardano.Node.MuxLocal.ChannelSendStart](#cardanonodemuxlocalchannelsendstart)
1. [Cardano.Node.MuxLocal.CleanExit](#cardanonodemuxlocalcleanexit)
1. [Cardano.Node.MuxLocal.ExceptionExit](#cardanonodemuxlocalexceptionexit)
1. [Cardano.Node.MuxLocal.HandshakeClientEnd](#cardanonodemuxlocalhandshakeclientend)
1. [Cardano.Node.MuxLocal.HandshakeClientError](#cardanonodemuxlocalhandshakeclienterror)
1. [Cardano.Node.MuxLocal.HandshakeServerError](#cardanonodemuxlocalhandshakeservererror)
1. [Cardano.Node.MuxLocal.HandshakeStart ](#cardanonodemuxlocalhandshakestart )
1. [Cardano.Node.MuxLocal.RecvDeltaQObservation](#cardanonodemuxlocalrecvdeltaqobservation)
1. [Cardano.Node.MuxLocal.RecvDeltaQSample](#cardanonodemuxlocalrecvdeltaqsample)
1. [Cardano.Node.MuxLocal.RecvEnd](#cardanonodemuxlocalrecvend)
1. [Cardano.Node.MuxLocal.RecvHeaderEnd](#cardanonodemuxlocalrecvheaderend)
1. [Cardano.Node.MuxLocal.RecvHeaderStart](#cardanonodemuxlocalrecvheaderstart)
1. [Cardano.Node.MuxLocal.RecvStart](#cardanonodemuxlocalrecvstart)
1. [Cardano.Node.MuxLocal.SDUReadTimeoutException](#cardanonodemuxlocalsdureadtimeoutexception)
1. [Cardano.Node.MuxLocal.SDUWriteTimeoutException](#cardanonodemuxlocalsduwritetimeoutexception)
1. [Cardano.Node.MuxLocal.SendEnd](#cardanonodemuxlocalsendend)
1. [Cardano.Node.MuxLocal.SendStart](#cardanonodemuxlocalsendstart)
1. [Cardano.Node.MuxLocal.Shutdown](#cardanonodemuxlocalshutdown)
1. [Cardano.Node.MuxLocal.StartEagerly](#cardanonodemuxlocalstarteagerly)
1. [Cardano.Node.MuxLocal.StartOnDemand](#cardanonodemuxlocalstartondemand)
1. [Cardano.Node.MuxLocal.StartedOnDemand](#cardanonodemuxlocalstartedondemand)
1. [Cardano.Node.MuxLocal.State](#cardanonodemuxlocalstate)
1. [Cardano.Node.MuxLocal.Terminating](#cardanonodemuxlocalterminating)
1. [Cardano.Node.PeerSelection.ChurnMode](#cardanonodepeerselectionchurnmode)
1. [Cardano.Node.PeerSelection.ChurnWait](#cardanonodepeerselectionchurnwait)
1. [Cardano.Node.PeerSelection.DemoteAsynchronous](#cardanonodepeerselectiondemoteasynchronous)
1. [Cardano.Node.PeerSelection.DemoteHotDone](#cardanonodepeerselectiondemotehotdone)
1. [Cardano.Node.PeerSelection.DemoteHotFailed](#cardanonodepeerselectiondemotehotfailed)
1. [Cardano.Node.PeerSelection.DemoteHotFailed](#cardanonodepeerselectiondemotehotfailed)
1. [Cardano.Node.PeerSelection.DemoteHotPeers](#cardanonodepeerselectiondemotehotpeers)
1. [Cardano.Node.PeerSelection.DemoteLocalHotPeers](#cardanonodepeerselectiondemotelocalhotpeers)
1. [Cardano.Node.PeerSelection.DemoteWarmDone](#cardanonodepeerselectiondemotewarmdone)
1. [Cardano.Node.PeerSelection.DemoteWarmFailed](#cardanonodepeerselectiondemotewarmfailed)
1. [Cardano.Node.PeerSelection.DemoteWarmPeers](#cardanonodepeerselectiondemotewarmpeers)
1. [Cardano.Node.PeerSelection.ForgetColdPeers](#cardanonodepeerselectionforgetcoldpeers)
1. [Cardano.Node.PeerSelection.GossipRequests](#cardanonodepeerselectiongossiprequests)
1. [Cardano.Node.PeerSelection.GossipResults](#cardanonodepeerselectiongossipresults)
1. [Cardano.Node.PeerSelection.GovernorWakeup](#cardanonodepeerselectiongovernorwakeup)
1. [Cardano.Node.PeerSelection.LocalRootPeersChanged](#cardanonodepeerselectionlocalrootpeerschanged)
1. [Cardano.Node.PeerSelection.PromoteColdDone](#cardanonodepeerselectionpromotecolddone)
1. [Cardano.Node.PeerSelection.PromoteColdFailed](#cardanonodepeerselectionpromotecoldfailed)
1. [Cardano.Node.PeerSelection.PromoteColdLocalPeers](#cardanonodepeerselectionpromotecoldlocalpeers)
1. [Cardano.Node.PeerSelection.PromoteColdPeers](#cardanonodepeerselectionpromotecoldpeers)
1. [Cardano.Node.PeerSelection.PromoteWarmDone](#cardanonodepeerselectionpromotewarmdone)
1. [Cardano.Node.PeerSelection.PromoteWarmFailed](#cardanonodepeerselectionpromotewarmfailed)
1. [Cardano.Node.PeerSelection.PromoteWarmLocalPeers](#cardanonodepeerselectionpromotewarmlocalpeers)
1. [Cardano.Node.PeerSelection.PromoteWarmPeers](#cardanonodepeerselectionpromotewarmpeers)
1. [Cardano.Node.PeerSelection.PublicRootsResults](#cardanonodepeerselectionpublicrootsresults)
1. [Cardano.Node.PeerSelection.PublicRootsResults](#cardanonodepeerselectionpublicrootsresults)
1. [Cardano.Node.PeerSelection.TargetsChanged](#cardanonodepeerselectiontargetschanged)
1. [Cardano.Node.PeerSelection.ublicRootsRequest](#cardanonodepeerselectionublicrootsrequest)
1. [Cardano.Node.PeerSelectionActions.MonitoringError](#cardanonodepeerselectionactionsmonitoringerror)
1. [Cardano.Node.PeerSelectionActions.MonitoringResult](#cardanonodepeerselectionactionsmonitoringresult)
1. [Cardano.Node.PeerSelectionActions.StatusChangeFailure](#cardanonodepeerselectionactionsstatuschangefailure)
1. [Cardano.Node.PeerSelectionActions.StatusChanged](#cardanonodepeerselectionactionsstatuschanged)
1. [Cardano.Node.PeerSelectionCounters](#cardanonodepeerselectioncounters)
1. [Cardano.Node.Peers](#cardanonodepeers)
1. [Cardano.Node.PublicRootPeers.PublicRootDomains](#cardanonodepublicrootpeerspublicrootdomains)
1. [Cardano.Node.PublicRootPeers.PublicRootFailure](#cardanonodepublicrootpeerspublicrootfailure)
1. [Cardano.Node.PublicRootPeers.PublicRootRelayAccessPoint](#cardanonodepublicrootpeerspublicrootrelayaccesspoint)
1. [Cardano.Node.PublicRootPeers.PublicRootResult](#cardanonodepublicrootpeerspublicrootresult)
1. [Cardano.Node.ReplayBlock.LedgerReplay](#cardanonodereplayblockledgerreplay)
1. [Cardano.Node.Resources](#cardanonoderesources)
1. [Cardano.Node.Server.AcceptConnection](#cardanonodeserveracceptconnection)
1. [Cardano.Node.Server.AcceptError](#cardanonodeserveraccepterror)
1. [Cardano.Node.Server.AcceptPolicy](#cardanonodeserveracceptpolicy)
1. [Cardano.Node.Server.Error](#cardanonodeservererror)
1. [Cardano.Node.Server.Started](#cardanonodeserverstarted)
1. [Cardano.Node.Server.Stopped](#cardanonodeserverstopped)
1. [Cardano.Node.Shutdown.AbnormalShutdown](#cardanonodeshutdownabnormalshutdown)
1. [Cardano.Node.Shutdown.RequestingShutdown](#cardanonodeshutdownrequestingshutdown)
1. [Cardano.Node.Shutdown.ShutdownArmedAtSlot](#cardanonodeshutdownshutdownarmedatslot)
1. [Cardano.Node.Shutdown.ShutdownRequested](#cardanonodeshutdownshutdownrequested)
1. [Cardano.Node.Shutdown.ShutdownUnexpectedInput](#cardanonodeshutdownshutdownunexpectedinput)
1. [Cardano.Node.Startup.Byron](#cardanonodestartupbyron)
1. [Cardano.Node.Startup.Common](#cardanonodestartupcommon)
1. [Cardano.Node.Startup.Network](#cardanonodestartupnetwork)
1. [Cardano.Node.Startup.ShelleyBased](#cardanonodestartupshelleybased)
1. [Cardano.Node.StateQueryClient.Acquire](#cardanonodestatequeryclientacquire)
1. [Cardano.Node.StateQueryClient.Acquired](#cardanonodestatequeryclientacquired)
1. [Cardano.Node.StateQueryClient.Acquired](#cardanonodestatequeryclientacquired)
1. [Cardano.Node.StateQueryClient.Done](#cardanonodestatequeryclientdone)
1. [Cardano.Node.StateQueryClient.Query](#cardanonodestatequeryclientquery)
1. [Cardano.Node.StateQueryClient.ReAcquire](#cardanonodestatequeryclientreacquire)
1. [Cardano.Node.StateQueryClient.Release](#cardanonodestatequeryclientrelease)
1. [Cardano.Node.StateQueryClient.Result](#cardanonodestatequeryclientresult)
1. [Cardano.Node.TxInbound.TxInboundCanRequestMoreTxs](#cardanonodetxinboundtxinboundcanrequestmoretxs)
1. [Cardano.Node.TxInbound.TxInboundCannotRequestMoreTxs](#cardanonodetxinboundtxinboundcannotrequestmoretxs)
1. [Cardano.Node.TxInbound.TxInboundTerminated](#cardanonodetxinboundtxinboundterminated)
1. [Cardano.Node.TxInbound.TxSubmissionCollected](#cardanonodetxinboundtxsubmissioncollected)
1. [Cardano.Node.TxInbound.TxSubmissionProcessed](#cardanonodetxinboundtxsubmissionprocessed)
1. [Cardano.Node.TxOutbound.ControlMessage](#cardanonodetxoutboundcontrolmessage)
1. [Cardano.Node.TxOutbound.TxSubmissionOutboundRecvMsgRequest](#cardanonodetxoutboundtxsubmissionoutboundrecvmsgrequest)
1. [Cardano.Node.TxOutbound.TxSubmissionOutboundSendMsgReply](#cardanonodetxoutboundtxsubmissionoutboundsendmsgreply)
1. [Cardano.Node.TxSubmission.NodeToNode.Send.Done](#cardanonodetxsubmissionnodetonodesenddone)
1. [Cardano.Node.TxSubmission.NodeToNode.Send.ReplyTxIds](#cardanonodetxsubmissionnodetonodesendreplytxids)
1. [Cardano.Node.TxSubmission.NodeToNode.Send.ReplyTxs](#cardanonodetxsubmissionnodetonodesendreplytxs)
1. [Cardano.Node.TxSubmission.NodeToNode.Send.RequestTxIds](#cardanonodetxsubmissionnodetonodesendrequesttxids)
1. [Cardano.Node.TxSubmission.NodeToNode.Send.RequestTxs](#cardanonodetxsubmissionnodetonodesendrequesttxs)
1. [Cardano.Node.TxSubmission2.NodeToNode.Send.Done](#cardanonodetxsubmission2nodetonodesenddone)
1. [Cardano.Node.TxSubmission2.NodeToNode.Send.MsgHello](#cardanonodetxsubmission2nodetonodesendmsghello)
1. [Cardano.Node.TxSubmission2.NodeToNode.Send.ReplyTxIds](#cardanonodetxsubmission2nodetonodesendreplytxids)
1. [Cardano.Node.TxSubmission2.NodeToNode.Send.ReplyTxs](#cardanonodetxsubmission2nodetonodesendreplytxs)
1. [Cardano.Node.TxSubmission2.NodeToNode.Send.RequestTxIds](#cardanonodetxsubmission2nodetonodesendrequesttxids)
1. [Cardano.Node.TxSubmission2.NodeToNode.Send.RequestTxs](#cardanonodetxsubmission2nodetonodesendrequesttxs)
1. [Cardano.Node.TxSubmissionClient.Send.AcceptTx](#cardanonodetxsubmissionclientsendaccepttx)
1. [Cardano.Node.TxSubmissionClient.Send.Done](#cardanonodetxsubmissionclientsenddone)
1. [Cardano.Node.TxSubmissionClient.Send.RejectTx](#cardanonodetxsubmissionclientsendrejecttx)
1. [Cardano.Node.TxSubmissionClient.Send.SubmitTx](#cardanonodetxsubmissionclientsendsubmittx)

## [Metrics](#metrics)
1. [Block replay progress (%)](#block replay progress (%))
1. [cardano.node.aboutToLeadSlotLast](#cardanonodeabouttoleadslotlast)
1. [cardano.node.aboutToLeadSlotLast](#cardanonodeabouttoleadslotlast)
1. [cardano.node.adoptedSlotLast](#cardanonodeadoptedslotlast)
1. [cardano.node.blockContext](#cardanonodeblockcontext)
1. [cardano.node.blockFromFuture](#cardanonodeblockfromfuture)
1. [cardano.node.blocks](#cardanonodeblocks)
1. [cardano.node.blocks](#cardanonodeblocks)
1. [cardano.node.chainSync.rollForward](#cardanonodechainsyncrollforward)
1. [cardano.node.chainSync.rollForward](#cardanonodechainsyncrollforward)
1. [cardano.node.connectedPeers](#cardanonodeconnectedpeers)
1. [cardano.node.connectionManager.duplexConns](#cardanonodeconnectionmanagerduplexconns)
1. [cardano.node.connectionManager.duplexConns](#cardanonodeconnectionmanagerduplexconns)
1. [cardano.node.connectionManager.fullDuplexConns](#cardanonodeconnectionmanagerfullduplexconns)
1. [cardano.node.connectionManager.fullDuplexConns](#cardanonodeconnectionmanagerfullduplexconns)
1. [cardano.node.connectionManager.inboundConns](#cardanonodeconnectionmanagerinboundconns)
1. [cardano.node.connectionManager.inboundConns](#cardanonodeconnectionmanagerinboundconns)
1. [cardano.node.connectionManager.outboundConns](#cardanonodeconnectionmanageroutboundconns)
1. [cardano.node.connectionManager.outboundConns](#cardanonodeconnectionmanageroutboundconns)
1. [cardano.node.connectionManager.unidirectionalConns](#cardanonodeconnectionmanagerunidirectionalconns)
1. [cardano.node.connectionManager.unidirectionalConns](#cardanonodeconnectionmanagerunidirectionalconns)
1. [cardano.node.couldNotForgeSlotLast](#cardanonodecouldnotforgeslotlast)
1. [cardano.node.couldNotForgeSlotLast](#cardanonodecouldnotforgeslotlast)
1. [cardano.node.currentKESPeriod](#cardanonodecurrentkesperiod)
1. [cardano.node.delegMapSize](#cardanonodedelegmapsize)
1. [cardano.node.density](#cardanonodedensity)
1. [cardano.node.density](#cardanonodedensity)
1. [cardano.node.epoch](#cardanonodeepoch)
1. [cardano.node.epoch](#cardanonodeepoch)
1. [cardano.node.forgedInvalidSlotLast](#cardanonodeforgedinvalidslotlast)
1. [cardano.node.forgedSlotLast](#cardanonodeforgedslotlast)
1. [cardano.node.ledgerState](#cardanonodeledgerstate)
1. [cardano.node.ledgerView](#cardanonodeledgerview)
1. [cardano.node.mempoolBytes](#cardanonodemempoolbytes)
1. [cardano.node.mempoolBytes](#cardanonodemempoolbytes)
1. [cardano.node.mempoolBytes](#cardanonodemempoolbytes)
1. [cardano.node.mempoolBytes](#cardanonodemempoolbytes)
1. [cardano.node.nodeCannotForge](#cardanonodenodecannotforge)
1. [cardano.node.nodeIsLeader](#cardanonodenodeisleader)
1. [cardano.node.nodeNotLeader](#cardanonodenodenotleader)
1. [cardano.node.notAdoptedSlotLast](#cardanonodenotadoptedslotlast)
1. [cardano.node.operationalCertificateExpiryKESPeriod](#cardanonodeoperationalcertificateexpirykesperiod)
1. [cardano.node.operationalCertificateStartKESPeriod](#cardanonodeoperationalcertificatestartkesperiod)
1. [cardano.node.peerSelection.cold](#cardanonodepeerselectioncold)
1. [cardano.node.peerSelection.hot](#cardanonodepeerselectionhot)
1. [cardano.node.peerSelection.warm](#cardanonodepeerselectionwarm)
1. [cardano.node.remainingKESPeriods](#cardanonoderemainingkesperiods)
1. [cardano.node.served.block](#cardanonodeservedblock)
1. [cardano.node.slotInEpoch](#cardanonodeslotinepoch)
1. [cardano.node.slotInEpoch](#cardanonodeslotinepoch)
1. [cardano.node.slotIsImmutable](#cardanonodeslotisimmutable)
1. [cardano.node.slots](#cardanonodeslots)
1. [cardano.node.slots](#cardanonodeslots)
1. [cardano.node.submissions.accepted](#cardanonodesubmissionsaccepted)
1. [cardano.node.submissions.rejected](#cardanonodesubmissionsrejected)
1. [cardano.node.submissions.submitted](#cardanonodesubmissionssubmitted)
1. [cardano.node.txsInMempool](#cardanonodetxsinmempool)
1. [cardano.node.txsInMempool](#cardanonodetxsinmempool)
1. [cardano.node.txsInMempool](#cardanonodetxsinmempool)
1. [cardano.node.txsInMempool](#cardanonodetxsinmempool)
1. [cardano.node.txsProcessedNum](#cardanonodetxsprocessednum)
1. [cardano.node.utxoSize](#cardanonodeutxosize)
1. [mem.resident](#memresident)
1. [peersFromNodeKernel](#peersfromnodekernel)
1. [rts.gcLiveBytes](#rtsgclivebytes)
1. [rts.gcMajorNum](#rtsgcmajornum)
1. [rts.gcMinorNum](#rtsgcminornum)
1. [rts.gcticks](#rtsgcticks)
1. [rts.mutticks](#rtsmutticks)
1. [rts.threads](#rtsthreads)
1. [stat.cputicks](#statcputicks)

## [Datapoints](#datapoints)
1. [NodeInfo](#nodeinfo)

## Trace Messages
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
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.AcceptPolicy.ConnectionHardLimit


***
Hard rate limit reached, waiting until the number of connections drops below n.
***


> Severity:   `Warning`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.BlockFetchClient.ClientTerminating


***
The client is terminating.  Log the number of outstanding requests.
***


> Severity:   `Notice`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

### Cardano.Node.BlockFetchClient.CompletedFetchBatch


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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.BlockFetchDecision


***
Throughout the decision making process we accumulate reasons to decline to fetch any blocks. This message carries the intermediate and final results.
***


> Severity:   `Info`
Privacy:   `Confidential`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.BlockchainTime.CurrentSlotUnknown


***
Current slot is not yet known
 This happens when the tip of our current chain is so far in the past that we cannot translate the current wallclock to a slot number, typically during syncing. Until the current slot number is known, we cannot produce blocks. Seeing this message during syncing therefore is normal and to be expected.
 We record the current time (the time we tried to translate to a 'SlotNo') as well as the 'PastHorizonException', which provides detail on the bounds between which we /can/ do conversions. The distance between the current time and the upper bound should rapidly decrease with consecutive 'CurrentSlotUnknown' messages during syncing.
***


> Severity:   `Warning`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

### Cardano.Node.BlockchainTime.StartTimeInTheFuture


***
The start time of the blockchain time is in the future
 We have to block (for 'NominalDiffTime') until that time comes.
***


> Severity:   `Warning`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

### Cardano.Node.BlockchainTime.SystemClockMovedBack


***
The system clock moved back an acceptable time span, e.g., because of an NTP sync.
 The system clock moved back such that the new current slot would be smaller than the previous one. If this is within the configured limit, we trace this warning but *do not change the current slot*. The current slot never decreases, but the current slot may stay the same longer than expected.
 When the system clock moved back more than the configured limit, we shut down with a fatal exception.
***


> Severity:   `Warning`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

### Cardano.Node.ChainDB.AddBlockEvent.AddBlockValidation.CandidateContainsFutureBlocks


***
An event traced during validating performed while adding a block. Candidate contains headers from the future which do no exceed the clock skew.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.ChainDB.AddBlockEvent.AddBlockValidation.CandidateContainsFutureBlocksExceedingClockSkew


***
An event traced during validating performed while adding a block. Candidate contains headers from the future which exceed the clock skew.
***


> Severity:   `Error`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.ChainDB.AddBlockEvent.AddBlockValidation.InvalidBlock


***
An event traced during validating performed while adding a block. A point was found to be invalid.
***


> Severity:   `Error`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

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
Filtered: `Visible` because the filter level is `Info`
Limiters: Limiter `ValidCandidateLimiter` with frequency `2.0`

### Cardano.Node.ChainDB.AddBlockEvent.AddedBlockToQueue


***
The block was added to the queue and will be added to the ChainDB by the background thread. The size of the queue is included..
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`
Limiters: Limiter `AddedBlockToQueueLimiter` with frequency `2.0`

### Cardano.Node.ChainDB.AddBlockEvent.AddedBlockToVolatileDB


***
A block was added to the Volatile DB
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`
Limiters: Limiter `AddedBlockToVolatileDBLimiter` with frequency `2.0`

### Cardano.Node.ChainDB.AddBlockEvent.AddedToCurrentChain


***
The new block fits onto the current chain (first fragment) and we have successfully used it to extend our (new) current chain (second fragment).
***


> Severity:   `Notice`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

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
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.ChainDB.AddBlockEvent.ChainSelectionForFutureBlock


***
Run chain selection for a block that was previously from the future. This is done for all blocks from the future each time a new block is added.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

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
Filtered: `Visible` because the filter level is `Info`

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
Filtered: `Visible` because the filter level is `Info`

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
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.ChainDB.AddBlockEvent.StoreButDontChange


***
The block fits onto some fork, we'll try to switch to that fork (if it is preferable to our chain).
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.ChainDB.AddBlockEvent.SwitchedToAFork


***
The new block fits onto some fork and we have switched to that fork (second fragment), as it is preferable to our (previous) current chain (first fragment).
***


> Severity:   `Notice`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.ChainDB.AddBlockEvent.TryAddToCurrentChain


***
The block fits onto the current chain, we'll try to use it to extend our chain.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

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
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.ChainDB.CopyToImmutableDBEvent.CopiedBlockToImmutableDB


***
A block was successfully copied to the ImmDB.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`
Limiters: Limiter `CopiedBlockToImmutableDBLimiter` with frequency `2.0`

### Cardano.Node.ChainDB.CopyToImmutableDBEvent.NoBlocksToCopyToImmutableDB


***
There are no block to copy to the ImmDB.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.ChainDB.FollowerEvent.FollowerNewImmIterator


***
The follower is in the 'FollowerInImmutableDB' state but the iterator is exhausted while the ImmDB has grown, so we open a new iterator to stream these blocks too.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.ChainDB.FollowerEvent.FollowerNoLongerInMem


***
The follower was in the 'FollowerInImmutableDB' state and is switched to the 'FollowerInMem' state.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.ChainDB.FollowerEvent.FollowerSwitchToMem


***
The follower was in the 'FollowerInImmutableDB' state and is switched to the 'FollowerInMem' state.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.ChainDB.FollowerEvent.NewFollower


***
A new follower was created.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.ChainDB.GCEvent.PerformedGC


***
There are no block to copy to the ImmDB.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.ChainDB.GCEvent.ScheduledGC


***
There are no block to copy to the ImmDB.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.ChainDB.ImmDbEvent.CacheEvent.CurrentChunkHit


***
Current chunk found in the cache.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.ChainDB.ImmDbEvent.CacheEvent.PastChunkEvict


***
The least recently used past chunk was evicted because the cache was full.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.ChainDB.ImmDbEvent.CacheEvent.PastChunkExpired


***

***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.ChainDB.ImmDbEvent.CacheEvent.PastChunkHit


***
Past chunk found in the cache
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.ChainDB.ImmDbEvent.CacheEvent.PastChunkMiss


***
Past chunk was not found in the cache
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.ChainDB.ImmDbEvent.ChunkFileDoesntFit


***
The hash of the last block in the previous epoch doesn't match the previous hash of the first block in the current epoch
***


> Severity:   `Warning`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.ChainDB.ImmDbEvent.ChunkValidation.InvalidChunkFile


***
Chunk file is invalid
***


> Severity:   `Warning`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.ChainDB.ImmDbEvent.ChunkValidation.InvalidPrimaryIndex


***
The primary index is invalid.
***


> Severity:   `Warning`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.ChainDB.ImmDbEvent.ChunkValidation.InvalidSecondaryIndex


***

***


> Severity:   `Warning`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.ChainDB.ImmDbEvent.ChunkValidation.MissingChunkFile


***
Chunk file is missing
***


> Severity:   `Warning`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.ChainDB.ImmDbEvent.ChunkValidation.MissingPrimaryIndex


***
The primary index is missing.
***


> Severity:   `Warning`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.ChainDB.ImmDbEvent.ChunkValidation.MissingSecondaryIndex


***
The secondary index is missing.
***


> Severity:   `Warning`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.ChainDB.ImmDbEvent.DBAlreadyClosed


***
The immutable DB is already closed
***


> Severity:   `Error`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.ChainDB.ImmDbEvent.DBClosed


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
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.ChainDB.ImmDbEvent.DeletingAfter


***
Delete after
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.ChainDB.ImmDbEvent.Migrating


***
Performing a migration of the on-disk files.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.ChainDB.ImmDbEvent.NoValidLastLocation


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
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.ChainDB.ImmDbEvent.ValidatedLastLocation


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
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.ChainDB.InitChainSelEvent.CandidateContainsFutureBlocks


***
Candidate contains headers from the future which do not exceed the clock skew.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.ChainDB.InitChainSelEvent.CandidateContainsFutureBlocksExceedingClockSkew


***
Candidate contains headers from the future which exceed the clock skew, making them invalid.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.ChainDB.InitChainSelEvent.InvalidBlock


***
A point was found to be invalid.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.ChainDB.InitChainSelEvent.ValidCandidate


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
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.ChainDB.IteratorEvent.BlockGCedFromVolatileDB


***
A block is no longer in the VolatileDB and isn't in the ImmDB either; it wasn't part of the current chain.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.ChainDB.IteratorEvent.BlockMissingFromVolatileDB


***
A block is no longer in the VolatileDB because it has been garbage collected. It might now be in the ImmDB if it was part of the current chain.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.ChainDB.IteratorEvent.BlockWasCopiedToImmutableDB


***
A block that has been garbage collected from the VolatileDB is now found and streamed from the ImmDB.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.ChainDB.IteratorEvent.StreamFromBoth


***
Stream from both the VolatileDB and the ImmDB.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.ChainDB.IteratorEvent.StreamFromImmutableDB


***
Stream only from the ImmDB.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.ChainDB.IteratorEvent.StreamFromVolatileDB


***
Stream only from the VolatileDB.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.ChainDB.IteratorEvent.SwitchBackToVolatileDB


***
We have streamed one or more blocks from the ImmDB that were part of the VolatileDB when initialising the iterator. Now, we have to look back in the VolatileDB again because the ImmDB doesn't have the next block we're looking for.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.ChainDB.IteratorEvent.UnknownRangeRequested


***
An unknown range was requested, see 'UnknownRange'.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.ChainDB.LedgerEvent.DeletedSnapshot


***
An old or invalid on-disk snapshot was deleted.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.ChainDB.LedgerEvent.InvalidSnapshot


***
An on disk snapshot was skipped because it was invalid.
***


> Severity:   `Error`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.ChainDB.LedgerEvent.ReplayFromGenesis


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
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.ChainDB.LedgerEvent.ReplayFromSnapshot


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
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.ChainDB.LedgerEvent.ReplayedBlock


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
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.ChainDB.LedgerEvent.TookSnapshot


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
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.ChainDB.OpenEvent.ClosedDB


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
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.ChainDB.OpenEvent.OpenedDB


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
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.ChainDB.OpenEvent.OpenedImmutableDB


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
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.ChainDB.OpenEvent.OpenedLgrDB


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
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.ChainDB.OpenEvent.OpenedVolatileDB


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
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.ChainDB.VolatileDbEvent.BlockAlreadyHere


***
A block was found to be already in the DB.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.ChainDB.VolatileDbEvent.DBAlreadyClosed


***
When closing the DB it was found itis closed already.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.ChainDB.VolatileDbEvent.InvalidFileNames


***
Reports a list of invalid file paths.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.ChainDB.VolatileDbEvent.Truncate


***
Truncates a file up to offset because of the error.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.ChainSyncClient.ChainSyncClientEvent.Exception


***
An exception was thrown by the Chain Sync Client.
***


> Severity:   `Warning`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.ChainSyncClient.ChainSyncClientEvent.RolledBack


***
While following a candidate chain, we rolled back to the given point.
***


> Severity:   `Notice`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

### Cardano.Node.ChainSyncClient.ChainSyncClientEvent.Termination


***
The client has terminated.
***


> Severity:   `Notice`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

### Cardano.Node.ChainSyncClient.NodeToClient.Send.AwaitReply


***
Acknowledge the request but require the consumer to wait for the nextupdate. This means that the consumer is synced with the producer, and the producer is waiting for its own chain state to change.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.ChainSyncClient.NodeToClient.Send.AwaitReply


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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.ChainSyncClient.NodeToClient.Send.Done


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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.ChainSyncClient.NodeToClient.Send.FindIntersect


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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.ChainSyncClient.NodeToClient.Send.IntersectFound


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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.ChainSyncClient.NodeToClient.Send.IntersectNotFound


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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.ChainSyncClient.NodeToClient.Send.RequestNext


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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.ChainSyncClient.NodeToClient.Send.RollBackward


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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.ChainSyncClient.NodeToClient.Send.RollForward


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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.ChainSyncNode.NodeToNode.Send.AwaitReply


***
Acknowledge the request but require the consumer to wait for the nextupdate. This means that the consumer is synced with the producer, and the producer is waiting for its own chain state to change.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.ChainSyncNode.NodeToNode.Send.AwaitReply


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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.ChainSyncSerialised.NodeToNode.Send.AwaitReply


***
Acknowledge the request but require the consumer to wait for the nextupdate. This means that the consumer is synced with the producer, and the producer is waiting for its own chain state to change.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.ChainSyncSerialised.NodeToNode.Send.AwaitReply


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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.ChainSyncServerBlock.ChainSyncServerEvent.RollBackward


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.ChainSyncServerBlock.ChainSyncServerEvent.RollForward


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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.ChainSyncServerBlock.ChainSyncServerEvent.ServerRead


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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.ChainSyncServerBlock.ChainSyncServerEvent.ServerReadBlocked


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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.ChainSyncServerHeader.ChainSyncServerEvent.RollBackward


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.ChainSyncServerHeader.ChainSyncServerEvent.RollForward


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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.ChainSyncServerHeader.ChainSyncServerEvent.ServerRead


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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.ChainSyncServerHeader.ChainSyncServerEvent.ServerReadBlocked


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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.ConnectionManager.Connect


***

***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.ConnectionManager.ConnectionCleanup


***

***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.ConnectionManager.ConnectionNotFound


***

***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.ConnectionManager.ConnectionTimeWait


***

***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.ConnectionManager.ConnectionTimeWaitDone


***

***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.ConnectionManager.IncludeConnection


***

***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.ConnectionManager.PruneConnections


***

***


> Severity:   `Notice`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.ConnectionManager.TerminatedConnection


***

***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.ConnectionManager.TerminatingConnection


***

***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.ConnectionManager.UnexpectedlyFalseAssertion


***

***


> Severity:   `Error`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

### Cardano.Node.ConnectionManager.UnregisterConnection


***

***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.DNSResolver.LookupAAAAError


***
AAAA lookup failed with an error.
***


> Severity:   `Error`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.DNSResolver.LookupAAAAResult


***
Lookup AAAA result.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.DNSResolver.LookupAError


***
A lookup failed with an error.
***


> Severity:   `Error`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.DNSResolver.LookupAResult


***
Lookup A result.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.DNSResolver.LookupException


***
A DNS lookup exception occurred.
***


> Severity:   `Error`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.DNSResolver.LookupIPv4First


***
Returning IPv4 address first.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.DNSResolver.LookupIPv6First


***
Returning IPv6 address first.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.DNSResolver.LookupIPv6First


***
Returning IPv6 address first.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.DNSSubscription.DNS.AllocateSocket


***
DNS Subscription: Allocate socket to address.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.DNSSubscription.DNS.ApplicationException


***
DNS Subscription: Application Exception occurred.
***


> Severity:   `Error`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.DNSSubscription.DNS.CloseSocket


***
DNS Subscription: Closed socket to address.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.DNSSubscription.DNS.ConnectEnd


***
DNS Subscription: Connection Attempt end with destination and outcome.
***


> Severity:   `Notice`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.DNSSubscription.DNS.ConnectException


***
DNS Subscription: Connection Attempt Exception with destination and exception.
***


> Severity:   `Error`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.DNSSubscription.DNS.ConnectStart


***
DNS Subscription: Connection Attempt Start with destination.
***


> Severity:   `Notice`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

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
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.DNSSubscription.DNS.MissingLocalAddress


***
DNS Subscription: Missing local address.
***


> Severity:   `Warning`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.DNSSubscription.DNS.Restart


***
DNS Subscription: Restarting Subscription after duration with desired valency and current valency.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

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
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.DNSSubscription.DNS.SocketAllocationException


***
DNS Subscription: Socket Allocation Exception with destination and the exception.
***


> Severity:   `Error`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.DNSSubscription.DNS.Start


***
DNS Subscription: Starting Subscription Worker with a valency.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.DNSSubscription.DNS.SubscriptionFailed


***
DNS Subscription: Failed to start all required subscriptions.
***


> Severity:   `Warning`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.DNSSubscription.DNS.SubscriptionRunning


***
DNS Subscription: Required subscriptions started.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.DNSSubscription.DNS.SubscriptionWaiting


***
DNS Subscription: Waiting on address with active connections.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.DNSSubscription.DNS.SubscriptionWaitingNewConnection


***
DNS Subscription: Waiting delay time before attempting a new connection.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

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
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.DNSSubscription.DNS.UnsupportedRemoteAddr


***
DNS Subscription: Unsupported remote target address.
***


> Severity:   `Warning`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.DebugPeerSelection.GovernorState


***

***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.DebugPeerSelectionResponder.GovernorState


***

***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Visible` because the filter level is `Info`

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
Filtered: `Visible` because the filter level is `Info`

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
Filtered: `Visible` because the filter level is `Info`

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
Filtered: `Visible` because the filter level is `Info`

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
Filtered: `Visible` because the filter level is `Info`

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
Filtered: `Visible` because the filter level is `Info`

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
Filtered: `Visible` because the filter level is `Info`

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
Filtered: `Visible` because the filter level is `Info`

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
Filtered: `Visible` because the filter level is `Info`

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
Filtered: `Visible` because the filter level is `Info`

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
Filtered: `Visible` because the filter level is `Info`

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
Filtered: `Visible` because the filter level is `Info`

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
Filtered: `Visible` because the filter level is `Info`

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
Filtered: `Visible` because the filter level is `Info`

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
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.ErrorPolicy.AcceptException


***
'accept' threw an exception.
***


> Severity:   `Error`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.ErrorPolicy.KeepSuspended


***
Consumer was suspended until producer will resume.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.ErrorPolicy.LocalNodeError


***
caught a local exception.
***


> Severity:   `Error`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.ErrorPolicy.ResumeConsumer


***
Resume consumer.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.ErrorPolicy.ResumePeer


***
Resume a peer (both consumer and producer).
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.ErrorPolicy.ResumeProducer


***
Resume producer.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.ErrorPolicy.SuspendConsumer


***
Suspending consumer.
***


> Severity:   `Notice`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.ErrorPolicy.SuspendPeer


***
Suspending peer with a given exception.
***


> Severity:   `Warning`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.ErrorPolicy.UnhandledApplicationException


***
An application threw an exception, which was not handled.
***


> Severity:   `Error`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.ErrorPolicy.UnhandledConnectionException


***
'connect' threw an exception, which was not handled by any 'ErrorPolicy'.
***


> Severity:   `Error`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

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
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.Forge.BlockContext


***
We found out to which block we are going to connect the block we are about  to forge.   We record the current slot number, the block number of the block to  connect to and its point.   Note that block number of the block we will try to forge is one more than  the recorded block number.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.Forge.BlockFromFuture


***
Leadership check failed: the current chain contains a block from a slot  /after/ the current slot   This can only happen if the system is under heavy load.   We record both the current slot number as well as the slot number of the  block at the tip of the chain.   See also <https://github.com/input-output-hk/ouroboros-network/issues/1462>
***


> Severity:   `Error`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.Forge.DidntAdoptBlock


***
We did not adopt the block we produced, but the block was valid. We  must have adopted a block that another leader of the same slot produced  before we got the chance of adopting our own block. This is very rare,  this warrants a warning.
***


> Severity:   `Error`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.Forge.ForgeStateUpdateError


***
Updating the forge state failed.   For example, the KES key could not be evolved anymore.   We record the error returned by 'updateForgeState'.
***


> Severity:   `Error`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

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
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.Forge.ForgedInvalidBlock


***
We forged a block that is invalid according to the ledger in the  ChainDB. This means there is an inconsistency between the mempool  validation and the ledger validation. This is a serious error!
***


> Severity:   `Error`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.Forge.LedgerState


***
We obtained a ledger state for the point of the block we want to  connect to   We record both the current slot number as well as the point of the block  we attempt to connect the new block to (that we requested the ledger  state for).
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.Forge.LedgerView


***
We obtained a ledger view for the current slot number   We record the current slot number.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.Forge.NoLedgerState


***
Leadership check failed: we were unable to get the ledger state for the  point of the block we want to connect to   This can happen if after choosing which block to connect to the node  switched to a different fork. We expect this to happen only rather  rarely, so this certainly merits a warning; if it happens a lot, that  merits an investigation.   We record both the current slot number as well as the point of the block  we attempt to connect the new block to (that we requested the ledger  state for).
***


> Severity:   `Error`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.Forge.NoLedgerView


***
Leadership check failed: we were unable to get the ledger view for the  current slot number   This will only happen if there are many missing blocks between the tip of  our chain and the current slot.   We record also the failure returned by 'forecastFor'.
***


> Severity:   `Error`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.Forge.NodeCannotForge


***
We did the leadership check and concluded that we should lead and forge  a block, but cannot.   This should only happen rarely and should be logged with warning severity.   Records why we cannot forge a block.
***


> Severity:   `Error`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

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
Filtered: `Visible` because the filter level is `Info`

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
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.Forge.SlotIsImmutable


***
Leadership check failed: the tip of the ImmutableDB inhabits the  current slot   This might happen in two cases.    1. the clock moved backwards, on restart we ignored everything from the      VolatileDB since it's all in the future, and now the tip of the      ImmutableDB points to a block produced in the same slot we're trying      to produce a block in    2. k = 0 and we already adopted a block from another leader of the same      slot.   We record both the current slot number as well as the tip of the  ImmutableDB.  See also <https://github.com/input-output-hk/ouroboros-network/issues/1462>
***


> Severity:   `Error`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

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
Filtered: `Visible` because the filter level is `Info`

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
Filtered: `Visible` because the filter level is `Info`

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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.Handshake.AcceptVersion


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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.Handshake.ProposeVersions


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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.Handshake.Refuse


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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.InboundGovernor.MuxCleanExit


***

***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.InboundGovernor.NewConnection


***

***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.InboundGovernor.RemoteState


***

***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.InboundGovernor.ResponderRestarted


***

***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.InboundGovernor.ResponderStartFailure


***

***


> Severity:   `Error`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

### Cardano.Node.InboundGovernor.ResponderStarted


***

***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.InboundGovernor.ResponderTerminated


***

***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.InboundGovernor.WaitIdleRemote


***

***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.IpSubscription.IP.AllocateSocket


***
IP Subscription: Allocate socket to address.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.IpSubscription.IP.ApplicationException


***
IP Subscription: Application Exception occurred.
***


> Severity:   `Error`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

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
Filtered: `Visible` because the filter level is `Info`

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
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.IpSubscription.IP.ConnectException


***
IP Subscription: Connection Attempt Exception with destination and exception.
***


> Severity:   `Error`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

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
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.IpSubscription.IP.ConnectionExist


***
IP Subscription: Connection exists to destination.
***


> Severity:   `Notice`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.IpSubscription.IP.MissingLocalAddress


***
IP Subscription: Missing local address.
***


> Severity:   `Warning`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

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
Filtered: `Visible` because the filter level is `Info`

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
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.IpSubscription.IP.SocketAllocationException


***
IP Subscription: Socket Allocation Exception with destination and the exception.
***


> Severity:   `Error`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.IpSubscription.IP.Start


***
IP Subscription: Starting Subscription Worker with a valency.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.IpSubscription.IP.SubscriptionFailed


***
IP Subscription: Failed to start all required subscriptions.
***


> Severity:   `Error`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.IpSubscription.IP.SubscriptionRunning


***
IP Subscription: Required subscriptions started.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.IpSubscription.IP.SubscriptionWaiting


***
IP Subscription: Waiting on address with active connections.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.IpSubscription.IP.SubscriptionWaitingNewConnection


***
IP Subscription: Waiting delay time before attempting a new connection.
***


> Severity:   `Notice`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

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
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.IpSubscription.IP.UnsupportedRemoteAddr


***
IP Subscription: Unsupported remote target address.
***


> Severity:   `Error`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.KeepAliveClient.KeepAliveClient


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.LedgerPeers.PickedPeer


***
Trace for a peer picked with accumulated and relative stake of its pool.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.LedgerPeers.ReusingLedgerState


***

***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.LedgerPeers.WaitingOnRequest


***

***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.LedgerPeers.WaitingOnRequest


***
RequestForPeers (NumberOfPeers 1)
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.LocalConnectionManager.Connect


***

***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.LocalConnectionManager.ConnectionCleanup


***

***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.LocalConnectionManager.ConnectionNotFound


***

***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.LocalConnectionManager.ConnectionTimeWait


***

***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.LocalConnectionManager.ConnectionTimeWaitDone


***

***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.LocalConnectionManager.IncludeConnection


***

***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.LocalConnectionManager.PruneConnections


***

***


> Severity:   `Notice`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.LocalConnectionManager.TerminatedConnection


***

***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.LocalConnectionManager.TerminatingConnection


***

***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.LocalConnectionManager.UnexpectedlyFalseAssertion


***

***


> Severity:   `Error`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

### Cardano.Node.LocalConnectionManager.UnregisterConnection


***

***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.LocalErrorPolicy.AcceptException


***
'accept' threw an exception.
***


> Severity:   `Error`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.LocalErrorPolicy.KeepSuspended


***
Consumer was suspended until producer will resume.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.LocalErrorPolicy.LocalNodeError


***
caught a local exception.
***


> Severity:   `Error`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.LocalErrorPolicy.ResumeConsumer


***
Resume consumer.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.LocalErrorPolicy.ResumePeer


***
Resume a peer (both consumer and producer).
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.LocalErrorPolicy.ResumeProducer


***
Resume producer.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Info`

### Cardano.Node.LocalErrorPolicy.SuspendConsumer


***
Suspending consumer.
***


> Severity:   `Notice`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.LocalErrorPolicy.SuspendPeer


***
Suspending peer with a given exception.
***


> Severity:   `Warning`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.LocalErrorPolicy.UnhandledApplicationException


***
An application threw an exception, which was not handled.
***


> Severity:   `Error`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.LocalErrorPolicy.UnhandledConnectionException


***
'connect' threw an exception, which was not handled by any 'ErrorPolicy'.
***


> Severity:   `Error`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.LocalHandshake.AcceptVersion


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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.LocalHandshake.ProposeVersions


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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.LocalHandshake.Refuse


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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.LocalInboundGovernor.MuxCleanExit


***

***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.LocalInboundGovernor.NewConnection


***

***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.LocalInboundGovernor.RemoteState


***

***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.LocalInboundGovernor.ResponderRestarted


***

***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.LocalInboundGovernor.ResponderStartFailure


***

***


> Severity:   `Error`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

### Cardano.Node.LocalInboundGovernor.ResponderStarted


***

***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.LocalInboundGovernor.ResponderTerminated


***

***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.LocalInboundGovernor.WaitIdleRemote


***

***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.LocalServer.AcceptConnection


***

***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.LocalServer.AcceptError


***

***


> Severity:   `Error`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

### Cardano.Node.LocalServer.AcceptPolicy


***

***


> Severity:   `Notice`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

### Cardano.Node.LocalServer.Error


***

***


> Severity:   `Critical`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

### Cardano.Node.LocalServer.Started


***

***


> Severity:   `Notice`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

### Cardano.Node.LocalServer.Stopped


***

***


> Severity:   `Notice`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Visible` because the filter level is `Info`

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
Filtered: `Visible` because the filter level is `Info`

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
Filtered: `Visible` because the filter level is `Info`

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
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.Mux.ChannelRecvEnd


***
Channel receive end.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.Mux.ChannelRecvStart


***
Channel receive start.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.Mux.ChannelSendEnd


***
Channel send end.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.Mux.ChannelSendStart


***
Channel send start.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.Mux.CleanExit


***
Miniprotocol terminated cleanly.
***


> Severity:   `Notice`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

### Cardano.Node.Mux.ExceptionExit


***
Miniprotocol terminated with exception.
***


> Severity:   `Notice`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.Mux.HandshakeClientError


***
Handshake client error.
***


> Severity:   `Error`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

### Cardano.Node.Mux.HandshakeServerError


***
Handshake server error.
***


> Severity:   `Error`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

### Cardano.Node.Mux.HandshakeStart 


***
Handshake start.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.Mux.RecvDeltaQObservation


***
Bearer DeltaQ observation.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.Mux.RecvDeltaQSample


***
Bearer DeltaQ sample.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.Mux.RecvEnd


***
Bearer receive end.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.Mux.RecvHeaderEnd


***
Bearer receive header end.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.Mux.RecvHeaderStart


***
Bearer receive header start.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.Mux.RecvStart


***
Bearer receive start.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.Mux.SDUReadTimeoutException


***
Timed out reading SDU.
***


> Severity:   `Notice`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

### Cardano.Node.Mux.SDUWriteTimeoutException


***
Timed out writing SDU.
***


> Severity:   `Notice`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

### Cardano.Node.Mux.SendEnd


***
Bearer send end.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.Mux.SendStart


***
Bearer send start.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.Mux.Shutdown


***
Mux shutdown.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.Mux.StartEagerly


***
Eagerly started.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.Mux.StartOnDemand


***
Preparing to start.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.Mux.StartedOnDemand


***
Started on demand.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.Mux.Terminating


***
Terminating.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.MuxLocal.ChannelRecvEnd


***
Channel receive end.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.MuxLocal.ChannelRecvStart


***
Channel receive start.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.MuxLocal.ChannelSendEnd


***
Channel send end.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.MuxLocal.ChannelSendStart


***
Channel send start.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.MuxLocal.CleanExit


***
Miniprotocol terminated cleanly.
***


> Severity:   `Notice`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

### Cardano.Node.MuxLocal.ExceptionExit


***
Miniprotocol terminated with exception.
***


> Severity:   `Notice`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.MuxLocal.HandshakeClientError


***
Handshake client error.
***


> Severity:   `Error`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

### Cardano.Node.MuxLocal.HandshakeServerError


***
Handshake server error.
***


> Severity:   `Error`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

### Cardano.Node.MuxLocal.HandshakeStart 


***
Handshake start.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.MuxLocal.RecvDeltaQObservation


***
Bearer DeltaQ observation.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.MuxLocal.RecvDeltaQSample


***
Bearer DeltaQ sample.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.MuxLocal.RecvEnd


***
Bearer receive end.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.MuxLocal.RecvHeaderEnd


***
Bearer receive header end.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.MuxLocal.RecvHeaderStart


***
Bearer receive header start.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.MuxLocal.RecvStart


***
Bearer receive start.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.MuxLocal.SDUReadTimeoutException


***
Timed out reading SDU.
***


> Severity:   `Notice`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

### Cardano.Node.MuxLocal.SDUWriteTimeoutException


***
Timed out writing SDU.
***


> Severity:   `Notice`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

### Cardano.Node.MuxLocal.SendEnd


***
Bearer send end.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.MuxLocal.SendStart


***
Bearer send start.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.MuxLocal.Shutdown


***
Mux shutdown.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.MuxLocal.StartEagerly


***
Eagerly started.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.MuxLocal.StartOnDemand


***
Preparing to start.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.MuxLocal.StartedOnDemand


***
Started on demand.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.MuxLocal.Terminating


***
Terminating.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.PeerSelection.GossipRequests


***
target known peers, actual known peers, peers available for gossip, peers selected for gossip
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.PeerSelection.GossipResults


***

***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.PeerSelection.LocalRootPeersChanged


***

***


> Severity:   `Notice`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.PeerSelection.TargetsChanged


***

***


> Severity:   `Notice`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

### Cardano.Node.PeerSelection.ublicRootsRequest


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.PeerSelectionActions.MonitoringError


***

***


> Severity:   `Error`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

### Cardano.Node.PeerSelectionActions.MonitoringResult


***

***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.PeerSelectionActions.StatusChangeFailure


***

***


> Severity:   `Error`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.PeerSelectionCounters


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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.Peers


***
TODO Doc
***


> Severity:   `Notice`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

### Cardano.Node.PublicRootPeers.PublicRootDomains


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.PublicRootPeers.PublicRootFailure


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.PublicRootPeers.PublicRootRelayAccessPoint


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.PublicRootPeers.PublicRootResult


***

***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Visible` because the filter level is `Info`

### Cardano.Node.Server.AcceptConnection


***

***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.Server.AcceptError


***

***


> Severity:   `Error`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

### Cardano.Node.Server.AcceptPolicy


***

***


> Severity:   `Notice`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

### Cardano.Node.Server.Error


***

***


> Severity:   `Critical`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

### Cardano.Node.Server.Started


***

***


> Severity:   `Notice`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

### Cardano.Node.Server.Stopped


***

***


> Severity:   `Notice`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

### Cardano.Node.Shutdown.AbnormalShutdown


***
non-isEOFerror shutdown request
***


> Severity:   `Error`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

### Cardano.Node.Shutdown.RequestingShutdown


***
Ringing the node shutdown doorbell
***


> Severity:   `Warning`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

### Cardano.Node.Shutdown.ShutdownArmedAtSlot


***
Setting up node shutdown at given slot.
***


> Severity:   `Warning`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

### Cardano.Node.Shutdown.ShutdownRequested


***
Node shutdown was requested.
***


> Severity:   `Warning`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

### Cardano.Node.Shutdown.ShutdownUnexpectedInput


***
Received shutdown request but found unexpected input in --shutdown-ipc FD: 
***


> Severity:   `Error`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

### Cardano.Node.Startup.Byron


***
_bibSystemStartTime_: TODO JNF 
_bibSlotLength_: gives the length of a slot as time interval. 
_bibEpochLength_: gives the number of slots which forms an epoch.
***


> Severity:   `Notice`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

### Cardano.Node.Startup.Common


***
_biConfigPath_: is the path to the config in use. 
_biProtocol_: is the name of the protocol, e.g. "Byron", "Shelley" or "Byron; Shelley". 
_biVersion_: is the version of the node software running. 
_biCommit_: is the commit revision of the software running. 
_biNodeStartTime_: gives the time this node was started.
***


> Severity:   `Notice`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

### Cardano.Node.Startup.Network


***
_niAddresses_: IPv4 or IPv6 socket ready to accept connectionsor diffusion addresses. 
_niDiffusionMode_: shows if the node runs only initiator or bothinitiator or responder node. 
_niDnsProducers_: shows the list of domain names to subscribe to. 
_niIpProducers_: shows the list of ip subscription addresses.
***


> Severity:   `Notice`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

### Cardano.Node.Startup.ShelleyBased


***
bisEra is the current era, e.g. "Shelley", "Allegra", "Mary" or "Alonzo". 
_bisSystemStartTime_: TODO JNF 
_bisSlotLength_: gives the length of a slot as time interval. 
_bisEpochLength_: gives the number of slots which forms an epoch. 
_bisSlotsPerKESPeriod_: gives the slots per KES period.
***


> Severity:   `Notice`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

### Cardano.Node.StateQueryClient.Acquire


***
The client requests that the state as of a particular recent point on the server's chain (within K of the tip) be made available to query, and waits for confirmation or failure.
From 'NodeToClient_V8' onwards if the point is not specified, current tipwill be acquired.  For previous versions of the protocol 'point' must begiven.
***


> Severity:   `Info`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.StateQueryClient.Acquired


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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.StateQueryClient.Acquired


***
The server can report that it cannot obtain the state for therequested point.
***


> Severity:   `Warning`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

### Cardano.Node.StateQueryClient.Done


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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.StateQueryClient.Query


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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.StateQueryClient.ReAcquire


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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.StateQueryClient.Release


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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.StateQueryClient.Result


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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.TxInbound.TxInboundCanRequestMoreTxs


***
There are no replies in flight, but we do know some more txs we can ask for, so lets ask for them and more txids.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.TxInbound.TxInboundCannotRequestMoreTxs


***
There's no replies in flight, and we have no more txs we can ask for so the only remaining thing to do is to ask for more txids. Since this is the only thing to do now, we make this a blocking call.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.TxInbound.TxInboundTerminated


***
Server received 'MsgDone'.
***


> Severity:   `Notice`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Visible` because the filter level is `Notice`

### Cardano.Node.TxInbound.TxSubmissionCollected


***
Number of transactions just about to be inserted.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.TxInbound.TxSubmissionProcessed


***
Just processed transaction pass/fail breakdown.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.TxOutbound.TxSubmissionOutboundRecvMsgRequest


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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.TxOutbound.TxSubmissionOutboundSendMsgReply


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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

### Cardano.Node.TxSubmission2.NodeToNode.Send.MsgHello


***
Client side hello message.
***


> Severity:   `Debug`
Privacy:   `Public`

From current configuration:
Details:   `DNormal`
Backends:
			`Stdout MachineFormat`,
			`EKGBackend`
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

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
Filtered: `Invisible` because the filter level is `Notice`

## Metrics
### Block replay progress (%)

***
Progress in percent
***


Dispatched by: 
Cardano.Node.ReplayBlock.LedgerReplay

From current configuration:
Filtered: `Invisible` because the filter level is `Notice`

### cardano.node.aboutToLeadSlotLast

***

***


Dispatched by: 
Cardano.Node.Forge.StartLeadershipCheck

From current configuration:
Filtered: `Visible` because the filter level is `Info`

### cardano.node.aboutToLeadSlotLast

***

***


Dispatched by: 
Cardano.Node.Forge.StartLeadershipCheckPlus

From current configuration:
Filtered: `Visible` because the filter level is `Info`

### cardano.node.adoptedSlotLast

***

***


Dispatched by: 
Cardano.Node.Forge.AdoptedBlock

From current configuration:
Filtered: `Visible` because the filter level is `Info`

### cardano.node.blockContext

***

***


Dispatched by: 
Cardano.Node.Forge.BlockContext

From current configuration:
Filtered: `Invisible` because the filter level is `Info`

### cardano.node.blockFromFuture

***

***


Dispatched by: 
Cardano.Node.Forge.BlockFromFuture

From current configuration:
Filtered: `Visible` because the filter level is `Info`

### cardano.node.blocks

***
Number of blocks in this chain fragment.
***


Dispatched by: 
Cardano.Node.ChainDB.AddBlockEvent.AddedToCurrentChain

From current configuration:
Filtered: `Visible` because the filter level is `Info`

### cardano.node.blocks

***
Number of blocks in this chain fragment.
***


Dispatched by: 
Cardano.Node.ChainDB.AddBlockEvent.SwitchedToAFork

From current configuration:
Filtered: `Visible` because the filter level is `Info`

### cardano.node.chainSync.rollForward

***

***


Dispatched by: 
Cardano.Node.ChainSyncServerHeader.ChainSyncServerEvent.RollForward

From current configuration:
Filtered: `Invisible` because the filter level is `Notice`

### cardano.node.chainSync.rollForward

***

***


Dispatched by: 
Cardano.Node.ChainSyncServerBlock.ChainSyncServerEvent.RollForward

From current configuration:
Filtered: `Invisible` because the filter level is `Notice`

### cardano.node.connectedPeers

***
Number of connected peers
***


Dispatched by: 
Cardano.Node.BlockFetchDecision

From current configuration:
Filtered: `Invisible` because the filter level is `Notice`

### cardano.node.connectionManager.duplexConns

***

***


Dispatched by: 
Cardano.Node.ConnectionManager.ConnectionManagerCounters

From current configuration:
Filtered: `Invisible` because the filter level is `Notice`

### cardano.node.connectionManager.duplexConns

***

***


Dispatched by: 
Cardano.Node.LocalConnectionManager.ConnectionManagerCounters

From current configuration:
Filtered: `Invisible` because the filter level is `Notice`

### cardano.node.connectionManager.fullDuplexConns

***

***


Dispatched by: 
Cardano.Node.ConnectionManager.ConnectionManagerCounters

From current configuration:
Filtered: `Invisible` because the filter level is `Notice`

### cardano.node.connectionManager.fullDuplexConns

***

***


Dispatched by: 
Cardano.Node.LocalConnectionManager.ConnectionManagerCounters

From current configuration:
Filtered: `Invisible` because the filter level is `Notice`

### cardano.node.connectionManager.inboundConns

***

***


Dispatched by: 
Cardano.Node.ConnectionManager.ConnectionManagerCounters

From current configuration:
Filtered: `Invisible` because the filter level is `Notice`

### cardano.node.connectionManager.inboundConns

***

***


Dispatched by: 
Cardano.Node.LocalConnectionManager.ConnectionManagerCounters

From current configuration:
Filtered: `Invisible` because the filter level is `Notice`

### cardano.node.connectionManager.outboundConns

***

***


Dispatched by: 
Cardano.Node.ConnectionManager.ConnectionManagerCounters

From current configuration:
Filtered: `Invisible` because the filter level is `Notice`

### cardano.node.connectionManager.outboundConns

***

***


Dispatched by: 
Cardano.Node.LocalConnectionManager.ConnectionManagerCounters

From current configuration:
Filtered: `Invisible` because the filter level is `Notice`

### cardano.node.connectionManager.unidirectionalConns

***

***


Dispatched by: 
Cardano.Node.ConnectionManager.ConnectionManagerCounters

From current configuration:
Filtered: `Invisible` because the filter level is `Notice`

### cardano.node.connectionManager.unidirectionalConns

***

***


Dispatched by: 
Cardano.Node.LocalConnectionManager.ConnectionManagerCounters

From current configuration:
Filtered: `Invisible` because the filter level is `Notice`

### cardano.node.couldNotForgeSlotLast

***

***


Dispatched by: 
Cardano.Node.Forge.NoLedgerState

From current configuration:
Filtered: `Visible` because the filter level is `Info`

### cardano.node.couldNotForgeSlotLast

***

***


Dispatched by: 
Cardano.Node.Forge.NoLedgerView

From current configuration:
Filtered: `Visible` because the filter level is `Info`

### cardano.node.currentKESPeriod

***

***


Dispatched by: 
Cardano.Node.Forge.ForgeStateUpdateError

From current configuration:
Filtered: `Visible` because the filter level is `Info`

### cardano.node.delegMapSize

***

***


Dispatched by: 
Cardano.Node.Forge.StartLeadershipCheckPlus

From current configuration:
Filtered: `Visible` because the filter level is `Info`

### cardano.node.density

***
The actual number of blocks created over the maximum expected number of blocks that could be created over the span of the last @k@ blocks.
***


Dispatched by: 
Cardano.Node.ChainDB.AddBlockEvent.AddedToCurrentChain

From current configuration:
Filtered: `Visible` because the filter level is `Info`

### cardano.node.density

***
The actual number of blocks created over the maximum expected number of blocks that could be created over the span of the last @k@ blocks.
***


Dispatched by: 
Cardano.Node.ChainDB.AddBlockEvent.SwitchedToAFork

From current configuration:
Filtered: `Visible` because the filter level is `Info`

### cardano.node.epoch

***
In which epoch is the tip of the current chain.
***


Dispatched by: 
Cardano.Node.ChainDB.AddBlockEvent.AddedToCurrentChain

From current configuration:
Filtered: `Visible` because the filter level is `Info`

### cardano.node.epoch

***
In which epoch is the tip of the current chain.
***


Dispatched by: 
Cardano.Node.ChainDB.AddBlockEvent.SwitchedToAFork

From current configuration:
Filtered: `Visible` because the filter level is `Info`

### cardano.node.forgedInvalidSlotLast

***

***


Dispatched by: 
Cardano.Node.Forge.ForgedInvalidBlock

From current configuration:
Filtered: `Visible` because the filter level is `Info`

### cardano.node.forgedSlotLast

***

***


Dispatched by: 
Cardano.Node.Forge.ForgedBlock

From current configuration:
Filtered: `Visible` because the filter level is `Info`

### cardano.node.ledgerState

***

***


Dispatched by: 
Cardano.Node.Forge.LedgerState

From current configuration:
Filtered: `Invisible` because the filter level is `Info`

### cardano.node.ledgerView

***

***


Dispatched by: 
Cardano.Node.Forge.LedgerView

From current configuration:
Filtered: `Invisible` because the filter level is `Info`

### cardano.node.mempoolBytes

***
Byte size of the mempool
***


Dispatched by: 
Cardano.Node.Mempool.AddedTx

From current configuration:
Filtered: `Visible` because the filter level is `Info`

### cardano.node.mempoolBytes

***
Byte size of the mempool
***


Dispatched by: 
Cardano.Node.Mempool.ManuallyRemovedTxs

From current configuration:
Filtered: `Visible` because the filter level is `Info`

### cardano.node.mempoolBytes

***
Byte size of the mempool
***


Dispatched by: 
Cardano.Node.Mempool.RejectedTx

From current configuration:
Filtered: `Visible` because the filter level is `Info`

### cardano.node.mempoolBytes

***
Byte size of the mempool
***


Dispatched by: 
Cardano.Node.Mempool.RemoveTxs

From current configuration:
Filtered: `Visible` because the filter level is `Info`

### cardano.node.nodeCannotForge

***

***


Dispatched by: 
Cardano.Node.Forge.NodeCannotForge

From current configuration:
Filtered: `Visible` because the filter level is `Info`

### cardano.node.nodeIsLeader

***

***


Dispatched by: 
Cardano.Node.Forge.NodeIsLeader

From current configuration:
Filtered: `Visible` because the filter level is `Info`

### cardano.node.nodeNotLeader

***

***


Dispatched by: 
Cardano.Node.Forge.NodeNotLeader

From current configuration:
Filtered: `Visible` because the filter level is `Info`

### cardano.node.notAdoptedSlotLast

***

***


Dispatched by: 
Cardano.Node.Forge.DidntAdoptBlock

From current configuration:
Filtered: `Visible` because the filter level is `Info`

### cardano.node.operationalCertificateExpiryKESPeriod

***

***


Dispatched by: 
Cardano.Node.Forge.ForgeStateUpdateError

From current configuration:
Filtered: `Visible` because the filter level is `Info`

### cardano.node.operationalCertificateStartKESPeriod

***

***


Dispatched by: 
Cardano.Node.Forge.ForgeStateUpdateError

From current configuration:
Filtered: `Visible` because the filter level is `Info`

### cardano.node.peerSelection.cold

***
Number of cold peers
***


Dispatched by: 
Cardano.Node.PeerSelectionCounters

From current configuration:
Filtered: `Invisible` because the filter level is `Notice`

### cardano.node.peerSelection.hot

***
Number of hot peers
***


Dispatched by: 
Cardano.Node.PeerSelectionCounters

From current configuration:
Filtered: `Invisible` because the filter level is `Notice`

### cardano.node.peerSelection.warm

***
Number of warm peers
***


Dispatched by: 
Cardano.Node.PeerSelectionCounters

From current configuration:
Filtered: `Invisible` because the filter level is `Notice`

### cardano.node.remainingKESPeriods

***

***


Dispatched by: 
Cardano.Node.Forge.ForgeStateUpdateError

From current configuration:
Filtered: `Visible` because the filter level is `Info`

### cardano.node.served.block

***

***


Dispatched by: 
Cardano.Node.BlockFetchServer.SendBlock

From current configuration:
Filtered: `Invisible` because the filter level is `Notice`

### cardano.node.slotInEpoch

***
Relative slot number of the tip of the current chain within theepoch..
***


Dispatched by: 
Cardano.Node.ChainDB.AddBlockEvent.AddedToCurrentChain

From current configuration:
Filtered: `Visible` because the filter level is `Info`

### cardano.node.slotInEpoch

***
Relative slot number of the tip of the current chain within theepoch..
***


Dispatched by: 
Cardano.Node.ChainDB.AddBlockEvent.SwitchedToAFork

From current configuration:
Filtered: `Visible` because the filter level is `Info`

### cardano.node.slotIsImmutable

***

***


Dispatched by: 
Cardano.Node.Forge.SlotIsImmutable

From current configuration:
Filtered: `Visible` because the filter level is `Info`

### cardano.node.slots

***
Number of slots in this chain fragment.
***


Dispatched by: 
Cardano.Node.ChainDB.AddBlockEvent.AddedToCurrentChain

From current configuration:
Filtered: `Visible` because the filter level is `Info`

### cardano.node.slots

***
Number of slots in this chain fragment.
***


Dispatched by: 
Cardano.Node.ChainDB.AddBlockEvent.SwitchedToAFork

From current configuration:
Filtered: `Visible` because the filter level is `Info`

### cardano.node.submissions.accepted

***

***


Dispatched by: 
Cardano.Node.TxInbound.TxSubmissionProcessed

From current configuration:
Filtered: `Invisible` because the filter level is `Notice`

### cardano.node.submissions.rejected

***

***


Dispatched by: 
Cardano.Node.TxInbound.TxSubmissionProcessed

From current configuration:
Filtered: `Invisible` because the filter level is `Notice`

### cardano.node.submissions.submitted

***

***


Dispatched by: 
Cardano.Node.TxInbound.TxSubmissionCollected

From current configuration:
Filtered: `Invisible` because the filter level is `Notice`

### cardano.node.txsInMempool

***
Transactions in mempool
***


Dispatched by: 
Cardano.Node.Mempool.AddedTx

From current configuration:
Filtered: `Visible` because the filter level is `Info`

### cardano.node.txsInMempool

***
Transactions in mempool
***


Dispatched by: 
Cardano.Node.Mempool.ManuallyRemovedTxs

From current configuration:
Filtered: `Visible` because the filter level is `Info`

### cardano.node.txsInMempool

***
Transactions in mempool
***


Dispatched by: 
Cardano.Node.Mempool.RejectedTx

From current configuration:
Filtered: `Visible` because the filter level is `Info`

### cardano.node.txsInMempool

***
Transactions in mempool
***


Dispatched by: 
Cardano.Node.Mempool.RemoveTxs

From current configuration:
Filtered: `Visible` because the filter level is `Info`

### cardano.node.txsProcessedNum

***

***


Dispatched by: 
Cardano.Node.Mempool.ManuallyRemovedTxs

From current configuration:
Filtered: `Visible` because the filter level is `Info`

### cardano.node.utxoSize

***

***


Dispatched by: 
Cardano.Node.Forge.StartLeadershipCheckPlus

From current configuration:
Filtered: `Visible` because the filter level is `Info`

### mem.resident

***
TODO JNF
***


Dispatched by: 
Cardano.Node.Resources

From current configuration:
Filtered: `Visible` because the filter level is `Info`

### peersFromNodeKernel

***
TODO Doc
***


Dispatched by: 
Cardano.Node.Peers

From current configuration:
Filtered: `Visible` because the filter level is `Notice`

### rts.gcLiveBytes

***
TODO JNF
***


Dispatched by: 
Cardano.Node.Resources

From current configuration:
Filtered: `Visible` because the filter level is `Info`

### rts.gcMajorNum

***
TODO JNF
***


Dispatched by: 
Cardano.Node.Resources

From current configuration:
Filtered: `Visible` because the filter level is `Info`

### rts.gcMinorNum

***
TODO JNF
***


Dispatched by: 
Cardano.Node.Resources

From current configuration:
Filtered: `Visible` because the filter level is `Info`

### rts.gcticks

***
TODO JNF
***


Dispatched by: 
Cardano.Node.Resources

From current configuration:
Filtered: `Visible` because the filter level is `Info`

### rts.mutticks

***
TODO JNF
***


Dispatched by: 
Cardano.Node.Resources

From current configuration:
Filtered: `Visible` because the filter level is `Info`

### rts.threads

***
TODO JNF
***


Dispatched by: 
Cardano.Node.Resources

From current configuration:
Filtered: `Visible` because the filter level is `Info`

### stat.cputicks

***
Reports the CPU ticks, sice the process was started
***


Dispatched by: 
Cardano.Node.Resources

From current configuration:
Filtered: `Visible` because the filter level is `Info`

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


Configuration: TraceConfig {tcOptions = fromList [([],[ConfBackend [Stdout MachineFormat,EKGBackend],ConfDetail DNormal,ConfSeverity Notice]),(["Node","AcceptPolicy"],[ConfSeverity Info]),(["Node","BlockFetchClient","CompletedBlockFetch"],[ConfLimiter "CompletedBlockFetchLimiter" 2.0]),(["Node","ChainDB"],[ConfSeverity Info]),(["Node","ChainDB","AddBlockEvent","AddBlockValidation","ValidCandidate"],[ConfLimiter "ValidCandidateLimiter" 2.0]),(["Node","ChainDB","AddBlockEvent","AddedBlockToQueue"],[ConfLimiter "AddedBlockToQueueLimiter" 2.0]),(["Node","ChainDB","AddBlockEvent","AddedBlockToVolatileDB"],[ConfLimiter "AddedBlockToVolatileDBLimiter" 2.0]),(["Node","ChainDB","CopyToImmutableDBEvent","CopiedBlockToImmutableDB"],[ConfLimiter "CopiedBlockToImmutableDBLimiter" 2.0]),(["Node","DNSResolver"],[ConfSeverity Info]),(["Node","DNSSubscription"],[ConfSeverity Info]),(["Node","DiffusionInit"],[ConfSeverity Info]),(["Node","ErrorPolicy"],[ConfSeverity Info]),(["Node","Forge"],[ConfSeverity Info]),(["Node","IpSubscription"],[ConfSeverity Info]),(["Node","LocalErrorPolicy"],[ConfSeverity Info]),(["Node","Mempool"],[ConfSeverity Info]),(["Node","Resources"],[ConfSeverity Info])], tcForwarder = TraceOptionForwarder {tofAddress = LocalSocket "/tmp/forwarder.sock", tofMode = Initiator, tofConnQueueSize = 2000, tofDisconnQueueSize = 200000}, tcNodeName = Nothing, tcPeerFreqency = Just 2000, tcResourceFreqency = Just 5000}

549 log messages.
Generated at 2022-01-14 22:58:35.715578147 MSK.