package network.manager.JVM

import language._
import Dependencies._
import com.typesafe.scalalogging.Logger
import network.manager.JVM.NetworkInterfaceReader.NoActiveNetworkInterfaces

/** An implementation of a Network Information provider for use in the Cluster */

case class JVMNetworkManager(remoteNetworking: NetworkDiscovery) extends NetworkManager with NetworkInterfaceReader {

  import JVMNetworkManager._

  /**
    * Retrieves all configured IP's on the host system.
    *
    * @return
    * [[Vector Vector]] of IP addresses represented in String format.
    * @throws NoActiveNetworkInterfaces
    * Will be thrown in the case where no active network interfaces are
    * detected on the host system.
    */

  override def getHostIPs: Vector[IP] = {

    logger info "Finding all configured network interface IP's ... "
    val allInterfaceIPs = retrieveIPs(withNoFilter)
    logger info s"Found interfaces: $allInterfaceIPs, on host."
    allInterfaceIPs
  }

  /**
    * Retrieves all configured IP's on the host system which match the predicate supplied.
    *
    * @return
    * [[Vector Vector]] of IP addresses represented in String format.
    * @param satisfyThisCondition
    * HOF which specifies the criteria by which an IP will be included in the return Vector.
    * A mapping to True implies the IP will be included in the return Vector.
    * @throws NoAddressMatchesFilter
    * Will be thrown in the case where no configured network interfaces
    * satisfy the filter supplied.
    * @throws NoActiveNetworkInterfaces
    * Will be thrown in the case where no active network interfaces are
    * detected on the host system.
    */

  override def getHostIPsWhich(satisfyThisCondition: IPFilter): Vector[IP] = {

    logger info s"Finding all configured network interface IP's which match the predicate $satisfyThisCondition ... "
    val allNetworkInterfaceIPsMatchingPredicate = retrieveIPs(which(satisfyThisCondition))
    logger info s"Found interfaces: $allNetworkInterfaceIPsMatchingPredicate, matching predicate on host."
    allNetworkInterfaceIPsMatchingPredicate
  }

  /**
    * Discovers and returns all adjacent devices on the network.
    *
    * @return
    * [[Vector Vector]] of IP addresses represented in String format.
    * @throws NoAddressMatchesFilter
    * Will be thrown in the case where no configured network interfaces
    * satisfy the filter supplied.
    * @throws NoActiveNetworkInterfaces
    * Will be thrown in the case where no active network interfaces are
    * detected on the host system.
    */

  override def getAllAdjacentDeviceIPs(runningOnPort: Port): Set[IP] = {

    logger info s"Scanning for adjacent device IP's on network on port $runningOnPort ... "
    val peersFound = remoteNetworking findPeersOnNetwork runningOnPort
    logger info s"Marking $peersFound as peers on the network."
    peersFound
  }

  /**
    * Starts a socket on this Node, which signifies its presence on the NW.
    */

  override def listenForJoiners(onPort: Port): ListenerServerThread = {

    logger info s"Starting listener server on port $onPort listening for joiners to network."
    remoteNetworking listenForJoiningNodes onPort
  }

  /**
    * Speaks to the NetworkInterfaceReader to return IP's of configured interfaces on the host system.
    *
    * @param possibleFilter
    *
    * Optional filter which will specify which returned configured IP's will
    * be returned to the caller as possible candidates.  A mapping from String => True
    * implies the IP will be returned.
    */

  private def retrieveIPs(possibleFilter: Option[IPFilter]): Vector[IP] = {

    /** Unpacks the optional filtering method.  If None, defaults to accepting all IP's. */

    val satisfiesFilter = possibleFilter getOrElse ((noFilter: IP) => true)

    /** Returns all IP's which are configured on the host, which also pass the filter supplied. */

    def getEligibleIPs = for {ip <- networkInterfaces if satisfiesFilter(ip)} yield ip

    getEligibleIPs match {
      case Vector() => throw NoAddressMatchesFilter("None of the available IP's match the filter supplied.")
      case foundIPs => foundIPs
    }
  }
}

/** Companion object for JVMNetworkManager to define custom Types and Exceptions. */

object JVMNetworkManager {

  val logger = Logger[JVMNetworkManager]
  case class NoAddressMatchesFilter(message: String) extends Exception

  val which: (IPFilter) => Option[IPFilter] = filterCondition => Some(filterCondition)
  val withNoFilter = None
}