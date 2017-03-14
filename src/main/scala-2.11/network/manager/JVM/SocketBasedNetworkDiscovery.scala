package network.manager.JVM

import language._
import network.utils.IPUtils
import scala.concurrent.Future
import scala.annotation.tailrec
import java.util.NoSuchElementException
import com.typesafe.scalalogging.Logger
import scala.util.{Failure, Success, Try}
import java.net.{InetSocketAddress, ServerSocket}
import scala.concurrent.ExecutionContext.Implicits.global
import Dependencies.{IP, ListenerServerThread, NetworkDiscovery, Port}

/** An implementation of a Network Discovery Component, which uses [[java.net]] Sockets and ServerSockets to ping peers. */

case class SocketBasedNetworkDiscovery() extends NetworkDiscovery with NetworkInterfaceReader {

  import SocketBasedNetworkDiscovery._

  /**
    * Curried function which will take a port and attempt to start a ServerSocket in a thread
    * which listens for potential joiners to the system on the network.
    *
    * @return
    * [[Future[Unit] ServerThread]] which runs in the background on this object listening for new joiners.
    */

  override val listenForJoiningNodes: Port => ListenerServerThread = {
    (listenOn: Port) => initializeServer(listenOn)
  }

  /**
    * Function which reaches out to all nodes on all configured subnets.  Those potential peers
    * which are connected to successfully on the port specified, will be included in the return
    * Set of the function as peers for this system.
    *
    * toSet is called as a final step to remove duplicates in the collection.
    *
    * @return
    * [[Set Set]] of all [[Dependencies.IP IP]] addresses which were reachable via sockets on the network.
    * @param whoShouldBeListeningOn
    * This specifies the port on which the other potential peers on the network may be listening.
    */

  override def findPeersOnNetwork(whoShouldBeListeningOn: Port): Set[IP] = {
    probeNetworkForListenerServers(whoShouldBeListeningOn) toSet
  }

  /**
    * This function will initialize a [[Future Future]] containing a running [[ServerSocket ServerSocket]].
    * This [[ServerSocket ServerSocket]] will block and wait until a connection attempt is
    * made to it.
    *
    * A successful connection attempt to this ServerSocket, indicates to the connecting part that
    * they should add this Nodes IP to their list of peers.
    *
    * @return
    * [[Set Set]] of all [[IP IP]] addresses which were reachable via sockets on the network.
    * @param listOnPort
    * This specifies the port on which the [[ServerSocket ServerSocket]] will listen for connections.
    */

  protected def initializeServer(listOnPort: Port) = Future {

    /**
      * A tail recursive function which will block and wait until a connection attempt is made to it
      * @param onServer The server on which to await connections.
      */

    @tailrec def loopWaitingForJoiners(onServer: ServerSocket): Unit = {
      val joiner = onServer.accept.close()
      logger info s"Listener Network Server Pinged by: $joiner"
      loopWaitingForJoiners(onServer)
    }

    val server = new ServerSocket(listOnPort)
    loopWaitingForJoiners(server)

  }

  protected def probeNetworkForListenerServers(onPort: Port): Vector[IP] = findOutReachablePeers(onPort) match {
    case Vector() => throw new NoSuchElementException("There were no peers found on the network")
    case seedNodesFound => seedNodesFound
  }

  /**
    * Function which finds out if an [[IP IP]] on the network is contactable on the specified port.
    *
    * @param onHost Host [[IP IP]] to attempt a connection to.
    * @param port   [[Port Port]] on which to attempt the connection.
    * @return       Boolean true for successful attempt, false for unsuccessful.
    */

  protected def ableToContact(onHost: IP, port: Port) = {

    logger debug s"Trying host: $onHost"

    ConnectionAttemptWithTimeout(onHost, port) match {
      case Success(considerReachable)   => logger debug s"Considering host: ${onHost} reachable ... "   ; true
      case Failure(considerUnreachable) => logger debug s"Considering host: ${onHost} unreachable ... " ; false
    }
  }

  /**
    * This function will find out all of the reachable Peers on the cluster, by performing the
    * following steps:
    *
    *   1. Generate Start and End of each network interface subnet.
    *   2. Generate a list of [[IP IP's]] between those ranges.
    *   3. Filter all [[IP IP]] ranges by using the services of the [[ableToContact() ableToContact]] function.
    *
    * @param onPort   [[Port Port]] on which to attempt the connections.
    * @return         [[Vector Vector]] of [[IP IP's]] which were successfully connected to.
    */

  protected def findOutReachablePeers(onPort: Port): Vector[IP] = (for {
    allAvailableIPs <- IPUtils startAndFinishOfEachSubnet networkInterfaces par;
    potentialPeer   <- allAvailableIPs match { case (start, finish) => start to finish par }
    if ableToContact(potentialPeer, onPort)
  } yield potentialPeer) seq

}

object SocketBasedNetworkDiscovery {

  val logger = Logger[SocketBasedNetworkDiscovery]

}

/** A convenience object in which a connection attempt to an [[Dependencies.IP IP]] on a Port is attempted */

object ConnectionAttemptWithTimeout {

  import java.net.Socket

  def apply(ip: IP, port: Int): Try[Unit] = Try {
    val connection: Socket = new Socket()
    connection.connect(new InetSocketAddress(ip.address, port), 1000)
    connection.close()
  }

}

