
package object Dependencies {

  import scala.concurrent.Future

  type Port = Int
  type Log = String
  type IPFilter = IP => Boolean
  type ListenerServerThread = Future[Unit]

  trait NetworkManager {
    def listenForJoiners(portToListenOn: Port): ListenerServerThread
    def getAllAdjacentDeviceIPs(runningOnPort: Port): Set[IP]
    def getHostIPs: Vector[IP]
    def getHostIPsWhich(satisfyThisCondition: IP => Boolean): Vector[IP]
  }

  trait NetworkDiscovery {
    def findPeersOnNetwork(onPort: Port): Set[IP]
    val listenForJoiningNodes: Port => ListenerServerThread
  }

  abstract case class IP(address: String, cidr: Short) {
    def >=(that         : IP) : Boolean
    def >(that          : IP) : Boolean
    def <=(that         : IP) : Boolean
    def <(that          : IP) : Boolean
    def +(inc           : Int): IP
    def -(dec           : Int): IP
    def to(here         : IP) : Vector[IP]
    def networkAddress  : IP
    def broadcastAddress: IP
  }

}