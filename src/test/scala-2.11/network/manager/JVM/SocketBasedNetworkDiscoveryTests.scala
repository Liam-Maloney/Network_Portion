package network.manager.JVM

import Dependencies.{IP, Port}
import network.utils.IPv4
import org.scalatest.{FeatureSpec, GivenWhenThen}

import scala.language._
import scala.util.{Failure, Success, Try}


class SocketBasedNetworkDiscoveryTests extends FeatureSpec with GivenWhenThen {

  import SocketBasedNetworkDiscoveryTests._

  info("Tests to ensure the correct functionality of the logic which decides the subnets to scan.")

  feature("Ability add Nodes which respond to a connection attempt to the Set of Peers on the network") {

    scenario("There are no available peers on the network") {

      Given("There exist no peers on the network.")

      val underTest = new SocketBasedNetworkDiscovery with NetworkInterfaceReader {
        override def networkInterfaces = Vector(IPv4("192.168.1.1", 29))
        override protected def ableToContact(onHost: IP, port: Port): Boolean = false
      }

      When("this node attempts to ping the subnet which is configured on its network interface in search of Peers.")

      val discoveredPeers = Try(underTest findPeersOnNetwork onMockPort)

      Then("the result should be an exception.")

      assert(
        discoveredPeers match {
          case Success(_)       => false
          case Failure(noPeers) => true
        }
      )

    }

    scenario("There is one available peer on the network") {

      Given("There exist some peers on the network which will be scanned.")
      And("this node is actively listening for joiners.")

      val underTest = new SocketBasedNetworkDiscovery with NetworkInterfaceReader {
        override def networkInterfaces = Vector(IPv4("192.168.1.1", 29))
        override def ableToContact(onHost: IP, port: Port): Boolean = onHost.address == "192.168.1.2"
      }

      When("this node attempts to ping the subnet which is configured on its network interface in search of Peers.")

      val discoveredPeers: Set[IP] = underTest findPeersOnNetwork onMockPort

      Then("the result should contain that nodes IP.")

      assert(discoveredPeers == Set(IPv4("192.168.1.2", 29)))

    }

    scenario("There are multiple available peers on the network") {

      Given("There exists more than one peer on the network which will be scanned.")

      val underTest = new SocketBasedNetworkDiscovery with NetworkInterfaceReader {

        override def networkInterfaces = Vector(IPv4("192.168.1.1", 27))

        override def ableToContact(onHost: IP, port: Port) =
          onHost match {
            case IPv4(ip, cidr) => (ip == "192.168.1.3" || ip == "192.168.1.4") && cidr == 27
          }

      }

      When("this node attempts to ping the subnet which is configured on its network interface in search of Peers.")

      val discoveredPeers: Set[IP] = underTest findPeersOnNetwork onMockPort

      Then("the result should contain the nodes which pass the connection filter.")

      assert(discoveredPeers == Set(IPv4("192.168.1.3", 27), IPv4("192.168.1.4", 27)))

    }

  }

}

object SocketBasedNetworkDiscoveryTests {

  val onMockPort = 30001
}