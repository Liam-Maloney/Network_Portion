package network.manager.JVM

import network.utils.IPv4
import Dependencies.{IP, NetworkManager}
import scala.util.{Failure, Success, Try}
import org.scalatest.{FeatureSpec, GivenWhenThen}
import network.manager.JVM.JVMNetworkManager.NoAddressMatchesFilter
import network.manager.JVM.NetworkInterfaceReader.NoActiveNetworkInterfaces

class JVMNetworkingTests extends FeatureSpec with GivenWhenThen {

  info("This test suit will ensure that functionality is correct when using the JVM supplied networking information.")

  feature("Ability to return the available networking interfaces on the system with or without filtering.") {

    scenario("Node has more than one active network interface, and a prefix filter is supplied.") {

      Given("that there is more than one active network interface on the node.")

        val mockedAvailableNetworkInterfaces: NetworkManager = new JVMNetworkManager(null) {
          override def networkInterfaces =
            Vector(IPv4("192.168.123.123", 16), IPv4("200.200.200.200", 16))
        }

      When("a request is made for an IP beginning with a certain prefix.")

        val returnedIPs: Try[Vector[IP]] = Try(mockedAvailableNetworkInterfaces getHostIPsWhich (_.address startsWith "192"))

      Then("the value which matches the prefix supplied should be returned.")

        assert(
          returnedIPs match {
            case Success(results) => results == Vector(IPv4("192.168.123.123", 16))
            case Failure(_) => false
          }
        )

    }

    scenario("Node has more than one active network interface, and a prefix filter is not supplied.") {

      Given("that there is more than one active network interface on the node.")

      val JVMNetworkManager: NetworkManager = new JVMNetworkManager(null) {
        override def networkInterfaces =
          Vector(IPv4("192.168.123.123", 16), IPv4("200.200.200.200", 16))
      }

      When("a request is made for the host IP without a prefix supplied.")

      val returnedIPs = Try(JVMNetworkManager getHostIPs)

      Then("then all available IP's should be returned.")

      assert(returnedIPs match {
        case Success(addresses)=> addresses == Vector(IPv4("192.168.123.123", 16), IPv4("200.200.200.200", 16))
        case Failure(_) => false
      })
    }

    scenario("Node has only one active network interface, and a prefix is supplied.") {

      Given("that there is only one active network interface.")

        val mockedAvailableNetworkInterfaces: NetworkManager = new JVMNetworkManager(null) {
          override def networkInterfaces =
          Vector(IPv4("192.168.123.123", 16))
        }

      When("a request is made for the host IP with a prefix filter supplied.")

        val returnedIPs = Try(mockedAvailableNetworkInterfaces getHostIPsWhich (_.address startsWith "192"))

      Then("the address which matches the prefix should be returned.")

      assert(returnedIPs match {
        case Success(addresses)=> addresses == Vector(IPv4("192.168.123.123", 16))
        case Failure(_) => false
      })
    }

    scenario("Node has only one active network interface, and a prefix is not supplied.") {

      Given("that there is only one active network interface.")

        val mockedAvailableNetworkInterfaces: NetworkManager = new JVMNetworkManager(null) {
          override def networkInterfaces =
            Vector(IPv4("192.168.123.123", 16))
        }

      When("a request is made for the host IP with no prefix filter.")

        val returnedIPs = Try(mockedAvailableNetworkInterfaces getHostIPs)

      Then("the only available address should be returned as is.")

        assert(returnedIPs match {
          case Success(addresses)=> addresses == Vector(IPv4("192.168.123.123", 16))
          case Failure(_) => false
        })
    }

    scenario("Node does not have any active network interfaces, and a prefix is supplied.") {

      Given("that there are no active network interfaces on the host.")

        val mockedAvailableNetworkInterfaces: NetworkManager = new JVMNetworkManager(null) {
          override def networkInterfaces =
            throw NoActiveNetworkInterfaces("There were no active network interfaces detected.")
        }

      When("a request is made for the host IP with a prefix filter supplied.")

        val addresses = Try(mockedAvailableNetworkInterfaces getHostIPsWhich (_.address startsWith "192"))

      Then("an exception should be returned.")

        assert(
          addresses match {
            case Failure(shouldBe: NoActiveNetworkInterfaces) =>
              shouldBe.message == "There were no active network interfaces detected."
            case Success(_) => false
          }
        )
    }

    scenario("Node does not have any active network interfaces, and a prefix is not supplied.") {

      Given("that there are no active network interfaces on the host.")

        val mockedAvailableNetworkInterfaces: NetworkManager = new JVMNetworkManager(null) {
          override def networkInterfaces =
            throw NoActiveNetworkInterfaces("There were no active network interfaces detected.")
        }

      When("a request is made for the host IP with no prefix filter supplied.")

        val addresses = Try(mockedAvailableNetworkInterfaces getHostIPs)

      Then("an exception should be returned.")

        assert(
          addresses match {
            case Failure(shouldBe: NoActiveNetworkInterfaces) =>
              shouldBe.message == "There were no active network interfaces detected."
            case Success(_) => false
          }
        )
    }

    scenario("Node has network interfaces available, but none match the prefix supplied.") {

      Given("that there are network interfaces available on the host")

        val mockedAvailableNetworkInterfaces: NetworkManager = new JVMNetworkManager(null) {
          override def networkInterfaces =
            Vector(IPv4("192.168.1.1", 16), IPv4("192.168.1.2", 16))
        }

      When("a request is made for the host IP with a prefix filter supplied,")
      And("none of the addresses match the filter.")

        val addresses = Try(mockedAvailableNetworkInterfaces getHostIPsWhich (_.address startsWith "200"))

      Then("an exception should be returned.")

        assert(
          addresses match {
            case Failure(shouldBe: NoAddressMatchesFilter) =>
              shouldBe.message == "None of the available IP's match the filter supplied."
            case Success(_) => false
          }
        )
    }

    scenario("Node has more than one network interfaces available, and more than 1 match prefix supplied.") {

      Given("that there are more than one network interfaces available.")

        val mockedAvailableNetworkInterfaces: NetworkManager = new JVMNetworkManager(null) {
          override def networkInterfaces =
            Vector(IPv4("192.168.1.1", 16), IPv4("192.168.1.2", 16), IPv4("200.200.200.200", 16))
        }

      When("a request is made for the host IP with a prefix filter supplied.")
      And("more than one of the addresses match the filter.")

        val addresses = Try(mockedAvailableNetworkInterfaces getHostIPsWhich (_.address startsWith "192"))

      Then("then all addresses which match the filter should be returned.")

        assert(
          addresses match {
            case Success(result) => result == Vector(IPv4("192.168.1.1", 16), IPv4("192.168.1.2", 16))
            case Failure(_) => false
          }
        )
    }
  }

  feature("Ability to notify presence to nodes trying to join the cluster") {

    scenario("A node pings this node in an attempt to discover a running instance on the cluster.") {

      Given("that ")

      val mockedAvailableNetworkInterfaces: NetworkManager = new JVMNetworkManager(null) {
        override def networkInterfaces =
          Vector(IPv4("192.168.123.123", 16), IPv4("200.200.200.200", 16))
      }

      When("a request is made for an IP beginning with a certain prefix.")

      val returnedIPs: Try[Vector[IP]] = Try(mockedAvailableNetworkInterfaces getHostIPsWhich (_.address startsWith "192"))

      Then("the value which matches the prefix supplied should be returned.")

      assert(
        returnedIPs match {
          case Success(results) => results == Vector(IPv4("192.168.123.123", 16))
          case Failure(_) => false
        }
      )

    }

  }


}