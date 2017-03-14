package network.utils

import org.scalatest.{FeatureSpec, GivenWhenThen}

import scala.util.{Failure, Success, Try}

class IPv4tests extends FeatureSpec with GivenWhenThen {

  info("These tests ensure correct functioning of the IP utility class.")

  feature("Ability to extract useful features from the IP object.") {

    scenario("User wishes to extract addressing information.") {

      Given("there is a valid IP object created")

      val validIP = IPv4("192.168.1.1", 24)

      When("the address information is extracted")

      val address = validIP.address

      Then("the address should be returned as a String")

      assert(address == "192.168.1.1")

    }

    scenario("User wishes to extract cidr information.") {

      Given("there is a valid IP object created")

      val validIP = IPv4("192.168.1.1", 24)

      When("the cidr information is extracted")

      val cidr = validIP.cidr

      Then("the cidr should be returned as a Short")

      assert(cidr == 24)

    }

    scenario("User wishes to extract a range of IP's between two valid IP's.") {

      Given("there are two valid IP objects created")

      val base = IPv4("192.168.1.9", 24)
      val limit = IPv4("192.168.1.13", 24)

      When("the range is requested")

      val range = base to limit

      Then("the range should match the expected")

      val firstOcts = "192.168.1"

      val expectedRange = Vector(
        IPv4(s"$firstOcts.9" , 24),
        IPv4(s"$firstOcts.10", 24),
        IPv4(s"$firstOcts.11", 24),
        IPv4(s"$firstOcts.12", 24),
        IPv4(s"$firstOcts.13", 24)
      )

      assert(range == expectedRange)

    }

    scenario("User requests a range of IP's, but in descending order.") {

      Given("there are two valid IP objects created")

      val bottom = IPv4("192.168.1.9", 24)
      val top = IPv4("192.168.1.13", 24)

      When("the range is requested from top to bottom")

      val range = top to bottom

      Then("the range should match the expected")

      val firstOcts = "192.168.1"

      val expectedRange = Vector(
        IPv4(s"$firstOcts.13" , 24),
        IPv4(s"$firstOcts.12", 24),
        IPv4(s"$firstOcts.11", 24),
        IPv4(s"$firstOcts.10", 24),
        IPv4(s"$firstOcts.9", 24)
      )

      assert(range == expectedRange)

    }

    scenario("User requests a range of IP's, but the two IPs specified are the same.") {

      Given("there are two valid IP objects created")

      val same = IPv4("192.168.1.9", 24)
      val as = IPv4("192.168.1.9", 24)

      When("the range is requested with two identical IP's")

      val range = same to as

      Then("the range should just be the IP passed in.")

      val firstOcts = "192.168.1"

      val expected = Vector(IPv4("192.168.1.9", 24))

      assert(range == expected)

    }

    scenario("User wishes to extract the broadcast address for a given IP.") {

      Given("there is a valid IP object created")

      val validIP = IPv4("192.168.1.9", 24)

      When("the broadcast address is requested")

      val broadcast = validIP.broadcastAddress

      Then("the address should match the expected")

      assert(broadcast == IPv4("192.168.1.255", 24))

    }

    scenario("User wishes to extract the network address for a given IP.") {

      Given("there is a valid IP object created")

      val validIP = IPv4("192.168.1.9", 24)

      When("the network address is requested")

      val networkAddress = validIP.networkAddress

      Then("the address should match the expected")

      assert(networkAddress == IPv4("192.168.1.0", 24))

    }

    scenario("User wishes to extract the IP in cidr notation as a string") {

      Given("there is a valid IP object created")

      val validIP = IPv4("192.168.1.9", 24)

      When("toString is called.")

      val cidrNotation = validIP.toString

      Then("the address should match the expected")

      assert(cidrNotation == "192.168.1.9/24")

    }

    scenario("User wishes to extract the broadcast and network address using the unapply method in a pattern match.") {

      Given("there is a valid IP object created")

      val validIP = IPv4("192.168.1.9", 24)

      When("the IPExtractor is invoked for the network and broadcast address.")

      val (network, broadcast) = validIP match {
        case IPExtractor(networkAddress, broadcastAddress) => (networkAddress, broadcastAddress)
      }

      Then("the addresses should match the expected for this subnet.")

      assert(network == IPv4("192.168.1.0", 24) && broadcast == IPv4("192.168.1.255", 24))

    }

    scenario("User wishes to turn a collection of IPs into a collection the ranges of the subnets which the IPs exist in.") {

      Given("the user has a collection of IP addresses")

      val exampleOfConfiguredNetworkInterfaces =
          Vector(IPv4("192.168.1.1", 24),
            IPv4("192.168.2.66", 24),
            IPv4("192.168.3.24", 24)
          )

      When("the the user asks for the subnet ranges in which the IPs exist")

      val ranges = IPUtils startAndFinishOfEachSubnet exampleOfConfiguredNetworkInterfaces

      Then("the ranges should start at the first available host of the network, and end at the last")
      And("not include broadcast and network addresses for that subnet in the scan")

      assert(ranges == Vector(
        (IPv4("192.168.1.1", 24), IPv4("192.168.1.254", 24)),
        (IPv4("192.168.2.1", 24), IPv4("192.168.2.254", 24)),
        (IPv4("192.168.3.1", 24), IPv4("192.168.3.254", 24)))
      )
    }

  }

  feature("Ability to perform arithmetic and logical operations on IP's.") {

    scenario("User wishes to increment the IP by an arbitrary amount.") {

      Given("there is a valid IP object created")

      val validIP = IPv4("192.168.1.9", 24)

      When("2 is added to the IP")

      val newIP = validIP + 2

      Then("the address should match the expected")

      assert(newIP == IPv4("192.168.1.11", 24))

    }

    scenario("User wishes to decrement the IP by an arbitrary amount.") {

      Given("there is a valid IP object created")

      val validIP = IPv4("192.168.1.11", 24)

      When("2 is added to the IP")

      val newIP = validIP - 2

      Then("the address should match the expected")

      assert(newIP == IPv4("192.168.1.9", 24))

    }

    scenario("User wishes to compare two IP's") {

      Given("there are two valid IP objects created")

      val greaterOfTwo = IPv4("192.168.1.12", 24)
      val lesserOfTwo = IPv4("192.168.1.11", 24)
      val equalsTheLesser = IPv4("192.168.1.11", 24)

      When("they are compared in multiple ways")

      val greaterThanPositive       = greaterOfTwo > lesserOfTwo
      val greaterEqaulThanPositive  = lesserOfTwo >= equalsTheLesser
      val lessThanPositive          = lesserOfTwo < greaterOfTwo
      val lessEqualThanPositive     = lesserOfTwo <= equalsTheLesser

      val greaterThanNegative       = lesserOfTwo > greaterOfTwo
      val greaterEqaulThanNegative  = lesserOfTwo >= greaterOfTwo
      val lessThanNegative          = greaterOfTwo < lesserOfTwo
      val lessEqualThanNegative     = greaterOfTwo <= lesserOfTwo

      Then("the operations should have produced the expected results.")

      assert(greaterThanPositive && greaterEqaulThanPositive && lessThanPositive && lessEqualThanPositive &&
              !greaterThanNegative && !greaterEqaulThanNegative && !lessThanNegative && !lessEqualThanNegative)

    }

  }

  feature("Provide suitable errors when invalid operations or IP's are requested.") {

    scenario("User supplies an IP in an incorrect format.") {

      Given("the user has an IP in an incorrect format")

      val invalidIP = "192.168.1.9."

      When("the user tries to instantiate an IP object.")

      val instance = Try(IPv4(invalidIP, 24))

      Then("an error should have been thrown.")

      val errorWasThrown = instance match {
        case Success(_) => false
        case Failure(_) => true
      }

      assert(errorWasThrown)

    }

    scenario("User supplies an IP in the correct format, but it does not fall within the valid ranges.") {

      Given("the user has an invalid IP")

      val invalidIP = "500.123.1.11"

      When("the user tries to instantiate an IP object")

      val error = Try(IPv4(invalidIP, 24))

      Then("an exception should be thrown.")

      val errorWasThrown = error match {
        case Success(_) => false
        case Failure(_) => true
      }

      assert(errorWasThrown)

    }

    scenario("User tries to perform equality on a type unrelated to IP's") {

      Given("the user has an valid IP")

      val validIp = IPv4("192.123.1.11", 24)

      When("the user tries to perform equality on a Vector object")

      val error = Try(validIp == Vector(1, 2, 3))

      Then("an exception should be thrown.")

      val errorWasThrown = error match {
        case Success(_) => false
        case Failure(e) => Console.println(e) ; true
      }

      assert(errorWasThrown)

    }

  }

}
