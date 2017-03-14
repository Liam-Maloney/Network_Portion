package network.utils

import language._
import Dependencies.IP
import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

/**
  * The following IP class provides a useful encapsulation of data and feature extraction when
  * working with IP addresses.
  *
  * @param address  A string representation of the IP.  EG: "192.168.1.1"
  * @param cidr     The cidr notation for representing the subnet. EG: 24, to denote that the IP belongs to /24 subnet.
  */

class IPv4(override val address: String, override val cidr: Short) extends IP(address, cidr) {

  /**
    * Defines greater than or equal to in terms of less than.
    *
    * @param that IP to compare this to.
    */

  override def >=(that: IP): Boolean = !(this < that)

  /**
    * Defines greater than in terms of less than.
    *
    * @param that IP to compare this to.
    */

  override def >(that: IP): Boolean = !(this <= that)

  /**
    * Defines less than or equal to in terms of equals and less than.
    *
    * @param that IP to compare this to.
    */

  override def <=(that: IP): Boolean = (this equals that) || (this < that)

  /**
    * The less than function is used to define less than, and all other comparison operators for the IP class.
    *
    * This function will progressively test the octets in this and that from the most significant octet value
    * to the least significant octet value.  Indexing into each octet is needed, as the JVM does not support
    * the concept of unsigned numbers greater than 16 bits (as of the Java 8 bytecode spec), where chars are
    * the only such example of an unsigned integer.
    *
    * @param that IP to compare this to.
    */

  override def <(that: IP): Boolean = {

    val numOctets = 5

    @tailrec
    def testMostSignificantToLeastSignificantOctet(i: Int): Boolean = {
      i match {
        case testedAllOctets if numOctets == i => false
        case _ =>
          readOctet(i)(addressBits) < readOctet(i)(that.asInstanceOf[IPv4].addressBits) match {
            case false  => testMostSignificantToLeastSignificantOctet(i + 1)
            case _      => true
          }
      }
    }
    testMostSignificantToLeastSignificantOctet(1)
  }

  /**
    * Increment of an IP is performed by adding the desired increment to the bitmapped representation of the
    * address String.
    *
    * @param inc  Defines the number to increment the address by.
    * @return     A new IP instance representing the incremented to IP.
    */

  override def +(inc: Int) = IPv4(bitmapToString(addressBits + inc), cidr)

  /**
    * Decrement of an IP is performed by subtracting the desired decrement to the bitmapped representation of the
    * address String.
    *
    * @param dec  Defines the number to decrement the address by.
    * @return     A new IP instance representing the decremented to IP.
    */

  override def -(dec: Int) = IPv4(bitmapToString(addressBits - dec), cidr)

  /**
    * The to function will produce a range of IP objects in either ascending or descending format.
    * The collection is handled using tail recursion, with each recursive call either incrementing or
    * decrementing the previous IP has been reached.
    *
    * @param here   The [[IP IP]] to generate the range up to.
    * @return       A [[Vector Vector]] of [[IP IP]] objects which exist in the range.
    */

  override def to(here: IP): Vector[IP] = {

    @tailrec
    def collect(rangeAcc: Vector[IP], incOrDec: IP => IP): Vector[IP] = {
      rangeAcc.last == here match {
        case true   => rangeAcc
        case false  => collect(rangeAcc :+ incOrDec(rangeAcc.last), incOrDec)
      }
    }

    this equals here match {
      case true => Vector(this)
      case false =>
        this < here match {
          case true => collect(Vector(this), (ip: IP) => ip + 1)
          case false => collect(Vector(this), (ip: IP) => ip - 1)
        }
    }
  }

  /**
    * Extracts the network address of an IP by initializing a new [[IP IP]] object with the String
    * representation of the network address bit pattern.
    *
    * @return [[IP IP]] object representing the network address of this IP object.
    */

  override def networkAddress = IPv4(bitmapToString(networkBitPattern), cidr)

  /**
    * Extracts the broadcast address of an IP by initializing a new [[IP IP]] object with the String
    * representation of the broadcast address bit pattern.
    *
    * @return [[IP IP]] object representing the broadcast address of this IP object.
    */

  override def broadcastAddress = IPv4(bitmapToString(broadcastBitPattern), cidr)

  /**
    * Override of the equals method which carries out comparison via equality check
    * of both the cidr value, and address bit pattern of this and that IP object.
    *
    * @param other  The target [[IP IP]] to compare this IP to.
    * @return       True if both address bitmap, and cidr are equal.
    */

  override def equals(other: scala.Any) = {
    Try(other.asInstanceOf[IPv4]) match {
      case Success(otherIp) => this.addressBits == otherIp.addressBits && this.cidr == otherIp.cidr
      case Failure(_)       => throw new Exception("Object must be of type IP for correct equality check.")
    }
  }

  /**
    * @return [[String String]] representation of the IP object in cidr notation
    * @example IP("192.168.1.1", 24).toString == "192.168.1.1/24"
    */

  override def toString = bitmapToString(this addressBits) + s"/$cidr"

  /**
    * Defines the network bits by shifting a 32bit all 1 bit pattern left by an
    * amount dictated by the cidr value.
    */

  private def networkBits = 0xFFFFFFFF << (32 - cidr)

  /**
    * Converts a bit mapped version of a 32 bit ip address to its string 4 octet counterpart.
    *
    * @param targetAddressBits  A 32 bit, bit mapped representation of an IP address.
    * @return                   String representation of the IP object.
    */

  private def bitmapToString(targetAddressBits: Int) = {
    (1 to 4 foldLeft "") {_ + readOctet(_) (targetAddressBits) + '.'} dropRight 1
  }

  /**
    * @return Returns the network address bit pattern of an IP object.
    */

  private def networkBitPattern = networkBits & addressBits

  /**
    * Performs validation on the String representation of the IP object,
    * before passing the validOctets to the bit mapper.
    *
    * @throws Exception   Will throw an exception in the case that the String
    *                     format of the IP is not validated.
    * @return             A bit mapped 32 ip address.
    */

  private def encodeAddressAsInt = {

    def betweenOneAndThreeDigits(octet: String) = (("\\d?\\d?\\d" r) pattern) matcher octet matches

    val parsedOctets = for {
      octet <- address split '.' if betweenOneAndThreeDigits(octet) && octetInValidNumberRange(Integer parseInt octet)
    } yield Integer parseInt octet

    parsedOctets match {
      case validOctets if parsedOctets.length == 4 && address.count(_ == '.') == 3 => bitMapInto32BitSpace(validOctets)
      case error => throw new Exception("The input IP must contain 4 octets with each char in the range [0-9], " +
        "eg: xxx.xxx.xxx.xxx.  No single octet may be above 255 or below 0")
    }
  }

  /**
    * Determines this IP addresses broadcast address bit pattern, by performing bitwise or on the
    * network address bit pattern, and the negation of the networkBits as dictated by the cidr value.
    *
    * @return bit pattern of the broadcast address for this IP object.
    */

  private def broadcastBitPattern = networkBitPattern | ~networkBits

  /**
    * Takes an array of Int octets, and bit maps them into one Int 32bit representation.
    *
    * @param octetsToEncode   Pre-validated IP octets in Int format.
    * @return                 Single Int value representing an IP address bit pattern.
    */

  private def bitMapInto32BitSpace(octetsToEncode: Array[Int]): Int = {
    (1 to 4 foldLeft 0) { (bitmap: Int, position: Int) => loadOctet(position)(bitmap, octetsToEncode(position - 1)) }
  }

  /**
    * Checks to see if an octet in Int representation is in the valid range for an ipv4 IP address.
    *
    * @param octet  Int octet to validate.
    * @return       True if valid, false otherwise.
    */

  private def octetInValidNumberRange(octet: Int) = octet >= 0 && octet < 256

  /**
    * Defines a [[Map Map]] of anonymous functions which are used to read the values
    * from a bit mapped 32 bit ipv4 address.
    *
    * @note     To index the most significant bits in the address and read them in a
    *           way which preserves their value in an unsigned representation, we must
    *           perform an unsigned right shift '>>>'.  This allows us to read values
    *           in an unsigned manner, and allows the creation of an unsigned int
    *           representation on the JVM for the 32 bit bit map of the ipv4 addresses.
    *
    * @return   A function which takes an 32bit integer bitmap of an IP address.
    *
    * @example  readOctet(1) <-- This is a function, which will now take a 32 bit IP
    *           address, and return the value of the most significant octet as an Int.
    */

  private val readOctet = Map(
    1 -> { bitmap: Int => (bitmap & 0xFF000000) >>> 24 },
    2 -> { bitmap: Int => (bitmap & 0x00FF0000) >> 16 },
    3 -> { bitmap: Int => (bitmap & 0x0000FF00) >> 8  },
    4 -> { bitmap: Int => (bitmap & 0x000000FF) >> 0  }
  )

  /**
    * Defines a [[Map Map]] of anonymous functions which are used to write values
    * to a bit mapped 32 bit ipv4 address via bitwise operation.
    *
    * @example loadOctet(1)(existingBitmap, 255}
    *          will load 255 into the most significant octet (top 8 bits) of 'existingBitmap'.
    */

  private val loadOctet = Map(
    1 -> { (bitmap: Int, value: Int) => (bitmap | value << 24) ^ (bitmap & 0xFF000000 ) },
    2 -> { (bitmap: Int, value: Int) => (bitmap | value << 16) ^ (bitmap & 0x00FF0000 ) },
    3 -> { (bitmap: Int, value: Int) => (bitmap | value << 8)  ^ (bitmap & 0x0000FF00 ) },
    4 -> { (bitmap: Int, value: Int) => (bitmap | value << 0)  ^ (bitmap & 0x000000FF ) }
  )

  /** Stores a bitmap representing a 32bit IP address for this Object. */

  private val addressBits: Int = encodeAddressAsInt

}

object IPUtils {

  /**
    * This function will take a [[Vector Vector]] of IP objects, from them determine all the possible Nodes
    * which may be connected to this Node.
    *
    * @param ipsToDetermineSubnetRangesOf A [[Vector Vector]] of IP objects from which to extract the ranges.
    *
    * @return                             The start and finish of each of the subnets the IPs belong to
    *                                     with duplicates omitted.
    */

  def startAndFinishOfEachSubnet(ipsToDetermineSubnetRangesOf: Vector[IP]): Vector[(IP, IP)] =
    ipsToDetermineSubnetRangesOf.foldLeft(Vector[(IP, IP)]()) {
      case (ipAcc, IPExtractor(address, network)) => ipAcc :+ (address + 1, network - 1)
    } distinct

}

object IPExtractor {

  /**
    * An extractor object, which allows the binding of the network and broadcast
    * address during a pattern match on an [[IP IP]] object.
    */

  def unapply(objectToExtractFroms: IP): Option[(IP, IP)] =
    Some((objectToExtractFroms networkAddress, objectToExtractFroms broadcastAddress))

}

object IPv4 {

  /**
    * The base type [[IP IP]] must extend serializable for transfer over messages.
    * For this reason, the IPv4 subtype must implement its own implementation of
    * unapply and apply, as Scala prevents case to case inheritance.
    *
    * @param address ip to instanciate with IE: 192.168.1.1
    * @param cidr CIDR/Subnet bits
    * @return A new instance of IPv4
    */

  def apply(address: String, cidr: Short) = new IPv4(address, cidr)

  /**
    * The base type [[IP IP]] must extend serializable for transfer over messages.
    * For this reason, the IPv4 subtype must implement its own implementation of
    * unapply and apply, as Scala prevents case to case inheritance.
    *
    * @param arg The IPv4 object to extract attributes from
    * @return Address and cidr as datamembers for use in pattern matching
    */

  def unapply(arg: IPv4): Option[(String, Short)] = Some((arg.address, arg.cidr))
}