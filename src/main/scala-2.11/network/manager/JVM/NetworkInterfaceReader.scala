package network.manager.JVM

import language._
import Dependencies.IP
import network.utils.IPv4
import java.net.NetworkInterface
import java.util.NoSuchElementException
import scala.collection.JavaConversions._
import network.manager.JVM.NetworkInterfaceReader.NoActiveNetworkInterfaces

/**
  * Wraps the [[java.net.NetworkInterface ]] object in the Java library, and provides a means of reading the network
  * interfaces available to the host system.
*/

trait NetworkInterfaceReader {

  /**
    * Specifies a constant which is used to chop the beginning '/' from the values contained in the enumerable
    * returned by [[java.net.NetworkInterface.getNetworkInterfaces getNetworkInterfaces()]].
    */

  val without_leading_slash = 1

  /**
    * @return
    * A Vector of IP's in [[String]] format.
    * @throws NoActiveNetworkInterfaces
    * In the event that no active network interfaces are detected on the host, the NoActiveNetworkInterfaces Exception
    * will be thrown.
    */

  def networkInterfaces: Vector[IP] = NetworkInterface getNetworkInterfaces match {
    case null =>
      throw new NoSuchElementException("There were no active network interfaces detected.")
    case availableInterfaces => {
      for {
        interface <- availableInterfaces
        ip <- interface.getInterfaceAddresses
        if ip.getBroadcast != null
      } yield IPv4(ip.getAddress.toString.substring(1), ip.getNetworkPrefixLength)
    } toVector
  }
}

/** A companion object to define custom Exceptions and Types */

object NetworkInterfaceReader {

  case class NoActiveNetworkInterfaces(message: String) extends Exception
}