package nl.kevinvandervlist.aoc2015.day04

import java.security.MessageDigest

// Probably should use hash collision weaknesses here
object StockingStuffer {
  def one(prefix: String): Long = {
    var no = 1
    while(true) {
      val hash = digestInstance.digest(s"$prefix$no".getBytes)
      if(fiveLeadingZeroes(hash)) {
        return no
      }
      no += 1
    }
    -1
  }

  def two(prefix: String): Long = {
    var no = 1
    while(true) {
      val hash = digestInstance.digest(s"$prefix$no".getBytes)
      if(sixLeadingZeroes(hash)) {
        return no
      }
      no += 1
    }
    -1
  }

  private def fiveLeadingZeroes(bytes: Array[Byte]): Boolean = {
    if(String.format("%02x", bytes(0)) != "00") {
      return false
    }
    if(String.format("%02x", bytes(1)) != "00") {
      return false
    }
    if(! String.format("%02x", bytes(2)).startsWith("0")) {
      return false
    }
    true
  }

  private def sixLeadingZeroes(bytes: Array[Byte]): Boolean = {
    if(String.format("%02x", bytes(0)) != "00") {
      return false
    }
    if(String.format("%02x", bytes(1)) != "00") {
      return false
    }
    if(String.format("%02x", bytes(2)) != "00") {
      return false
    }
    true
  }

  private lazy val digestInstance = MessageDigest.getInstance("MD5")
}
