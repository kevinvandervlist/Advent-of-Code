package nl.kevinvandervlist.aoc2020.day04

import scala.util.Try

object PassportScanning {
  def one(batch: String): Int =
    passportsFromBatch(batch).count(_.basicValidation)

  def two(batch: String): Int =
    passportsFromBatch(batch).count(_.extendedValidation)

  private def passportsFromBatch(batch: String): List[Passport] =
    batch.split("\n\n").map(Passport.apply).toList
}

private case class Passport(byr: Option[String] = None, iyr: Option[String] = None,
                            eyr: Option[String] = None, hgt: Option[String] = None,
                            hcl: Option[String] = None, ecl: Option[String] = None,
                            pid: Option[String] = None, cid: Option[String] = None) {
  def basicValidation: Boolean = {
    if(byr.isEmpty) {
      return false
    }
    if(iyr.isEmpty) {
      return false
    }
    if(eyr.isEmpty) {
      return false
    }
    if(hgt.isEmpty) {
      return false
    }
    if(hcl.isEmpty) {
      return false
    }
    if(ecl.isEmpty) {
      return false
    }
    if(pid.isEmpty) {
      return false
    }
    // Skip cid -- valid regardless
    true
  }

  def extendedValidation: Boolean = Try {
    if(! basicValidation) {
      return false
    }
    if(! isValidBirthYear) {
      return false
    }
    if(! isValidIssueYear) {
      return false
    }
    if(! isValidExpirationYear) {
      return false
    }
    if(! isValidHeight) {
      return false
    }
    if(! isValidHairColor) {
      return false
    }
    if(! isValidEyeColor) {
      return false
    }
    if(! isValidPassportID) {
      return false
    }
    // cid (Country ID) - ignored, missing or not.
    true
  }.getOrElse(false)

  private def isValidYear(lower: Int, upper: Int, raw: String): Boolean = {
    if(raw.length != 4) {
      return false
    }
    isValidNumberInRange(lower, upper, raw)
  }

  private def isValidNumberInRange(lower: Int, upper: Int, raw: String): Boolean = {
    val y = raw.toInt
    y >= lower && y <= upper
  }

  private def isValidBirthYear: Boolean = {
    // byr (Birth Year) - four digits; at least 1920 and at most 2002.
    isValidYear(1920, 2002, byr.get)
  }

  private def isValidIssueYear: Boolean = {
    // iyr (Issue Year) - four digits; at least 2010 and at most 2020.
    isValidYear(2010, 2020, iyr.get)
  }

  private def isValidExpirationYear: Boolean = {
    // eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
    isValidYear(2020, 2030, eyr.get)
  }

  private def isValidHeight: Boolean = {
    // hgt (Height) - a number followed by either cm or in:
    //  - If cm, the number must be at least 150 and at most 193.
    //  - If in, the number must be at least 59 and at most 76.
    val h = hgt.get
    if(h.length <= 2) {
      return false
    }
    val (len, unit) = h.splitAt(h.length - 2)
    unit match {
      case "cm" => isValidNumberInRange(150, 193, len)
      case "in" => isValidNumberInRange(59, 76, len)
      case _ => false
    }
  }

  private def isValidHairColor: Boolean = {
    // hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
    hcl.get.matches("^#[0-9a-f]{6}$")
  }

  private def isValidEyeColor: Boolean = {
    // ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
    ecl.get match {
      case "amb" => true
      case "blu" => true
      case "brn" => true
      case "gry" => true
      case "grn" => true
      case "hzl" => true
      case "oth" => true
      case _ => false
    }
  }

  private def isValidPassportID: Boolean = {
    // pid (Passport ID) - a nine-digit number, including leading zeroes.
    pid.get.matches("^[0-9]{9}$")
  }
}

private object Passport {
  //byr (Birth Year)
  //iyr (Issue Year)
  //eyr (Expiration Year)
  //hgt (Height)
  //hcl (Hair Color)
  //ecl (Eye Color)
  //pid (Passport ID)
  //cid (Country ID)
  def apply(chunk: String): Passport = {
    val parts = chunk.split(Array(':', '\n', ' '))
    var passport = Passport()
    var idx = 0
    while(idx < parts.length) {
      parts(idx) match {
        case "byr" => passport = passport.copy(byr = Option(parts(idx + 1)))
        case "iyr" => passport = passport.copy(iyr = Option(parts(idx + 1)))
        case "eyr" => passport = passport.copy(eyr = Option(parts(idx + 1)))
        case "hgt" => passport = passport.copy(hgt = Option(parts(idx + 1)))
        case "hcl" => passport = passport.copy(hcl = Option(parts(idx + 1)))
        case "ecl" => passport = passport.copy(ecl = Option(parts(idx + 1)))
        case "pid" => passport = passport.copy(pid = Option(parts(idx + 1)))
        case "cid" => passport = passport.copy(cid = Option(parts(idx + 1)))
      }
      idx += 2
    }
    passport
  }
}
