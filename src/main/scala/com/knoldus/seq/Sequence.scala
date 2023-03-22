package com.knoldus.seq

class Sequence {

  private var sequenceString: Seq[String] = Seq.empty[String]
  private var sequenceInt: Seq[Int] = Seq.empty[Int]

  // Check the type of element, if it is int save it to sequenceInt, if it is string save it to sequenceString, else return IllegalArgumentException,  return the size
  def store(element: Any): Int = {
    element match {
      case string: String => {
        sequenceString = sequenceString :+ string
        sequenceString.size
      }
      case number: Int => {
        sequenceInt = sequenceInt :+ number
        sequenceInt.size
      }
      case _ => throw new IllegalArgumentException("Invalid element type")
    }
  }

  // Check the type of element, if it is int remove it from sequenceInt, if it is string remove it from sequenceString, else return IllegalArgumentException,  return the size
  def removeElement(element: Any): Int = {
    element match {
      case string: String => {
        sequenceString = sequenceString.filter(_ == string)
        sequenceString.size
      }
      case number: Int => {
        sequenceInt = sequenceInt.filter(_ == number)
        sequenceInt.size
      }
      case _ => throw new IllegalArgumentException("Invalid element type")
    }
  }

}
