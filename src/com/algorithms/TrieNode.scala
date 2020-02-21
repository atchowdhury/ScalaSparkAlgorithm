package com.algorithms


import scala.collection.mutable
import scala.collection.mutable.Map


class TrieNode {
  var child: Map[Char, TrieNode] = Map.empty
  var isEnd: Boolean = false
  var childContacts: Int = 0
  var phoneNumber: String = ""
  var contactName = ""

}

object TrieNode {
  var rootNode = new TrieNode()

  def saveContact(name: String, phoneNumber: String) = {

    saveContactRecursive(name, phoneNumber, 0, rootNode)

  }

  private def saveContactRecursive(name: String, phoneNumber: String, index: Int, currentNode: TrieNode): Unit = {
    if (index <= name.length - 1) {
      val c = name.toLowerCase.charAt(index)

      var pointerNode = None: Option[TrieNode]

      currentNode.child.get(c) match {
        case Some(a) => {
          pointerNode = currentNode.child.get(c)
        }
        case None => {
          val newNode = new TrieNode()
          currentNode.child += (c -> newNode)
          pointerNode = Some(newNode)
        }
      }

      if (index == name.length - 1) {
        pointerNode.get.isEnd = true
        pointerNode.get.contactName = name
        pointerNode.get.phoneNumber = phoneNumber
      } else {
        saveContactRecursive(name, phoneNumber, index + 1, pointerNode.get)
      }
    }
    currentNode.childContacts += 1

  }

  type nameNumber = (String, String)
  var nameTuple = scala.collection.mutable.ArrayBuffer.empty[(nameNumber)]

  def nameSearch(name: String): Option[mutable.ArrayBuffer[nameNumber]] = {
    nameTuple.clear()
    var currentNode = rootNode
    for (i <- 0 to name.length - 1) {
      val c = name.toLowerCase.charAt(i)
      var nextNode = currentNode.child.get(c)

      nextNode match {
        case Some(a) => {
          currentNode = nextNode.get
        }
        case None => None
      }

    }

    getNames(currentNode)
    Some(nameTuple)
  }

  private def getNames(startNode: TrieNode): Unit = {
    if (startNode.isEnd == true) {
      // println(startNode.contactName+" "+startNode.phoneNumber)
      nameTuple += ((startNode.contactName, startNode.phoneNumber))

    }
    if (startNode.child.size == 0) {

      return
    }
    startNode.child.foreach(f => {
      val node = f._2
      getNames(node)
    })


  }

  var isDeletePossible = false

  def deleteContact(contact: String): Boolean = {
    isDeletePossible = false
    deleteIfPossible(contact, rootNode, 0)
    return isDeletePossible
  }

  private def deleteIfPossible(contact: String, currentNode: TrieNode, index: Int): Unit = {
    val c = contact.toLowerCase.charAt(index)
    val pointerNode = currentNode.child.get(c)
    if ((index + 1 <= contact.length - 1) && currentNode.child.size > 0) {

      //val nextNode=pointerNode
      pointerNode match {
        case Some(a) => {
          deleteIfPossible(contact, pointerNode.get, index + 1)
        }
        case None => {
          //nextNode=new TrieNode()
          return
        }

      }

    }

    if (pointerNode.get.isEnd && pointerNode.get.contactName == contact) {

      isDeletePossible = true
      //return
    }
    if (!isDeletePossible && currentNode.child.size == 0) {
      return
    }
    if (isDeletePossible) {
      pointerNode.get.child.size match {
        case 0 => {
          ///case 1||0 =>{
          currentNode.child -= c
        }
        case _ => currentNode.childContacts -= 1
      }
      //currentNode.child.
    }

  }

  def printResult(searchResult: Option[mutable.ArrayBuffer[nameNumber]]): Unit = {
    searchResult match {
      case Some(r) => {
        searchResult.get.foreach(f => println("name: " + f._1 + " number: " + f._2))
      }
      case None => println("Not found")
    }
    println("==================")
  }


}

object Main {
  def main(args: Array[String]) = {


    TrieNode.saveContact("Atique", "514-000-0000")
    TrieNode.saveContact("Atk", "438-237-2990")
    TrieNode.saveContact("Jack", "400-237-2990")
    TrieNode.saveContact("Jill", "900-237-000")


    println(if (TrieNode.deleteContact("Atk")) "Contact Deleted" else "Contact not found in DB")

    println(if (TrieNode.deleteContact("AtkPP")) "Contact Deleted" else "Contact not found in DB")

    TrieNode.printResult(TrieNode.nameSearch("AT"))

    TrieNode.printResult(TrieNode.nameSearch("j"))


  }
}
