package com.raphtory.examples.blockchain.analysers

import com.raphtory.core.analysis.API.Analyser
import com.raphtory.core.utils.Utils

import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.immutable
import scala.util.Random

class EthereumTaintTracking(args:Array[String]) extends Analyser(args) {

  val infectedNode = args(0).trim.toLowerCase
  val infectionStartingBlock = args(1).trim.toLong
  override def setup(): Unit =
    view.getVertices().foreach { vertex =>
      val walletID = vertex.getPropertyValue("id").get.asInstanceOf[String]
//      number of hops from the starting node
      val numberOfHops = 0
//      by nude degree here and everywhere else I mean the number of nodes this node has infected. I would rename the variable in the future!
      var nodeDegree = 0
//      score
      var score = 1000.0
//      this thing views the history of number of infected wallets and chooses the maximum
      if (vertex.getOutEdgesAfter(infectionStartingBlock).map(edge => {edge.getHistory().size}).nonEmpty)
      {nodeDegree = vertex.getOutEdgesAfter(infectionStartingBlock).map(edge => {edge.getHistory().size}).max}
//      initialize the starting node with all of the parameters
      if(walletID equals infectedNode) {
        vertex.getOrSetState("infected", infectionStartingBlock)
        vertex.getOrSetState("infectedBy", "Start")
        vertex.getOrSetState("numberOfHops", 0)
        vertex.getOrSetState("nodeDegree", nodeDegree)
        vertex.getOrSetState("score", score)

        vertex.getOutEdgesAfter(infectionStartingBlock).foreach { neighbour =>
          neighbour.send((walletID,neighbour.firstActivityAfter(infectionStartingBlock), numberOfHops, nodeDegree, score))
        }
      }
    }

  override def analyse(): Unit =
    view.getMessagedVertices().foreach { vertex =>
//      for each other ethereum transaction do the following:
//       initialize the fields like in the setup
      var infectionBlock = infectionStartingBlock
      var infector = infectedNode
      var numberOfHops = 1
//      this node degree is still the number of nodes this node has infected I'm so sorry
      var nodeDegree = 0
//      and each score in the beginning gets a score of 1000 bc we assume it is the bad guy
      var score = 1000.0

      val queue  = vertex.messageQueue[(String,Long,Int,Int, Double)]


      try{
        //  it is extracting the minimum along the second line of mappers
        // choosing minimal timestamp among timestamps
        infectionBlock = queue.map(x=>x._2).min


        //      send a pair (string, long)
        //      set a new long to this one
        //      add number of hops
        numberOfHops = queue.map(x=>x._3).min
        numberOfHops = numberOfHops + 1

// this is clear calculation of the number of nodes this one has infected
        if (vertex.getOutEdgesAfter(infectionStartingBlock).map(edge => {edge.getHistory().size}).nonEmpty)
        {nodeDegree = vertex.getOutEdgesAfter(infectionStartingBlock).map(edge => {edge.getHistory().size}).max
        println(nodeDegree)}


//if it has infected someone, recalculate the score
        if (nodeDegree!=0){
          score = nodeDegree.toDouble/numberOfHops.toDouble
        }
        infector = queue.filter(x=>x._2==infectionBlock).head._1 //todo check if multiple
//        println(infector)

//save results
        val walletID = vertex.getPropertyValue("id").get.asInstanceOf[String]
        vertex.getOrSetState("infected", infectionBlock)
        vertex.getOrSetState("infectedBy",infector)
        vertex.getOrSetState("numberOfHops", numberOfHops)
        vertex.getOrSetState("nodeDegree", nodeDegree)
        vertex.getOrSetState("score", score)
        vertex.getOutEdgesAfter(infectionBlock).foreach { neighbour =>
          neighbour.send((walletID,neighbour.firstActivityAfter(infectionBlock), numberOfHops, nodeDegree, score))

        }

      }
      catch {
        case e: UnsupportedOperationException =>
      }




      //if (vertex.containsCompValue("infected"))
      //  vertex.voteToHalt() //already infected
      //else {



      //}
    }
//return results: filter out all of the transactions that are not needed
  override def returnResults(): Any =
    view.getVertices().map { vertex =>
      if (vertex.containsState("infected"))
        (vertex.getPropertyValue("id").get.asInstanceOf[String], vertex.getState("infected").asInstanceOf[Long],vertex.getState("infectedBy").asInstanceOf[String], vertex.getState("numberOfHops").asInstanceOf[Int], vertex.getState("nodeDegree").asInstanceOf[Int], vertex.getState("score").asInstanceOf[Double])
      else
        ("", -1L,"", "", "","")

    }
      .filter(f => f._2 >= 0).par

  override def defineMaxSteps(): Int = 100

  override def processResults(results: ArrayBuffer[Any], timeStamp: Long, viewCompleteTime: Long): Unit = {
    val endResults = results.asInstanceOf[ArrayBuffer[immutable.ParIterable[(String, Long,String, Int, Int, Double)]]].flatten
    var data = s"{block:$timeStamp,edges:["
    //println(s"Run as of ${System.currentTimeMillis()}")
    for (elem <- endResults)
      data+=s"""{"infected":"${elem._1}","block":"${elem._2}","infector":"${elem._3}", "numberOfHops":"${elem._4}", "nodeDegree":"${elem._5}","score":"${elem._6}",}"""
    data+="]}"
    publishData(data)
  }




}
