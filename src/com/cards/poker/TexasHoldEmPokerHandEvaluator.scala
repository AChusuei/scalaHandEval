package com.cards.poker

import com.cards._
import scala.collection.JavaConverters._

object TexasHoldEmPokerHandEvaluator {

  def findHandFromCardSet(cardSet: Set[Card]): Hand = {
    findHighestFlushOrStraight(cardSet) match {
      case Some(hand) => hand
      case None => findDuplicateTypeHand(cardSet)
    }
  }
  
  def findHighestFlushOrStraight(cardSet: Set[Card]): Option[Hand] = {
    getAllFlushCards(cardSet) match {
      case None => findHighestStraight(cardSet)
      case Some(flush) => findHighestStraight(flush) match { 
        case None => Some(Hand.Flush(flush.toList.sorted.asJava, flush.head.suit))
        case Some(straight) => Some(Hand.StraightFlush(straight.ranks.get(0), flush.head.suit))
      }
    }
  }
  
  def findHighestStraight(cardSet: Set[Card]): Option[Hand] = {
    val sortedRanks = cardSet.map(_.rank).toList.sorted
    val straight = sortedRanks match {
      case z :: rest => rest.foldLeft(List(z))((sl,r) => if (sl.last.value - 1 == r.value) sl :+ r else List(r))
      case Nil => List()
    }
    if (straight.size >= 5 || (straight.size == 4 && straight.head == Rank.Five && sortedRanks.contains(Rank.Ace))) {
      Some(Hand.Straight(straight.head))
    } else {
      None
    }
  }
  
  def getAllFlushCards(cardSet: Set[Card]): Option[Set[Card]] = {
    cardSet.groupBy(c => c.suit).filter(kvp => kvp._2.size > 4).values.toList match {
      case Nil => None
      case h :: t => Some(h)
    }
  }
  
  def findDuplicateTypeHand(cardSet: Set[Card]): Hand = {
    val rankMap = cardSet.groupBy(_.rank).map(kvp => (kvp._1, kvp._2.size))
    findQuads(rankMap) match {
      case Some(hand) => hand
      case None => findTripsOrFullHouse(rankMap) match {
        case Some(hand) => hand
        case None => findPairs(rankMap) match {
          case Some(hand) => hand
          case None => findHighCards(rankMap)
        }
      }
    }
  }
  
  def findQuads(rankMap: Map[Rank, Int]): Option[Hand] = {
    rankMap.filter(_._2 == 4).keys.toList match {
      case quads :: t => Some(Hand.FourOfAKind(quads, rankMap.keys.filter(_ != quads).toList.sorted.head))
      case Nil => None
    }
  }
  
  def findTripsOrFullHouse(rankMap: Map[Rank, Int]): Option[Hand] = {
    rankMap.filter(_._2 == 3).keys.toList.sorted match {
      case firstTrips :: secondTrips :: rest => Some(Hand.FullHouse(firstTrips, secondTrips))
      case onlyTrips :: Nil => findPairs(rankMap.filter(_._2 != onlyTrips)) match {
        case Some(pairHand) => Option(Hand.FullHouse(onlyTrips, pairHand.ranks.get(0)))
        case None => {
          val highCards = findHighCards(rankMap.filter(kvp => kvp._1 != onlyTrips))
          Option(Hand.ThreeOfAKind(onlyTrips, highCards.ranks.get(0), highCards.ranks.get(1)))
        }  
      } 
      case Nil => None
    }
  }
  
  def findPairs(rankMap: Map[Rank, Int]): Option[Hand] = {
    rankMap.filter(_._2 == 2).keys.toList.sorted match {
      case firstPair :: secondPair :: t => {
        val highCards = findHighCards(rankMap.filter(kvp => kvp._1 != firstPair && kvp._1 != secondPair))
        Option(Hand.TwoPair(firstPair, secondPair, highCards.ranks.get(0)))
      }
      case onlyPair :: Nil => {
        val highCards = findHighCards(rankMap.filter(kvp => kvp._1 != onlyPair))
        Option(Hand.OnePair(onlyPair, highCards.ranks.get(0), highCards.ranks.get(1), highCards.ranks.get(2)))
      }
      case Nil => None
    }
  }
  
  def findHighCards(rankMap: Map[Rank, Int]): Hand = {
    Hand.HighCard(rankMap.keys.toList.sorted.asJava)
  }
}