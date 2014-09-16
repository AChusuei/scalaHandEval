package com.cards.poker

import org.scalatest._
import scala.collection.mutable.Stack

class TexasHoldEmPokerHandEvaluatorTests extends FlatSpec with Matchers {

  "A Stack" should "pop values in last-in-first-out order" in {
    val stack = new Stack[Int]
    stack.push(1)
    stack.push(2)
    stack.pop() should be (2)
    stack.pop() should be (1)
  }
}