package com.boxofrats.raytracing

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import scala.math

@RunWith(classOf[JUnitRunner])
class Vector3dSpec extends FlatSpec with Matchers {

  "Vector3d" should "be initialized" in {
    val v = Vector3d(1, 2, 3)
    v.x should be (1)
    v.y should be (2)
    v.z should be (3)
  }
  it should "add a vector" in {
    val v1 = Vector3d(1, 2, 3)
    val v2 = Vector3d(1, 2, 3)
    val res = v1 + v2

    res.x should be (2)
    res.y should be (4)
    res.z should be (6)
  }

  it should "subtract a vector" in {
    val v1 = Vector3d(8, 9, 10)
    val v2 = Vector3d(4, 3, 7)
    val res = v1 - v2

    res.x should be (4)
    res.y should be (6)
    res.z should be (3)
  }
  it should "calculate the dot product of 2 vectors" in {
    val v1 = Vector3d(8, 9, 10)
    val v2 = Vector3d(4, 3, 7)
    (v1 dot v2) should be (129)
  }
  it should "return its length" in {
    val v = Vector3d(1, 2, 3)
    v.length should be (math.sqrt(14))
  }

  it should "return its normalized version" in {
    val v = Vector3d(1, 2, 3)
    val vn = v.normalized
    vn.length should be (1)
  }
}
