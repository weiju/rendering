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
    (Vector3d(1, 2, 3) + Vector3d(1, 2, 3)) should be (Vector3d(2, 4, 6))
  }

  it should "subtract a vector" in {
    (Vector3d(8, 9, 10) - Vector3d(4, 3, 7)) should be (Vector3d(4, 6, 3))
  }
  it should "calculate the dot product of 2 vectors" in {
    (Vector3d(8, 9, 10) dot Vector3d(4, 3, 7)) should be (129)
  }
  it should "calculate the cross product of a vector with itself" in {
    val v1 = Vector3d(1, 2, 3)
    (v1 cross v1) should be (Vector3d(0, 0, 0))
  }
  it should "calculate the cross product of 2 vectors" in {
    (Vector3d(1, 2, 3) cross Vector3d(2, 3, 4)) should be (Vector3d(-1, 2, -1))
  }

  it should "return its length" in {
    (Vector3d(1, 2, 3)).length should be (math.sqrt(14))
  }

  it should "return its normalized version" in {
    (Vector3d(1, 2, 3)).normalized.length should be (1)
  }

  it should "multiply a vector with a scalar" in {
    (Vector3d(1, 2, 3) * 3) should be (Vector3d(3, 6, 9))
  }

  it should "divide a vector by a scalar" in {
    ((Vector3d(3, 6, 9)) / 3) should be (Vector3d(1, 2, 3))
  }
}
