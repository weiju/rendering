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

@RunWith(classOf[JUnitRunner])
class ColorTupleSpec extends FlatSpec with Matchers {

  "ColorTuple" should "be initialized" in {
    val v = ColorTuple(0.1f, 0.2f, 0.3f)
    v.r should be (0.1f)
    v.g should be (0.2f)
    v.b should be (0.3f)
  }
  it should "trim a component" in {
    ColorTuple.trimComponent(1.3f) should be (1.0f)
    ColorTuple.trimComponent(-0.3f) should be (0.0f)
  }
  it should "return a trimmed color" in {
    ColorTuple(-1.0f, 1.2f, 0.5f).trimmed should be (ColorTuple(0.0f, 1.0f, 0.5f))
  }
  it should "add another color" in {
    val sum = (ColorTuple(0.1f, 0.2f, 0.3f) + ColorTuple(0.2f, 0.3f, 0.4f))
    sum.r should be (0.3f)
    sum.g should be (0.5f)
    sum.b should be (0.7f +- 0.00001f)
  }
  it should "sum without problems" in {
    val colors = Seq(ColorTuple(0.1f, 0.2f, 0.3f), ColorTuple(0.2f, 0.3f, 0.4f))
    val sum = colors.reduce(_ + _)
    sum.r should be (0.3f)
    sum.g should be (0.5f)
    sum.b should be (0.7f +- 0.00001f)
  }

  it should "divide with a scalar" in {
    val res = ColorTuple(0.2f, 0.4f, 0.6f) / 2.0f
    res.r should be (0.1f)
    res.g should be (0.2f)
    res.b should be (0.3f)
  }

  it should "compute an average" in {
    val c1 = ColorTuple(0.1f, 0.2f, 0.3f)
    val c2 = ColorTuple(0.2f, 0.3f, 0.4f)
    ColorTuple.average(Seq(c1, c1)) should be (c1)
    val a2 = ColorTuple.average(Seq(c1, c2))
    a2.r should be (0.15f +- 0.00001f)
    a2.g should be (0.25f +- 0.00001f)
    a2.b should be (0.35f +- 0.00001f)
  }


  it should "convert to java.awt.Color" in {
    val awtColor = ColorTuple(0.0f, 0.5f, 1.0f).toColor
    awtColor.getRed() should be (0)
    awtColor.getGreen() should be (128)
    awtColor.getBlue() should be (255)
  }
}
