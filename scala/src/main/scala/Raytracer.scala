package com.boxofrats.raytracing

import javax.swing.{JFrame, JViewport}
import java.awt.{Dimension, Color}
import scala.util.Random

class StochasticSampler(numSections: Int=3, pixelWidth: Float=1.0f, pixelHeight: Float=1.0f) {

  val random = new Random

  private def jitter(jsize: Float) = {
    val signum = (random.nextInt % 2) == 0
    val absval = (random.nextFloat * jsize) / 2
    if (signum) absval else -absval
  }
  def sampleOffsets = {
    val xsecSize = pixelWidth / numSections
    val ysecSize = pixelHeight / numSections
    val xdiv = for (i <- (0 until numSections)) yield  xsecSize * i + xsecSize / 2 + jitter(xsecSize)
    val ydiv = for (i <- (0 until numSections)) yield  ysecSize * i + ysecSize / 2 + jitter(ysecSize)
    for (y <- ydiv; x <- xdiv) yield (x, y)
  }
}

object Raytracer {

  def diffuseComponent(obj: GeometryObject, ray: Ray, intersection: Intersection,
    light: Light): Array[Float] = {
    val intersectPoint = ray.origin + ray.direction * intersection.t
    val intersectToLight1 = light.position - intersectPoint
    val intersectToLight2 = intersectToLight1.normalized
    val ldotNormal = intersection.normal dot intersectToLight2
    val ldn = if (ldotNormal < 0.0) 0.0 else ldotNormal
    val rgb = obj.material.diffuseColor.getRGBComponents(null)
    val diffuse = rgb.map(c => (obj.material.diffuseCoeff * c * light.color * ldn).toFloat)
    diffuse
  }

  def illuminate(scene: Scene, obj: GeometryObject, ray: Ray, intersection: Intersection) = {
    val diffuse = diffuseComponent(obj, ray, intersection, scene.lights(0))
    val rgb = obj.material.diffuseColor.getRGBComponents(null)
    val ambient = rgb.map(c => (scene.ambientLight.coeff * obj.material.diffuseCoeff * scene.ambientLight.color * c).toFloat)
    val total = Array(diffuse(0) + ambient(0), diffuse(1) + ambient(1), diffuse(2) + ambient(2))
    val res = total.map(c => if (c > 1.0f) 1.0f else c)
    new Color(res(0), res(1), res(2))
  }

  private def findClosest(scene: Scene, ray: Ray): Option[Tuple2[GeometryObject, Intersection]] = {
    var closest: Option[Tuple2[GeometryObject, Intersection]] = None
    for (obj <- scene.objects) {
      for (intersection <- obj.intersect(ray)) {
        if (closest.isEmpty) closest = Some(Tuple2(obj, intersection))
        else {
          for (c <- closest) {
            if (intersection.t < c._2.t) closest = Some(Tuple2(obj, intersection))
          }
        }
      }
    }
    closest
  }

  def traceRay(scene: Scene, ray: Ray): Color = {
    val closest = findClosest(scene, ray)
    if (closest.isEmpty) scene.backgroundColor
    else {
      val c = closest.get
      illuminate(scene, c._1, ray, c._2)
    }
  }

  def main(args: Array[String]) {
    val path = "../scene.json"
    val scene = SceneReader.read(path).get
    //println(scene)
    val frame = new JFrame("Raytracing Demo (Scala) 1.0")
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    val contents = new JViewport
    contents.setPreferredSize(new Dimension(scene.viewport.width, scene.viewport.height))
    frame.getContentPane.add(contents)
    frame.pack
    frame.setVisible(true)
    val g = contents.getGraphics

    val sampler = new StochasticSampler()
    val sampleOffsets = sampler.sampleOffsets
    val numSamples = sampleOffsets.length
    //println(sampleOffsets)

    for (y <- 0 until scene.viewport.height; x <- 0 until scene.viewport.width) {
      var r_sum: Int = 0
      var g_sum: Int = 0
      var b_sum: Int = 0
      for (offset <- sampleOffsets) {
        val color = traceRay(scene, scene.camera.makeRay(x + offset._1, y + offset._2))
        r_sum += color.getRed
        g_sum += color.getGreen
        b_sum += color.getBlue
      }
      val finalColor = new Color(r_sum / numSamples, g_sum / numSamples, b_sum / numSamples)
      g.setColor(finalColor)
      g.drawLine(x, y, x + 1, y + 1)
    }
  }
}
