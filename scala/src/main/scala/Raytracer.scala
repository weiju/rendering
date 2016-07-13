package com.boxofrats.raytracing

import javax.swing.{JFrame, JViewport}
import java.awt.{Dimension, Color, Graphics}
import java.awt.image.BufferedImage
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
    val xdiv = for (i <- (0 until numSections)) yield  xsecSize * i + xsecSize / 2
    val ydiv = for (i <- (0 until numSections)) yield  ysecSize * i + ysecSize / 2
    for (y <- ydiv; x <- xdiv) yield (x + jitter(xsecSize), y + jitter(ysecSize))
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

  def renderLine(y: Int, scene: Scene, g: Graphics, sampleOffsets: Seq[(Float, Float)]) {
    val numSamples = sampleOffsets.length
    for (x <- 0 until scene.viewport.width) {
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

      // ensuring we are performing this section in a synchronized way because of
      // race conditions
      synchronized {
        g.setColor(finalColor)
        g.drawLine(x, y, x + 1, y + 1)
      }
    }
  }

  // Control the number of threads rendering will be performed in
  def setParallelismGlobally(numThreads: Int): Unit = {
    val parPkgObj = scala.collection.parallel.`package`
    val defaultTaskSupportField = parPkgObj.getClass.getDeclaredFields.find{
      _.getName == "defaultTaskSupport"
    }.get

    defaultTaskSupportField.setAccessible(true)
    defaultTaskSupportField.set(
      parPkgObj,
      new scala.collection.parallel.ForkJoinTaskSupport(
        new scala.concurrent.forkjoin.ForkJoinPool(numThreads)
      )
    )
  }

  def main(args: Array[String]) {
    if (args.length == 0) {
      println("Usage: run <scenefile>.json")
    } else {
      val path = args(0)
      val scene = SceneReader.read(path).get
      val frame = new JFrame("Raytracing Demo (Scala) 1.0")
      frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
      val contents = new JViewport
      contents.setPreferredSize(new Dimension(scene.viewport.width, scene.viewport.height))
      frame.getContentPane.add(contents)
      frame.pack
      frame.setVisible(true)
      val image = new BufferedImage(scene.viewport.width, scene.viewport.height, BufferedImage.TYPE_INT_RGB)
      val gImg = image.getGraphics
      val g = contents.getGraphics
      val sampler = new StochasticSampler()
      val sampleOffsets = sampler.sampleOffsets

      // parallelized by using parallelized sequence, but the Graphics
      // object is not thread-safe, so we need to render into an image
      setParallelismGlobally(4)
      val startTime = System.currentTimeMillis
        (0 until scene.viewport.height).par.map(y => renderLine(y, scene, gImg, sampleOffsets))
      val elapsed = System.currentTimeMillis - startTime
      println(s"rendering finished in $elapsed ms.")
      g.drawImage(image, 0, 0, contents)
    }
  }
}
