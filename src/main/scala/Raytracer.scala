package com.boxofrats.raytracing

import javax.swing.{JFrame, JViewport}
import java.awt.{Dimension, Color}


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
    val scene = SceneReader.read.get
    println(scene)
    val frame = new JFrame("Raytracing Demo (Scala) 1.0")
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    val contents = new JViewport
    contents.setPreferredSize(new Dimension(scene.viewport.width, scene.viewport.height))
    frame.getContentPane.add(contents)
    frame.pack
    frame.setVisible(true)
    val g = contents.getGraphics

    for (y <- 0 until scene.viewport.height; x <- 0 until scene.viewport.width) {
      g.setColor(traceRay(scene, scene.camera.makeRay(x, y)))
      g.drawLine(x, y, x + 1, y + 1)
    }
  }
}
