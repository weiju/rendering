package com.boxofrats.raytracing

import java.awt.Color
import org.jblas.{DoubleMatrix}

import play.api.libs.json._
import play.api.libs.functional.syntax._

/**
  * A 3D vector class for making computations a little clearer. JBLAS is used if
  *  possible, but it does not come with a vector class
  */
case class Vector3d(x: Double, y: Double, z: Double) {
  def -(v:  Vector3d): Vector3d = Vector3d(x - v.x, y - v.y, z - v.z)
  def +(v:  Vector3d): Vector3d = Vector3d(x + v.x, y + v.y, z + v.z)
  def *(s: Double): Vector3d = Vector3d(x * s, y * s, z * s)
  def /(s: Double): Vector3d = Vector3d(x / s, y / s, z / s)

  def dot(v: Vector3d): Double = {
    val a = new DoubleMatrix(Array(x, y, z))
    val b = new DoubleMatrix(Array(v.x, v.y, v.z))
    a.dot(b)
  }
  def cross(v: Vector3d): Vector3d = {
    Vector3d(y * v.z - z * v.y, z * v.x - x * v.z, x * v.y - y * v.x)
  }

  def length: Double = math.sqrt(x * x + y * y + z * z)
  def normalized: Vector3d = {
    val l = this.length
    Vector3d(x / l, y / l, z / l)
  }
}

/**
  * Helper class to facilitate color calculation
  */
case class ColorTuple(r: Float, g: Float, b: Float, alpha: Float=1.0f) {
  def +(other: ColorTuple) = ColorTuple(r + other.r, g + other.g, b + other.b)
  def /(s: Float) = ColorTuple(r / s, g / s, b / s)
  def toColor: Color = new Color(ColorTuple.trimComponent(r), ColorTuple.trimComponent(g),
    ColorTuple.trimComponent(b))
  def trimmed = ColorTuple(ColorTuple.trimComponent(r), ColorTuple.trimComponent(g),
    ColorTuple.trimComponent(b))
  def rgbComponents = Seq(r, g, b)
}

object ColorTuple {
  def trimComponent(comp: Float): Float = math.max(0.0f, math.min(1.0f, comp))
  def average(colors: Seq[ColorTuple]) = colors.reduce(_ + _) / colors.length
}

case class Ray(origin: Vector3d, direction: Vector3d)
case class Intersection(t: Double, normal: Vector3d)

/**
  * Representation of a material entry:
  */
case class Material(diffuseColor: ColorTuple,
  diffuseCoeff: Float, specularCoeff: Float, hardness: Float)

trait GeometryObject {

  /**
    * returns the parameter t for the visible intersection point P_h on the
    * line described by P_h = r.origin + t * r.direction
    */
  def intersect(ray: Ray): Option[Intersection]
  def material: Material
}


case class Sphere(center: Vector3d, radius: Double, val material: Material)
extends GeometryObject {

  val EPS = 1.0e-7
  private def normalAt(ray: Ray, t: Double): Vector3d = {
    val l = ray.origin - center
    ((l + ray.direction * t) / radius).normalized
  }

  /**
    * Analytic approach to finding intersection point of a ray with a sphere.
    */
  private def intersect_analytic(ray: Ray): Option[Intersection] = {
    val l = ray.origin - center
    val a = ray.direction dot ray.direction
    val b = 2 * (ray.direction dot l)
    val c = (l dot l) - (radius * radius)

    val discriminant = b * b - 4.0 * a * c
    if (discriminant < 0.0) None
    else if (discriminant == 0.0) {
      val t = -0.5 * b / a
      Some(Intersection(t, normalAt(ray, t)))
    } else {
      val e = math.sqrt(discriminant)
      val denom = 2 * a
      val t0 = (-b - e) / denom
      val t1 = (-b + e) / denom
      val t = if (t0 > EPS) t0 else if (t1 > EPS) t1 else return None
      Some(Intersection(t, normalAt(ray, t)))
    }
  }

  /**
    * Geometric approach to finding intersection point of a ray with a sphere.
    */
  private def intersect_geometric(ray: Ray): Option[Intersection] = {
    val l = center - ray.origin
    val tca = l dot ray.direction
    if (tca < 0) return None
    val d2 = (l dot l) - tca * tca
    val radius2 = radius * radius
    if (d2 > radius2) return None
    val thc = math.sqrt(radius2 - d2)
    val t0 = tca - thc
    val t1 = tca + thc
    val t = if (t0 > EPS) t0 else if (t1 > EPS) t1 else return None
    Some(Intersection(t, normalAt(ray, t)))
  }

  def intersect(ray: Ray) = intersect_geometric(ray)
}

class Camera(viewport: Viewport, val eye: Vector3d, lookat: Vector3d,
  up: Vector3d=Vector3d(0.0, 1.0, 0.0), focalLength: Double=0.5,
  pixelWidth: Double=1.0, pixelHeight: Double=1.0) {

  private lazy val fov: Seq[Double] = Seq(viewport.width, viewport.height).map(x => 2 * math.atan(x / (2.0 * focalLength)))
  private lazy val direction = lookat - eye
  private lazy val u = (direction cross up).normalized
  private lazy val v = (u cross direction).normalized
  private lazy val viewPlaneHalfWidth = math.tan(fov(0) / 2.0)
  private lazy val viewPlaneHalfHeight = {
    val aspectRatio = viewport.height.toDouble / viewport.width.toDouble
    aspectRatio * viewPlaneHalfWidth
  }
  private lazy val viewPlaneBottomLeft = lookat - (v * viewPlaneHalfHeight) - (u * viewPlaneHalfWidth)
  private lazy val xinc = (u * 2 * viewPlaneHalfWidth) / viewport.width * pixelWidth
  private lazy val yinc = (v * 2 * viewPlaneHalfHeight) / viewport.height * pixelHeight

  def makeRay(x: Double, y: Double) = {
    val viewPlanePoint = viewPlaneBottomLeft + (xinc * x) + (yinc * y)
    val rayDir = (viewPlanePoint - eye).normalized
    Ray(eye, rayDir)
  }
}

case class Viewport(width: Int, height: Int)
case class AmbientLight(color: Float, coeff: Float)
case class Light(position: Vector3d, color: Float)
case class Scene(viewport: Viewport, camera: Camera, backgroundColor: ColorTuple,
  ambientLight: AmbientLight, lights: Array[Light], objects: Seq[GeometryObject])

object SceneReader {

  implicit val colorReads: Reads[ColorTuple] = (
    (JsPath \ "r").read[Float] and
      (JsPath \ "g").read[Float] and
      (JsPath \ "b").read[Float]
  )((r, g, b) => ColorTuple(r, g, b))

  implicit val vector3dReads: Reads[Vector3d] = (
    (JsPath \ "x").read[Double] and
      (JsPath \ "y").read[Double] and
      (JsPath \ "z").read[Double]
  )(Vector3d.apply _)

  implicit val ambientReads: Reads[AmbientLight] = (
    (JsPath \ "color").read[Float] and
      (JsPath \ "coeff").read[Float]
  )(AmbientLight.apply _)

  implicit val viewportReads: Reads[Viewport] = (
    (JsPath \ "width").read[Int] and
      (JsPath \ "height").read[Int]
  )(Viewport.apply _)

  implicit val cameraReads: Reads[Camera] = (
    (JsPath \ "viewport").read[Viewport] and
      (JsPath \ "camera" \ "eye").read[Vector3d] and
      (JsPath \ "camera" \ "lookat").read[Vector3d]
  )((viewport, eye, lookat) => new Camera(viewport, eye, lookat))

  implicit val lightReads: Reads[Light] = (
    (JsPath \ "position").read[Vector3d] and
      (JsPath \ "color").read[Float]
  )(Light.apply _)

  implicit val materialReads: Reads[Material] = (
    (JsPath \ "diffuse_color").read[ColorTuple] and
      (JsPath \ "diffuse_coeff").read[Float] and
      (JsPath \ "specular_coeff").read[Float] and
      (JsPath \ "hardness").read[Float]
  )(Material.apply _)

  implicit val sphereReads: Reads[Sphere] = (
    (JsPath \ "sphere" \ "center").read[Vector3d] and
      (JsPath \ "sphere" \ "radius").read[Double] and
      (JsPath \ "sphere" \ "material").read[Material]
  )(Sphere.apply _)

  implicit val sceneReads: Reads[Scene] = (
    (JsPath \ "viewport").read[Viewport] and
      (JsPath).read[Camera] and
      (JsPath \ "background_color").read[ColorTuple] and
      (JsPath \ "ambient_light").read[AmbientLight] and
      (JsPath \ "lights").read[Array[Light]] and
      (JsPath \ "objects").read[Seq[Sphere]]
  )(Scene.apply _)

  def read(path: String) = Json.parse(scala.io.Source.fromFile(path).mkString).validate[Scene]
}
