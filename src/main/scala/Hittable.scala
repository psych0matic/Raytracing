case class HitRecord(var p:Vec3, var normal:Vec3, var t:Double,mat:Material)

abstract class Hittable {
  def hit(r:Ray,tMin:Double,tMax:Double):Option[HitRecord]
}
