case class Sphere(center:Vec3,radius:Double) extends Hittable {
  override def hit(r: Ray, tMin: Double, tMax: Double): Option[HitRecord] = {
    val oc = r.origin - center
    val a = r.direction.lengthSquared()
    val halfB = new Vec3().dot(oc,r.direction)
    val c = oc.lengthSquared() - radius*radius
    val disc = halfB*halfB -a *c

    val temp1 = (-halfB-Math.sqrt(disc))/a
    val temp2 = (-halfB+Math.sqrt(disc))/a
    val b1 = tMin < temp1 && temp1 < tMax
    val b2 = tMin < temp2 && temp2 < tMax
    if (disc > 0 && (b1 || b2)) {
      val t = if (b1) temp1 else temp2
      val p = r.at(t)
      val normal = (p-center) /radius
      Some(HitRecord(
        t = t,
        p = p,
        normal = normal
      ))
    } else {
      None
    }
  }
}
