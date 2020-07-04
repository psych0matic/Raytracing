case class ScatterRecord(attenuation:Vec3,scattered:Ray)

abstract class Material {
  def scatter(rIn:Ray,rec:HitRecord):Option[ScatterRecord]
}

case class Lambertian(albedo:Vec3) extends Material {
  override def scatter(rIn: Ray, rec: HitRecord): Option[ScatterRecord] = {
    val target = rec.p + rec.normal + Raytracing.randomInUnitSphere()
    Some(ScatterRecord(
      attenuation = albedo,
      scattered = Ray(rec.p,target-rec.p)
    ))
  }


}

case class Metal(albedo:Vec3) extends Material {
  def reflect(v:Vec3,n:Vec3):Vec3 = {
    v - n*new Vec3().dot(v,n)*2
  }

  override def scatter(rIn: Ray, rec: HitRecord): Option[ScatterRecord] = {
    val reflected = reflect(new Vec3().unitVec(rIn.direction),rec.normal)
    val scattered = Ray(rec.p,reflected)

    if (new Vec3().dot(scattered.direction,rec.normal) > 0 ) {
      Some(ScatterRecord(
        attenuation = albedo,
        scattered = scattered
      ))
    } else {
      None
    }
  }
}


