import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class HittableList() extends Hittable {
  var hittables:ListBuffer[Hittable] = new ListBuffer[Hittable]()

  def add(obj:Hittable) = hittables.append(obj)

  override def hit(r: Ray, tMin: Double, tMax: Double): Option[HitRecord] = {
    var closest = tMax
    var tempRec:Option[HitRecord] = None

    for (hittable <- hittables) {
      hittable.hit(r,tMin,closest) match {
        case Some(hitRecord) => {
          closest = hitRecord.t
          tempRec = Some(hitRecord)
        }
        case None => {}
      }
    }
    tempRec
  }
}
