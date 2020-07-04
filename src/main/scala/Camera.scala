class Camera {
  val aspect = 16.0 / 9.0
  val vpHeight = 2.0
  val vpWidth = aspect * vpHeight
  val focalLength = 1.0

  val origin = new Vec3(0,0,0)
  val horizontal = new Vec3(vpWidth,0,0)
  val vertical = new Vec3(0,vpHeight,0)
  val lowerLeft = origin - horizontal/2 - vertical/2 - new Vec3(0,0,focalLength)

  def getRay(u:Double,v:Double):Ray = {
    new Ray(origin,lowerLeft + horizontal*u + vertical*v - origin)
  }
}
