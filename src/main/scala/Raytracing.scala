import java.io.{File, FileWriter}

import Raytracing.out

object Raytracing extends App{

  val aspect = 16.0 / 9.0
  val iWidth:Int = 384
  val iHeight:Int = (iWidth/aspect).asInstanceOf[Int]

  val out = new FileWriter("output.ppm")
  out.write("P3\n" + iWidth + ' ' + iHeight + "\n255\n")

  val vpHeight = 2.0
  val vpWidth = aspect * vpHeight
  val focalLength = 1.0

  val origin = Vec3(0,0,0)
  val horizontal = Vec3(vpWidth,0,0)
  val vertical = Vec3(0,vpHeight,0)
  val lowerLeft = origin - horizontal/2 - vertical/2 - Vec3(0,0,focalLength)

  for (j <- iHeight-1 to 0 by -1) {
    println("\rScanlines remaining: " + j)
    for (i <- 0 until iWidth by 1) {
      val u = i.asInstanceOf[Double]/(iWidth-1)
      val v = j.asInstanceOf[Double]/(iHeight-1)
      val r = Ray(origin,lowerLeft+horizontal*u + vertical*v - origin)
      val pixelColor = rayColor(r)
      writeColor(out,pixelColor)
    }
  }
  out.close()
  println("\nDone\n")

  def rayColor(r:Ray): Vec3 = {
    val unitDir = new Vec3().unitVec(r.direction)
    val t = 0.5*(unitDir.y + 1.0)
    Vec3(1.0,1.0,1.0)*(1.0-t)+Vec3(0.5,0.7,1.0)*t
  }

  def writeColor(out: FileWriter,pixelColor:Vec3) = {
    var r = (255.999 * pixelColor.x).asInstanceOf[Int]
    var g = (255.999 * pixelColor.y).asInstanceOf[Int]
    var b = (255.999 * pixelColor.z).asInstanceOf[Int]
    out.write(f"$r%s $g%s $b%s\n")
  }
}
