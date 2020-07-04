import java.io.{File, FileWriter}

import scala.util.Random

object Raytracing extends App{

  val random = new Random()



  val aspect = 16.0 / 9.0
  val iWidth:Int = 384
  val iHeight:Int = (iWidth/aspect).asInstanceOf[Int]
  val samplesPerPixel = 100
  val maxDepth = 50

  val out = new FileWriter("output.ppm")
  out.write("P3\n" + iWidth + ' ' + iHeight + "\n255\n")


  var world = new HittableList()
  world.add(new Sphere(new Vec3(0,0,-1),0.5, new Lambertian(new Vec3(0.7,0.3,0.3))))
  world.add(new Sphere(new Vec3(0,-100.5,-1),100, new Lambertian(new Vec3(0.8,0.8,0.0))))

  world.add(new Sphere(new Vec3(1,0,-1),0.5,new Metal(new Vec3(0.8,0.6,0.2))))
  world.add(new Sphere(new Vec3(-1,0,-1),0.5,new Metal(new Vec3(0.8,0.8,0.8))))

  val cam = new Camera()



  for (j <- iHeight-1 to 0 by -1) {
    println("\rScanlines remaining: " + j)
    for (i <- 0 until iWidth by 1) {
      var pixelColor = new Vec3(0,0,0)
      for (s <- 0 until samplesPerPixel) {
        val u = (i + random.nextDouble())/(iWidth-1)
        val v = (j + random.nextDouble())/(iHeight-1)
        val r = cam.getRay(u,v)
        pixelColor += rayColor(r,world,maxDepth)
      }

      writeColor(out,pixelColor,samplesPerPixel)
    }
  }
  out.close()
  println("\nDone\n")

  def rayColor(r:Ray,world:Hittable, depth:Int): Vec3 = {
    if (depth <= 0) return new Vec3(0,0,0)

    world.hit(r,0.001,Double.MaxValue) match {
      case Some(hitRecord) => {
        //val target = hitRecord.p + hitRecord.normal + randomInUnitSphere()
        //val target = hitRecord.p + hitRecord.normal + randomUnitVector()
        //val target = hitRecord.p + randomInHemisphere(hitRecord.normal)
        hitRecord.mat.scatter(r,hitRecord) match {
          case Some(ScatterRecord(attenuation,scattered)) =>
            return attenuation*rayColor(scattered,world,depth+1)
          case None => {
            return new Vec3(0,0,0)
          }
        }
      }
      case None => {

      }
    }

    val unitDir = new Vec3().unitVec(r.direction)
    val t = 0.5*(unitDir.y + 1.0)
    new Vec3(1.0,1.0,1.0)*(1.0-t) + new Vec3(0.5,0.7,1.0) * t
  }

  def writeColor(out: FileWriter,pixelColor:Vec3,samplesPerPixel:Int) = {
    var r = pixelColor.x
    var g = pixelColor.y
    var b = pixelColor.z

    // Divide color total by number of samples
    val scale = 1.0/samplesPerPixel
    r = Math.sqrt(scale * r)
    g = Math.sqrt(scale * g)
    b = Math.sqrt(scale * b)

    val ir = (256*clamp(r,0.0,0.999)).asInstanceOf[Int]
    val ig = (256*clamp(g,0.0,0.999)).asInstanceOf[Int]
    val ib = (256*clamp(b,0.0,0.999)).asInstanceOf[Int]
    out.write(f"$ir $ig $ib\n")
  }

  def clamp(x:Double,min:Double,max:Double):Double = {
    if (x < min) min else if (x>max) max else x
  }

  def randomDouble(r:Random,min:Double,max:Double): Double = {
    min + (max-min)*r.nextDouble()
  }

  def randomInUnitSphere(): Vec3 = {
    var p = new Vec3(0,0,0)
    do {
      p = new Vec3(random.nextDouble(),random.nextDouble(),random.nextDouble()) * 2.0 - new Vec3(1,1,1)
    } while (p.lengthSquared() >= 1.0)
    p
  }

  def randomUnitVector():Vec3 = {
    val a = randomDouble(random,0,2*Math.PI)
    val z = randomDouble(random,-1,1)
    val r = Math.sqrt(1-z*z)
    new Vec3(r*Math.cos(a),r*Math.sin(a),z)
  }

  def randomInHemisphere(normal:Vec3) = {
    val inUnitSphere = randomInUnitSphere()
    if (new Vec3().dot(inUnitSphere,normal) > 0.0) inUnitSphere else inUnitSphere.-;
  }

//  def hitSphere(center:Vec3,radius:Double,r:Ray):Double = {
//    val oc = r.origin - center
//    val a = r.direction.lengthSquared()
//    val halfB = new Vec3().dot(oc,r.direction)
//    val c = oc.lengthSquared() - radius*radius
//    val disc = halfB*halfB -a *c
//    if (disc < 0) {
//      -1.0
//    } else {
//      (-halfB - Math.sqrt(disc)) / a
//    }
//  }
}
