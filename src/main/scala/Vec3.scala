class Vec3(var x:Double, var y:Double, var z:Double) {

  def this() {
    this(0,0,0)
  }

  def -():Vec3 = new Vec3(-this.x, -this.y, -this.z)
  def -(v:Vec3):Vec3 = new Vec3(this.x-v.x, this.y-v.y, this.z-v.z)
  def +(v:Vec3):Vec3 = new Vec3(this.x + v.x, this.y + v.y, this.z + v.z)
  def *(t:Double):Vec3 = new Vec3(this.x * t,this.y * t, this.z * t)
  def *(v:Vec3):Vec3 = new Vec3(this.x * v.x,this.y * v.y, this.z * v.z)
  def /(t:Double):Vec3 = new Vec3(this.x * 1/t,this.y *1/t,this.z*1/t)
  def /(v:Vec3):Vec3 = new Vec3(this.x / v.x,this.y /v.y,this.z/v.z)

  def length():Double = Math.sqrt(lengthSquared())
  def lengthSquared():Double = this.x*this.x + this.y*this.y + this.z*this.z

  // Utility functions
  def +(u:Vec3,v:Vec3):Vec3 = new Vec3(u.x + v.x, u.y+v.y,u.z+v.z)
  def -(u:Vec3,v:Vec3):Vec3 = new Vec3(u.x-v.x,u.y-v.y,u.z-v.z)
  def *(u:Vec3,v:Vec3):Vec3 = new Vec3(u.x*v.x,u.y*v.y,u.z*v.z)
  def *(t:Double,v:Vec3):Vec3 =  new Vec3(t*v.x,t*v.y,t*v.z)
  def *(v:Vec3,t:Double):Vec3 = this.*(t,v)
  def /(v:Vec3,t:Double):Vec3 = this.*((1/t),v)

  def dot(u:Vec3,v:Vec3):Double =  u.x*v.x + u.y * v.y + u.z*v.z
  def cross(u:Vec3,v:Vec3):Vec3 =  new Vec3(u.y*v.z-u.z*v.y,u.z*v.x-u.x*v.z,u.x*v.y-u.y*v.x)
  def unitVec(v:Vec3) = this./(v,v.length())
}
