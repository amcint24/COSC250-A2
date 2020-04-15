package cosc250.boids

import org.scalatest._


/**
  *
  */
class Vec2Spec extends FlatSpec with Matchers {

  "Vec2" should "be able to add vectors"  in {
    Vec2(1, 2) + Vec2(3, 4) should be (Vec2(4, 6))
  }

  it should "be able to subtract vectors" in {
    Vec2(8, 9) - Vec2(3, 4) should be (Vec2(5, 5))
    Vec2(100, 100) - Vec2(100, 100) should be (Vec2(0, 0))
  }

  it should "be able to multiply a vector by a number" in {
    Vec2(8, 9) * 4 should be (Vec2(32, 36))
  }

  it should "be able to divide a vector by a number" in {
    Vec2(8, 6) / 2 should be (Vec2(4, 3))
  }

  it should "be able to find the magnitude of a vector" in {
    Vec2(0,1).magnitude should be (1)
    Vec2(0,0).magnitude should be (0)

  }

  it should "be able to find the distance between two vectors" in {
    Vec2(0,0).distance(Vec2(1,0)) should be (1)
    Vec2(0,0).distance(Vec2(2,2)) should be (Vec2(2,2).magnitude)
    Vec2(0,0).distance(Vec2(3,4)) should be (5)
  }

  it should "limit the size of a vector" in {
    Vec2(10, 0).limit(5) should be (Vec2(5, 0))
  }

  it should "calculcate vectors from a direction and an angle in radians" in {
    val Vec2(x, y) = Vec2.fromRTheta(4, Math.PI)

    // There could be a rounding error -- these are doubles, so floor them as
    // we know where Math.PI should go
    Vec2(x.floor, y.floor) should be (Vec2(-4, 0))
  }


}



