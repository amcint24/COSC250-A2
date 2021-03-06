package cosc250.boids

/**
  * A boid (bird-oid). It has a position and a velocity.
  *
  *
  * https://processing.org/examples/flocking.html
  */
case class Boid(
                 position:Vec2, velocity:Vec2
               ) {
  /** Converts a vector which represents the desired direction and converts it an optimal steering vector
    * normalised times maxSpeed ensures the steer is not negative after subtracting velocity
    * subtract velocity to convert this into a change that will be applied to velocity, not new absolute value
    * limit to max steering force for smooth movement
    * credit to processing.org flock example for concept
    * */
  def steerFactor(inputVec: Vec2):Vec2 = {
    ((inputVec.normalised * Boid.maxSpeed) - velocity).limit(Boid.maxForce)
  }

  /** Returns the distance to another boid */
  def distanceTo(otherBoid : Boid):Double = {
    position.distance(otherBoid.position)
  }

  /**
    *
    * Calculates an acceleration vector that will cause it to maintain a minimum
    * separation from its closest neighbours
    * This steer is limited to maxForce
    */
  def separate(others:Seq[Boid]):Vec2 = {

    if (others.nonEmpty) {
      // For each other boid calculate the vector pointing away from that boid. Divide this vector by the distance
      // to that boid to give more weight to close boids. Sum all the results together into a vector.
      val pointingAway:Vec2 = others.foldLeft(Vec2(0.0, 0.0)) { (awaySum, otherBoid) =>
        awaySum + (position - otherBoid.position).normalised / (distanceTo(otherBoid))
      }
      // Take the average of the vectors pointing away and create a steering vector in that direction.
      val weightedAverage:Vec2 = pointingAway / others.length
      steerFactor(weightedAverage)
    } else {
      Vec2(0.0,0.0)
    }
  }

  /**
    * Calculates an acceleration vector that will cause it align its direction and
    * velocity with other birds.
    */
  def align(others:Seq[Boid]):Vec2 = {
    if (others.nonEmpty){
      // Create a vector which is the sum of all other boids' velocities
      val alignedDirection:Vec2 = others.foldLeft(Vec2(0.0, 0.0)) { (alignSum, otherBoid) =>
        alignSum + otherBoid.velocity
      }
      // Take the average velocity and create steering vector toward it
      val weightedAverage:Vec2 = alignedDirection / others.length
      steerFactor(weightedAverage)
    } else {
      Vec2(0.0, 0.0)
    }
  }

  /**
    * Calculates an acceleration that will keep it near its neighbours and maintain
    * the flock cohesion
    */
  def cohesion(others:Seq[Boid]):Vec2 = {

    if (others.nonEmpty){
      // Create a vec2 which is the sum of the positions of all neighbours
      val positionsSum:Vec2 = others.foldLeft(Vec2(0.0,0.0)){ (posSum,otherBoid) =>
         posSum + otherBoid.position
      }
      // Calculate the mean position and create a Vec2 towards that position
      val neighsCentre:Vec2 = positionsSum / others.length
      val towardsTarget:Vec2 = neighsCentre - position

      steerFactor(towardsTarget)
    } else {
      Vec2(0.0,0.0)
    }
  }

  /**
    * Makes new Seqs of boids for flocking calculation - Other boids (not this) with desired separation (DS)
    * and neighbour distance (ND) Then calls the flocking behaviour functions on the appropriate Seqs, and sums
    * the return into a vector. The separate function has additional weighting to achieve the desired
    * effect - credit to processing.org
    */
  def flock():Vec2 = {
    val allBoids = Simulation.currentFrame
    // Function to check if a boid is within a certain range and not the same boid (based on position)
    val withinRange:(Boid, Double) => Boolean = { (boid, range) =>
      distanceTo(boid) < range && boid != this
    }

    val withinDS:Seq[Boid] = allBoids.filter(otherBoid => withinRange(otherBoid, Boid.desiredSeparation))
    val withinND:Seq[Boid] = allBoids.filter(otherBoid => withinRange(otherBoid, Boid.neighBourDist))

    (separate(withinDS) * 1.5) + align(withinND) + cohesion(withinND)
  }

  /**
    * Update the position of this boid. Sets the new velocity using the flock function, limiting to max speed.
    * Adds wind to new velocity if present. Adds the updated velocity to the previous position, using the wrap functions.
    * Returns a new boid with updated details.
    */
  def update(wind:Vec2):Boid = {
    val newVelocity:Vec2 = (velocity + flock()).limit(Boid.maxSpeed) + wind
    val newX:Double = wrapX(position.x + newVelocity.x)
    val newY:Double = wrapY(position.y + newVelocity.y)
    val newPosition:Vec2 = Vec2(newX, newY)
    Boid(newPosition, newVelocity)
  }

  def wrapX(x:Double):Double = {
    if (x > Boid.maxX) x - Boid.maxX else if (x < 0) x + Boid.maxX else x
  }

  def wrapY(y:Double):Double = {
    if (y > Boid.maxY) y - Boid.maxY else if (y < 0) y + Boid.maxY else y
  }

  /** Inequality method based on position - position is the only equality that impacts the arithmetic of flocking */
  def !=(other:Boid):Boolean = {
    this.position != other.position
  }
}

object Boid {
  /** How far apart the boids want to be */
  val desiredSeparation = 25

  /** Maximum flying velocity of a boid */
  val maxSpeed = 1.5

  /** maximum accelaration of a boid */
  val maxForce = 0.03

  /** Other boids within this range are considered neighbours */
  val neighBourDist = 50

  /** Wrap width of the simulation. ie, for any Boid, 0 <= x < 640 */
  def maxX:Int = Simulation.width

  /** Wrap height of the simulation. ie, for any Boid, 0 <= y < 480 */
  def maxY:Int = Simulation.height

}


