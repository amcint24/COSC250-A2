package cosc250.boids

import scala.collection.mutable

object Simulation {

  /** Simulation Parameters */

    /** Wrap width of the simulation. ie, for any Boid, 0 <= x < 640 */
    val width = 640

    /** Wrap height of the simulation. ie, for any Boid, 0 <= y < 480 */
    val height = 480

    /** How many frames of the simulation to hold */
    val frameMemory = 60

    /** How manby boids to start with in the simulation */
    val numBoids = 150

    /** When the wind is blowing, how strongly it blows */
    val windStrength = 0.03

    /** When the boids are startled, the strength of the vector that is applied to each of them */
    val startleStrength:Double = Boid.maxSpeed

  /** Wind Management */

    /** The wind -- an optional acceleration vector */
    var wind:Option[Vec2] = None

    /**
      * Sets a wind blowing at windStrength, at this angle.
      * Note that a northerly wind blows **from** the north, so we multiply the vector by -1.
      */
    def setWindDirection(theta:Double):Unit = {
      val newWind = Vec2.fromRTheta(windStrength,theta) * -1
      wind = Option(newWind)
    }

  /** Queue Management */

    /** A mutable queue containing the last `frameMemory frames` */
    val queue:mutable.Queue[Seq[Boid]] = mutable.Queue.empty[Seq[Boid]]

    /** Manages the memory buffer. Adds newest frame to the tail of the buffer and drops oldest off the head */
    def pushState(boids:Seq[Boid]):Unit = {
      queue.enqueue(boids)

      // Drops a frame from the queue if we've reached the maximum number of frames to remember
      if (queue.lengthCompare(frameMemory) > 0) queue.dequeue()
    }

    /** Called by the Action Replay button to jump back in the memory buffer
      * Sets the next current frame to be the previous oldest frame using pushState
      */
    def resetQueue():Unit = {
      pushState(queue(0))
    }

    /** Returns the last frame in the queue aka the current frame */
    def currentFrame:Seq[Boid] = {
      queue(queue.length-1)
    }

  /** Boid Management */

    /** Generates boids in the centre of the simulation, moving at v=1 in a random direction */
    def explosionOfBoids(i:Int):Seq[Boid] = {
      (0 until i).map(_ => Boid(Vec2(Boid.maxX/2,Boid.maxY/2),Vec2.randomDir(1)))
    }

    /** Creates a new state of boids which have been 'startled' and pushes that state to the queue */
    def startleFunction():Unit = {
      // Function to startle a single boid
      val doStartle:Boid => Boid = boid => Boid(boid.position, boid.velocity + Vec2.randomDir(startleStrength))

      val startledBoids = currentFrame.map(boid => doStartle(boid))
      pushState(startledBoids)
    }

    /** Called by a click to the canvas, pushes a new state with an additional boid as requested by the canvas */
    def pushBoid(boid:Boid):Unit = {
      val addBoid = currentFrame :+ boid
      pushState(addBoid)
    }

    /** Updates all boids with normal flocking behavior into a new state and pushes that state to the queue */
    def update():Seq[Boid] = {
      val windVec:Vec2 = wind.getOrElse(Vec2(0,0))
      // Function to update a single boid with wind
      val boidUpdate:Boid => Boid = boid=>boid.update(windVec)

      // Do an update of all boids
      val frameUpdate:Seq[Boid] = currentFrame.map(boid => boidUpdate(boid))

      // Push update to queue and return it
      pushState(frameUpdate)
      frameUpdate
    }
}
