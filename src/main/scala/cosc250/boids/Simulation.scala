package cosc250.boids

import scala.collection.mutable

object Simulation {

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

  /** Takes a boid and adds the startleStrength, returning new boid */
  val startleFunction:Boid => Boid = {
    boid:Boid => {Boid(boid.position, boid.velocity + Vec2.randomDir(startleStrength))}
  }

  /** A mutable queue containing the last `frameMemory frames` */
  val queue:mutable.Queue[Seq[Boid]] = mutable.Queue.empty[Seq[Boid]]

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

  /** A container that can hold a boid to add on the next frame */
  var insertBoid:Option[Boid] = None

  /**
    * If the startle boids button is pressed this variable will be populated with the startleFunction
    * This allows the function be 'stored' waiting to be called in the next frame update and cleared when finished
    */
  var oneTimeFunction:Option[Boid => Boid] = None

  /**
    * Resets the events that should occur one time only
    */
  def resetOneTimeEvents():Unit = {
    oneTimeFunction = None
    insertBoid = None
  }

  /** Generates boids in the centre of the simulation, moving at v=1 in a random direction */
  def explosionOfBoids(i:Int):Seq[Boid] = {
    (0 until numBoids).map(i=>Boid(Vec2(Boid.maxX/2,Boid.maxY/2),Vec2.randomDir(1)))
  }

  /** Manages the memory buffer. Adds newest frame to the tail of the buffer and drops oldest off the head */
  def pushState(boids:Seq[Boid]):Seq[Boid] = {
    queue.enqueue(boids)

    // Drops a frame from the queue if we've reached the maximum number of frames to remember
    if (queue.lengthCompare(frameMemory) > 0) queue.dequeue()

    boids
  }

  /** Called by a click to the canvas, to say that in the next frame, a boid should be inserted */
  def pushBoid(boid:Boid):Unit = {
    insertBoid = Some(boid)
  }

  /** Called by the Action Replay button to jump back in the memory buffer */
  def resetQueue():Seq[Boid] = {
    queue(queue.length-1) = queue(0)
    queue(queue.length-1)
  }

  /** Generate the next frame in the simulation */
  def update():Seq[Boid] = {
    val windVec = wind.getOrElse(Vec2(0,0))
    // Function to update a single boid with wind
    val boidUpdate:Boid => Boid = boid=>boid.update(windVec)

    // Does a conventional update of all boids, then adds an additional one and startles them all if triggered
    val lastFrame = queue(queue.length-1)
    val frameUpdate = lastFrame.map(boid => boidUpdate(boid))
    val addBoid = if (insertBoid.isEmpty) {frameUpdate} else {frameUpdate :+ insertBoid.get}
    val startledBoids = addBoid.map(boid => if (oneTimeFunction.nonEmpty) {oneTimeFunction.get(boid)} else boid)

    resetOneTimeEvents()

    pushState(startledBoids)
  }

}
