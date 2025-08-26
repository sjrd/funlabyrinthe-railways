package user.sjrd.railways

import com.funlabyrinthe.core.*
import com.funlabyrinthe.core.graphics.*
import com.funlabyrinthe.mazes.*
import com.funlabyrinthe.mazes.std.*

object Railways extends Module:
  override protected def dependsOn: Set[Module] = Set(Mazes)

  override protected def preInitialize()(using Universe): Unit =
    val containingCarriage = newAttribute[Option[Carriage]](None)
  end preInitialize
  
  override protected def createComponents()(using Universe): Unit =
    val railsCreator = new RailsCreator
    val railsSwitchCreator = new RailsSwitchCreator
    val railsLightCreator = new RailsLightCreator
    val locomotiveCreator = new LocomotiveCreator
    val carriageCreator = new CarriageCreator
  end createComponents

  override protected def startGame()(using universe: Universe): Unit =
    for light <- universe.components[RailsLight] do
      light.startGame()
    for locomotive <- universe.components[Locomotive] do
      locomotive.startGame()
  end startGame

  def containingCarriage(using Universe): Attribute[Option[Carriage]] = myAttributeByID("containingCarriage")
  def railsCreator(using Universe): RailsCreator = myComponentByID("railsCreator")
  def railsSwitchCreator(using Universe): RailsSwitchCreator = myComponentByID("railsSwitchCreator")
  def locomotiveCreator(using Universe): LocomotiveCreator = myComponentByID("locomotiveCreator")
  def carriageCreator(using Universe): CarriageCreator = myComponentByID("carriageCreator")
end Railways

export Railways.*

case object GoOnRails extends Ability

def angleBetweenDirections(fromDir: Direction, toDir: Direction): Double =
  def dirToAngle(dir: Direction): Double = dir match
    case Direction.North => 0.0
    case Direction.East  => Math.PI * 0.5
    case Direction.South => Math.PI
    case Direction.West  => Math.PI * 1.5

  val fromAngle = dirToAngle(fromDir)
  val toAngle = dirToAngle(toDir)

  if Math.abs(fromAngle - toAngle) > Math.PI then
    (fromAngle + toAngle + 2*Math.PI) / 2
  else
    (fromAngle + toAngle) / 2
end angleBetweenDirections

def withRotation(gc: GraphicsContext, center: Point2D, angle: Double)(op: => Unit): Unit =
  gc.save()
  gc.translate(center.x, center.y)
  gc.rotate(angle)
  gc.translate(-center.x, -center.y)
  op
  gc.restore()
end withRotation

class RailsCreator(using ComponentInit) extends ComponentCreator:
  type CreatedComponentType = Rails

  category = ComponentCategory("rails", "Rails")

  icon += "Rails/Horizontal"
  icon += "Creators/Creator"
  
  protected def createComponent()(using init: ComponentInit): CreatedComponentType =
    new Rails()
end RailsCreator

class Rails(using ComponentInit) extends Ground derives Reflector:
  category = ComponentCategory("rails", "Rails")

  var baseField: Field = grass
  var direction: Option[Direction] = None
  var isStop: Boolean = false
  var autoStart: Boolean = true
  var additionalDelay: Int = 0

  override def reflect() = autoReflect[Rails]

  override protected def doDraw(context: DrawSquareContext): Unit =
    baseField.drawTo(context)

    // Hack not to draw the rails when dissipating neighbors
    if context.getClass() == classOf[DrawSquareContext] then
      super.doDraw(context)
  end doDraw

  override def entering(context: MoveContext): Unit = {
    import context.*
    if player.cannot(GoOnRails) then
      cancel()
  }
end Rails

class RailsSwitchCreator(using ComponentInit) extends ComponentCreator:
  type CreatedComponentType = RailsSwitch

  category = ComponentCategory("rails", "Rails")

  icon += "Buttons/SwitchOff"
  icon += "Creators/Creator"

  protected def createComponent()(using init: ComponentInit): CreatedComponentType =
    new RailsSwitch()
end RailsSwitchCreator

/** A switch that flips the direction of a target `Rails`. */
class RailsSwitch(using ComponentInit) extends Switch derives Reflector:
  category = ComponentCategory("rails", "Rails")

  var targetRails: Option[Rails] = None
  var onDirection: Option[Direction] = None
  var offDirection: Option[Direction] = None

  override def reflect() = autoReflect[RailsSwitch]

  override def switchOn(context: MoveContext): Unit =
    for rails <- targetRails do
      rails.direction = onDirection

  override def switchOff(context: MoveContext): Unit =
    for rails <- targetRails do
      rails.direction = offDirection

  override def execute(context: MoveContext): Unit =
    import context.*
    // Prevent switching if there is any train on one the target rails
    val blocked = universe.components[TrainPart].exists { trainPart =>
      trainPart.position.exists(pos => targetRails.contains(pos().field))
    }
    if !blocked then
      super.execute(context)
  end execute
end RailsSwitch

class RailsLightCreator(using ComponentInit) extends ComponentCreator:
  type CreatedComponentType = RailsLight

  category = ComponentCategory("rails", "Rails")

  icon += "Rails/LightOffNorth"
  icon += "Creators/Creator"

  protected def createComponent()(using init: ComponentInit): CreatedComponentType =
    new RailsLight()
end RailsLightCreator

class RailsLight(using ComponentInit) extends Switch with FrameUpdates derives Reflector:
  category = ComponentCategory("rails", "Rails")

  var delay: Int = 0
  var delayBeforeNextLight: Int = 0
  var nextLight: Option[RailsLight] = None
  var mirror: Option[RailsLight] = None

  offPainter = offPainter.empty + "Rails/LightOffNorth"
  onPainter = onPainter.empty + "Rails/LightOnNorth"

  @noinspect
  val turnLightOnOffQueue = TimerQueue[Boolean] { value =>
    if value then
      turnLightOn()
    else
      turnLightOff()
  }

  override def reflect() = autoReflect[RailsLight]

  def startGame(): Unit =
    if isOn then
      turnLightOn()
  end startGame

  def turnLightOn(): Unit =
    isOn = true
    if delay > 0 then
      turnLightOnOffQueue.schedule(delay, false)
    mirror.foreach(_.turnLightOn())
  end turnLightOn

  def turnLightOff(): Unit =
    isOn = false
    for next <- nextLight do
      next.turnLightOnOffQueue.schedule(delayBeforeNextLight, true)
    mirror.foreach(_.turnLightOff())
  end turnLightOff

  override def execute(context: MoveContext): Unit =
    () // disable the normal Switch behavior
end RailsLight

class TrainPart(using ComponentInit) extends PosComponent

class LocomotiveCreator(using ComponentInit) extends ComponentCreator:
  type CreatedComponentType = Locomotive

  category = ComponentCategory("trains", "Trains")

  icon += "Trains/LocomotiveNorth"
  icon += "Creators/Creator"
  
  protected def createComponent()(using init: ComponentInit): CreatedComponentType =
    new Locomotive()
end LocomotiveCreator

class Locomotive(using ComponentInit) extends TrainPart derives Reflector:
  /** The current direction. */
  var direction: Direction = Direction.North
  /** The previous direction, used to draw with the appropriate angle. */
  var previousDirection: Direction = Direction.North
  /** Time between two moves. */
  var delay: Int = 250
  /** Carriage that this locomotive tracks. */
  var tracks: Option[Carriage] = None
  /** Is this locomotive active? */
  var enabled: Boolean = true
  /** If yes, the player can move within the train. */
  var canMoveInTrain: Boolean = false

  @noinspect
  var moving: Boolean = false
  @noinspect
  var startScheduled: Boolean = false

  @noinspect
  val nextMoveQueue = TimerQueue[Unit](_ => tryMove())

  painter += "Trains/LocomotiveNorth"
  category = ComponentCategory("trains", "Trains")

  override def reflect() = autoReflect[Locomotive]

  def startGame(): Unit =
    position.map(_().field) match
      case Some(rails: Rails) if rails.autoStart =>
        startScheduled = true
        scheduleNextMove(delay = 0L)
      case _ =>
        ()
  end startGame

  def scheduleNextMove(delay: Long): Unit =
    nextMoveQueue.schedule(delay, ())

  private def tryMove(): Unit =
    for pos <- position do
      tryMove(pos)

  def tryMove(pos: SquareRef): Unit =
    // Check that we are currently on rails
    if !pos().field.isInstanceOf[Field] then
      return

    // Check that we are enabled
    if !enabled then
      return ()

    // Check the light
    pos().effect match
      case light: RailsLight if !light.isOn =>
        scheduleNextMove(delay)
        return
      case _ =>
        ()

    if false then
      scheduleNextMove(delay)
      return ()

    // Check that the destination square is free
    val destPos = pos +> direction
    val destIsOccupied = destPos.map.posComponentsBottomUp(destPos.pos).exists {
      case _: Locomotive | _: Carriage => true
      case _                           => false
    }
    if destIsOccupied then
      scheduleNextMove(delay)
      return ()

    // Update state
    moving = true
    startScheduled = false

    // Move
    previousDirection = direction
    position = Some(destPos)

    // Schedule next move
    destPos().field match
      case rails: Rails =>
        direction = rails.direction.getOrElse(direction)

        if rails.isStop then
          moving = false

        if !rails.isStop || rails.autoStart then
          scheduleNextMove(delay + rails.additionalDelay)

      case _ =>
        // not on rails anymore; stop
        moving = false
        ()
  end tryMove

  def playerEnteredTrain(): Unit =
    // Start the train if the current rails have !autoStart
    
    if moving || startScheduled then
      // already moving or starting
      ()
    else
      position.map(_().field) match
        case Some(rails: Rails) if !rails.autoStart =>
          startScheduled = true
          scheduleNextMove(rails.additionalDelay)
        case _ =>
          ()
    end if
  end playerEnteredTrain

  override protected def positionChanged(oldPos: Option[SquareRef], newPos: Option[SquareRef]): Unit =
    super.positionChanged(oldPos, newPos)

    if universe.gameStarted then
      tracks.foreach(_.position = oldPos)
  end positionChanged

  override protected def doDraw(context: DrawSquareContext): Unit =
    val center = context.rect.center
    val angle = angleBetweenDirections(previousDirection, direction)
    withRotation(context.gc, center, angle) {
      super.doDraw(context)
    }
  end doDraw
end Locomotive

class CarriageCreator(using ComponentInit) extends ComponentCreator:
  type CreatedComponentType = Carriage

  category = ComponentCategory("trains", "Trains")

  icon += "Trains/CarriageNorth"
  icon += "Creators/Creator"
  
  protected def createComponent()(using init: ComponentInit): CreatedComponentType =
    new Carriage()
end CarriageCreator

class Carriage(using ComponentInit) extends TrainPart derives Reflector:
  /** The locomotive at the head of this carriage's train. */
  var locomotive: Option[Locomotive] = None
  /** The current direction, used to draw with the appropriate angle. */
  var direction: Direction = Direction.North
  /** The previous direction, used to draw with the appropriate angle. */
  var previousDirection: Direction = Direction.North
  /** Carriage that this carriage tracks. */
  var tracks: Option[Carriage] = None
  
  category = ComponentCategory("trains", "Trains")

  painter += "Trains/CarriageNorth"

  override def reflect() = autoReflect[Carriage]

  override protected def hookEntering(context: MoveContext): Unit = {
    import context.*
    
    locomotive match
      case None =>
        () // ok, can come into free carriage
      case Some(locomotive) =>
        if locomotive.moving then
          cancel()
        else if locomotive.canMoveInTrain then
          () // ok, can move inside that train
        else
          val withinTrain = player.attributes(containingCarriage).exists {
            carriage => carriage.locomotive.contains(locomotive)
          }
          if withinTrain then
            cancel()
        end if
  }

  override protected def hookExiting(context: MoveContext): Unit = {
    import context.*

    if locomotive.exists(_.moving) then
      cancel()
  }

  override protected def hookEntered(context: MoveContext): Unit = {
    import context.*

    player.attributes(containingCarriage) = Some(this)
    locomotive.foreach(_.playerEnteredTrain())
  }

  override protected def hookExited(context: MoveContext): Unit = {
    import context.*

    player.attributes(containingCarriage) = None
  }

  override protected def positionChanged(oldPos: Option[SquareRef], newPos: Option[SquareRef]): Unit =
    super.positionChanged(oldPos, newPos)

    if universe.gameStarted && newPos.isDefined then
      previousDirection = direction

      newPos.map(_().field) match
        case Some(rails: Rails) =>
          direction = rails.direction.getOrElse(direction)
        case _ =>
          ()

      for corePlayer <- universe.players do
        val player = corePlayer.reified[Player]
        if player.attributes(containingCarriage).contains(this) then
          player.direction = Some(direction)
          player.moveTo(newPos.get)
      end for

      tracks.foreach(_.position = oldPos)
    end if
  end positionChanged

  override protected def doDraw(context: DrawSquareContext): Unit =
    val center = context.rect.center
    val angle = angleBetweenDirections(previousDirection, direction)
    withRotation(context.gc, center, angle) {
      super.doDraw(context)
    }
  end doDraw
end Carriage
