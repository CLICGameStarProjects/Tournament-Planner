package planner

import scala.math.Ordered.orderingToOrdered

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime}
import scala.io.Source
import java.time.Duration
import scala.util.Random

@main def findSchedule(): Unit =

  import Planner.*

  val (gameNames, conflicts) = parseConflictMatrix("src/main/resources/conflict_matrix.csv")

  // Random.setSeed(42)

  val updatedConflicts = adjustConflicts(conflicts, independentSets)

  assert(updatedConflicts.getOrElse(("LoL", "Valo"), 0) == 1000)

  val constraints = Constraints(
    totalDuration = endHour,
    breaks = breaks
  )

  val schedule = optimizedSchedule(games, updatedConflicts, constraints, 20).sorted

  assert(schedule.size == games.size)

  println("Optimized schedule conflicts: " + calculateTotalConflicts(schedule, updatedConflicts))
  printSchedule(schedule, startTime)

object Planner:
  import Time.*

  type ConflictMatrix = Map[(String, String), Int]

  case class Constraints(
      totalDuration: Int,
      breaks: List[Slot]
  )

  case class Game(name: String, duration: Int, forbiddenSlots: List[Slot] = List())

  case class Slot(start: Int, end: Int):
    def this(start: Time, end: Time) =
      this(startTime.timeTo(start), startTime.timeTo(end))
    def overlaps(that: Slot): Boolean =
      Math.max(this.start, that.start) < Math.min(this.end, that.end)
  object Slot:
    def apply(start: Time, end: Time): Slot = new Slot(start, end)

  case class ScheduleSlot(start: Int, game: Game) extends Ordered[ScheduleSlot]:

    override def compare(that: ScheduleSlot): Int =
      (start, end, game.name).compare((that.start, that.end, game.name))

    def duration = game.duration
    def end = start + game.duration
    def toSlot = Slot(start, start + game.duration)
    def shift(shift: Int) = ScheduleSlot(start + shift, game)

    def overlaps(that: ScheduleSlot): Boolean =
      this.toSlot.overlaps(that.toSlot)
    def overlaps(that: Slot): Boolean =
      this.toSlot.overlaps(that)

  def parseConflictMatrix(path: String): (List[String], ConflictMatrix) =
    val lines = Source.fromFile(path).getLines().toList
    val headers = lines.head.split(",").tail.map(_.trim).toList
    val matrix = lines.tail.flatMap { line =>
      val cols = line.split(",").map(_.trim)
      val rowGame = cols.head
      cols.tail.zipWithIndex.map {
        case (value, colIndex) =>
          val colGame = headers(colIndex)
          ((rowGame, colGame), value.toInt)
      }
    }.toMap
    (headers, matrix)

  def adjustConflicts(conflicts: ConflictMatrix, independentSets: Set[Set[String]]): ConflictMatrix =
    conflicts.map {
      case (games @ (g1, g2), conflict) if g1 == g2                                                     => (games, 0)
      case (games @ (g1, g2), _) if independentSets.exists(set => set.contains(g1) && set.contains(g2)) => (games, 1000)
      case other                                                                                        => other
    }

  def maxTotalConflicts(conflicts: ConflictMatrix): Int =
    calculateTotalConflicts(games.map(ScheduleSlot(0, _)), conflicts)

  def calculateConflict(game1: Game, game2: Game, conflicts: ConflictMatrix): Int =
    conflicts.getOrElse((game1.name, game2.name), 0)

  def calculateGameConflict(gameSlot: ScheduleSlot, schedule: List[ScheduleSlot], conflicts: ConflictMatrix): Int =
    schedule.filter(gameSlot.overlaps(_)).map(game => calculateConflict(game.game, gameSlot.game, conflicts)).sum

  def calculateTotalConflicts(schedule: List[ScheduleSlot], conflicts: ConflictMatrix): Int =
    schedule.combinations(2).collect {
      case List(slot1, slot2) if slot1.overlaps(slot2) =>
        calculateConflict(slot1.game, slot2.game, conflicts)
    }.sum

  def isValidSlot(gameSlot: ScheduleSlot, constraints: Constraints) =
    gameSlot.end <= constraints.totalDuration
      && constraints.breaks.forall(!gameSlot.overlaps(_))
      && gameSlot.game.forbiddenSlots.forall(!gameSlot.overlaps(_))

  // Find the closest start time that results in a valid slot
  def closestValidSlot(gameSlot: ScheduleSlot, constraints: Constraints): Option[ScheduleSlot] =
    if isValidSlot(gameSlot, constraints) then
      Some(gameSlot)
    else
      if gameSlot.start + gameSlot.duration > constraints.totalDuration then None
      else closestValidSlot(gameSlot.shift(1), constraints)

  def createBasicSchedule(games: List[Game], constraints: Constraints): List[ScheduleSlot] =
    games.flatMap(game => closestValidSlot(ScheduleSlot(0, game), constraints))

  def allValidGameSlots(game: Game, constraints: Constraints): List[ScheduleSlot] =
    (0 until constraints.totalDuration).toList
      .flatMap(start => closestValidSlot(ScheduleSlot(start, game), constraints))
      .distinct

  def optimizedSchedule(
      games: List[Game],
      conflicts: ConflictMatrix,
      constraints: Constraints,
      iterations: Int = 10
  ): List[ScheduleSlot] =
    def findBestSlot(game: Game, schedule: List[ScheduleSlot]): Option[ScheduleSlot] =
      Random.shuffle(allValidGameSlots(game, constraints))
        .minByOption(slot => calculateGameConflict(ScheduleSlot(slot.start, game), schedule, conflicts))

    def optimize(schedule: List[ScheduleSlot], iterations: Int): List[ScheduleSlot] =
      if iterations <= 0 then schedule
      else
        val newSchedule =
          schedule.foldLeft(schedule)((sched, slot) =>
            val schedWithoutSlot = sched.filterNot(_ == slot)
            findBestSlot(slot.game, schedWithoutSlot) match
              case Some(newSlot) => newSlot :: schedWithoutSlot
              case _             => sched
          )

        optimize(
          if calculateTotalConflicts(newSchedule, conflicts) < calculateTotalConflicts(schedule, conflicts) then
            newSchedule
          else schedule,
          iterations - 1
        )

    val initialOptimizedSchedule = createBasicSchedule(games, constraints)
    // games.foldLeft((List[ScheduleSlot]()))((schedule, game) =>
    //   findBestSlot(game, schedule) match
    //     case Some(slot) =>
    //       slot :: schedule
    //     case None =>
    //       schedule
    // )

    optimize(initialOptimizedSchedule, iterations)

  def printSchedule(schedule: List[ScheduleSlot], startHour: Time): Unit =
    schedule.foreach(x =>
      println(
        s"${startHour.plusHours(x.start)}h - " +
          s"${startHour.plusHours(x.start + x.game.duration)}h : " +
          s"${gameFancyNames.getOrElse(x.game.name, x.game.name)}"
      )
    )

  enum Time(hour: Int, day: Int, month: Int, year: Int):
    case Sat(hour: Int) extends Time(hour, 7, 12, 2024)
    case Sun(hour: Int) extends Time(hour, 8, 12, 2024)

    val formatter = DateTimeFormatter.ofPattern("EEE HH")
    val dateTime: LocalDateTime = LocalDateTime.of(year, month, day, hour, 0)

    override def toString(): String = dateTime.format(formatter)
    def timeTo(that: Time): Int =
      Duration.between(this.dateTime, that.dateTime).toHours.toInt
    def plusHours(hours: Int): Time =
      this match
        case Sat(hour) if hour + hours < 24 => Sat(hour + hours)
        case Sat(hour)                      => Sun((hour + hours) % 24)
        case Sun(hour) if hour + hours < 24 => Sun(hour + hours)
        case _                              => throw IllegalArgumentException("Cannot add hours beyond Sunday")

  val startTime = Sat(10)
  val endTime = Sun(7)
  val startHour = 0
  val endHour = startTime.timeTo(endTime)

  val breaks = List(Slot(Sat(12), Sat(13)), Slot(Sat(19), Sat(20)))

  val noNight = List(Slot(Sat(23), Sun(8)))
  val noDeepNight = List(Slot(Sun(2), endTime))
  val noLateAf = List(Slot(Sun(3), endTime))
  val noMorning = List(Slot(startTime, Sat(12)))
  val noEarly = List(Slot(startTime, Sat(17)))
  val onlyAfternoon = List(
    Slot(startTime, Sat(12)),
    Slot(Sat(18), endTime)
  )

  val games = List(
    Game("Among_Us", 2, noDeepNight),
    Game("Babyfoot", 3, noNight),
    Game("Balatro", 1, noNight),
    Game("Geoguessr", 1, noNight),
    Game("LoL", 4),
    Game("LG_One_night", 1, noNight ++ noEarly),
    Game("MK8dx", 4, noNight),
    Game("HG_Minecraft", 2, noNight),
    Game("OSU", 1, noDeepNight),
    Game("Overcooked2", 1, noNight),
    Game("Overwatch2", 3),
    Game("Ping-pong", 4, noNight),
    Game("RL_3v3", 3),
    Game("Starcraft2", 5),
    Game("Smash_1v1", 4, noLateAf),
    Game("Smash_2v2", 4),
    Game("Valo", 3),
    Game("Echecs", 2, noNight),
    Game("TriviaPoly", 1, onlyAfternoon)
  )

  val independentSets = Set(
    Set("MK8dx", "Smash_1v1", "Smash_2v2"),
    Set("Echecs", "Ping-pong", "LG_One_night"),
    Set("LoL", "Valo"),
    Set("Among_Us", "LG_One_night")
  )

  val gameFancyNames = Map(
    "Among_Us" -> "Among Us",
    "Babyfoot" -> "Babyfoot",
    "Balatro" -> "Balatro",
    "Geoguessr" -> "Geoguessr",
    "LoL" -> "League of Legends",
    "LG_One_night" -> "Loup-garou pour une nuit",
    "MK8dx" -> "Mario Kart 8DX (Nintendo Switch)",
    "HG_Minecraft" -> "Minecraft (Hunger Games)",
    "OSU" -> "OSU",
    "Overcooked2" -> "Overcooked 2",
    "Overwatch2" -> "Overwatch 2",
    "Ping-pong" -> "Ping-pong",
    "RL_3v3" -> "Rocket League (3 vs 3)",
    "Starcraft2" -> "StarCraft 2",
    "Smash_1v1" -> "Super Smash Bros Ultimate 1 vs 1 (Nintendo Switch)",
    "Smash_2v2" -> "Super Smash Bros Ultimate 2 vs 2 (Nintendo Switch)",
    "Valo" -> "Valorant",
    "Echecs" -> "Échecs",
    "TriviaPoly" -> "TriviaPoly"
  )
