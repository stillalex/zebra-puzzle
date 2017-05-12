
object Zebra {

  type HOUSE = String
  val green: HOUSE = "Green"
  val red: HOUSE = "Red"
  val ivory: HOUSE = "Ivory"
  val yellow: HOUSE = "Yellow"
  val blue: HOUSE = "Blue"
  val hl: List[HOUSE] = List(green, red, ivory, yellow, blue)

  type NATIONALITY = String
  val english: NATIONALITY = "English"
  val spanish: NATIONALITY = "Spanish"
  val ukrainian: NATIONALITY = "Ukrainian"
  val norwegian: NATIONALITY = "Norwegian"
  val japanese: NATIONALITY = "Japanese"
  val nl: List[NATIONALITY] = List(english, spanish, ukrainian, norwegian, japanese)

  type DRINK = String
  val tea: DRINK = "tea"
  val milk: DRINK = "milk"
  val coffee: DRINK = "coffee"
  val water: DRINK = "water"
  val oj: DRINK = "oj"
  val dl: List[DRINK] = List(tea, milk, coffee, water, oj)

  type PET = String
  val horse: PET = "horse"
  val dog: PET = "dog"
  val fox: PET = "fox"
  val zebra: PET = "zebra"
  val snails: PET = "snails"
  val pl: List[PET] = List(horse, dog, fox, zebra, snails)

  type SMOKE = String
  val kools: SMOKE = "kools"
  val parliaments: SMOKE = "parliaments"
  val lucky: SMOKE = "lucky"
  val chesterfield: SMOKE = "chesterfield"
  val oldgold: SMOKE = "oldgold"
  val sl: List[SMOKE] = List(kools, parliaments, lucky, chesterfield, oldgold)

  case class Entry(h: List[HOUSE], n: Option[List[NATIONALITY]], d: Option[List[DRINK]], p: Option[List[PET]], s: Option[List[SMOKE]]) {

    override def toString(): String = {
      val h0 = "-- Sol --\n"
      val l0 = h.mkString("|", "|", "|" + "\n")
      val l1 = n.get.mkString("|", "|", "|" + "\n")
      val l2 = d.get.mkString("|", "|", "|" + "\n")
      val l3 = p.get.mkString("|", "|", "|" + "\n")
      val l4 = s.get.mkString("|", "|", "|" + "\n")
      val f0 = "-- --- --"
      h0 + l0 + l1 + l2 + l3 + l4 + f0
    }

  }

  def genSols(): Iterator[Entry] = {

    val h = hl.permutations.flatMap(filter(_, None, None, None, None))

    val hn = h.flatMap(e => {
      nl.permutations.flatMap(n => filter(e.h, Some(n), None, None, None))
    })

    val hnd = hn.flatMap(e => {
      dl.permutations.flatMap(d => filter(e.h, e.n, Some(d), None, None))
    })

    val hndp = hnd.flatMap(e => {
      pl.permutations.flatMap(p => filter(e.h, e.n, e.d, Some(p), None))
    })

    val hndps = hndp.flatMap(e => {
      sl.permutations.flatMap(s => filter(e.h, e.n, e.d, e.p, Some(s)))
    })

    hndps
  }

  def filter(h: List[HOUSE], nats: Option[List[NATIONALITY]], drinks: Option[List[DRINK]], pets: Option[List[PET]], smokes: Option[List[SMOKE]]): Option[Entry] = {

    //    The green house is immediately to the right of the ivory house.
    if (h.indexOf(green) != h.indexOf(ivory) + 1) {
      return None
    }
    if (nats.isEmpty && drinks.isEmpty && pets.isEmpty && smokes.isEmpty) {
      return Some(Entry(h, None, None, None, None))
    }

    val n = nats.get
    //    The Norwegian lives in the first house.
    if (n.indexOf(norwegian) != 0) {
      return None
    }
    //    The Englishman lives in the red house.
    if (n.indexOf(english) != h.indexOf(red)) {
      return None
    }
    //    The Norwegian lives next to the blue house.
    if (n.indexOf(norwegian) != h.indexOf(blue) - 1) {
      return None
    }
    if (drinks.isEmpty && pets.isEmpty && smokes.isEmpty) {
      return Some(Entry(h, nats, None, None, None))
    }

    val d = drinks.get
    //    Coffee is drunk in the green house.
    if (d.indexOf(coffee) != h.indexOf(green)) {
      return None
    }
    //    The Ukrainian drinks tea.
    if (n.indexOf(ukrainian) != d.indexOf(tea)) {
      return None
    }
    //    Milk is drunk in the middle house.
    if (d.indexOf(milk) != 2) {
      return None
    }
    if (pets.isEmpty && smokes.isEmpty) {
      return Some(Entry(h, nats, drinks, None, None))
    }

    val p = pets.get
    //    The Spaniard owns the dog.
    if (n.indexOf(spanish) != p.indexOf(dog)) {
      return None
    }
    if (smokes.isEmpty) {
      return Some(Entry(h, nats, drinks, pets, None))
    }

    val s = smokes.get
    //    The Old Gold smoker owns snails.
    if (s.indexOf(oldgold) != p.indexOf(snails)) {
      return None
    }
    //    Kools are smoked in the yellow house.
    if (s.indexOf(kools) != h.indexOf(yellow)) {
      return None
    }
    //    The Lucky Strike smoker drinks orange juice.
    if (s.indexOf(lucky) != d.indexOf(oj)) {
      return None
    }
    //    The Japanese smokes Parliaments.
    if (s.indexOf(parliaments) != n.indexOf(japanese)) {
      return None
    }
    //    The man who smokes Chesterfields lives in the house next to the man with the fox.
    if (s.indexOf(chesterfield) != (p.indexOf(fox) + 1) && s.indexOf(chesterfield) != (p.indexOf(fox) - 1)) {
      return None
    }
    //    Kools are smoked in the house next to the house where the horse is kept.
    if (s.indexOf(kools) != (p.indexOf(horse) + 1) && s.indexOf(kools) != (p.indexOf(horse) - 1)) {
      return None
    }

    return Some(Entry(h, nats, drinks, pets, smokes))
  }

  def main(args: Array[String]): Unit = {
    val s = System.currentTimeMillis();
    val sols = genSols()
    val e = System.currentTimeMillis() - s;
    System.err.println(s"took $e ms.")
    sols.foreach(println(_))
  }

}