import scala.util.{Random, Success, Try, Failure}
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

// some exceptions for things that might go wrong in the individual steps
// (we'll need some of them later, use the others when experimenting
// with the code):
case class GrindingException(msg: String) extends Exception(msg)
case class FrothingException(msg: String) extends Exception(msg)
case class WaterBoilingException(msg: String) extends Exception(msg)
case class BrewingException(msg: String) extends Exception(msg)

// Some type aliases, just for getting more meaningful method signatures:
type CoffeeBeans = String
type GroundCoffee = String
case class Water(temperature: Int)
type Milk = String
type FrothedMilk = String
type Espresso = String
type Cappuccino = String
// dummy implementations of the individual steps:
def grind(beans: CoffeeBeans): Future[GroundCoffee] = Future {
  println("start grinding...")
  Thread.sleep(Random.nextInt(200))
  if (beans == "baked beans") throw GrindingException("are you joking?")
  println("finished grinding...")
  s"ground coffee of $beans"
}

def heatWater(water: Water): Future[Water] = Future {
  println("heating the water now")
  Thread.sleep(Random.nextInt(200))
  println("hot, it's hot!")
  water.copy(temperature = 85)
}

def frothMilk(milk: Milk): Future[FrothedMilk] = Future {
  println("milk frothing system engaged!")
  Thread.sleep(Random.nextInt(200))
  println("shutting down milk frothing system")
  s"frothed $milk"
}

def brew(coffee: GroundCoffee, heatedWater: Water): Future[Espresso] = Future {
  println("happy brewing :)")
  Thread.sleep(Random.nextInt(200))
  println("it's brewed!")
  "espresso"
}

def combine(espresso: Espresso, frothedMilk: FrothedMilk): Cappuccino = "cappuccino"
// going through these steps sequentially:
def prepareCappuccinoSeq(): Future[Cappuccino] = for {
  ground <- grind("arabica beans")
  water <- heatWater(Water(25))
  espresso <- brew(ground, water)
  foam <- frothMilk("milk")
} yield combine(espresso, foam)

def prepareCappuccino(): Future[Cappuccino] = {
  val groundCoffee = grind("arabica beans")
  val heatedWater = heatWater(Water(20))
  val frothedMilk = frothMilk("milk")
  for {
    ground <- groundCoffee
    water <- heatedWater
    foam <- frothedMilk
    espresso <- brew(ground, water)
  } yield combine(espresso, foam)
}

Await.result(prepareCappuccino(),atMost = 5 seconds)

