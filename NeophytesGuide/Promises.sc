import concurrent.{Future, Promise, Await}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

case class TaxCut(reduction: Int)

val taxcut = Promise[TaxCut]()
val taxcut2: Promise[TaxCut] = Promise()

val taxcutF: Future[TaxCut] = taxcut.future

taxcut.success(TaxCut(20))
println( Await.result(taxcutF,atMost = 2 seconds).toString )

object Government {
  def redeemCampaignPledge(): Future[TaxCut] = {
    val p = Promise[TaxCut]()
    Future {
      println("Starting the new legislative period.")
      Thread.sleep(2000)
      p.success(TaxCut(20))
      println("We reduced the taxes! You must reelect us!!!!1111")
    }
    p.future
  }
}

println( Await.result(Government.redeemCampaignPledge(),atMost = 5 seconds).toString )