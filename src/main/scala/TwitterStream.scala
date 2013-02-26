package bcomposes.twitter

import twitter4j._

object StatusStreamer {
  def main(args: Array[String]) {
    val twitterStream = new TwitterStreamFactory().getInstance
    twitterStream.addListener(Util.simpleStatusListener)
    twitterStream.sample
    Thread.sleep(2000)
    twitterStream.cleanUp
    twitterStream.shutdown
  }
}

object FollowIdsStreamer {
  def main(args: Array[String]) {
    val twitterStream = new TwitterStreamFactory().getInstance
    twitterStream.addListener(Util.simpleStatusListener)
    twitterStream.filter(new FilterQuery(Array(1344951,5988062,807095,3108351)))
    Thread.sleep(10000)
    twitterStream.cleanUp
    twitterStream.shutdown
  }
}

object SearchStreamer {
  def main(args: Array[String]) {
    val twitterStream = new TwitterStreamFactory().getInstance
    twitterStream.addListener(Util.simpleStatusListener)
    twitterStream.filter(new FilterQuery().track(args))
    Thread.sleep(10000)
    twitterStream.cleanUp
    twitterStream.shutdown
  }
}

object AustinStreamer {
  def main(args: Array[String]) {
    val twitterStream = new TwitterStreamFactory().getInstance
    twitterStream.addListener(Util.simpleStatusListener)
    val austinBox = Array(Array(-97.8,30.25),Array(-97.65,30.35))
    twitterStream.filter(new FilterQuery().locations(austinBox))
    Thread.sleep(10000)
    twitterStream.cleanUp
    twitterStream.shutdown
  }
}

object LocationStreamer {
  def main(args: Array[String]) {
    val boundingBoxes = args.map(_.toDouble).grouped(2).toArray
    val twitterStream = new TwitterStreamFactory().getInstance
    twitterStream.addListener(Util.simpleStatusListener)
    twitterStream.filter(new FilterQuery().locations(boundingBoxes))
    Thread.sleep(10000)
    twitterStream.cleanUp
    twitterStream.shutdown
  }
}

