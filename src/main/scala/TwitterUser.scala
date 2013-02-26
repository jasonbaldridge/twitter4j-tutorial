package bcomposes.twitter

import twitter4j._
import twitter4j.conf.Configuration
import collection.JavaConversions._

/**
 * Gets a Twitter instance set up and ready to use. Also provides a
 * function for checking a RateLimitStatus and waiting if necessary.
 */
trait TwitterInstance {
  val twitter = new TwitterFactory().getInstance
}

/**
 * Given a command line query, search for tweets and print
 * them.
 */
object QuerySearch extends TwitterInstance {

  def main(args: Array[String]) { 
    val statuses = twitter.search(new Query(args(0))).getTweets
    statuses.foreach(status => println(status.getText + "\n"))
  }

}

/**
 * Shows the N most recent tweets from the user's home timeline
 * (tweets by people they follow).
 */
object GetHomeTimeline extends TwitterInstance {

  def main(args: Array[String]) { 
    val num = if (args.length == 1) args(0).toInt else 10
    val statuses = twitter.getHomeTimeline.take(num)
    statuses.foreach(status => println(status.getText + "\n"))
  }

}

/**
 * Update the status of the authenticating user from the
 * command line.
 */
object UpdateStatus extends TwitterInstance {
  def main(args: Array[String]) { 
    twitter.updateStatus(new StatusUpdate(args(0)))
  }
}

/**
 * Go through the N most recent tweets that have mentioned
 * the authenticating user, and reply "OK." to them (ensuring
 * to include the author of the original tweet and any other
 * entities mentioned in it).
 */
object ReplyOK extends TwitterInstance {

  def main(args: Array[String]) { 
    val num = if (args.length == 1) args(0).toInt else 10
    val userName = twitter.getScreenName
    val statuses = twitter.getMentionsTimeline.take(num)
    statuses.foreach { status => {
      val statusAuthor = status.getUser.getScreenName
      val mentionedEntities = status.getUserMentionEntities.map(_.getScreenName).toList
      val participants = (statusAuthor :: mentionedEntities).toSet - userName
      val text = participants.map(p=>"@"+p).mkString(" ") + " OK."
      val reply = new StatusUpdate(text).inReplyToStatusId(status.getId)
      println("Replying: " + text)
      twitter.updateStatus(reply)
    }}

  }

}

/**
 * A trait with checkAndWait function that check whether the
 * rate limit has been hit and wait if so.
 *
 * This ignores the fact that different request types have different
 * limits, but it keeps things simple.
 */
trait RateChecker {

  /**
   * See whether the rate limit has been hit, and wait until it
   * resets if so. Waits 10 seconds longer than the reset time to
   * ensure the time is sufficient.
   *
   * This is surely not an optimal solution, but it seems to do
   * the trick.
   */
  def checkAndWait(response: TwitterResponse, verbose: Boolean = false) {
    val rateLimitStatus = response.getRateLimitStatus
    if (verbose) println("RLS: " + rateLimitStatus)

    if (rateLimitStatus != null && rateLimitStatus.getRemaining == 0) {
      println("*** You hit your rate limit. ***")
      val waitTime = rateLimitStatus.getSecondsUntilReset + 10
      println("Waiting " + waitTime + " seconds ( "
	      + waitTime/60.0 + " minutes) for rate limit reset.")
      Thread.sleep(waitTime*1000)
    }
  }

}


/**
 * Describe the followers of a Twitter user by counting the words
 * in their descriptions and outputting in a format suitable for
 * Wordle's advanced word cloud input:
 *     http://www.wordle.net/advanced
 */
object DescribeFollowers extends TwitterInstance with RateChecker {

  def main(args: Array[String]) { 
    val screenName = args(0)
    val maxUsers = if (args.length==2) args(1).toInt else 500
    val followerIds = twitter.getFollowersIDs(screenName,-1).getIDs

    val descriptions = followerIds.take(maxUsers).flatMap { id => {
      val user = twitter.showUser(id)
      checkAndWait(user)
      if (user.isProtected) None else Some(user.getDescription)
    }}

    val tword = """(?i)[a-z#@]+""".r.pattern
    val words = descriptions.flatMap(_.toLowerCase.split("\\s+"))
    val filtered = words.filter(_.length > 3).filter(tword.matcher(_).matches)
    val counts = filtered.groupBy(x=>x).mapValues(_.length)
    val rankedCounts = counts.toSeq.sortBy(- _._2)

    import java.io._
    val wordcountFile = "/tmp/follower_wordcount.txt"
    val writer = new BufferedWriter(new FileWriter(wordcountFile))
    for ((w,c) <- rankedCounts)
      writer.write(w+":"+c+"\n")
    writer.flush
    writer.close
  }

}

/**
 * Get all the accounts the authenticating user follows,
 * grab twenty of them, filter them to get interesting ones,
 * and then take up to 10 of the remaining ones and retweet
 * the most recent statuses (provided they aren't replies to
 * someone else).
 */
object RetweetFriends extends TwitterInstance with RateChecker {

  def main(args: Array[String]) { 
    val friendIds = twitter.getFriendsIDs(-1).getIDs
    val friends = friendIds.take(20).map { id => {
      val user = twitter.showUser(id)
      checkAndWait(user)
      user
    }}

    val filtered = friends.filter(admissable)
    val ranked = filtered.map(f => (f.getFollowersCount, f)).sortBy(- _._1).map(_._2)

    ranked.take(10).foreach { friend => {
      val status = friend.getStatus
      if (status!=null && status.getInReplyToStatusId == -1) {
	println("\nRetweeting " + friend.getName + ":\n" + status.getText)
    	twitter.retweetStatus(status.getId)
	Thread.sleep(30000)
      }
    }}
  }

  // Determine whether a user is admissable for retweeting. Very
  // simple, but filters out some uninteresting accounts.
  def admissable(user: User) = {
    val ratio = user.getFollowersCount.toDouble/user.getFriendsCount 
    user.getFriendsCount < 1000 && ratio > 0.5
  }

}
