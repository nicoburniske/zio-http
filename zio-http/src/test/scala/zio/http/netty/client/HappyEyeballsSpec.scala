package zio.http.netty.client

import zio._
import zio.test.Assertion.equalTo
import zio.test.TestAspect.withLiveClock
import zio.test.{ZIOSpecDefault, assertZIO}

object HappyEyeballsSpec extends ZIOSpecDefault {

  def spec = suite("Happy eyeballs spec") {
    test("Ensure later effects don't execute.") {
      val shouldNotSet = for {
        ref    <- Ref.make(false)
        result <- NettyConnectionPool.executeHappyEyeballsMemo(
          NonEmptyChunk(
            ZIO.succeed(1).delay(10.millis),
            ref.set(true) *> ZIO.succeed(2),
          ),
          1.minute,
        )
        second <- ref.get
      } yield (result, second)

      assertZIO(shouldNotSet)(equalTo(1 -> false))
    } +
      test("Start Both. First Succeeds, Second Interrupted before execution.") {
        val shouldSet = for {
          executed    <- Ref.make(false)
          interrupted <- Ref.make(false)
          result      <- NettyConnectionPool.executeHappyEyeballsMemo(
            NonEmptyChunk(
              ZIO.succeed(1).delay(20.millis),
              executed.set(true).delay(1.second).as(2).onInterrupt(interrupted.set(true)),
            ),
            10.millis,
          )
          executed    <- executed.get
          interrupted <- interrupted.get
        } yield (result, executed, interrupted)

        assertZIO(shouldSet)(equalTo((1, false, true)))
      } + test("Start Both. Second succeeds, First Interrupted.") {
        val result = for {
          executed    <- Ref.make(false)
          interrupted <- Ref.make(false)
          result      <- NettyConnectionPool.executeHappyEyeballsMemo(
            NonEmptyChunk(
              executed.set(true).delay(10.seconds).as(1).onInterrupt(interrupted.set(true)),
              ZIO.succeed(2).delay(10.millis),
            ),
            10.millis,
          )
          executed    <- executed.get
          interrupted <- interrupted.get
        } yield (result, executed, interrupted)

        assertZIO(result)(equalTo((2, false, true))).timeoutFail("Test timed out.")(1.second)
      } + test("Start Both. First Errors before Delay. Second Succeeds.") {
        val result = NettyConnectionPool.executeHappyEyeballsMemo(
          NonEmptyChunk(
            ZIO.fail(new Exception()).delay(1.millis),
            ZIO.succeed(2).delay(1.millis),
          ),
          50.millis,
        )

        assertZIO(result)(equalTo(2)).timeoutFail("Test timed out.")(20.millis)
      }
//    + test("Start Both. End Same Time.") {
//
//        val result = NettyConnectionPool.executeHappyEyeballsMemo(
//          NonEmptyChunk(
//            ZIO.succeed(1).delay(10.millis),
//            ZIO.succeed(2).delay(5.millis),
//          ),
//          5.millis,
//        )
//
//        assertZIO(result)(equalTo(1))
//      }
  } @@ withLiveClock

}
