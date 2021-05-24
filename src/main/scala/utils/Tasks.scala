package utils

import java.util.concurrent.{ForkJoinPool, ForkJoinTask, ForkJoinWorkerThread, RecursiveTask}
import scala.util.DynamicVariable

/**
 * Object including methods for parallel task creation and
 * execution
 */
object Tasks {
  /**
   * pool of tasks
   */
  val forkJoinPool = new ForkJoinPool

  /**
   * abstract class acting as scheduler
   */
  abstract class TaskScheduler {

    /**
     * schedule a new task for execution
     * @param body task to be executed
     * @tparam T type parameter returned by the task
     * @return result of task
     */
    def schedule[T](body: => T): ForkJoinTask[T]

    /**
     * method to execute in parallel to tasks
     * @param taskA first task
     * @param taskB second task
     * @tparam A type parameter for first task result
     * @tparam B type parameter for second task result
     * @return tuple with results for both tasks
     */
    def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
      // create a task for taskB
      val right = task {
        taskB
      }

      // taskA will be execute within current thread
      val left = taskA

      // execute both tasks ans wait for results
      (left, right.join())
    }
  }

  /**
   * Class for scheduling tasks
   */
  class DefaultTaskScheduler extends TaskScheduler {

    /**
     * @param body task to be executed
     * @tparam T type parameter returned by the task
     * @return result of task
     */
    def schedule[T](body: => T): ForkJoinTask[T] = {
      // creates a recursive task for the task
      val t = new RecursiveTask[T] {
        // sets the target for the new task
        def compute: T = body
      }

      // order the execution depending on the type of the
      // current thread
      Thread.currentThread match {
        // wt can not be removed even if it is not used
        // for avoiding a compilation error with the class
        // name
        case _ : ForkJoinWorkerThread =>
          t.fork()
        case _ =>
          forkJoinPool.execute(t)
      }

      // return the task itself
      t
    }
  }

  /**
   * Creates the scheduler, kept as a new dynamic variable
   * of TaskScheduler type
   */
  val scheduler =
    new DynamicVariable[TaskScheduler](new DefaultTaskScheduler)

  /**
   * defines a new task and schedule its execution
   * @param body code to be executed by the thread
   * @tparam T type parameter with the result of the task
   * @return new task
   */
  def task[T](body: => T): ForkJoinTask[T] = {
    scheduler.value.schedule(body)
  }

  /**
   * schedules two tasks to be executed in parallel
   * @param taskA first task
   * @param taskB second task
   * @tparam A type parameter for the first task
   * @tparam B type parameter for the second task
   * @return
   */
  def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
    scheduler.value.parallel(taskA, taskB)
  }

  /**
   * defines four tasks to be executed in parallel
   * @param taskA first task
   * @param taskB second task
   * @param taskC third task
   * @param taskD fourth task
   * @tparam A type parameter for the result of first task
   * @tparam B type parameter for the result of second task
   * @tparam C type parameter for the result of third task
   * @tparam D type parameter for the result of fourth task
   * @return tuple with the results of the tasks
   */
  def parallel[A, B, C, D](taskA: => A, taskB: => B, taskC: => C, taskD: => D): (A, B, C, D) = {
    val ta = task { taskA }
    val tb = task { taskB }
    val tc = task { taskC }
    val td = taskD
    (ta.join(), tb.join(), tc.join(), td)
  }
}
