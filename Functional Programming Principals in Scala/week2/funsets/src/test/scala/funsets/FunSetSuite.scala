package funsets

import org.junit._

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite {

  import FunSets._

  @Test def `contains is implemented`: Unit = {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remvoe the
   * @Ignore annotation.
   */
  @Test def `singleton set one contains one`: Unit = {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  @Test def `union contains all elements of each set`: Unit = {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  @Test def `intersect contains comomon elements from each set`: Unit = {
    new TestSets {
      val s = intersect(union(s1, s2), s1);
      
      assert(contains(s, 1), "check 1")
      assert(!contains(s, 2), "check 2")
      assert(!contains(s, 3), "check 3")
    }
  }

  @Test def `diff contains elements from one set not in other`: Unit = {
    new TestSets {
      val s = diff(union(s1, s2), s3);
      
      assert(contains(s, 1), "check 1")
      assert(contains(s, 2), "check 2")
      assert(!contains(s, 3), "check 3")
    }
  }

  @Test def `filter contains elements from set satisfying a predicate`: Unit = {
    new TestSets {
      val s = filter(union(union(s1, s2), s3), x => x * x == 9);
      
      assert(!contains(s, 1), "check 1")
      assert(!contains(s, 2), "check 2")
      assert(contains(s, 3), "check 3")
    }
  }

  @Test def `forall checks whether all elements from set satisfying a predicate`: Unit = {
    new TestSets {
      val s = union(union(s1, s2), s3);
      
      assert(forall(s, x => x * x <= 9), "forall true")
      assert(!forall(s, x => x > 2), "forall false")
    }
  }

  @Test def `exists checks whether any element from set satisfying a predicate`: Unit = {
    new TestSets {
      val s = union(union(s1, s2), s3);
      
      assert(exists(s, x => x > 2), "exists true")
      assert(!exists(s, x => x * x > 9), "exists false")
    }
  }

  @Test def `map applies a tranformation on all element from set`: Unit = {
    new TestSets {
      val s = map(union(s1, s3), x => x * x * x);
      
      assert(contains(s, 1), "check 1")
      assert(!contains(s, 8), "check 2")
      assert(contains(s, 27), "check 3")
    }
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
