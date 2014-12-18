import list.ListFunc
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestList extends FunSuite {
  trait TestLists {
    val emptyListInt = List[Int]()
    val emptyListString = List[String]()
    val oneItemListInt = List(100)
    val oneItemListString = List("hello")
    val manyInts = List(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
    val manyStrings = List("long", "list", "of", "strings", "to", "test", "list", "functions", "ok", "hello")
  }

  test("get last item of empty list") {
    new TestLists {
      intercept[NoSuchElementException] {
        ListFunc.last(emptyListInt)
      }
      intercept[NoSuchElementException] {
        ListFunc.last(emptyListString)
      }
    }
  }

  test("get last item of one item list") {
    new TestLists {
      val item = ListFunc.last(oneItemListInt)
      assert(item === 100)
      val strItem = ListFunc.last(oneItemListString)
      assert(strItem === "hello")
    }
  }

  test("get last item of many itemed list") {
    new TestLists {
      val item = ListFunc.last(manyInts)
      assert(item === 100)
      val strItem = ListFunc.last(manyStrings)
      assert(strItem === "hello")
    }
  }

  test("penultimate of empty list") {
    new TestLists {
      intercept[NoSuchElementException] {
        ListFunc.penultimate(emptyListInt)
      }
      intercept[NoSuchElementException] {
        ListFunc.penultimate(emptyListString)
      }
    }
  }

  test("penultimate of 1 item list") {
    new TestLists {
      intercept[NoSuchElementException] {
        ListFunc.penultimate(oneItemListInt)
      }
      intercept[NoSuchElementException] {
        ListFunc.penultimate(oneItemListString)
      }
    }
  }

  test("get penultimate item of many itemed list") {
    new TestLists {
      val item = ListFunc.penultimate(manyInts)
      assert(item === 90)
      val strItem = ListFunc.penultimate(manyStrings)
      assert(strItem === "ok")
    }
  }

  test("get 0th item from empty list") {
    new TestLists {
      intercept[NoSuchElementException] {
        ListFunc.nth(0, emptyListInt)
      }
      intercept[NoSuchElementException] {
        ListFunc.nth(0, emptyListString)
      }
    }
  }

  test("get middle item of many itemed list") {
    new TestLists {
      val item = ListFunc.nth(5, manyInts)
      assert(item === 60)
      val strItem = ListFunc.nth(5, manyStrings)
      assert(strItem === "test")
    }
  }

  test("1th item of 1 item list") {
    new TestLists {
      intercept[NoSuchElementException] {
        ListFunc.nth(1, oneItemListInt)
      }
      intercept[NoSuchElementException] {
        ListFunc.nth(1, oneItemListString)
      }
    }
  }

  test("0th item of one item list") {
    new TestLists {
      val item = ListFunc.nth(0, oneItemListInt)
      assert(item === 100)
      val strItem = ListFunc.nth(0, oneItemListString)
      assert(strItem === "hello")
    }
  }

  test("length of empty list") {
    new TestLists {
      val sizeInt = ListFunc.length(emptyListInt)
      assert(sizeInt === 0)
      val sizeStr = ListFunc.length(emptyListString)
      assert(sizeStr === 0)
    }
  }

  test("length of 1 item list") {
    new TestLists {
      val sizeInt = ListFunc.length(oneItemListInt)
      assert(sizeInt === 1)
      val sizeStr = ListFunc.length(oneItemListString)
      assert(sizeStr === 1)
    }
  }

  test("length of many itemed list") {
    new TestLists {
      val sizeInt = ListFunc.length(manyInts)
      assert(sizeInt === 10)
      val sizeStr = ListFunc.length(manyStrings)
      assert(sizeStr === 10)
    }
  }

  test("reversing empty list is empty list") {
    new TestLists {
      val listInt = ListFunc.reverse(emptyListInt)
      assert(listInt === List[Int]())
      val listStr = ListFunc.reverse(emptyListString)
      assert(listStr === List[String]())
    }
  }

  test("reversing 1 item list is same 1 item list") {
    new TestLists {
      val listInt = ListFunc.reverse(oneItemListInt)
      assert(listInt === oneItemListInt)
      val listStr = ListFunc.reverse(oneItemListString)
      assert(listStr === oneItemListString)
    }
  }

  test("reversing 10 item list") {
    new TestLists {
      val listInt = ListFunc.reverse(manyInts)
      val revInt = List(100, 90, 80, 70, 60, 50, 40, 30, 20, 10)
      assert(listInt === revInt)
      val listStr = ListFunc.reverse(manyStrings)
      val revStr = List("hello", "ok", "functions", "list", "test", "to", "strings", "of", "list", "long")
      assert(listStr === revStr)
    }
  }

  test("empty list is palindrome") {
    new TestLists {
      val intPal = ListFunc.isPalindrome(emptyListInt)
      assert(intPal)
      val strPal = ListFunc.isPalindrome(emptyListString)
      assert(strPal)
    }
  }

  test("1 item list is palindrome") {
    new TestLists {
      val intPal = ListFunc.isPalindrome(oneItemListInt)
      assert(intPal)
      val strPal = ListFunc.isPalindrome(oneItemListString)
      assert(strPal)
    }
  }

  test("non palindrome list is not a palindrome") {
    new TestLists {
      val intPal = ListFunc.isPalindrome(manyInts)
      assert(!intPal)
      val strPal = ListFunc.isPalindrome(manyStrings)
      assert(!strPal)
    }
  }

  test("different palindromes") {
    val intPal = ListFunc.isPalindrome(List[Int](1,5,5,5,1))
    assert(intPal)
    val strPal = ListFunc.isPalindrome(List[String]("a","b","b","a"))
    assert(strPal)
  }

  test("flatten an empty list is an empty list") {
    new TestLists {
      val listInt = ListFunc.flatten(emptyListInt)
      assert(listInt === List())
      val listStr = ListFunc.flatten(emptyListString)
      assert(listStr === List())
    }
  }

  test("flattening an already flattened list is the same list") {
    new TestLists {
      val flatListOneInt = ListFunc.flatten(oneItemListInt)
      assert(flatListOneInt === oneItemListInt)
      val flatListOneStr = ListFunc.flatten(oneItemListString)
      assert(flatListOneStr === oneItemListString)
      val flatListInt = ListFunc.flatten(manyInts)
      assert(flatListInt === manyInts)
      val flatListStr = ListFunc.flatten(manyStrings)
      assert(flatListStr === manyStrings)
    }
  }

  test("flattening lists that need to be flattened") {
    val oneList = ListFunc.flatten(List(List(1,2,3)))
    assert(oneList === List(1,2,3))
    val twoLists = ListFunc.flatten(List(List("a", "b"), List("c", "d")))
    assert(twoLists === List("a", "b", "c", "d"))
    val list = ListFunc.flatten(List(List(1, 1), 2, List(3, List(5, 8))))
    assert(list === List(1, 1, 2, 3, 5, 8))
  }

  test("compressing empty list gives empty list") {
    new TestLists {
      val listInt = ListFunc.compress(emptyListInt)
      assert(listInt === List())
      val listStr = ListFunc.compress(emptyListString)
      assert(listStr === List())
    }
  }

  test("compressing 1 item list is just a one item list") {
    new TestLists {
      val listInt = ListFunc.compress(oneItemListInt)
      assert(listInt === List(100))
      val listStr = ListFunc.compress(oneItemListString)
      assert(listStr === List("hello"))
    }
  }

  test("compressing many itemed list with no duplicates") {
    new TestLists {
      val listInt = ListFunc.compress(manyInts)
      assert(listInt === manyInts)
      val listStr = ListFunc.compress(manyStrings)
      assert(listStr === listStr)
    }
  }

  test("compress lists with one value duplicated") {
    val list2 = ListFunc.compress(List(42, 42))
    assert(list2 === List(42))
    val list3 = ListFunc.compress(List("ok", "ok", "ok"))
    assert(list3 === List("ok"))
    val list10 = ListFunc.compress(List(10, 10, 10, 10, 10, 10, 10, 10, 10, 10))
    assert(list10 === List(10))
  }

  test("compress lists with multiple values duplicated") {
    val list23 = ListFunc.compress(List(7, 7, 1, 1, 1))
    assert(list23 === ListFunc.compress(List(7, 1)))
    val list131 = ListFunc.compress(List("hello", "ok", "ok", "ok", "yo"))
    assert(list131 === ListFunc.compress(List("hello", "ok", "yo")))
  }
}
