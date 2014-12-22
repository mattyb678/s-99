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

  test("test length of really long list should not stack overflow") {
    val longLength = ListFunc.length(List.range(1, 10001))
    assert(longLength === 10000)
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

  test("reverse really long list shouldn't stack overflow") {
    val longlist = ListFunc.reverse(List.range(1, 10001))
    assert(longlist === List.range(10000, 0, -1))
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

  test("flatten a really long list should not stack overflow") {
    val longlist = ListFunc.flatten(List.range(1, 10001))
    assert(longlist === List.range(1, 10001))
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

  test("pack empty list is an empty list with an empty list") {
    new TestLists {
      val packedInt = ListFunc.pack(emptyListInt)
      assert(packedInt === List(List()))
      val packedStr = ListFunc.pack(emptyListString)
      assert(packedStr === List(List()))
    }
  }

  test("pack 1 item list is a 1 item list of 1 item") {
    new TestLists {
      val packedInt = ListFunc.pack(oneItemListInt)
      assert(packedInt === List(List(100)))
      val packedStr = ListFunc.pack(oneItemListString)
      assert(packedStr === List(List("hello")))
    }
  }

  test("pack many item list with no duplicates") {
    new TestLists {
      val packedInt = ListFunc.pack(manyInts)
      assert(packedInt ===
        List(List(10), List(20), List(30), List(40), List(50), List(60), List(70), List(80), List(90), List(100)))
      val packedStr = ListFunc.pack(manyStrings)
      assert(packedStr ===
        List(List("long"), List("list"), List("of"), List("strings"), List("to"),
          List("test"), List("list"), List("functions"), List("ok"), List("hello")))
    }
  }

  test("pack lists with 1 item duplicated") {
    val list2 = ListFunc.pack(List(42, 42))
    assert(list2 === List(List(42, 42)))
    val list3 = ListFunc.pack(List("ok", "ok", "ok"))
    assert(list3 === List(List("ok", "ok", "ok")))
    val list10 = ListFunc.pack(List(10, 10, 10, 10, 10, 10, 10, 10, 10, 10))
    assert(list10 === List(List(10, 10, 10, 10, 10, 10, 10, 10, 10, 10)))
  }

  test("pack list from example") {
    val packed = ListFunc.pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    assert(packed ===
      List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
  }

  test("pack really long list should not stack overflow") {
    ListFunc.pack(List.range(1, 10001))
  }

  test("encode empty list is empty list") {
    new TestLists {
      val encodedInt = ListFunc.encode(emptyListInt)
      assert(encodedInt === List())
      val  encodedStr = ListFunc.encode(emptyListString)
      assert(encodedStr === List())
    }
  }

  test("encode 1 item list") {
    new TestLists {
      val encodedInt = ListFunc.encode(oneItemListInt)
      assert(encodedInt === List((1, 100)))
      val encodedStr = ListFunc.encode(oneItemListString)
      assert(encodedStr === List((1, "hello")))
    }
  }

  test("encode many itemed list with no duplicates") {
    new TestLists {
      val packedInt = ListFunc.encode(manyInts)
      assert(packedInt ===
        List((1,10), (1,20), (1,30), (1,40), (1,50), (1,60), (1,70), (1,80), (1,90), (1,100)))
      val packedStr = ListFunc.encode(manyStrings)
      assert(packedStr ===
        List((1,"long"), (1,"list"), (1,"of"), (1,"strings"), (1,"to"),
          (1,"test"), (1,"list"), (1,"functions"), (1,"ok"), (1,"hello")))
    }
  }

  test("encode lists with duplicates") {
    val list2 = ListFunc.encode(List(42, 42))
    assert(list2 === List((2, 42)))
    val list3 = ListFunc.encode(List("ok", "ok", "ok"))
    assert(list3 === List((3, "ok")))
    val list10 = ListFunc.encode(List(10, 10, 10, 10, 10, 10, 10, 10, 10, 10))
    assert(list10 === List((10, 10)))
  }

  test("encode really long list should not stack overflow") {
    ListFunc.encode(List.range(1, 10001))
  }

  test("modified encode empty list is empty list") {
    new TestLists {
      val encodedInt = ListFunc.encodeModified(emptyListInt)
      assert(encodedInt === List())
      val  encodedStr = ListFunc.encodeModified(emptyListString)
      assert(encodedStr === List())
    }
  }

  test("modified encode 1 item list") {
    new TestLists {
      val encodedInt = ListFunc.encodeModified(oneItemListInt)
      assert(encodedInt === oneItemListInt)
      val encodedStr = ListFunc.encodeModified(oneItemListString)
      assert(encodedStr === oneItemListString)
    }
  }

  test("modified encode many itemed list with no duplicates") {
    new TestLists {
      val packedInt = ListFunc.encodeModified(manyInts)
      assert(packedInt === manyInts)
      val packedStr = ListFunc.encodeModified(manyStrings)
      assert(packedStr === manyStrings)
    }
  }

  test("modified encode really long list should not stack overflow") {
    ListFunc.encodeModified(List.range(1, 10001))
  }

  test("modified encode lists with duplicates") {
    val list2 = ListFunc.encodeModified(List(42, 42))
    assert(list2 === List((2, 42)))
    val list3 = ListFunc.encodeModified(List("ok", "ok", "ok"))
    assert(list3 === List((3, "ok")))
    val list10 = ListFunc.encodeModified(List(10, 10, 10, 10, 10, 10, 10, 10, 10, 10))
    assert(list10 === List((10, 10)))
  }

  test("decode empty list") {
    new TestLists {
      val decodedInt = ListFunc.decode(List[(Int, Int)]())
      assert(decodedInt === emptyListInt)
      val decodedStr = ListFunc.decode(List[(Int, String)]())
      assert(decodedStr === emptyListString)
    }
  }

  test("decode 1 item list") {
    new TestLists {
      val decodedInt = ListFunc.decode(List((1, 100)))
      assert(decodedInt === oneItemListInt)
      val decodedStr = ListFunc.decode(List((1, "hello")))
      assert(decodedStr === oneItemListString)
    }
  }

  test("decode many itemed list, no duplicates") {
    new TestLists {
      val decodedInt = ListFunc.decode(List((1,10), (1,20), (1,30), (1,40), (1,50), (1,60), (1,70), (1,80), (1,90), (1,100)))
      assert(decodedInt === manyInts)
      val decodedStr = ListFunc.decode(List((1,"long"), (1,"list"), (1,"of"), (1,"strings"), (1,"to"),
        (1,"test"), (1,"list"), (1,"functions"), (1,"ok"), (1,"hello")))
      assert(decodedStr === manyStrings)
    }
  }

  test("decode list with duplicates") {
    val decoded1 = ListFunc.decode(List((2, 42)))
    assert(decoded1 === List(42, 42))
    val decoded2 = ListFunc.decode(List((3, "ok")))
    assert(decoded2 === List("ok", "ok", "ok"))
    val decoded3 = ListFunc.decode(List((10, 10)))
    assert(decoded3 === List(10, 10, 10, 10, 10, 10, 10, 10, 10, 10))
  }

  test("decode example list") {
    val decoded = ListFunc.decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
    assert(decoded === List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  }

  test("duplicate of empty list") {
    new TestLists {
      val duplicateInt = ListFunc.duplicate(emptyListInt)
      assert(duplicateInt === List())
      val duplicateStr = ListFunc.duplicate(emptyListString)
      assert(duplicateStr === List())
    }
  }

  test("duplicate of 1 item list") {
    new TestLists {
      val duplicateInt = ListFunc.duplicate(oneItemListInt)
      assert(duplicateInt === List(100, 100))
      val duplicateStr = ListFunc.duplicate(oneItemListString)
      assert(duplicateStr === List("hello", "hello"))
    }
  }

//  List(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
//  List("long", "list", "of", "strings", "to", "test", "list", "functions", "ok", "hello")
  test("duplicate of many item list") {
    new TestLists {
      val duplicateInt = ListFunc.duplicate(manyInts)
      assert(duplicateInt === List(10, 10, 20, 20, 30, 30, 40, 40, 50, 50, 60, 60, 70, 70, 80, 80, 90, 90, 100, 100))
      val duplicateStr = ListFunc.duplicate(manyStrings)
      assert(duplicateStr === List("long", "long", "list", "list", "of", "of", "strings", "strings",
        "to", "to", "test", "test", "list", "list", "functions", "functions", "ok", "ok", "hello", "hello"))
    }
  }
}
