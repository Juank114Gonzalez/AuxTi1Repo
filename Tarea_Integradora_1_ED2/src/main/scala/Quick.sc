/**
 * This function creates a sublist of integers with all the elements equal
 * or smaller than the <b>pivot</b>
 *
 * @param list List[Int], the list which the smaller integers will
 *             be taken from
 * @param pivot Int, the integer used to compare and select
 *              the elements of the list
 * @return List[Int], sublist of the <b>list</b> that contains all the
 *              integers equal or smaller than <b>pivot</b>
 */
def lowerSplit(list: List[Int], pivot: Int): List[Int] = {
  list match{
    case Nil => Nil
    case h :: t =>
      if (h <= pivot ) h::lowerSplit(t,pivot) else lowerSplit(t,pivot)
  }
}

/**
 * This function creates a sublist of integers with all the elements equal
 * or greater than the <b>pivot</b>
 *
 * @param list List[Int], the list which the greater integers will
 *             be taken from
 * @param pivot Int, the integer used to compare and select
 *              the elements of the list
 * @return List[Int], sublist of the <b>list</b> that contains all the
 *              integers equal or greater than <b>pivot</b>
 */
def higherSplit(list: List[Int], pivot: Int): List[Int] = {
  list match{
    case Nil => Nil
    case h :: t =>
      if (h > pivot) h::higherSplit(t,pivot) else higherSplit(t,pivot)
  }
}

/**
 * This method iterates both <b>lowerSplit</b> and <b>higherSplit<b> by
 * taking the head of the <b>list</b> as the <b>pivot</b> in order to create
 * a sorted list such that the result of <b>lowerSplit</b> function applied
 * to the tail and head as parameters becomes the left part of the list
 * and the result of the <b>higherSplit<b> function with the same
 * parameters becomes the right part.
 *
 * @param list List[Int], the list that will be sorted
 * @return List[Int], the sorted list with all the elements that the
 *                    <b>list</b> contains
 */
def quickSort(list: List[Int]) : List[Int] = {
  list match {
    case Nil => Nil
    case h :: Nil => h :: Nil
    case h :: t =>
      quickSort(lowerSplit(t,h)) ::: h :: quickSort(higherSplit(t,h))
  }
}

quickSort(List(4,6,1,7,2,8,3,0,5,9))

/*
---------------------------------- High Order ---------------------------
*/


def conditionLET (a: Int,b: Int) = a<=b

def conditionHT (a: Int,b: Int) = a>b


/**
 * This function creates a sublist of integers with all the elements equal
 * or smaller than the <b>pivot</b>
 *
 * @param list List[Int], the list which the smaller integers will
 *             be taken from
 * @param pivot Int, the integer used to compare and select
 *              the elements of the list
 * @return List[Int], sublist of the <b>list</b> that contains all the
 *              integers equal or smaller than <b>pivot</b>
 */
def splitHO(list: List[Int], pivot: Int, condition: (a: Int,b: Int) => Boolean): List[Int] = {
  list match{
    case Nil => list
    case h :: t =>
      if (condition(h,pivot)) h::splitHO(t,pivot,condition) else splitHO(t,pivot,condition)
  }
}

/**
 * This method iterates both <b>lowerSplit</b> and <b>higherSplit<b> by
 * taking the head of the <b>list</b> as the <b>pivot</b> in order to create
 * a sorted list such that the result of <b>lowerSplit</b> function applied
 * to the tail and head as parameters becomes the left part of the list
 * and the result of the <b>higherSplit<b> function with the same
 * parameters becomes the right part.
 *
 * @param list List[Int], the list that will be sorted
 * @return List[Int], the sorted list with all the elements that the
 *                    <b>list</b> contains
 */
def quickSortHO(list: List[Int]) : List[Int] = {
  list match {
    case Nil => list
    case _ :: Nil => list
    case h :: t =>
      quickSortHO(splitHO(t,h, conditionLET)) ::: h :: quickSortHO(splitHO(t,h, conditionHT))
  }
}

quickSortHO(List(4,6,1,7,2,8,3,0,5,9))