import scala.annotation.tailrec

/**
 * This function receives an integer <b>h</b> in order to compare it with
 * all the elements on the <b>t</b> list until it reaches an integer greater
 * than itself; Then, it returns a list of integers with <b>h</b> and
 * all the elements of the <b>t</b> list, such that <b>h</b> is on the
 * position right before a greater integer from the <b>t</b> list.
 *
 * @param h Int, the element that will be inserted
 *          in its right position.
 * @param t List[Int], list of integers whose elements will be
 *          compared with <b>h</b>.
 * @param acc List[Int], list of integers that are less than <b>h</b>. This
 *            list is initialized as empty, but as <b>h</b> is smaller than
 *            some elements of the <b>t</b> list, this <b>acc</b> list gets
 *            filled with these smaller integers.
 * @return List[Int], list that contains <b>h</b>, <b>acc</b> and <b>list</b>,
 *                    where <b>h</b> is on the first position where it is smaller or
 *                    equal than another number of the list. The return will have
 *                    the form <b>h :: list</b> if <b>h</b> is smaller than the
 *                    first element of the list, else, it will have the form
 *                    <b>acc :: h :: t</b>, such that <b>acc</b> is the first
 *                    part of the output, <b>h</b> is the element smaller
 *                    than any other on the list, and <b>t</b> is the rest
 *                    of the <b>list</b>
 */
@tailrec
def insert(h: Int, t: List[Int], acc: List[Int]): List[Int] = {
  t match {
    case Nil => acc.reverse ::: h :: Nil
    case ht :: tt =>
      if (h <= ht) acc.reverse ::: h :: t else insert(h, tt, ht :: acc)
  }
}

/**
 * This function receives a disordered list of integers and sorts it by
 * running <b>insertTR</b> function for all the elements of the list
 *
 * @param list List[Int], This is a disordered list of integers
 * @return List[Int], This is the sorted list that contains all the elements
 *                    of the <b>List</b>
 */
def insertionSort(list: List[Int]): List[Int] = {
  list match{
    case Nil => Nil
    case h :: t => insert(h,insertionSort(t),Nil)
  }
}

insertionSort(List(4,6,1,7,2,8,3,0,5,9))

/*
------------------------ High Order ------------------------
*/


def conditionLET (a: Int,b: Int) = a<=b


/**
 * This function receives an integer <b>h</b> in order to compare it with
 * all the elements on the <b>t</b> list until it reaches an integer greater
 * than itself; Then, it returns a list of integers with <b>h</b> and
 * all the elements of the <b>t</b> list, such that <b>h</b> is on the
 * position right before a greater integer from the <b>t</b> list.
 *
 * @param h Int, the element that will be inserted
 *          in its right position.
 * @param t List[Int], list of integers whose elements will be
 *          compared with <b>h</b>.
 * @param acc List[Int], list of integers that are less than <b>h</b>. This
 *            list is initialized as empty, but as <b>h</b> is smaller than
 *            some elements of the <b>t</b> list, this <b>acc</b> list gets
 *            filled with these smaller integers.
 * @return List[Int], list that contains <b>h</b>, <b>acc</b> and <b>list</b>,
 *                    where <b>h</b> is on the first position where it is smaller or
 *                    equal than another number of the list. The return will have
 *                    the form <b>h :: list</b> if <b>h</b> is smaller than the
 *                    first element of the list, else, it will have the form
 *                    <b>acc :: h :: t</b>, such that <b>acc</b> is the first
 *                    part of the output, <b>h</b> is the element smaller
 *                    than any other on the list, and <b>t</b> is the rest
 *                    of the <b>list</b>
 */
@tailrec
def insertHO(h: Int, t: List[Int], acc: List[Int], condition: (a: Int,b: Int) => Boolean): List[Int] = {
  t match {
    case Nil => acc.reverse ::: h :: Nil
    case ht :: tt =>
      if (condition(h,ht)) acc.reverse ::: h :: t else insertHO(h, tt, ht :: acc, condition)
  }
}

/**
 * This function receives a disordered list of integers and sorts it by
 * running <b>insertTR</b> function for all the elements of the list
 *
 * @param list List[Int], This is a disordered list of integers
 * @return List[Int], This is the sorted list that contains all the elements
 *                    of the <b>List</b>
 */
def insertionSortHO(list: List[Int]): List[Int] = {
  list match{
    case Nil => Nil
    case h :: t => insertHO(h,insertionSort(t),Nil, conditionLET)
  }
}

insertionSortHO(List(4,6,1,7,2,8,3,0,5,9))