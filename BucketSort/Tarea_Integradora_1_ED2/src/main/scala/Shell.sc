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

/**
 * This function creates a sub-list of integers with some
 * elements of a <b>list</b> separated by a <b>gap</b>
 * starting for the head of the list. For example:
 * given an input [1::0::1::0::1::Nil with a gap=2],
 * results in an output => [1::1::1::Nil]
 *
 * @param list List[Int], the list of integers from
 *             which the sublist is generated.
 * @param gap Int, the gap between the elements of the
 *            list
 * @param iter Int, this is the control variable. It is
 *             always initialized as zero (0) and it is
 *             increased by 1, such that when its module
 *             with the gap is zero (iter%gap == 0), the
 *             current integer is taken as a new element
 *             of the sub-list.
 * @return List[Int], this is the sub-list of integers from
 *         the <b>list</b> separated by a <b>gap</b>
 */
def createList(list: List[Int], gap: Int, iter: Int): List[Int] = {
  list match{
    case Nil => Nil
    case h :: t =>
      if(iter%gap == 0) h :: createList(t, gap, iter+1) else createList(t, gap, iter+1)
  }
}



/**
 * This function reincorporates a sublist to its original
 * list by replacing the elements of the original list separated
 * by a gap, starting with the head. For example:
 * given an input [
 * original list = 1::0::1::0::1::Nil
 * sublist = 2::2::2::Nil
 * with a gap = 2
 * ], results in an output => [2::0::2::0::2::Nil]
 *
 * @param original List[Int], the list with the elements
 *                 that will be replaced by elements of
 *                 the sub-list
 * @param sublist List[Int], the sublist with the elements
 *                that will replace some elements in the
 *                original list
 * @param output List[Int], the list that contains the replaced
 *               elements of the sublist and the elements that
 *               are preserved from the original list
 * @param gap, Int, the gap between the elements of the
 *            list
 * @param iter Int, this is the control variable. It is
 *             always initialized as zero (0) and it is
 *             increased by 1, such that when its module
 *             with the gap is zero (iter%gap == 0), the
 *             current integer of the original list is
 *             replaced with the current element of the
 *             sub-list.
 * @return List[Int] a list that contains the replaced
 *               elements of the sublist and the elements that
 *               are preserved from the original list
 */
@tailrec
def reinsert(original: List[Int], sublist: List[Int], output: List[Int], gap: Int, iter: Int): List[Int] = {
  original match {
    case Nil => output.reverse
    case h :: t =>
      if (iter%gap == 0) reinsert(t, sublist.tail, sublist.head :: output, gap, iter + 1) else reinsert(t, sublist, h :: output, gap, iter + 1)
  }
}

/**
 * This function iterates the <b>reinsert</b> function, in order to reinsert
 * every sorted sub-list (taking the elements of the rest of the list separated
 * by a <b>gap</b>) to the original list on the position where the elements
 * of the sublist were taken from. For example:
 * Given a list: 33::31::40::8::12::17::25::42::Nil, after applying this
 * function with a gap = 4:
 *
 *  33::31::40::8::12::17::25::42::Nil  --> Original list
 *  |   |   |   |  |   |   |   |
 *  33  |   |   |  12  |   |   |   First sublist = 33::12::Nil --> 12::33::Nil (Sorted)
 *      31  |   |      17  |   |   Second sublist = 31::17::Nil --> 17::31::Nil (Sorted)
 *          40  |          25  |   Third sublist = 40::25::Nil --> 25::40::Nil (Sorted)
 *              8              42  Fourth sublist = 8::42::Nil --> 8::42::Nil (Sorted)
 *
 * So, the output after this function would result in:
 *
 * 12::17::25::8::33::31::40::42::Nil (every sorted sublist reinserted on their original position)
 * The call of this method would be:
 *
 *  iterReinsert( 33::31::40::8::12::17::25::42::Nil, 12::33::Nil, 4 ) = 12::17::25::8::33::31::40::42::Nil
 *
 * @param listToMix List[Int], the list where the elements of the sublists will
 *                  be taken from. This variable will be initialized as a full list
 *                  but it will be changing with the iterations of the method
 *                  (always the tail after the call of the method).
 * @param createdList List[Int], this is the sorted sublist taken from the
 *                    <b>listToMix</b>
 * @param gap Int, the gap that will be used to generate the sublists.
 * @return List[Int], the list with the reinserted sublists, with their correspondent
 *                   positions
 */
def iterReinsert(listToMix: List[Int],createdList: List[Int], gap: Int) : List[Int] = {
  listToMix match{
    case Nil => Nil
    case h :: Nil => h::Nil
    case h :: t =>
      reinsert(h::iterReinsert(t,insertionSort(createList(t, gap, 0)),gap),createdList,Nil,gap,0)
  }
}

/**
 * This function iterates the <b>iterReinsert</b> function using different gaps while the gap
 * (<b>k</b>) is greater than 1. The result of this operation is the completely sorted list.
 *
 * @param list, List[Int], the list that will be used to launch the <b>iterReinsert</b> method
 * @param k Int, the gap that will be updated by every iteration
 * @return List[Int], completely sorted list.
 */
@tailrec
def shellSortIter(list: List[Int], k: Int) : List[Int] = {
  if(k>1)
    shellSortIter(iterReinsert(list,insertionSort(createList(list,k/2,0)),k/2),k/2)
  else
    list
}

/**
 * This function launches the <b>shellSortIter</b> function
 *
 * @param list List[Int], the disordered list
 * @return List[Int], the sorted list
 */
def shellSort(list: List[Int]) : List[Int] = {
  shellSortIter(list,list.length)
}

shellSort(List(4,6,1,7,2,8,3,0,5,9))

/*
------------------------------- High Order --------------------------------------
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
def insertionSortHO(list: List[Int], condition : (a : Int, b : Int) => Boolean): List[Int] = {
  list match{
    case Nil => Nil
    case h :: t => insertHO(h,insertionSort(t),Nil, condition)
  }
}

/**
 * This function creates a sub-list of integers with some
 * elements of a <b>list</b> separated by a <b>gap</b>
 * starting for the head of the list. For example:
 * given an input [1::0::1::0::1::Nil with a gap=2],
 * results in an output => [1::1::1::Nil]
 *
 * @param list List[Int], the list of integers from
 *             which the sublist is generated.
 * @param gap Int, the gap between the elements of the
 *            list
 * @param iter Int, this is the control variable. It is
 *             always initialized as zero (0) and it is
 *             increased by 1, such that when its module
 *             with the gap is zero (iter%gap == 0), the
 *             current integer is taken as a new element
 *             of the sub-list.
 * @return List[Int], this is the sub-list of integers from
 *         the <b>list</b> separated by a <b>gap</b>
 */
def createListHO(list: List[Int], gap: Int, iter: Int): List[Int] = {
  list match{
    case Nil => Nil

    case h :: t =>
      if(iter%gap == 0) h :: createListHO(t, gap, iter+1)
      else createListHO(t, gap, iter+1)
  }
}

/**
 * This function reincorporates a sublist to its original
 * list by replacing the elements of the original list separated
 * by a gap, starting with the head. For example:
 * given an input [
 * original list = 1::0::1::0::1::Nil
 * sublist = 2::2::2::Nil
 * with a gap = 2
 * ], results in an output => [2::0::2::0::2::Nil]
 *
 * @param original List[Int], the list with the elements
 *                 that will be replaced by elements of
 *                 the sub-list
 * @param sublist List[Int], the sublist with the elements
 *                that will replace some elements in the
 *                original list
 * @param output List[Int], the list that contains the replaced
 *               elements of the sublist and the elements that
 *               are preserved from the original list
 * @param gap, Int, the gap between the elements of the
 *            list
 * @param iter Int, this is the control variable. It is
 *             always initialized as zero (0) and it is
 *             increased by 1, such that when its module
 *             with the gap is zero (iter%gap == 0), the
 *             current integer of the original list is
 *             replaced with the current element of the
 *             sub-list.
 * @return List[Int] a list that contains the replaced
 *               elements of the sublist and the elements that
 *               are preserved from the original list
 */
@tailrec
def reinsertHO(original: List[Int], sublist: List[Int], output: List[Int], gap: Int, iter: Int): List[Int] = {
  original match {
    case Nil => output.reverse
    case h :: t =>
      if (iter%gap == 0) reinsertHO(t, sublist.tail, sublist.head :: output, gap, iter + 1)
      else reinsertHO(t, sublist, h :: output, gap, iter + 1)
  }
}

/**
 * This function iterates the <b>reinsert</b> function, in order to reinsert
 * every sorted sub-list (taking the elements of the rest of the list separated
 * by a <b>gap</b>) to the original list on the position where the elements
 * of the sublist were taken from. For example:
 * Given a list: 33::31::40::8::12::17::25::42::Nil, after applying this
 * function with a gap = 4:
 *
 *  33::31::40::8::12::17::25::42::Nil  --> Original list
 *  |   |   |   |  |   |   |   |
 *  33  |   |   |  12  |   |   |   First sublist = 33::12::Nil --> 12::33::Nil (Sorted)
 *      31  |   |      17  |   |   Second sublist = 31::17::Nil --> 17::31::Nil (Sorted)
 *          40  |          25  |   Third sublist = 40::25::Nil --> 25::40::Nil (Sorted)
 *              8              42  Fourth sublist = 8::42::Nil --> 8::42::Nil (Sorted)
 *
 * So, the output after this function would result in:
 *
 * 12::17::25::8::33::31::40::42::Nil (every sorted sublist reinserted on their original position)
 * The call of this method would be:
 *
 *  iterReinsert( 33::31::40::8::12::17::25::42::Nil, 12::33::Nil, 4 ) = 12::17::25::8::33::31::40::42::Nil
 *
 * @param listToMix List[Int], the list where the elements of the sublists will
 *                  be taken from. This variable will be initialized as a full list
 *                  but it will be changing with the iterations of the method
 *                  (always the tail after the call of the method).
 * @param createdList List[Int], this is the sorted sublist taken from the
 *                    <b>listToMix</b>
 * @param gap Int, the gap that will be used to generate the sublists.
 * @return List[Int], the list with the reinserted sublists, with their correspondent
 *                   positions
 */
def iterReinsertHO(listToMix: List[Int],createdList: List[Int], gap: Int, condition : (a : Int, b : Int) => Boolean) : List[Int] = {

  listToMix match

    case Nil => listToMix

    case _ :: Nil => listToMix

    case h :: t =>

      reinsert(h::iterReinsertHO(t,insertionSortHO(createListHO(t, gap, 0), condition),gap, condition),createdList,Nil,gap,0)

}

/**
 * This function iterates the <b>iterReinsert</b> function using different gaps while the gap
 * (<b>k</b>) is greater than 1. The result of this operation is the completely sorted list.
 *
 * @param list, List[Int], the list that will be used to launch the <b>iterReinsert</b> method
 * @param k Int, the gap that will be updated by every iteration
 * @return List[Int], completely sorted list.
 */
@tailrec
def shellSortIterHO(list: List[Int], k: Int, condition : (a : Int, b : Int) => Boolean) : List[Int] = {
  if(k>1)
    shellSortIterHO(iterReinsertHO(list,insertionSortHO(createListHO(list,k/2,0), condition),k/2, condition),k/2, condition)
  else
    list
}

/**
 * This function launches the <b>shellSortIter</b> function
 *
 * @param list List[Int], the disordered list
 * @return List[Int], the sorted list
 */
def shellSortHO(list: List[Int]) : List[Int] = {
  shellSortIterHO(list,list.length, conditionLET)
}

shellSortHO(List(4,6,1,7,2,8,3,0,5,9))