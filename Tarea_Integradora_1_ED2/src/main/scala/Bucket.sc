import scala.annotation.tailrec
import scala.math.*

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
 * This function returns the <b>largest</b> element from a list.
 *
 * @param list <b>List[Int]</b>, list of integers, where we extract the <b>largest</b> element.
 * @param a <b>Int</b>,This is the <b>largest</b> element in the list so far, when the
 *           list has been traversed, it is returned.
 * @return <b>Int</b>, it´s the <b>largest</b> element from <b>list</b>.
 */

@tailrec
def max(list : List[Int], a : Int): Int={

  list match
    case Nil => throw new Exception("Empty List doesn't have any element to compare")

    case h :: Nil => if(h > a) h else a

    case h :: t => if(h > a) max(t, h)else max(t,a)

}


/**
 * This function returns the <b>minor</b> element from a list.
 *
 * @param list <b>List[Int]</b>, list of integers, where we extract the <b>minor</b> element.
 * @param a <b>Int</b>,This is the <b>minor</b> element in the list so far, when the
 *           list has been traversed, it is returned.
 * @return <b>Int</b>, it´s the <b>minor</b> element from <b>list</b>.
 */
@tailrec
def min(list : List[Int], a : Int): Int={
  list match
    case Nil => throw new Exception("Empty List doesn't have any element to compare")

    case h :: Nil => if(h < a) h else a

    case h :: t => if(h < a) min(t, h) else min(t,a)
}

/**
 * This function <b>rounds</b> a decimal number to its nearest integer.
 * @param a <b>Double<b>, This is the decimal number to approximate.
 * @return <b>Int</b>, This is the approximation of <b>a</b>.
 */
def approach(a : Double): Int={

  if(a < a.toInt+0.5) a.toInt else a.toInt + 1

}


/**
 * This function returns an approximation of the amplitude of each
 * interval. Each interval represents a bucket.
 * @param min <b>Int</b>, This is the <b>minor</b> element from the sample elements.
 * @param max <b>Int</b>, This is the <b>largest</b> element from the sample elements.
 * @param k <b>Int</b>, This is the quantity of intervals from the sample elements.
 * @return <b>Int</b>, This is an approximation of the optimal amplitude of each interval.
 */
def width(min : Int, max : Int, k : Int): Int={

  approach((max-min)/sqrt(k.toDouble))

}

/**
 * This function returns the optimal quantity of intervals from the sample elements.
 * @param n This is the number of items in the sample.
 * @return <b>Int</b>, This is the optimal number of intervals to distribute
 *         the elements of the sample
 */
def intervalsQuantity(n : Int): Int={
  approach(sqrt(n.toDouble))

}


/**
 * This function returns a <b>disordered list of elements</b> that originally belonged
 * to the original list, these elements are given by this interval: a <= x < b.
 * @param a <b>Int</b>, This is the <b>lower</b> bound of the interval.
 * @param b <b>Int</b>, This is the <b>upper</b> limit of the interval.
 * @param list <b>List[Int]</b>, this is a <b>disordered</b>list is by the
 *        form <b>h :: t</b> such that <b>t</b> it's also by that form.
 *        This is the original list, from where the elements of each bucket are extracted.
 * @return <b>List[Int]</b>, This is a <b>disordered list of elements</b>, that contains
 *         elements from original sample. Those elements be since <b>a</b> until <b>b</b>.
 */
def fillBuckets(a : Int, b : Int, list : List[Int]): List[Int] ={
  list match
    case Nil => Nil

    case h::Nil => if((h>a || h == a) && (h<b)) list else Nil

    case h :: t => if((h>a || h == a) && (h<b)) h :: fillBuckets(a, b, t) else fillBuckets(a,b,t)

}

/**
 * This function returns a <b>list of disordered lists</b>, those lists are the
 * <b>buckets by intervals</b>, in which we previously classify the elements of
 * the original list.
 * @param list <b>List[Int]</b>, this is a <b>disordered</b>list is by the
 *        form <b>h :: t</b> such that <b>t</b> it's also by that form.
 *        This is the original list, from where the elements of each bucket are
 *        extracted.
 * @param i <b>Int</b>, This is the helper iterator to determine the stop of
 *        the function.
 * @param step <b>Int</b>, This is the width of each interval. It is also the
 *        step with which the iterator <b>i</b> is increased.
 * @param n <b>Int</b>, This is the number of buckets we need to store in the
 *        return list. It is also the stop condition, because while <b>i < n</b>,
 *        the function will be executing recursively
 * @return <b>List[List[Int]]</b>, Each element of the list of lists returned by
 *         this function is a bucket filled by the <b>fillBuckets</b> function.
 */
def mergeBuckets(list : List[Int], i : Int, step : Int, n : Int): List[List[Int]] ={

  if(i<n || i == n)
    fillBuckets(i,i+step, list) :: mergeBuckets(list, i +step, step, n)
  else
    fillBuckets(i,i+step, list) :: Nil

}

/**
 * This function takes a list of integer lists and transforms it
 * into a single integer list, such that the last element of the
 * first list is x_1 and x_1 :: h_2 ... and x_n-1 :: h_n.
 * @param matrix <b>List[List[Int]]</b>, This is a list of lists
 *        containing the lists to concatenate
 * @return <b>List[Int]</b>, This is the list that contains all the
 *         elements of the <b>matrix</b>´s lists had.
 */
def concatList(matrix :List[List[Int]]): List[Int]={
  matrix match

    case Nil => Nil

    case h :: Nil => insertionSort(h)

    case h :: t => insertionSort(h) ::: concatList(t)

}

/**
 * This function get a <b>Disordered</b> List of integers,
 * and returns an <b>Ordered</b> list.
 * @param list <b>List[Int]</b>, List of Integers to Order
 * @return <b>List[Int]</b>, An <b>Ordered</b> List that contains
 *         the same elements that <b>list</b> had.
 */
def bucketSort(list : List[Int]): List[Int] = {

  concatList(mergeBuckets(list,min(list, list.head),intervalsQuantity(list.length),width(min(list, list.head),max(list, list.head),list.length)*intervalsQuantity(list.length)))

}

bucketSort(List(4,6,1,7,2,8,3,0,5,9))

/*
------------------------------- High order functions -------------------------------
 */

def conditionLTD (a: Double, b: Double): Boolean = a<b

def conditionLET (a: Int, b: Int): Boolean = a<=b

def conditionHET (a: Int, b: Int): Boolean = a>=b

def conditionBTW (a: Int, b: Int, c :Int): Boolean = a>=b && a<c

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
 * This function returns the <b>largest</b> element from a list.
 *
 * @param list <b>List[Int]</b>, list of integers, where we extract the <b>largest</b> element.
 * @param a <b>Int</b>,This is the <b>largest</b> element in the list so far, when the
 *           list has been traversed, it is returned.
 * @param condition <b> (a, b) => Boolean </b>, This is an order function.
 * @return <b>Int</b>, it´s the <b>largest</b> or <b>minor</b> element from <b>list</b>. Its
 *         depends of function parameter.
 */

@tailrec
def limitsHO(list : List[Int], a : Int, condition : (a: Int, b: Int) => Boolean): Int={

  list match
    case Nil => throw new Exception("Empty List doesn't have any element to compare")

    case h :: Nil => if(condition(h,a)) h else a

    case h :: t => if(condition(h,a)) limitsHO(t, h, condition)else limitsHO(t,a, condition)

}


/**
 * This function <b>rounds</b> a decimal number to its nearest integer.
 * @param a <b>Double<b>, This is the decimal number to approximate.
 * @param condition <b> (a, b) => Boolean </b>, This is an order function.
 * @return <b>Int</b>, This is the approximation of <b>a</b>.
 */
def approachHO(a : Double, condition : (a: Double, b: Double) => Boolean): Int={

  if(condition(a,a.toInt+0.5)) a.toInt else a.toInt + 1

}


/**
 * This function returns an approximation of the amplitude of each
 * interval. Each interval represents a bucket.
 * @param min <b>Int</b>, This is the <b>minor</b> element from the sample elements.
 * @param max <b>Int</b>, This is the <b>largest</b> element from the sample elements.
 * @param k <b>Int</b>, This is the quantity of intervals from the sample elements.
 * @param condition <b> (a, b) => Boolean </b>, This is an order function.
 * @return <b>Int</b>, This is an approximation of the optimal amplitude of each interval.
 */
def widthHO(min : Int, max : Int, k : Int, condition : (a: Double, b: Double) => Boolean): Int={

  approachHO((max-min)/sqrt(k.toDouble), condition)

}

/**
 * This function returns the optimal quantity of intervals from the sample elements.
 * @param n This is the number of items in the sample.
 * @param condition <b> (a, b) => Boolean </b>, This is an order function.
 * @return <b>Int</b>, This is the optimal number of intervals to distribute
 *         the elements of the sample
 */
def intervalsQuantityHO(n : Int, condition : (a: Double, b: Double) => Boolean): Int={
  approachHO(sqrt(n.toDouble), condition)

}


/**
 * This function returns a <b>disordered list of elements</b> that originally belonged
 * to the original list, these elements are given by this interval: a <= x < b.
 * @param a <b>Int</b>, This is the <b>lower</b> bound of the interval.
 * @param b <b>Int</b>, This is the <b>upper</b> limit of the interval.
 * @param list <b>List[Int]</b>, this is a <b>disordered</b>list is by the
 *        form <b>h :: t</b> such that <b>t</b> it's also by that form.
 *        This is the original list, from where the elements of each bucket are extracted.
 * @param condition <b> (a, b) => Boolean </b>, This is an order function.
 * @return <b>List[Int]</b>, This is a <b>disordered list of elements</b>, that contains
 *         elements from original sample. Those elements be since <b>a</b> until <b>b</b>.
 */
def fillBucketsHO(a : Int, b : Int, list : List[Int], condition : (a: Int, b: Int, c: Int) => Boolean): List[Int] ={
  list match

    case Nil => list

    case h::Nil => if(condition(h,a,b)) list else Nil

    case h :: t => if(condition(h,a, b)) h :: fillBuckets(a, b, t) else fillBuckets(a,b,t)

}

/**
 * This function returns a <b>list of disordered lists</b>, those lists are the
 * <b>buckets by intervals</b>, in which we previously classify the elements of
 * the original list.
 * @param list <b>List[Int]</b>, this is a <b>disordered</b>list is by the
 *        form <b>h :: t</b> such that <b>t</b> it's also by that form.
 *        This is the original list, from where the elements of each bucket are
 *        extracted.
 * @param i <b>Int</b>, This is the helper iterator to determine the stop of
 *        the function.
 * @param step <b>Int</b>, This is the width of each interval. It is also the
 *        step with which the iterator <b>i</b> is increased.
 * @param n <b>Int</b>, This is the number of buckets we need to store in the
 *        return list. It is also the stop condition, because while <b>i < n</b>,
 *        the function will be executing recursively.
 * @param condition1 <b> (a, b) => Boolean </b>, This is an order function.
 * @param condition2 <b> (a, b) => Boolean </b>, This is an order function.
 * @return <b>List[List[Int]]</b>, Each element of the list of lists returned by
 *         this function is a bucket filled by the <b>fillBuckets</b> function.
 */
def mergeBucketsHO(list : List[Int], i : Int, step : Int, n : Int,  condition1 : (a: Int, b: Int) => Boolean, condition2 : (a: Int, b: Int, c : Int) => Boolean): List[List[Int]] ={

  if(condition1(i,n))
    fillBucketsHO(i,i+step, list, condition2) :: mergeBucketsHO(list, i +step, step, n, condition1, condition2)
  else
    fillBucketsHO(i,i+step, list, condition2) :: Nil

}

/**
 * This function takes a list of integer lists and transforms it
 * into a single integer list, such that the last element of the
 * first list is x_1 and x_1 :: h_2 ... and x_n-1 :: h_n.
 * @param matrix <b>List[List[Int]]</b>, This is a list of lists
 *        containing the lists to concatenate.
 * @param condition <b> (a, b) => Boolean </b>, This is an order function.
 * @return <b>List[Int]</b>, This is the list that contains all the
 *         elements of the <b>matrix</b>´s lists had.
 */
def concatListHO(matrix :List[List[Int]], condition : (Int,Int) => Boolean): List[Int]={
  matrix match

    case Nil => Nil

    case h :: Nil => insertionSortHO(h, condition)

    case h :: t => insertionSortHO(h, condition) ::: concatListHO(t, condition)

}

/**
 * This function get a <b>Disordered</b> List of integers,
 * and returns an <b>Ordered</b> list.
 * @param list <b>List[Int]</b>, List of Integers to Order
 * @return <b>List[Int]</b>, An <b>Ordered</b> List that contains
 *         the same elements that <b>list</b> had.
 */
def bucketSortHO(list : List[Int]): List[Int] = {

  concatListHO(mergeBucketsHO(list, limitsHO(list, list.head, conditionLET), intervalsQuantityHO(list.length, conditionLTD), widthHO(limitsHO(list, list.head, conditionLET), limitsHO(list, list.head, conditionHET), list.length, conditionLTD) *intervalsQuantityHO(list.length, conditionLTD), conditionLET, conditionBTW), conditionLET)

}

bucketSortHO(List(4,6,1,7,2,8,3,0,5,9))