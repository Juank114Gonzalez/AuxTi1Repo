import scala.annotation.tailrec
import scala.math.*

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

/**
 * This function receives an <b>x</b> to compare whit the head
 * of the sub-list that its gotten from original <b>Unordered</b>
 * list of the function <b>insertionSort</b>.
 *
 * @param x <b>Int</b>, the element that went to the left
 *          of the original list.
 * @param list <b>List[Int]</b>, sub-list, such that <b>list</b> is of the form
 *                        <b>h :: t</b>gotten from original <b>Unordered</b>
 *                        list of the function <b>insertionSort</b>.
 * @return <b>List[Int]</b>, list that contains <b>x</b> and <b>list</b>, where
 *                    if <b>x <= h</b>, return will be of the form
 *                    <b>x :: list</b>, else its will be of
 *                    the form <b>h :: x :: t</b>, such that
 *                    h is the <b>head</b> of <b>list</b>, and
 *                    <b>t</b> is the rest of <b>list</b>
 */
def insert(x: Int, list: List[Int]): List[Int] = {
  list match{
    case Nil =>
      return x :: list
    case h :: Nil =>
      if(x <= h){
        return x:: list
      }else{
        return h :: x :: Nil
      }
    case h :: _ =>
      if(x <= h){
        return x :: list
      }else{
        return h :: x :: list.tail
      }

  }

  if (list.isEmpty || x <= list.head){
    x :: list
  }
  else {
    list.head :: insert(x, list.tail)
  }
}

/**
 * This function get a <b>Disordered</b> List of integers,
 * and returns an <b>Ordered</b> list.
 * @param list <b>List[Int]</b>, List of Integers to Order
 * @return <b>List[Int]</b>, An <b>Ordered</b> List that contains
 *         the same elements that <b>list</b> had.
 */
def insertionSort(list: List[Int]): List[Int] = {
  list match

    case Nil => Nil

    case h :: t => insert(h, insertionSort(t))

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

bucketSort(List(33,12,31,19,7,0,21,43,1))