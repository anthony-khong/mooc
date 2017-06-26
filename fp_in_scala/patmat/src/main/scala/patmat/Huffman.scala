package patmat

import common._

object Huffman {
  abstract class CodeTree {
    val weight: Int
  }

  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree

  case class Leaf(char: Char, weight: Int) extends CodeTree

  def weight(tree: CodeTree): Int = tree match {
    case Fork(_, _, _, weight) => weight
    case Leaf(_, weight) => weight
  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case Fork(_, _, chars, _) => chars
    case Leaf(char, _) => List(char)
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

  def string2Chars(string: String): List[Char] = string.toList

  def times(chars: List[Char]): List[(Char, Int)] = {
    chars.groupBy(identity).mapValues(_.size).toList
  }

  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
    freqs.map({ case (char, freq) => Leaf(char, freq) }).sortBy(_.weight)
  }

  def singleton(trees: List[CodeTree]): Boolean = trees.size == 1

  def combine(trees: List[CodeTree]): List[CodeTree] = trees match {
    case t1 :: t2 :: rest => (makeCodeTree(t1, t2) :: rest).sortBy(_.weight)
    case tree :: Nil => List(tree)
    case Nil => Nil
  }

  def until(cond: List[CodeTree] => Boolean,  op: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] = {
    if (cond(trees)) trees else until(cond, op)(op(trees))
  }

  def createCodeTree(chars: List[Char]): CodeTree = {
    val trees = makeOrderedLeafList(times(chars))
    val List(huffmanTree) = until(singleton, combine)(trees)
    huffmanTree
  }

  // Part 3: Decoding

  type Bit = Int

  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    def decodeAcc(bits: List[Bit], acc: List[Char]): List[Char] = bits match {
      case Nil => acc
      case _ => {
        val (char, newBits) = decodeChar(tree, bits)
        decodeAcc(newBits, char :: acc)
      }
    }
    decodeAcc(bits, Nil).reverse
  }

  def decodeChar(tree: CodeTree, bits: List[Bit]): (Char, List[Bit]) = {
    tree match {
      case Fork(l, r, _, _) => {
        val (firstBit :: restBits) = bits
        if (firstBit == 0) decodeChar(l, restBits) else decodeChar(r, restBits)
      }
      case Leaf(char, _) => (char, bits)
    }
  }

  /**
   * A Huffman coding tree for the French language.
   * Generated from the data given at
   *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

  /**
   * What does the secret message say? Can you decode it?
   * For the decoding use the `frenchCode' Huffman tree defined above.
   */
  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

  def decodedSecret: List[Char] = decode(frenchCode, secret)


  // Part 4a: Encoding using Huffman tree

  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    text.flatMap(lookupChar(tree))
  }

  def lookupChar(tree: CodeTree)(char: Char): List[Bit] = {
    def inTree(tree: CodeTree): Boolean = tree match {
      case Fork(_, _, chars, _) => chars contains char
      case Leaf(c, _) => char == c
    }

    def lookupCharAcc(tree: CodeTree, acc: List[Bit]): List[Bit] = tree match {
      case Fork(l, r, _, _) => if (inTree(l)) lookupCharAcc(l, 0 :: acc) else lookupCharAcc(r, 1 :: acc)
      case Leaf(_, _) => acc.reverse
    }

    lookupCharAcc(tree, Nil)
  }

  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  def codeBits(table: CodeTable)(char: Char): List[Bit] = table match {
    case (c, bs) :: tableTail => if (c == char) bs else codeBits(tableTail)(char)
    case Nil => throw new NoSuchElementException("Cannot find char in table.")
  }

  // def convert(tree: CodeTree): CodeTable = {
  //   val chars = tree match {
  //     case Fork(_, _, chars, _) => chars
  //     case Leaf(char, _) => List(char)
  //   }
  //   chars zip (chars map lookupChar(tree))
  // }

  def convert(tree: CodeTree): CodeTable = {
    def convertAcc(tree: CodeTree, prefix: List[Bit], table: CodeTable): CodeTable = tree match {
      case Fork(l, r, _, _) => {
        val leftTable = convertAcc(l, 0 :: prefix, table)
        convertAcc(r, 1 :: prefix, leftTable)
      }
      case Leaf(char, _) => (char, prefix.reverse) :: table
    }
    convertAcc(tree, List(), List())
    }

  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = a ::: b

  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    val table = convert(tree)
    text.flatMap(codeBits(table))
  }
}

