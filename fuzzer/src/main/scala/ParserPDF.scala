import java.io.{BufferedOutputStream, File, FileOutputStream}
import java.nio.channels.FileChannel
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, Paths, StandardOpenOption}
import java.nio.file.{Files, Paths}

import scala.io.Source
import scala.util.Random

class ParserPDF(fileName: String, actorNumber: Int) {

  /* sabloni koje pretrazujemo u datotekama */
  /* stringovi se mogu pojaviti kao karakteri izmedju () ili heksadekadno izmedju <> */
  val stringPattern = """\([\s\S]+?\)|\<[0-9a-fA-F]*?\>""".r
  val streamPattern = """stream""".r
  val endStreamPattern = """endstream""".r
  val streamCompletePattern = """stream[\s\S]+?endstream""".r
  val methodPattern = """[0-9a-zA-Z]+\(\)""".r
  val numberPattern = """[+-]?((\d+\.?\d+)|(\d*\.?\d+)|(\d+\.?\d*))""".r

  /* Funkcija koja u zavisnosti od broja cifara broja, vraca slucajan broj koji ce sluziti kao broj podataka koji se menjaju u odnosu na ukupan broj podataka */
  def getAPiece(number: Int): Int = {
    def numberOfDigitsTailRecursive(num: Int, help :Int): Int = {
      if(num>=0 && num<10) 1+help
      else numberOfDigitsTailRecursive(num/10, help+1)
    }
    val numberOfDigits = numberOfDigitsTailRecursive(number, 0)
    numberOfDigits match {
      case 1 => 1
      case 2 => Random.nextInt(number/3)
      case 3 => Random.nextInt(number/6)
      case 4 => Random.nextInt(number/50)
      case 5 => Random.nextInt(number/300)
      case 6 => Random.nextInt(number/3000)
      case 7 => Random.nextInt(number/8000)
      case 8 => Random.nextInt(number/60000)
      case 9 => Random.nextInt(number/2000000)
      case _ => println("Vise od 9 cifara?"); 1
    }
  }

  /* Funkcija koja generise stringove u formatu <...> */
  def generateHexString(length: Int): String ={

    /* pomocna funkcija koja generise string pomocu karaktera prosledjenih kao drugi argument */
    def randomStringFromCharList(length: Int, chars: Seq[Char]): String = {
      val sb = new StringBuilder
      for (i <- 1 to length) {
        val randomNum = util.Random.nextInt(chars.length)
        sb.append(chars(randomNum))
      }
      sb.toString
    }

    val s: StringBuilder = new StringBuilder();
    s.append("<")
    val chars: Seq[Char] = Seq[Char]('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F', 'a','b', 'c', 'd','e', 'f');
    s.append(randomStringFromCharList(length, chars));
    s.append(">")
    s.toString()
  }

  /* Funkcija koja generise stringove u formatu <...> ako je hex true, inace stringove u formatu (....)  */
  def generateString(hex: Boolean, length: Int): String ={
    if(hex){
      generateHexString(length)
    }
    else {
      /* Ova funkcija koristi pomocnu funkciju koja vraca listu karaktera odredjene duzine; zbog efikasnosti koristi se repna rekurzija */
      def randomStringTailRecursive(n: Int, list: List[Char]): List[Char] = {
        if (n == 1) util.Random.nextPrintableChar :: list
        else randomStringTailRecursive(n - 1, util.Random.nextPrintableChar :: list)
      }

      /* slucajno se odredjuje da li se string uzima iz skupa stringova (heuristike) ili se slucajno generise */
      if(Random.nextInt(5)>2){
        var filename: String = null;
        Random.nextInt(4) match {
          case 0 => filename = "strings\\formatStrings.txt";
          case 1 => filename = "strings\\naughtyStrings.txt";
          case 2 => filename = "strings\\traversals.txt";
          case 3 => filename = "strings\\upsideDownText.txt";
        }
        var result = new StringBuilder("(");
        result.append(getHeuristicString(filename));
        result.append(")")
        result.toString()
      }
      else {
        val s: StringBuilder = new StringBuilder();
        s.append("(")
        val list: List[Char] = List[Char]();
        val sMiddle = randomStringTailRecursive(length, list).mkString;
        s.append(sMiddle);
        s.append(")")
        s.toString()
      }
    }
  }

  def generateNumber(integer: Boolean): String = {
    if(Random.nextInt(2) == 1){
      var filename: String = if (integer) "ints\\integerOverflows.txt" else "reals\\realOverflows.txt";
      getHeuristicString(filename);
    }
    else if(integer)
      Random.nextInt().toString
    else
      Random.nextDouble().toString
  }

  def generateMethod(): String = {
    getHeuristicString("commonNames.txt\\commonMethodNames.txt")
  }

  def getHeuristicString(fileName: String): String = {
    var lines = Source.fromFile(".\\heuristics\\" + fileName, "ISO-8859-1").getLines().toArray
    val numberOfLines = lines.size
    println("\tFajl " + fileName + " ima " + numberOfLines + " linija")
    val lineNumber = Random.nextInt(numberOfLines)
    println("\tLine number: " + lineNumber)
    val chosenOne = lines(lineNumber)
    println("\tThe chosen one is: " + chosenOne)
    chosenOne
  }
/*
  def replaceAllSB(builder: StringBuilder, from: String, to: String, startIndex: Int)
  {
    var index = builder.indexOf(from, startIndex);
    while (index != -1)
    {
      builder.replace(index, index + from.length(), to);
      index += to.length(); // Move to the end of the replacement
      index = builder.indexOf(from, index);
    }
  }
*/
  /*
  def probam() = {
    val s = "%PDF-1.5\n%лн┼п\n5 0 obj <<\n/Type /ObjStm\n/N 100\n/First 808\n/Length 1326      \n/Filter /FlateDecode\n>>";
    val list = numberPattern.findAllMatchIn(s).map(_.start).toList
   // numberPattern.fin
    for(l<-list)
      println(" " + l.toString)
    println("\n")
  }
*/
  def changeStream(str: String): String = {
      println("--------- Menja se stream -----------")
      /* izdvajamo samo binarni deo */
      var stream = (str.substring(6, str.length))
      stream = stream.substring(0, stream.length - 9)
      //      println("\t"+s.substring(0, 5))
      //println("Nadjen stream: \n" + stream + "\n");
      val streamComplete: StringBuilder = new StringBuilder();
      streamComplete.append("\nstream\n")
      //streamComplete.append("\n")
      // pretvaramo ga u bajtove
      val streamInBytes = stream.getBytes(Charset.forName("ISO-8859-1"));
      val streamBytesSize = streamInBytes.size
      val numberOfChanges = Random.nextInt(2)
      println("\t\t ---- Broj parcica koje menjam: " + numberOfChanges)
      println("\t\t ---- Pre izmena velicina bajtova je: " + streamBytesSize)
      /* pravimo promene onoliko puta kolika je vrednost promenljive numberOfChanges*/
      for(i <- 0 until numberOfChanges) {
        /* odredjujemo velicinu novog niza bajtova: gledamo da bude neko manje parce */
        val smallChunk = streamBytesSize / 50
        println("\t\t ------------ (streamBytesSize/50): " + smallChunk)
        if (smallChunk != 0) {
          val newBytesSize = Random.nextInt(smallChunk)
          /* pravimo novi stream te velicine */
          val newBytes: Array[Byte] = new Array[Byte](newBytesSize)
          //streamInBytes.copyToArray(newBytes) // ovo je za kopiranje istih podataka
          Random.nextBytes(newBytes)
          /* ovo parce moramo umetnuti na neko mesto u postojecim bajtovima */
          val startIndex = Random.nextInt(streamBytesSize - newBytesSize - 1)
          for (i <- 0 until newBytesSize)
            streamInBytes(i+startIndex) = newBytes(i)
        }
        println("\t\t ---- Posle izmena velicina bajtova je: " + streamInBytes.size)
      }
    streamComplete.append(new String(streamInBytes, Charset.forName("ISO-8859-1")))
    streamComplete.append("\nendstream\n")
    streamComplete.toString();
  }

  /* ------------------------------------------------------------------------------------------------------- */
  def readFileBinary() = {
    val byteArray: Array[Byte] = Files.readAllBytes(Paths.get(fileName));
    /* pravimo string koji sadrzi podatke iz fajla */
    var contentString = new String(byteArray, Charset.forName("ISO-8859-1"))

    /* sve osim stream-ova trazimo u podacima bez streamova */
    val matchStreams = streamCompletePattern.findAllMatchIn(contentString).toList
    var deleteStreams = new String("");
    var noStreamsBuilder = new StringBuilder()
    var startIndex = 0;
    for(elem <- matchStreams) {
      var index = contentString.indexOf(elem.toString(), startIndex)
      //println("Prvo pojavljivanje je od: " + index + " do: " + (index + (elem.toString()).size))
      noStreamsBuilder.append(contentString.substring(startIndex, index))
      noStreamsBuilder.append("stream\nendstream")
      startIndex = index + (elem.toString()).size;
    }
    noStreamsBuilder.append(contentString.substring(startIndex));
    /* u deleteStreams se nalaze podaci bez stream-ova; upisujemo podatke u fajl fajlBezStreamova.txt radi provere */
    deleteStreams = noStreamsBuilder.toString()
    var tempFileNoStreams: String = "fajlBezStreamova.pdf";
    val bosns = new BufferedOutputStream(new FileOutputStream(tempFileNoStreams))
    bosns.write(deleteStreams.getBytes(Charset.forName("ISO-8859-1")))
    bosns.close()

    /* iz stringa (bez stream-ova) izvlacimo PDF stringove */
    val matchStrings = stringPattern.findAllMatchIn(deleteStreams).toList
    val matchStringsMap = stringPattern.findAllMatchIn(deleteStreams).map(_.start).toList
    val numberOfStrings = matchStrings.size
    println("Ukupno ima: " + numberOfStrings + " stringova, ")
    var percentOfChange = numberOfStrings/10;
    var numberOfChanged = 0
    var indexes = scala.collection.mutable.ArrayBuffer.empty[Tuple3[Int, Int, String]]
    if(percentOfChange != 0) {
       numberOfChanged = Random.nextInt(percentOfChange+1)
       println("\ta mi biramo da promenimo: " + numberOfChanged)
      var newElems = new Array[Int](numberOfChanged)
       for(i <- 0 until numberOfChanged){
        var newElem = Random.nextInt(numberOfStrings)
         while(newElems.contains(newElem)) newElem = Random.nextInt(numberOfStrings)
         var str = matchStrings(newElem).toString()
         var indexAppear = matchStringsMap(newElem)
         var replacement = if(str.startsWith("(")) generateString(false, str.size) else generateString(true, 0);
         indexes.append(new Tuple3(indexAppear, indexAppear+(str.size), replacement))
         println("Menjamo string: " + str + " koji se pojavljuje od: " + indexAppear + " do: " + (indexAppear+str.size) +
           " sa stringom: " + replacement)
       }
    }

    println("*************************************************************************")

    /* trazimo brojeve */
    val matchNumbers = numberPattern.findAllMatchIn(deleteStreams).toList
    val matchNumbersMap = numberPattern.findAllMatchIn(deleteStreams).map(_.start).toList
    val numberOfNumbers = matchNumbers.size
    println("Ukupno ima: " + numberOfNumbers + " brojeva, ")
    percentOfChange = numberOfNumbers/100;
    numberOfChanged = 0
    if(percentOfChange != 0) {
      numberOfChanged = Random.nextInt(percentOfChange+1)
      println("\ta mi biramo da promenimo: " + numberOfChanged)
      var newElems = new Array[Int](numberOfChanged)
      for(i <- 0 until numberOfChanged){
        var newElem = Random.nextInt(numberOfNumbers)
        while(newElems.contains(newElem)) newElem = Random.nextInt(numberOfNumbers)
        var str = matchNumbers(newElem).toString()
        var indexAppear = matchNumbersMap(newElem)
        var replacement = generateNumber(!str.contains("."));
        indexes.append(new Tuple3(indexAppear, indexAppear+(str.size), replacement))
        //oldIndexAppear = indexAppear + str.size
        println("Menjamo broj: " + str + " koji se pojavljuje od: " + indexAppear + " do: " + (indexAppear+str.size) +
          " sa brojem: " + replacement)
      }
    }

    /* trazimo metode */
    val matchMethods = methodPattern.findAllMatchIn(deleteStreams).toList // //
    val matchMethodsMap = methodPattern.findAllMatchIn(deleteStreams).map(_.start).toList
    val numberOfMethods = matchMethods.size // //
    println("Ukupno ima: " + numberOfMethods + " metoda, ") //
    percentOfChange = numberOfMethods/10; //
    numberOfChanged = 0
    if(percentOfChange != 0) {
      numberOfChanged = Random.nextInt(percentOfChange+1)
      println("\ta mi biramo da promenimo: " + numberOfChanged)
      var newElems = new Array[Int](numberOfChanged)
      for(i <- 0 until numberOfChanged){
        var newElem = Random.nextInt(numberOfMethods) //
        while(newElems.contains(newElem)) newElem = Random.nextInt(numberOfMethods) //
        var str = matchMethods(newElem).toString() //
        var indexAppear = matchMethodsMap(newElem)
        var replacement = generateMethod(); //
        indexes.append(new Tuple3(indexAppear, indexAppear+(str.size), replacement))
        println("Menjamo metod: " + str + " koji se pojavljuje od: " + indexAppear + " do: " + (indexAppear+str.size) +
          " sa metodom: " + replacement)
      }
    }

    /* trazimo streamove */
    val matchStreamsNotComplete = streamCompletePattern.findAllMatchIn(deleteStreams).toList // //
    val matchStreamsMap = streamCompletePattern.findAllMatchIn(deleteStreams).map(_.start).toList
    val numberOfStreamsNotComplete = matchStreamsNotComplete.size // //
    println("Ukupno ima: " + numberOfStreamsNotComplete + " streamova, ") //
    for(i <- 0 until matchStreamsNotComplete.size){
      indexes.append(new Tuple3(matchStreamsMap(i), matchStreamsMap(i)+matchStreamsNotComplete(i).toString().size, null))
    }

    //println("Ukupno menjamo: " + indexes.size + " elemenata.")

    indexes = indexes.sortBy(_._1)
    println("\n ----------------- Sortirani elementi: -------------- \n")
    for(element <- indexes)
      println("Element: (" + element._1 + ", " + element._2 + ", " + element._3 + ")")

    /* sada sve zapisujem u novi fajl */
    //val matchStreams2 = streamCompletePattern.findAllMatchIn(contentString).toList
    val newContent = new StringBuilder();
    var beginIndex = 0;
    var prevousElement: Tuple3[Int, Int, String] = null;
    var dataChanged = 0;
    var nextStream = 0;
    for(i <- 0 until indexes.size) {
      println("i je " + i)
      var element = indexes(i);
      var startElement = element._1;
      var endElement = element._2;
      var replacement = element._3;
      /* onda je u pitanju stream */
      if(replacement == null){
        //println("replacement je null: " + replacement)
        val currentStream = matchStreams(nextStream).toString()
        if(Random.nextInt(20)>=18)
          replacement = changeStream(currentStream)
        else {
          replacement = new String(currentStream);
          dataChanged-=1
        }
        nextStream+=1;
      }
//      else
      //  println("replacement nije null: " + replacement)
      if(i == 0 || startElement > prevousElement._2) {
        newContent.append(deleteStreams.substring(beginIndex, startElement))
        newContent.append(replacement)
        beginIndex = endElement;
        //println("Element: (" + element._1 + ", " + element._2 + ", " + element._3 + ")")
        prevousElement = element;
        dataChanged+=1;
      }
    }
    newContent.append(deleteStreams.substring(beginIndex))

    println("\t ----------- Ukupno promenjeno: " + dataChanged + " podataka.")
    /* zapisujemo novi fajl */
    var newContentFile: String = ".\\fuzzedPDFcorpus\\" + actorNumber.toString +"_fuzzed.pdf";
    //var newContentFile = new File(newContentName);
    //newContent.
    if (!new File(newContentFile).exists()) Files.createFile(Paths.get(newContentFile))// createDirectory(Paths.get(newContentFile))
    val buffOutFinal = new BufferedOutputStream(new FileOutputStream(newContentFile))
    buffOutFinal.write((newContent.toString()).getBytes(Charset.forName("ISO-8859-1")))
    buffOutFinal.close()
}

  /*
def readFileAsString() = {
val content: String =  Source.fromFile(fileName, "ISO-8859-1").getLines().mkString
Source.fromFile(fileName, "ISO-8859-1").getLines().mkString
/* u celom sadrzaju pretrazujemo pojavljivanje stringova ********************************************************/
val matchStringInLine = stringPattern.findAllIn(content)
matchStringInLine.foreach(s => {
  println("Nadjen string: " + s);
  //println("Extended character: " + Character.toChars(142).toString);
  /* ovaj string treba zameniti sa: */
  if (s.startsWith("(")) {
    for (i <- 1 until 2) {
      /*var next = Random.nextString(20);
                                    println("\t\tMenjamo ga sa: " + next)
                                    next.foreach(c => println("\t\t\tKarakter: " + c))*/
      println("\t\tMenjamo ga sa: " + generateString(false, s.size))

    }
    //println("\t\tMenjamo ga sa: " + Random.alphanumeric.take(10).mkString)
  } //()
  else {
    for (i <- 1 until 2) {
      /*var next = Random.nextString(20);
                                    println("\t\tMenjamo ga sa: " + next)
                                    next.foreach(c => println("\t\t\tKarakter: " + c))*/
      println("\t\tMenjamo ga sa: " + generateHexString(s.size))
    }
  } //<>
}//foreach
)
/*matchStringInLine match {
  case Some(s) => println("Nadjen string: " + s)
  case None => ;
}*/

println("**********************************************************************************************************************")
/* u celom sadrzaju pretrazujemo pojavljivanje stream-ova ********************************************************/
val matchStream = streamCompletePattern.findAllIn(content)
matchStream.foreach(s => {
  var stream = (s.substring(6, s.length))
  stream = stream.substring(0, stream.length - 9)
//      println("\t"+s.substring(0, 5))
  println("Nadjen stream: \n" + stream + "\n");
  val streamByteArray: Array[Byte] = stream.getBytes("ISO-8859-1");
  println("\t\tKao ByteArray: ")
//      streamByteArray.foreach(b => print("" + b.toString + ", "))
//      println()
  /* stream pisemo u privremeni fajl da bismo ga procitali kao sekvencu bajtova */
/*    var tempFileName: String = "tempStream.pdf";
  /* pravimo datoteku za upis */
  if (!Files.exists(Paths.get(tempFileName))) {
    val file = new File(tempFileName)
    file.createNewFile();
  }
  else
    FileChannel.open(Paths.get(fileName), StandardOpenOption.WRITE).truncate(0).close()

  Files.write(Paths.get(fileName), stream.getBytes(StandardCharsets.ISO_8859_1), StandardOpenOption.APPEND)
*/
  //println("\t" + s.substring(0, 6) + " " + s.substring(s.length-9, s.length))
  //s.slice(0, 5);
});

}
*/
  /*
def readLines(): Unit = {

/*    if (!Files.exists(Paths.get("prepisivanje.pdf"))) {
  val file = new File("prepisivanje.pdf")
  file.createNewFile();
}
else
  FileChannel.open(Paths.get("prepisivanje.pdf"), StandardOpenOption.WRITE).truncate(0).close()
*/
var skip = false;
for(line <- Source.fromFile(fileName, "ISO-8859-1").getLines()) {
  println("Linija:\t" + line);
  val line2 = line + "\n";
  //Files.write(Paths.get("prepisivanje.pdf"), line2.getBytes(StandardCharsets.ISO_8859_1), StandardOpenOption.APPEND)

  val words = line.split(" ");

  //ako smo naisli na endstream, onda linije vise ne treba preskakati
  val matchEndStreamInLine = endStreamPattern.findFirstIn(line)
  matchEndStreamInLine match {
    case Some(s) => skip = false; println("Nadjen endstream: " + s);
    case None => ;
  }

  val matchStreamInLine = streamPattern.findFirstIn(line)
  matchStreamInLine match {
    case Some(s) => skip = true; println("Nadjen stream: " + s)
    case None => ;
  }

  if(!skip) {
    for (word <- words) {
      val Digit = """\d""".r
      val date = """(\d\d\d\d)-(\d\d)-(\d\d)""".r

      "2004-01-20" match {
        case date(year, month, day) => println(s"$year was a good year for PLs.")
      }
      word match {
        case Digit() => println(s"Cifra!")
        case stringPattern() => println("Niska karaktera!")
        case _ => println("Nista od navadenog!")
      }
      println("\tword: " + word)
    }
    println("")
  }//if !skip
}
}*/
}
