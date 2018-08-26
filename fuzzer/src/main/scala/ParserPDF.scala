import java.io.{BufferedOutputStream, File, FileOutputStream}
import java.nio.channels.FileChannel
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, Paths, StandardOpenOption}
import java.nio.file.{Files, Paths}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Random
import scala.util.matching.Regex

class ParserPDF(fileName: String, actorNumber: Int) {
  /* sabloni koje pretrazujemo u datotekama */
  /* stringovi se mogu pojaviti kao karakteri izmedju () ili heksadekadno izmedju <> */
  val stringPattern: Regex = """\([\s\S]+?\)|\<[0-9a-fA-F]*?\>""".r
  val streamCompletePattern: Regex = """stream[\s\S]+?endstream""".r
  //val methodPattern: Regex = """[0-9a-zA-Z]+\(\)""".r
  val numberPattern: Regex = """[+-]?((\d+\.?\d+)|(\d*\.?\d+)|(\d+\.?\d*))""".r
  val namePattern: Regex = """/[a-zA-Z0-9]+""".r
  val objectPattern: Regex = """(\d+) (\d+) (obj)([\s\S]+?)(endobj)""".r

  var byteArray: Array[Byte] = Array.empty[Byte]

  val knownNames = Array[String]("/Title", "/Dest", "/Parent", "/Next", "/Prev", "/XYZ", "/Size", "/Root", "/Page", "/ID", "/Info",
    "/Type", "/Resources", "/MediaBox", "/Contents", "/XObject", "/Image", "/Name", "/Width", "/Height", "/BitsPerComponent", "/Length",
    "/ColorSpace", "/DeviceRGB", "/Filter", "/FlateDecode", "/Subtype", "/MediaBox", "/Name", "/wpt336", "/Link", "/Rect", "/Border",
    "/Annot", "/Action", "/URI", "/S", "/A", "/FontDescriptor", "/FontName", "/Arial,Bold", "/Ascent", "/CapHeight", "/Descent",
    "/Flags", "/FontBBox", "/ItalicAngle", "/StemV", "/AvgWidth", "/Leading", "/MaxWidth", "/XHeight", "/Encoding", "/FirstChar",
    "/LastChar", "/Widths", "/TrueType", "/Arial", "/Encoding", "/BaseFont", "/Verdana", "/CourierNew,Bold", "/TimesNewRoman,Italic",
    "/Symbol", "/ProcSet", "/PDF", "/Text", "/ImageC", "/F1", "/F12", "/Dest", "/First")
  /* Funkcija koja u zavisnosti od broja cifara broja, vraca slucajan broj koji ce sluziti kao broj podataka koji se menjaju u odnosu na ukupan broj podataka */
  def getAPiece(number: Int): Int = {
    def numberOfDigitsTailRecursive(num: Int, help :Int): Int = {
      if(num>=0 && num<10) 1+help
      else numberOfDigitsTailRecursive(num/10, help+1)
    }
    if(number == 0) 0
    else {
      val numberOfDigits = numberOfDigitsTailRecursive(number, 0)
      numberOfDigits match {
        case 1 => 1
        case 2 => Random.nextInt(number / 3)
        case 3 => Random.nextInt(number / 6)
        case 4 => Random.nextInt(number / 50)
        case 5 => Random.nextInt(number / 300)
        case 6 => Random.nextInt(number / 3000)
        case 7 => Random.nextInt(number / 8000)
        case 8 => Random.nextInt(number / 60000)
        case 9 => Random.nextInt(number / 500000)
        case _ => println("Vise od 9 cifara?"); 1
      }
    }
  }

  /* funkcija koja konvertuje string u njegov heksadecimalni zapis */
  def string2hex(str: String): String = {
    str.toList.map(_.toInt.toHexString).mkString
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

    if(Random.nextInt(3)==2) {
      val s: StringBuilder = new StringBuilder();
      s.append("<")
      val chars: Seq[Char] = Seq[Char]('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F', 'a', 'b', 'c', 'd', 'e', 'f');
      s.append(randomStringFromCharList(length, chars));
      s.append(">")
      s.toString()
    }
    else{
      string2hex(generateString(false, Random.nextInt(1000)+1))
    }

  }

  /* Funkcija koja generise stringove u formatu <...> ako je hex true, inace stringove u formatu (....)  */
  def generateString(hex: Boolean, length: Int): String ={
    if(length == 0)
      return ""
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
      if(Random.nextInt(5)>1){
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
      var filename: String = null;
      if(Random.nextInt(2) == 1)
        filename = "strings\\naughtyStrings.txt";
      else
        filename = if (integer) "ints\\integerOverflows.txt" else "reals\\realOverflows.txt";
      getHeuristicString(filename);
    }
    else if(integer)
      Random.nextInt().toString
    else
      Random.nextDouble().toString
  }

  def generateMethod(): String = {
    getHeuristicString("commonNames\\commonMethodNames.txt")
  }

  def getHeuristicString(fileName: String): String = {
    var lines = Source.fromFile(".\\heuristics\\" + fileName, "ISO-8859-1").getLines().toArray
    val numberOfLines = lines.size
    //println("\tFajl " + fileName + " ima " + numberOfLines + " linija")
    val lineNumber = Random.nextInt(numberOfLines)
    //println("\tLine number: " + lineNumber)
    val chosenOne = lines(lineNumber)
    //println("\tThe chosen one is: " + chosenOne)
    chosenOne
  }

  def generateName(): String = {
    var r = Random.nextInt(2);
    var s = new StringBuilder();
    //print("generateName(): ")
    r match {
      case 0 => {
        //println("0")
        s.append("/");
        s.append(generateString(false, Random.nextInt(50)+1))
        s.toString()
      }
      case 1 => {
        //println("1")
        knownNames(Random.nextInt(knownNames.size))
      }
    }
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
      //println("--------- Menja se stream -----------")
    /* prvi slucaj je umetanje cele druge pdf datoteke kao sadrzaj stream-a */
    val wholePdf = Random.nextInt(1000)
    if(wholePdf>990){
      println("Umecemo ceo pdf u drugi pdf!")
      Random.nextInt(3) match {
        case 0 => new String(byteArray, Charset.forName("ISO-8859-1"))
        case 1 => {
          val bytes = Files.readAllBytes(Paths.get("pokvareni.pdf"))
          new String(bytes, Charset.forName("ISO-8859-1"))
        }
        case 2 =>{
          val bytes = Files.readAllBytes(Paths.get("nepokvareni.pdf"))
          new String(bytes, Charset.forName("ISO-8859-1"))
        }
      }
    }
    /* drugi slucaj je menjanje datog stream-a mutacijom, dodavanjem ili brisanjem bajtova */
    else {
      /* izdvajamo samo binarni deo */
      var stream = (str.substring(6, str.length))
      stream = stream.substring(0, stream.length - 9)
      //      println("\t"+s.substring(0, 5))
      //println("Nadjen stream: \n" + stream + "\n");
      val streamComplete: StringBuilder = new StringBuilder();
      streamComplete.append("\nstream\n")
      //streamComplete.append("\n")
      // pretvaramo ga u bajtove
      var streamInBytes = stream.getBytes(Charset.forName("ISO-8859-1"));
      val streamBytesSize = streamInBytes.size
      val numberOfChanges = Random.nextInt(4) //?!
      //println("\t\t ---- Broj parcica koje menjam: " + numberOfChanges)
      //println("\t\t ---- Pre izmena velicina bajtova je: " + streamBytesSize)
      /* pravimo promene onoliko puta kolika je vrednost promenljive numberOfChanges*/
      for (i <- 0 until numberOfChanges) {
        /* odredjujemo velicinu novog niza bajtova: gledamo da bude neko manje parce */
        val smallChunk = getAPiece(streamBytesSize)
        //println("\t\t ------------ (streamBytesSize/50): " + smallChunk)
        if (smallChunk != 0) {
          val newBytesSize = Random.nextInt(smallChunk)
          /* pravimo novi stream te velicine */
          val newBytes: Array[Byte] = new Array[Byte](newBytesSize)
          //streamInBytes.copyToArray(newBytes) // ovo je za kopiranje istih podataka
          Random.nextBytes(newBytes)
          /* ovo parce moramo umetnuti na neko mesto u postojecim bajtovima */
          val startIndex = Random.nextInt(streamBytesSize - newBytesSize - 1)
          for (i <- 0 until newBytesSize)
            streamInBytes(i + startIndex) = newBytes(i)
        }
        //println("\t\t ---- Posle izmena velicina bajtova je: " + streamInBytes.size)
      }
      /* slucajno odredjujemo da li brisemo, umecemo elemente, ili nista od toga */
      val delIns = Random.nextInt(1000)
      if (delIns > 990) {
        val streamBytesBuffer = streamInBytes.to[ArrayBuffer]
        if (delIns > 995) {
          val numberElementsToRemove = Random.nextInt(getAPiece(streamBytesSize) + 1)
          streamBytesBuffer.remove(Random.nextInt(streamBytesSize - numberElementsToRemove), numberElementsToRemove)
        }
        else {
          val piece = getAPiece(streamBytesSize) + 1
          println("streamBytesSize: " + streamBytesSize + ", getAPiece: " + piece)
          val numberElementsToInsert = Random.nextInt(piece)
          val bytes: Array[Byte] = new Array[Byte](numberElementsToInsert)
          Random.nextBytes(bytes)
          streamBytesBuffer.insertAll(Random.nextInt(streamBytesSize), bytes)
        }
        streamInBytes = streamBytesBuffer.toArray
      }
      streamComplete.append(new String(streamInBytes, Charset.forName("ISO-8859-1")))
      streamComplete.append("\nendstream\n")
      streamComplete.toString();
    }
  }

  /* ------------------------------------------------------------------------------------------------------- */
  def readFileBinary() = {
    byteArray = Files.readAllBytes(Paths.get(fileName));
    /* pravimo string koji sadrzi podatke iz datoteke */
    var contentString = new String(byteArray, Charset.forName("ISO-8859-1"))

    /* brisemo mali broj objekata tj. menjamo ih sa praznim stringom */
    val noObjects = new StringBuilder();
    var startIndex = 0;
    objectPattern.findAllMatchIn(contentString).foreach(m => {
      noObjects.append(contentString.substring(startIndex, m.start))
      if(Random.nextInt(1000)>997){
        //println("Fajl: " + actorNumber +"; Obrisan objekat: " + m.group(1) + " " + m.group(2))
        noObjects.append("\n")
      }
      else {
        noObjects.append(m)
      }
      startIndex = m.end
    })
    noObjects.append(contentString.substring(startIndex))

    /* sve osim stream-ova trazimo u podacima bez streamova */
    val matchStreams = streamCompletePattern.findAllMatchIn(noObjects).toList
    var deleteStreams = new String("");
    var noStreamsBuilder = new StringBuilder()
    startIndex = 0;
    for(elem <- matchStreams) {
      var index = noObjects.indexOf(elem.toString(), startIndex)
      //println("Prvo pojavljivanje je od: " + index + " do: " + (index + (elem.toString()).size))
      noStreamsBuilder.append(noObjects.substring(startIndex, index))
      noStreamsBuilder.append("stream\nendstream")
      startIndex = index + (elem.toString()).size;
    }
    noStreamsBuilder.append(noObjects.substring(startIndex));

    /* u deleteStreams se nalaze podaci bez stream-ova; upisujemo podatke u fajl fajlBezStreamova.pdf radi provere */
    deleteStreams = noStreamsBuilder.toString()
    var tempFileNoStreams: String = "fajlBezStreamova.pdf"// +actorNumber + ".pdf";
    val bosns = new BufferedOutputStream(new FileOutputStream(tempFileNoStreams))
    bosns.write(deleteStreams.getBytes(Charset.forName("ISO-8859-1")))
    bosns.close()
    val deleteStreamsSize = deleteStreams.size

    /* trazimo imena */
    val buf = scala.collection.mutable.ListBuffer.empty[Regex.Match]
    val bufInt = scala.collection.mutable.ListBuffer.empty[Int]
    namePattern.findAllMatchIn(deleteStreams).foreach(m => {
     // if(m.toString() == "/Length")
     //   println("Ime /Length")
      if(Random.nextInt(100)>95) {
        //println("Pronasli smo ime: " + m.toString())
        buf += m
        bufInt+=m.start
      }
    })
    val matchNames = buf.toList
    val matchNamesMap = bufInt.toList

    val numberOfNames = matchNames.size
       println("Ukupno ima: " + numberOfNames + " imena, ")
    var percentOfChange = getAPiece(numberOfNames);
    var numberOfChanged = 0
    var indexes = scala.collection.mutable.ArrayBuffer.empty[Tuple3[Int, Int, String]]
    if(percentOfChange != 0) {
      numberOfChanged = percentOfChange//Random.nextInt(percentOfChange+1)
      println("\ta mi biramo da promenimo: " + numberOfChanged)
      var newElems = new Array[Int](numberOfChanged)
      for(i <- 0 until numberOfChanged){
        var newElem = Random.nextInt(numberOfNames)
        while(newElems.contains(newElem)){
          println("while")
          newElem = Random.nextInt(numberOfNames)
        }
        var str = matchNames(newElem).toString()
        var indexAppear = matchNamesMap(newElem)
        var replacement = ""
        if(str == "/Length" && Random.nextInt(100)>95){
          replacement = ""
        }
        else replacement = generateName()
        indexes.append(new Tuple3(indexAppear, indexAppear+(str.size), replacement))
        //println("Menjamo ime: " + str + " koji se pojavljuje od: " + indexAppear + " do: " + (indexAppear+str.size) +
        //  " sa imenom: " + replacement)
      }
    }

/*    var t = 0;
    objectPattern.findAllMatchIn(deleteStreams).foreach(m => {
      if(t<2) {
        //m.group(1).toString
        println("Fajl" + fileName + "\n\tObjekat1: "+m.group(1));
        println("\tObjekat2: "+m.group(2) + "\n");
        println("\tObjekat3: "+m.group(3) + "\n");

        var s = new StringBuilder()
        s.append("\tObjekat4: ")
        s.append(m.group(4).replaceAll("""<""", "\\<"))
        println(s.toString());
        //println(s"Fajl $fileName \tObjekat2: $m.group(2)")
        println("\tObjekat5: "+m.group(5) + "\n");
        //println(m.group(2).toString())
        t+=1
      }
    })
  */
    /* iz stringa (bez stream-ova) izvlacimo PDF stringove */
    val matchStrings = stringPattern.findAllMatchIn(deleteStreams).toList
    val matchStringsMap = matchStrings.map(_.start)// stringPattern.findAllMatchIn(deleteStreams).map(_.start).toList
    val numberOfStrings = matchStrings.size
 //!   println("Ukupno ima: " + numberOfStrings + " stringova, ")
    percentOfChange = getAPiece(numberOfStrings);
    numberOfChanged = 0
    //var indexes = scala.collection.mutable.ArrayBuffer.empty[Tuple3[Int, Int, String]]
    if(percentOfChange != 0) {
       numberOfChanged = percentOfChange//Random.nextInt(percentOfChange+1)
 //!      println("\ta mi biramo da promenimo: " + numberOfChanged)
      var newElems = new Array[Int](numberOfChanged)
       for(i <- 0 until numberOfChanged){
        var newElem = Random.nextInt(numberOfStrings)
         while(newElems.contains(newElem)) newElem = Random.nextInt(numberOfStrings)
         var str = matchStrings(newElem).toString()
         var indexAppear = matchStringsMap(newElem)
         var replacement = if(str.startsWith("(")) generateString(false, str.size) else generateString(true, 0);
         indexes.append(new Tuple3(indexAppear, indexAppear+(str.size), replacement))
         //println("Menjamo string: " + str + " koji se pojavljuje od: " + indexAppear + " do: " + (indexAppear+str.size) +
         //  " sa stringom: " + replacement)
       }
    }
   //! println("*************************************************************************")

    /* trazimo brojeve */
    val matchNumbers = numberPattern.findAllMatchIn(deleteStreams).toList
    val matchNumbersMap = matchNumbers.map(_.start)
    val numberOfNumbers = matchNumbers.size
    println("Ukupno ima: " + numberOfNumbers + " brojeva, ")
    percentOfChange = getAPiece(numberOfNumbers);
    numberOfChanged = 0
    if(percentOfChange != 0) {
      numberOfChanged = percentOfChange//Random.nextInt(percentOfChange+1)
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
        //println("Menjamo broj: " + str + " koji se pojavljuje od: " + indexAppear + " do: " + (indexAppear+str.size) +
        //  " sa brojem: " + replacement)
      }
    }

    /* trazimo metode */
/*    val matchMethods = methodPattern.findAllMatchIn(deleteStreams).toList // //
    val matchMethodsMap = matchMethods.map(_.start)
    val numberOfMethods = matchMethods.size // //
    println("Ukupno ima: " + numberOfMethods + " metoda, ") //
    percentOfChange = getAPiece(numberOfMethods); //
    numberOfChanged = 0
    if(percentOfChange != 0) {
      numberOfChanged = percentOfChange//Random.nextInt(percentOfChange+1)
      println("\ta mi biramo da promenimo: " + numberOfChanged)
      var newElems = new Array[Int](numberOfChanged)
      for(i <- 0 until numberOfChanged){
        var newElem = Random.nextInt(numberOfMethods) //
        while(newElems.contains(newElem)) newElem = Random.nextInt(numberOfMethods) //
        var str = matchMethods(newElem).toString() //
        var indexAppear = matchMethodsMap(newElem)
        var replacement = generateMethod(); //
        indexes.append(new Tuple3(indexAppear, indexAppear+(str.size), replacement))
        //println("Menjamo metod: " + str + " koji se pojavljuje od: " + indexAppear + " do: " + (indexAppear+str.size) +
        //  " sa metodom: " + replacement)
      }
    }
*/
    /* trazimo streamove */
    val matchStreamsNotComplete = streamCompletePattern.findAllMatchIn(deleteStreams).toList // //
    val matchStreamsMap = matchStreamsNotComplete.map(_.start)
    val numberOfStreamsNotComplete = matchStreamsNotComplete.size // //
 //!   println("Ukupno ima: " + numberOfStreamsNotComplete + " streamova, ") //
    for(i <- 0 until numberOfStreamsNotComplete){
      indexes.append(new Tuple3(matchStreamsMap(i), matchStreamsMap(i)+matchStreamsNotComplete(i).toString().size, null))
    }

    //println("Ukupno menjamo: " + indexes.size + " elemenata.")

    indexes = indexes.sortBy(_._1)
   /* println("\n ----------------- Sortirani elementi: -------------- \n")
    for(element <- indexes)
      println("Element: (" + element._1 + ", " + element._2 + ", " + element._3 + ")")
*/
    /* sada sve zapisujem u novi fajl */
    val newContent = new StringBuilder();
    var beginIndex = 0;
    var prevousElement: Tuple3[Int, Int, String] = null;
    var dataChanged = 0;
    var nextStream = 0;
    var streamsChanged = 0;
    for(i <- 0 until indexes.size) {
      //println("i je " + i)
      var element = indexes(i);
      var startElement = element._1;
      var endElement = element._2;
      var replacement = element._3;
      //println("startElement: " + startElement + ", endElement: " + endElement)
      /* onda je u pitanju stream */
      if(replacement == null){
        //println("replacement je null: " + replacement)
        val currentStream = matchStreams(nextStream).toString()
        if(Random.nextInt(20)>=18){
          replacement = changeStream(currentStream)
          streamsChanged+=1;
        }
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
//!    println("\t\t a biramo da menjamo: " + streamsChanged + " streamova")
    println("\t ----------- Ukupno promenjeno: " + dataChanged + " podataka od " + indexes.size + " podataka.")
    /* zapisujemo novi fajl */
    var newContentFile: String = ".\\fuzzedPDFcorpus\\" + actorNumber.toString +"_fuzzed.pdf";
    //println("Novi fajl: " + newContentFile)
    //var newContentFile = new File(newContentName);
    //newContent.
    if (!new File(newContentFile).exists()) Files.createFile(Paths.get(newContentFile))// createDirectory(Paths.get(newContentFile))
    val buffOutFinal = new BufferedOutputStream(new FileOutputStream(newContentFile))
    buffOutFinal.write((newContent.toString()).getBytes(Charset.forName("ISO-8859-1")))
    buffOutFinal.close()
  }
}
