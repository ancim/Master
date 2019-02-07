import java.io.{BufferedOutputStream, File, FileOutputStream}
import java.nio.channels.FileChannel
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, Path, Paths, StandardOpenOption}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Random
import scala.util.matching.Regex

class ParserPDF(fileName: String, actorNumber: Int) {
  /* sabloni koje pretrazujemo u datotekama */
  /* stringovi se mogu pojaviti kao karakteri izmedju () ili heksadekadno izmedju <> */
  val stringPattern: Regex =
  """\([\s\S]+?\)|\<[0-9a-fA-F]*?\>""".r
  val streamCompletePattern: Regex = """(stream)([\s\S]+?)(endstream)""".r
  val streamNewLineEndstreamPattern: Regex = """(stream)\n(endstream)""".r
  //val methodPattern: Regex = """[0-9a-zA-Z]+\(\)""".r
  val numberPattern: Regex =
    """[+-]?((\d+\.?\d+)|(\d*\.?\d+)|(\d+\.?\d*))""".r
  val namePattern: Regex = """/[a-zA-Z0-9]+""".r
  val objectPattern: Regex = """(\d+) (\d+) (obj)([\s\S]+?)(endobj)""".r
  val objDictPattern = """(obj)([\s]*)(<<)([\s\S]+?)(>>)""".r

  val r = new Random(System.currentTimeMillis())

  val knownNames = Array[String]("/Title", "/Dest", "/Parent", "/Next", "/Prev", "/XYZ", "/Size", "/Root", "/Page", "/ID", "/Info",
    "/Type", "/Resources", "/MediaBox", "/Contents", "/XObject", "/Image", "/Name", "/Width", "/Height", "/BitsPerComponent", "/Length",
    "/ColorSpace", "/DeviceRGB", "/Filter", "/FlateDecode", "/Subtype", "/MediaBox", "/Name", "/wpt336", "/Link", "/Rect", "/Border",
    "/Annot", "/Action", "/URI", "/S", "/A", "/FontDescriptor", "/FontName", "/Arial,Bold", "/Ascent", "/CapHeight", "/Descent",
    "/Flags", "/FontBBox", "/ItalicAngle", "/StemV", "/AvgWidth", "/Leading", "/MaxWidth", "/XHeight", "/Encoding", "/FirstChar",
    "/LastChar", "/Widths", "/TrueType", "/Arial", "/Encoding", "/BaseFont", "/Verdana", "/CourierNew,Bold", "/TimesNewRoman,Italic",
    "/Symbol", "/ProcSet", "/PDF", "/Text", "/ImageC", "/F1", "/F12", "/Dest", "/First")

  def numberOfDigitsTailRecursive(num: Int, help: Int): Int = {
    if (num >= 0 && num < 10) 1 + help
    else numberOfDigitsTailRecursive(num / 10, help + 1)
  }

  /* Funkcija koja u zavisnosti od broja cifara broja, vraca slucajan broj koji ce sluziti kao broj podataka koji se menjaju u odnosu na ukupan broj podataka */
  def getAPiece(number: Int): Int = {
    if (number == 0) 0
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
  def generateHexString(length: Int): String = {
    //println("generateHexString()")
    /* pomocna funkcija koja generise string pomocu karaktera prosledjenih kao drugi argument */
    def randomStringFromCharList(length: Int, chars: Seq[Char]): String = {
      val sb = new StringBuilder
      for (i <- 1 to length) {
        val randomNum = util.Random.nextInt(chars.length)
        sb.append(chars(randomNum))
      }
      sb.toString
    }
    /* prva mogucnost je generisanje stringa iste duzine, slucajno */
    if (Random.nextInt(3) == 2) {
      val s: StringBuilder = new StringBuilder();
      s.append("<")
      val chars: Seq[Char] = Seq[Char]('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F', 'a', 'b', 'c', 'd', 'e', 'f');
      s.append(randomStringFromCharList(length, chars));
      s.append(">")
      s.toString()
    }
    /* druga mogucnost je generisanje stringa proizvoljne duzine funkcijom generateString i njeno pretvaranje u heksadecimalni zapis */
    else {
      val s = new StringBuilder()
      s.append("<")
      val helpString = generateString(false, Random.nextInt(1000) + 1, false)
      if (helpString.size >= 2)
        s.append(string2hex(helpString.substring(1, helpString.size - 1)))
      else
        s.append(string2hex(helpString))
      s.append(">")
      s.toString()
    }
  }

  /* Funkcija koja generise stringove u formatu <...> ako je hex true, inace stringove u formatu (....)  */
  def generateString(hex: Boolean, length: Int, name: Boolean): String = {
    //println("generateString()")
    if (hex == true) {
      generateHexString(length)
    }
    else {
      /* Ova funkcija koristi pomocnu funkciju koja vraca listu karaktera odredjene duzine; zbog efikasnosti koristi se repna rekurzija */
      def randomStringTailRecursive(n: Int, list: List[Char]): List[Char] = {
        if (n == 1) util.Random.nextPrintableChar :: list
        else randomStringTailRecursive(n - 1, util.Random.nextPrintableChar :: list)
      }

      /* slucajno se odredjuje da li se string uzima iz skupa stringova (heuristike) ili se slucajno generise, ili se umece drugi fajl kao string */
      var choice = 0
      choice = Random.nextInt(1000)

      if (choice >= 0 && choice < 500) {
        var filename: StringBuilder = new StringBuilder();
        Random.nextInt(4) match {
          case 0 => filename.append("strings/formatStrings.txt");
          case 1 => filename.append("strings/naughtyStrings.txt");
          case 2 => filename.append("strings/traversals.txt");
          case 3 => filename.append("strings/upsideDownText.txt");
        }
        var result = new StringBuilder("(");
        result.append(getHeuristicString(filename.toString()));
        result.append(")")
        result.toString()
      }
      else if ((choice >= 500 && choice <= 996) || name == true) {
        val s: StringBuilder = new StringBuilder();
        s.append("(")
        val list: List[Char] = List[Char]();
        var newLength = length
        if (Random.nextInt(1000) > 990) newLength = Random.nextInt(1000)
        val sMiddle = randomStringTailRecursive(newLength, list).mkString;
        s.append(sMiddle);
        s.append(")")
        s.toString()
      }
      else {
        println("Umecemo ceo pdf u drugi pdf u vidu stringa!")
        getAnotherPDF()
      }
    }
  }

  def generateNumber(integer: Boolean): String = {
    if (Random.nextInt(2) == 1) {
      var filename: String = null;
      if (Random.nextInt(2) == 1)
        filename = "strings/naughtyStrings.txt";
      else
        filename = if (integer) "ints/integerOverflows.txt" else "reals/realOverflows.txt";
      getHeuristicString(filename);
    }
    else if (integer)
      Random.nextInt().toString
    else
      Random.nextDouble().toString
  }
/*
  def generateMethod(): String = {
    getHeuristicString("commonNames/commonMethodNames.txt")
  }
*/
  def getHeuristicString(fileHName: String): String = {
    import scala.collection.JavaConverters._
    //val lines: Array[String] = Source.fromFile("./heuristics/" + fileHName, "ISO-8859-1").getLines().toArray
    var lines: Array[String] = Files.readAllLines(Paths.get("./heuristics/" + fileHName), Charset.forName("ISO-8859-1")).asScala.toArray
    /*if(lines.deep != lines2.deep)
      println(" ___________________________________ Nisu jednaki!" + " Velicine: " + lines.size + " " + lines2.size)
    else println("_ Jednaki su!")*/
    val numberOfLines = lines.size
    //println("\tFajl " + fileHName + " ima " + numberOfLines + " linija")
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
        val pro = generateString(false, Random.nextInt(50) + 1, true)
        if (pro.size >= 2) s.append(pro.substring(1, pro.size - 1))
        else s.append(pro)
        s.toString()
      }
      case 1 => {
        //println("1")
        knownNames(Random.nextInt(knownNames.size))
      }
    }
  }

  /* ALARM */
  def getAnotherPDF(): String = {
    var s: String = null
    /*s = ""
    s*/

    var bytes: Array[Byte] = null
    Random.nextInt(2) match {
      //case 0 => new String(byteArray, Charset.forName("ISO-8859-1"))
      case 0 => {
        bytes = Files.readAllBytes(Paths.get("pokvareni.pdf"))
        s = new String(bytes, Charset.forName("ISO-8859-1"))
      }
      case 1 => {
        bytes = Files.readAllBytes(Paths.get("nepokvareni.pdf"))
        s = new String(bytes, Charset.forName("ISO-8859-1"))
      }
    }
    bytes = null
    s
  }

  def changeStream(str: String): String = {
    //println("--------- Menja se stream -----------")
    /* prvi slucaj je umetanje cele druge pdf datoteke kao sadrzaj stream-a */
    val wholePdf = Random.nextInt(1000)
    if (wholePdf > 995) {
      println("Umecemo ceo pdf u drugi pdf u vidu toka!")
      getAnotherPDF()
    }
    /* drugi slucaj je menjanje datog stream-a mutacijom, dodavanjem ili brisanjem bajtova */
    else {
      /* izdvajamo samo binarni deo */
      var stream1 = (str.substring(6, str.length))
      var stream = stream1.substring(0, stream1.length - 9)
      stream1 = null
      //      println("\t"+s.substring(0, 5))
      //println("Nadjen stream: \n" + stream + "\n");
      val streamComplete: StringBuilder = new StringBuilder();
      streamComplete.append("\nstream\n")
      //streamComplete.append("\n")
      // pretvaramo ga u bajtove
      var streamInBytes = stream.getBytes(Charset.forName("ISO-8859-1"));
      stream = null
      val streamBytesSize = streamInBytes.size
      val numberOfChanges = Random.nextInt(4) //
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
        var streamBytesBuffer = streamInBytes.to[ArrayBuffer]
        if (delIns > 995) {
          val numberElementsToRemove = Random.nextInt(getAPiece(streamBytesSize) + 1)
          streamBytesBuffer.remove(Random.nextInt(streamBytesSize - numberElementsToRemove), numberElementsToRemove)
        }
        else {
          val piece = getAPiece(streamBytesSize) + 1
          //println("streamBytesSize: " + streamBytesSize + ", getAPiece: " + piece)
          val numberElementsToInsert = Random.nextInt(piece)
          val bytes: Array[Byte] = new Array[Byte](numberElementsToInsert)
          Random.nextBytes(bytes)
          streamBytesBuffer.insertAll(Random.nextInt(streamBytesSize), bytes)
        }
        streamInBytes = streamBytesBuffer.toArray
        streamBytesBuffer = null
      }
      streamComplete.append(new String(streamInBytes, Charset.forName("ISO-8859-1")))
      streamComplete.append("\nendstream\n")
      streamInBytes = null

      streamComplete.toString();
    }
  }

  def checkMemory() = {
    /*    while(!actors.readMemory()) {
      println("----- Cekamo iz readFileBinary() -----------")
      System.gc()
      Thread.sleep(2000)
      System.gc()
    }*/
  }
// 9876543
  def sizeToSkip(sizeContent: Int): Int = {
    if (sizeContent == 0) 0
    else {
      val numberOfDigits = numberOfDigitsTailRecursive(sizeContent, 0)
      numberOfDigits match {
        case 1 => 0 //1
        case 2 => r.nextInt(10)+1//sizeContent / 3
        case 3 => r.nextInt(21)+30//sizeContent / 10//6
        case 4 => r.nextInt(51)+100//sizeContent / 200//100
        case 5 => /*println("5 cifara");*/ r.nextInt(600)+1000//sizeContent / 5000//500
        case 6 => r.nextInt(20000)+10000//sizeContent / 2000//1000
        case 7 => r.nextInt(200000)+100000//sizeContent*3 / 10000//1000
        case 8 => r.nextInt(3000000)+1000000//sizeContent*3 / 10000//1000
        case 9 => r.nextInt(30000000)+10000000//sizeContent / 500000//1000
        case _ => println("Vise od 9 cifara?"); 1
      }
    }

  }

  /* ------------------------------------------------------------------------------------------------------- */
  def readFileBinary() = {

    /** ............................ citanje datoteke: smestanje sadrzaja u StringBuilder contentStringBuilder .............................. */
    println("readFileBinary()")
    var byteArray: Array[Byte] = Files.readAllBytes(Paths.get(fileName))
    var contentString = new String(byteArray, Charset.forName("ISO-8859-1"))
    byteArray = null
    var contentStringBuilder = new StringBuilder()
    contentStringBuilder.append(contentString)
    val contentSize = contentString.size
    contentString = null

    //System.gc()
    /** .................................... velicina za preskakanje ........................................... */
    val sizeContent = contentStringBuilder.size
    var skipContentStart = sizeToSkip(sizeContent)//*100//sizeContent/100 * 1;
    println("sizeContent: " + sizeContent + "\t" + sizeToSkip(sizeContent))

    /** .................................... brisanje indirektnih objekata i smestanje novog sadrzaja u noObjectsStringBuilder .......................................... */
    var noObjectsStringBuilder = new StringBuilder()
    /* prvo brisemo neke objekte */
    while (!contentStringBuilder.isEmpty) {
      var obj: Option[Regex.Match] = objectPattern.findFirstMatchIn(contentStringBuilder)
      if (obj == None) {
        //println("Nema vise objekata")
        noObjectsStringBuilder.append(contentStringBuilder)
        contentStringBuilder.clear()
      }
      else {
        var startIndex = obj.get.start
        var endIndex = startIndex + obj.get.toString().size
        noObjectsStringBuilder.append(contentStringBuilder.slice(0, startIndex))
        if (Random.nextInt(1000) > 500) {
          println("Brisemo objekat: " + obj.get.group(1) + " " + obj.get.group(2) + " u Worker-u: " + actorNumber)
          noObjectsStringBuilder.append("\n")
        }
        else
          noObjectsStringBuilder.append(obj.get.toString())
        noObjectsStringBuilder.append(contentStringBuilder.slice(endIndex, endIndex+skipContentStart))
        contentStringBuilder.delete(0, endIndex + skipContentStart)
        skipContentStart = sizeToSkip(sizeContent)
      }
      obj = null
    }
    contentStringBuilder = null
    println("........................................... Zavrsili smo sa brisanjem objekata .............................................")

    System.gc()
    /** .................................... stream-ovi se cuvaju u ArrayBuffer objektu streams (jer se drugi objekti ne smeju traziti unutar stream-ova);
      * .................................... novi sadrzaj je u noStreamsBuilder .......................................... */
    /* sad cuvamo stream-ove na drugom mestu */
    val streams = scala.collection.mutable.ArrayBuffer.empty[String]
    var noStreamsBuilder = new StringBuilder()
    while(!noObjectsStringBuilder.isEmpty){
      var stream: Option[Regex.Match] = streamCompletePattern.findFirstMatchIn(noObjectsStringBuilder)
      if (stream == None) {
        //println("Nema vise stream-ova")
        noStreamsBuilder.append(noObjectsStringBuilder)
        noObjectsStringBuilder.clear()
      }
      else {
        streams.append(stream.get.toString())
        var startIndex = stream.get.start
        var endIndex = startIndex + stream.get.toString().size
        noStreamsBuilder.append(noObjectsStringBuilder.slice(0, startIndex))
        noStreamsBuilder.append("stream\nendstream")
        noObjectsStringBuilder.delete(0, endIndex)
      }
      stream = null
    }
    noObjectsStringBuilder = null
    println("........................................... Zavrsili smo sa brisanjem stream-ova .............................................")
    //println("Stream-ova ima: " + streams.size)

    System.gc()
    val sizeNoStreams = noStreamsBuilder.size

    /** .................................... sadrzaj za preskakanje .......................................... */
    var skipContent = sizeToSkip(sizeNoStreams)//sizeNoStreams/200;
    //println("sizeContent: " + sizeContent + "\t" + sizeToSkip(sizeContent))
    println("sizeNoStreams: " + sizeNoStreams + ", skipContent: " + skipContent)

    /** .................................... menjanje objekata redom na osnovu tipa; novi sadrzaj je u changedObjectsBuilder .......................................... */
    var changedObjectsBuilder = new StringBuilder()
    var skipping = 0
    /* funkcija za menjanje objekata tipa zadatog kao argument */
    def changeObjects(typeOfObject: String) = {
      //println("changeObjects()")
      var changed = 0
      var replObjDict: String = null
      while(!noStreamsBuilder.isEmpty){
        var objMatch: Option[Regex.Match] = null

        typeOfObject match {
          case "string" => {
            objMatch = stringPattern.findFirstMatchIn(noStreamsBuilder)
          }
          case "name" => {
            objMatch = namePattern.findFirstMatchIn(noStreamsBuilder)
          }
          case "number" => {
            objMatch = numberPattern.findFirstMatchIn(noStreamsBuilder)
          }
          case "objDict" => {
            objMatch = objDictPattern.findFirstMatchIn(noStreamsBuilder)
          }
        }
        if(objMatch == None) {
          changedObjectsBuilder.append(noStreamsBuilder)
          noStreamsBuilder.clear()
        }
        else {
          var stringStartIndex = objMatch.map(_.start).get
          var stringEndIndex = stringStartIndex + objMatch.get.toString().size

          changedObjectsBuilder.append(noStreamsBuilder.slice(0, stringStartIndex))
          if(Random.nextInt(2)> -1){
            changed += 1
            val replacement = new StringBuilder("")
            typeOfObject match {
              case "string" => {
                println("ObjMatch: " + objMatch.get.toString() + " string")
                if(objMatch.get.toString.startsWith("(")) {
                  replacement.append(generateString(false, objMatch.get.toString().size, false))
                }
                else {
                  replacement.append(generateString(true, objMatch.get.toString().size, false))
                  }
              }
              case "name" => {
                if (objMatch.get.toString() == "/Length" && Random.nextInt(100) > 95) {
                replacement.append("")
                }
                else
                  replacement.append(generateName())
              }
              case "number" => {
                replacement.append(generateNumber(!(objMatch.get.toString().contains("."))))
              }
              case "objDict" => {
                if (replObjDict != null) {
                  replacement.append(replObjDict)
                  replObjDict = objMatch.get.toString()
                }
                else {
                  replObjDict = objMatch.get.toString()
                  replacement.append(replObjDict)
                  changed -= 1
                }
              }
            }

            //println("Menjamo objekat tipa " + typeOfObject + ": " + objMatch.get.toString() + " objektom: " + replacement)
            changedObjectsBuilder.append(replacement)
          }
          else
            changedObjectsBuilder.append(objMatch.get.toString())

          /* nakon sto je objekat promenjen ili ne, sledi preskakanje nekog dela datoteke */
          var toSkip = 0
          skipContent = sizeToSkip(sizeNoStreams)
          if(skipping == 3 || skipping == 6){
            skipping+=1
            toSkip = skipContent
            //println("Preskacemo srednji deo..." + toSkip)
          }
          else if(skipping == 10){
            skipping = 0
            toSkip = skipContent*2
            //println("Preskacemo veliki deo..." + toSkip)
          }
          else {
            skipping+=1
            toSkip = skipContent/2
            //println("Preskacemo mali deo..." + toSkip)
          }
          changedObjectsBuilder.append(noStreamsBuilder.slice(stringEndIndex, stringEndIndex+toSkip))

          noStreamsBuilder.delete(0, stringEndIndex+toSkip)
        }
      }

      println("Promenjeno: " + changed + " objekata tipa " + typeOfObject)
      noStreamsBuilder.append(changedObjectsBuilder)
      changedObjectsBuilder.clear()
      //System.gc()
    }

    println("------------------------------------------ Menjamo stringove -------------------------------------- \n")
    changeObjects("string")
    println("\n------------------------------------------ Menjamo brojeve -------------------------------------- \n")
    changeObjects("number")
    println("\n------------------------------------------ Menjamo imena -------------------------------------- \n")
    changeObjects("name")
    println("\n------------------------------------------ Menjamo recnike objekata -------------------------------------- \n")
    changeObjects("objDict")

    changedObjectsBuilder = null
    println("----------------------------------------- Menjamo tokove podataka ----------------------------------- \n")

    System.gc()

    /** .................................... vracanje streamova i menjanje nekih od njih; novi sadrzaj je u streamsBuilder .......................................... */
    val strSize = streams.size
    var i = 0
    var strChanged = 0;
    var streamsBuilder = new StringBuilder()
    while(!noStreamsBuilder.isEmpty){
      var stream: Option[Regex.Match] = streamNewLineEndstreamPattern.findFirstMatchIn(noStreamsBuilder)
      if (stream == None) {
        //println("Nema vise stream-ova")
        streamsBuilder.append(noStreamsBuilder)
        noStreamsBuilder.clear()
      }
      else {
        i+=1;
        var startIndex = stream.get.start
        var endIndex = startIndex + stream.get.toString().size
        streamsBuilder.append(noStreamsBuilder.slice(0, startIndex))
        if(Random.nextInt(1000)>985){
          streamsBuilder.append(changeStream(streams.head))
          strChanged+=1
          //println("Menjamo stream: " + streams.head)
        }
        else {
          if(streams.isEmpty)
            println(" streams je prazan!")
          streamsBuilder.append(streams.head)
        }
        streams.remove(0)
        noStreamsBuilder.delete(0, endIndex)
      }
      stream = null
    }
    noStreamsBuilder = null

    println("Promenjeno: " + strChanged + " objekata tipa stream\n")


    println("Stream-ova ima: " + strSize + " u actoru:" + actorNumber)
    /* broj streamova sa pocetka treba da se poklapa sa brojem streamova sada */
    System.gc()
    println("Stream-ova ima (i): " + i + " u actoru:" + actorNumber)
    if(strSize != i)
      println(" NE SLAZE SE !")

    /** .................................... zapisujemo sav novi sadrzaj u novi fajl ....................................... */
    var newContentFile: String = "./fuzzedPDFcorpus/" + actorNumber.toString + "_fuzzed.pdf";
    //println("Novi fajl: " + newContentFile)
    //var newContentFile = new File(newContentName);
    //newContent.
    if (!new File(newContentFile).exists()) Files.createFile(Paths.get(newContentFile))
    // createDirectory(Paths.get(newContentFile))
    var buffOutFinal = new BufferedOutputStream(new FileOutputStream(newContentFile))
    buffOutFinal.write((streamsBuilder.toString()).getBytes(Charset.forName("ISO-8859-1")))
    buffOutFinal.close()
    buffOutFinal = null

    System.gc()

    /** ........... kraj ........... */
    /*
    var newContentBuilder = new StringBuilder()

    var mapIndexes = scala.collection.mutable.Map[String, Tuple3[Int, Int, String]]()
    var replObjDict: String = null
    
    var string: Option[Regex.Match] = stringPattern.findFirstMatchIn(noStreamsBuilder)
    var stringStartIndex = if (string != None) string.map(_.start).get else -1
    var stringEndIndex = if (string != None) stringStartIndex + string.get.toString().size else -1
    if(string != None)
      mapIndexes.put("string", new Tuple3[Int, Int, String](stringStartIndex, stringEndIndex, string.get.toString()))

    var number: Option[Regex.Match] = numberPattern.findFirstMatchIn(noStreamsBuilder)
    var numberStartIndex = if (number != None) number.map(_.start).get else -1
    var numberEndIndex = if (number != None) numberStartIndex + number.get.toString().size else -1
    if(number != None)
      mapIndexes.put("number", new Tuple3[Int, Int, String](numberStartIndex, numberEndIndex, number.get.toString()))

    var objDict: Option[Regex.Match] = objDictPattern.findFirstMatchIn(noStreamsBuilder)
    var objDictStartIndex = if (objDict != None) objDict.map(_.start).get else -1
    var objDictEndIndex = if (objDict != None) objDictStartIndex + objDict.get.toString().size else -1
    if(objDict != None)
      mapIndexes.put("objDict", new Tuple3[Int, Int, String](objDictStartIndex, objDictEndIndex, objDict.get.toString()))

    var name: Option[Regex.Match] = namePattern.findFirstMatchIn(noStreamsBuilder)
    var nameStartIndex = if (name != None) name.map(_.start).get else -1
    var nameEndIndex = if (name != None) nameStartIndex + name.get.toString().size else -1
    if(name != None)
      mapIndexes.put("name", new Tuple3[Int, Int, String](nameStartIndex, nameEndIndex, name.get.toString()))

    var seqIndexes = mapIndexes.toSeq.sortBy(_._2._1)
    println("Seq indexes: " + seqIndexes.toString())
    mapIndexes = null
    var lastChanged = 0
    var start = 0
    for(seq <- seqIndexes){
      if(start < seq._2._1) {
        newContentBuilder.append(noStreamsBuilder.slice(start, seq._2._1))
        start = seq._2._2
      }
      /* mozemo menjati ovaj objekat ukoliko nije unutar prethodno promenjenog objekta */
      if(Random.nextInt(1000)>995 && seq._2._1 > lastChanged){
        var replacement = new StringBuilder();
        seq._1 match {
          case "string" => {
            println("Menjamo string " + string.get.toString())
            replacement.append(generateString(true, 0, false))
          }
          case "number" => {
            replacement.append(generateNumber(!(number.get.toString().contains("."))))
          }
          case "name" => {
            println("Menjamo name " + name.get.toString())
            if (name.get.toString() == "/Length" && Random.nextInt(100) > 95) {
              replacement.append("")
            }
            else
              replacement.append(generateName())
          }
          case "objDict" => {
            if(replObjDict != null)
              replacement.append(replObjDict)
            else {
              replObjDict = objDict.get.toString()
            }
          }
        }
        newContentBuilder.append(replacement.toString())
        lastChanged = seq._2._2
      }
      else {
        newContentBuilder.append(seq._2._3)
      }
    }
*/
  /*  println("Pojavljivanja: ")
    println("Datoteka: " + actorNumber)
    println("Name: \n\t" + name.get + "\n")
    println("String: \n\t" + string.get + "\n")
    println("Number: \n\t" + number.get + "\n")
    println("ObjDict: \n\t" + objDict.get.group(4).toString() + "\n\n")
    */
/*
    if (stringIndex != -1) {
      println("Nadjen je string")
      mapIndexes.put("string", stringIndex)
      var size = stringIndex + string.get.toString().size
      if (size > maxIndex)
        maxIndex = size
    }
    if (numberIndex != -1) {
      println("Nadjen je number")
      mapIndexes.put("number", numberIndex)
      var size = numberIndex + number.get.toString().size
      if (size > maxIndex)
        maxIndex = size
    }

    if (objDictIndex != -1) {
      println("Nadjen je objDict")
      var size = objDictIndex + objDict.get.toString().size
      if (size > maxIndex)
        maxIndex = size
    }
*/
  }
    /* Tuple(objIndex, streamCompleteIndex, nameIndex, stringIndex, numberIndex, objDictIndex) */
    /*    var newDict = new StringBuilder()
    /* dodajemo tekst u newContentStringBuilder dok ima sadrzaja u contentStringBuilder tj. dok je t = true */
    while (!contentStringBuilder.isEmpty) {
      var mapIndexes = scala.collection.mutable.Map[String, Int]()
      var maxIndex = 0

      var obj: Option[Regex.Match] = objectPattern.findFirstMatchIn(contentStringBuilder)
      var objIndex = if (obj != None) obj.map(_.start).get else -1
      if (objIndex != -1) {
        println("Nadjen je objekat")
        var size = objIndex + obj.get.toString().size
        if (size > maxIndex)
          maxIndex = size
      }

      var streamComplete: Option[Regex.Match] = streamCompletePattern.findFirstMatchIn(contentStringBuilder)
      var streamCompleteIndex = if (streamComplete != None) streamComplete.map(_.start).get else -1
      if (streamCompleteIndex != -1) {
        println("Nadjen je stream")
        mapIndexes.put("streamComplete", streamCompleteIndex)
        var size = streamCompleteIndex + streamComplete.get.toString().size
        if (size > maxIndex)
          maxIndex = size
      }

      var name: Option[Regex.Match] = namePattern.findFirstMatchIn(contentStringBuilder)
      var nameIndex = if (name != None) name.map(_.start).get else -1
      if (nameIndex != -1) {
        println("Nadjen je name")
        mapIndexes.put("name", nameIndex)
        var size = nameIndex + name.get.toString().size
        if (size > maxIndex)
          maxIndex = size
      }
      var string: Option[Regex.Match] = stringPattern.findFirstMatchIn(contentStringBuilder)
      var stringIndex = if (string != None) string.map(_.start).get else -1
      if (stringIndex != -1) {
        println("Nadjen je string")
        mapIndexes.put("string", stringIndex)
        var size = stringIndex + string.get.toString().size
        if (size > maxIndex)
          maxIndex = size
      }
      var number: Option[Regex.Match] = numberPattern.findFirstMatchIn(contentStringBuilder)
      var numberIndex = if (number != None) number.map(_.start).get else -1
      if (numberIndex != -1) {
        println("Nadjen je number")
        mapIndexes.put("number", numberIndex)
        var size = numberIndex + number.get.toString().size
        if (size > maxIndex)
          maxIndex = size
      }

      var objDict: Option[Regex.Match] = objDictPattern.findFirstMatchIn(contentStringBuilder)
      var objDictIndex = if (objDict != None) objDict.map(_.start).get else -1
      if (objDictIndex != -1) {
        println("Nadjen je objDict")
        var size = objDictIndex + objDict.get.toString().size
        if (size > maxIndex)
          maxIndex = size
      }

      if(maxIndex == 0){
        println("Na kraju prepisujemo tekst u kome nijedan objekat nije nadjen")
        newContentStringBuilder.append(contentStringBuilder)
        contentStringBuilder.clear()
      }
      else {
        var startPos = -1
        var endPos = -1
        var replacement = new StringBuilder()
        /* u jako malom broju slucajeva brisemo ceo objekat */
        val objModify = Random.nextInt(10000)
        if (objModify > 9996 && objIndex != -1) {
          println("Brisemo objekat: " + obj.get.group(1).toString + " " + obj.get.group(2).toString)
          startPos = objIndex
          endPos = objIndex + obj.get.toString().size
          replacement.append("\n")
        }
        else if (Random.nextInt(1000) > 997 && objDictIndex != -1) {
          /* 1 */
          if (!newDict.isEmpty) {
            println("Menjamo objekat: " + objDict.get.toString() + " objektom: " + newDict)
            startPos = objDictIndex + objDict.get.group(1).toString.size + objDict.get.group(2).toString.size +
              objDict.get.group(3).toString.size
            endPos = startPos + +objDict.get.group(4).toString.size
            replacement.append(newDict)
          }
          newDict.clear()
          newDict.append(objDict.get.group(4).toString)
        }
        /* sve ostalo */
        else if (Random.nextInt(1000) > 997) {
          /* najvece vrednosti su na pocetku -> nama treba prvo pojavljivanje tako da je to na kraju */
          if (!mapIndexes.isEmpty) {
            var s = mapIndexes.last._1
            var i = mapIndexes.last._2

            startPos = i
            s match {
              case "streamComplete" => {
                endPos = startPos + streamComplete.get.toString().size
                replacement.append(changeStream(streamComplete.get.toString()))
                println("Menjamo stream: " + streamComplete.get.toString() + " sa: " + replacement.toString())
              }
              case "string" => {
                var str = string.get.toString()
                endPos = startPos + str.size
                if (str.startsWith("(")) replacement.append(generateString(false, str.size, false))
                else replacement.append(generateString(true, 0, false))
                println("Menjamo string: " + str + " sa: " + replacement.toString())
              }
              case "name" => {
                var str = name.get.toString()
                endPos = startPos + name.size
                if (str == "/Length" && Random.nextInt(100) > 95) {
                  replacement.append("")
                }
                else
                  replacement.append(generateName())
                println("Menjamo name: " + str + " sa: " + replacement.toString())
              }
              case "number" => {
                var str = number.get.toString()
                endPos = startPos + number.size
                replacement.append(generateNumber(!str.contains(".")))
                println("Menjamo number: " + str + " sa: " + replacement.toString())
              }
            } // s match
          }
        }
        if (startPos != -1) {
          println("Menjamo...")
          newContentStringBuilder.append(contentStringBuilder.slice(0, startPos))
          newContentStringBuilder.append(replacement.toString())
          replacement.clear()
          newContentStringBuilder.append(contentStringBuilder.slice(endPos, maxIndex))
          contentStringBuilder.delete(0, maxIndex)
        }
        /* ako nista ne menjamo */
        else {
          newContentStringBuilder.append(contentStringBuilder.slice(0, maxIndex))
          contentStringBuilder.delete(0, maxIndex)
        }
      }*/
    /*
      if (actorNumber == 1) {
        println("Datoteka: " + actorNumber)
        println("Obj: \n\t" + obj.get.group(1).toString() + "\nObj size: \t" + obj.get.toString().size + "\n")
        println("StreamComplete: \n\t" + streamComplete.get + "\n")
        println("Name: \n\t" + name.get + "\n")
        println("String: \n\t" + string.get + "\n")
        println("Number: \n\t" + number.get + "\n")
        println("ObjDict: \n\t" + raw"" + objDict.get.group(4).toString() + "\n\n")
      }
      */

  /* sada sve zapisujem u novi fajl */
  /*    var newContent = new StringBuilder();
    var beginIndex = 0;
    var prevousElement: Tuple3[Int, Int, String] = null;
    var dataChanged = 0;
    var nextStream = 0;
    var streamsChanged = 0;
    for (i <- 0 until indexesSorted.size) {
      //println("i je " + i)
      var element = indexesSorted(i);
      var startElement = element._1;
      var endElement = element._2;
      var replacement = element._3;
      //println("startElement: " + startElement + ", endElement: " + endElement)
      /* onda je u pitanju stream */
      if (replacement == null) {
       // println("replacement je null: " + replacement)
        val currentStream = matchStreams(nextStream).toString()
        //matchStreams.drop(nextStream)
        //println("currentStream: " + currentStream)
        if (Random.nextInt(20) >= 18) {
          replacement = changeStream(currentStream)
          streamsChanged += 1;
          //println("Menjamo stream u:" + replacement)
        }
        else {
          replacement = new String(currentStream);
          dataChanged -= 1
        }
        //println("replacement nakon promene: " + replacement)
        nextStream += 1;
      }
      //      else
      //  println("replacement nije null: " + replacement)
      if (i == 0 || startElement > prevousElement._2) {
        newContent.append(deleteStreams.substring(beginIndex, startElement))
        newContent.append(replacement)
        beginIndex = endElement;
        //println("Element: (" + element._1 + ", " + element._2 + ", " + element._3 + ")")
        prevousElement = element;
        dataChanged += 1;
      }
    }
    newContent.append(deleteStreams.substring(beginIndex))
    deleteStreams = null
    matchStreams.clear()
    matchStreams = null
    //!    println("\t\t a biramo da menjamo: " + streamsChanged + " streamova")
    println("\t ----------- Ukupno promenjeno: " + dataChanged + " podataka od " + indexesSorted.size + " podataka.")

    indexesSorted = null
    checkMemory()
    */


}
  /*
    /* trazimo streamove */
    val matchStreamsNotComplete = streamCompletePattern.findAllMatchIn(deleteStreams).toList // //
    val matchStreamsMap = matchStreamsNotComplete.map(_.start)
    val numberOfStreamsNotComplete = matchStreamsNotComplete.size // //
 //!   println("Ukupno ima: " + numberOfStreamsNotComplete + " streamova, ") //
    for(i <- 0 until numberOfStreamsNotComplete){
      indexes.append(new Tuple3(matchStreamsMap(i), matchStreamsMap(i)+matchStreamsNotComplete(i).toString().size, null))
    }

    //println("Ukupno menjamo: " + indexes.size + " elemenata.")


  }
}
*/