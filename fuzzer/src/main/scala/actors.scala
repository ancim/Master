import akka.actor._
import akka.routing.ActorRefRoutee

import scala.concurrent.{Await, Future}
import scala.sys.process._
import java.io._

import scala.concurrent.duration._
import akka.actor.Timers

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}
import java.nio.channels.FileChannel
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file._
import java.util.Locale
import java.io.IOException
import java.nio.file.DirectoryStream
import java.nio.file.Files
import java.nio.file.Paths

import akka.dispatch.Dispatchers

import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

/* kontrolise sve aktore koji generisu i pokrecu pdf datoteke */
class Controler extends Actor {
  import actors._
  /* broj zavrsenih PDF citanja i broj Actor-a */
  var numberOfTerminations = 0;
  val numberOfActors = numberOfWorkers;
  var numberOdPDFsTested = 0;
  var numberOfErrors = 0;
  val workers: Array[ActorRef] = new Array[ActorRef](numberOfActors);

  def start() = {
    import better.files._
    import better.files.File._
    println("Pokrecem Worker-e");
    for(i <- 0 until numberOfActors) {
      /* pravimo Actor-a i saljemo mu poruku za pokretanje */
      workers(i) = context.actorOf(Props(new Worker(i, numberOfActors)), name = "myWorker-" + i);
      context.watch(workers(i));
      workers(i) ! "Pokreni";
    }// end for
  }

  def writeTestResults(msg: String): Unit ={
    var fileName = "./results/allTestResults.txt";
    //      println("Filename: " + fileName)
    /* pravimo datoteku za ispis ako vec ne postoji */
    if (!Files.exists(Paths.get(fileName))) {
      val file = new File(fileName)
      file.createNewFile();
    }
    var info = new StringBuilder("\n--------------------------------------------------\n");
    reader match {
      case 1 => info.append("\tTestiran citac: Foxit PDF Reader\n");
      case 2 => info.append("\tTestiran citac: Slim PDF Reader\n");
      case 3 => info.append("\tTestiran citac: Sumatra PDF Reader\n");
      case _ => info.append("\tTestiran citac na putanji: " + path + "\n");
    }
    info.append("\t"+msg)
    info.append("\n--------------------------------------------------\n");
    Files.write(Paths.get(fileName), info.toString().getBytes(StandardCharsets.UTF_8), StandardOpenOption.APPEND)
  }

  def quit(): Unit ={
    /* Upisujemo podatke o testiranju */
    val msg = "............... Ukupno je testirano: " + numberOdPDFsTested + " datoteka, a pronadjeno je: " + numberOfErrors + " gresaka. ................";
    //writeTestResults(msg);
    println(msg)
    println("............... Gasim sistem ................")
    context.system.terminate();
  }

  def receive = {
    case "Pokrenuto" => println("Pokrenuto: " + context.sender().path.name);
    case "Pocni!" => println("\nMain kaze pocni!"); start();
    case "Testirano" => {
      numberOdPDFsTested+=1
      if(numberOdPDFsTested % 10 == 0)
        println("Za sada je testirano " + numberOdPDFsTested + " datoteka i pronadjeno " + numberOfErrors + " gresaka.")
    };
    case "Greska" => {
      numberOfErrors+=1
      numberOdPDFsTested+=1;
      if(numberOdPDFsTested % 10 == 0)
        println("Za sada je testirano " + numberOdPDFsTested + " datoteka i pronadjeno " + numberOfErrors + " gresaka.")
    };
      case Terminated(x) => {
      println(x + " terminated")
      numberOfTerminations+=1;
      if(numberOfTerminations == numberOfActors) {
        quit()
      }
    };
    case x => println("Posiljaoc " + context.sender().toString() + " kaze " + x);
  } // end receive blok

}//end Class Controler

/* pokrece pdf */
class Worker(number: Int, numberOfActors: Int) extends Actor with Timers {
  import actors._
  /* podaci koji se dobijaju pokretanjem PDF citaca metodom run */
  /* proces koji predstavlja pokrenut PDF citac, bice ugasen nakon 5 sekundi od pokretanja */
  var PDFprocess : Process = null;
  var out = scala.collection.mutable.ArrayBuffer.empty[String];
  var err = scala.collection.mutable.ArrayBuffer.empty[String];
  var exitCode: Int = 0;
  var destroyedManually: Boolean = true;
  var numberOfIterations = 1;
  var numberComplete = number;

  def getNumber() = numberComplete

  def receive = {
    case "Pokreni" => {
      System.gc()
        /*if (readMemory() == false) {
//          println(".............. Cekam 2 sekunde ..............")
          timers.startSingleTimer("key", "Pokreni", 0 milliseconds); // !!!!!!!!!!!! bilo 2000
        }*/
      if(false){}
        else {
          try {
          /* pravimo datoteku */
          val fileNumber = numberComplete % numberOfPdfs;
          val fileName = Files.list(Paths.get("./PDFcorpus/")).filter(p => {
            p.getFileName.toString.startsWith(fileNumber + "___")
          }).findFirst().get().toString
          //val fileName: String = "./PDFcorpus/" + fileNumber + ".pdf";
          val PDFparser = new ParserPDF(fileName, numberComplete);
          PDFparser.readFileBinary()
          //PDFparser = null

          //!  println("Actor broj " + numberComplete + " je izmenio svoj fajl i sada ga pokrece.")
          val readerPart = new StringBuilder();
          /* ako se program izvrsava na Linux operativnom sistemu, pre komande citaca ide komanda 'wine' */
          if(OS == "Linux") {
            //println("Linux operativni sistem")
            val listOfExecutablesWindows = Array(".bat", ".exe", ".bin", ".cmd", ".com", ".cpl", ".gadget", ".inf", ".ins", ".inx", ".inx", ".isu", ".job", ".jse",
              ".lnk", ".msc", ".msi", ".msp", ".mst", ".paf", ".pif", ".ps1", ".reg", ".rgs", ".scr", ".sct", ".shb", ".shs", ".u3p", ".vb",
              ".vbe", ".vbs", ".vbscript", ".ws", ".wsf", ".wsh")
            var b = false
            listOfExecutablesWindows.foreach(ext => if(path.endsWith(ext)) b=true)
            if(b || path == "")
              readerPart.append("wine ")
          }
          if (path == "") {
            readerPart.append("./PDF_citaci/")
            reader match {
              case 1 => readerPart.append("Foxit/FoxitReader.exe ");
              case 2 => readerPart.append("slim/SlimPDFReader.exe ");
              case 3 => readerPart.append("sumatra/SumatraPDF.exe ");
              //case 4 => readerPart.append("visagesoft/vspdfreader.exe ");
              //case 5 => readerPart.append("evince/Evince.lnk ")
            }
          }
          else {
            readerPart.append(path)
            readerPart.append(" ")
          }
          val command: String = readerPart.toString + "./fuzzedPDFcorpus/" + numberComplete + "_fuzzed.pdf";
          //val command: String = "cmd java -jar ./JavaZaProbu.jar"
          println("Komanda " + command)
          /* za "milisecs" milisekundi saljemo poruku stop sami sebi */
          timers.startSingleTimer("key", "stop", millisecs milliseconds);

          val qb: ProcessBuilder = Process(command) // scala.sys.process.ProcessBuilder
          PDFprocess = qb run (ProcessLogger((s) => {
            /*println("PL: " + s); */ out.append(s)
          },
            (s) => {
              err.append(s); /*println("ER: " + s + "     duzina err: " + err.length); */
            }));
        }
        catch {
          case x => println("-------------- Exception " + x.getMessage + " caught: ---------------\n" + x.printStackTrace() + "\n");
            self ! "stop"; //
        }
        finally {
        }
      }
    }
    case "stop" => {
      //exitCode = PDFprocess.exitValue();
      println("Gasim proces: " + context.self.path.name)
      //        "powershell taskkill /F /IM FoxitReader.exe".!
      /* ako je uhvacen neki izuzetak, onda je PDFprocess = null */
      if(PDFprocess == null){
        println("Program " + numberComplete + " je zavrsio jer je uhvacen izuzetak.")
      }
      else {
        /* pri normalnom izvrsavanju programa, proces je i dalje ziv jer se nije sam ugasio zbog greske */
        if (PDFprocess.isAlive()) {
          println("Proces " + numberComplete + " je ziv!")
          context.parent ! "Testirano";
          //!   println("Program " + numberComplete + " je zavrsio sa unistavanjem tj. ugasen je rucno!");
          PDFprocess.destroy();
          Thread.sleep(500)
          exitCode = PDFprocess.exitValue()
          println("Povratna vrednost je: " + exitCode)
          writeToFile(numberComplete, out, err, exitCode, destroyedManually);
         /* if(PDFprocess.isAlive())
            println("Proces " + numberComplete + " je ziv nakon ubijanja!")*/
        }
        /* ako se ugasio sam, to treba zabeleziti */
        else {
          println("Proces " + numberComplete + " nije ziv!")
          destroyedManually = false;
          context.parent ! "Greska";
          //!  println("********** Program " + numberComplete + " je zavrsio bez unistavanja tj. sam se ugasio!")

          exitCode = PDFprocess.exitValue()
          /* upisivanje rezultata u datoteku */
          println("Proces " + numberComplete + " ima rezultate: " + exitCode + "\nOut: \n" + out + "\nErr: \n" + err + "\n")
          Thread.sleep(500)
          if (out == null || err == null || PDFprocess == null) {
            writeToFileNullValues(numberComplete, out, err, PDFprocess, destroyedManually);
          }
          else
            writeToFile(numberComplete, out, err, exitCode, destroyedManually);

        }

        PDFprocess = null
      }
      out.clear()
      err.clear()

      /* pokrecemo novu iteraciju */
      if(numberOfIterations == iterations)
        context.stop(self);
      else{
        numberOfIterations+=1;
        //println("Povecali smo broj iteracija: " + numberOfIterations);
        numberComplete = numberComplete+numberOfActors;
        destroyedManually = true;
        self ! "Pokreni";
      }
    };
    case x => println("Sta kazes? " + x);
  }//end receive
}

object actors {

  var reader: Int = -1;
  var iterations: Int = -1;
  var numberOfWorkers = 0;
  var millisecs = 0;
  var path = ""
  var numberOfPdfs = {
    var i = 0
    /* pre nego sto vratimo broj datoteka, numerisemo ih od 0-... tako da im nazivi budu 0.pdf, 1.pdf, itd... */
    try {
      val tempDir: Path = Files.createTempDirectory(Paths.get("."), "temp")
      val PDFcorpus: Path = Paths.get("./PDFcorpus/")
      println("Preimenovane datoteke:")
      Files.list(PDFcorpus).forEach(file => {
        var fileName = file.getFileName.toString
        //println("Putanja: " + file + ", ime datoteke: " + fileName)
        if(fileName.matches("[0-9]+___(.)*")){
          fileName = fileName.substring(fileName.indexOf("___")+3)
        }
        val newFile = Files.move(file, tempDir.resolve("" + i + "___" + fileName), StandardCopyOption.REPLACE_EXISTING)
        println("Datoteka: " + file.toString.substring(file.toString.lastIndexOf("\\")+1) + " -> " + newFile.toString.substring(newFile.toString.lastIndexOf("\\")+1))
        i += 1
      })

      Files.list(tempDir).forEach(file => {
        val newFile = Files.move(file, PDFcorpus.resolve(file.getFileName), StandardCopyOption.REPLACE_EXISTING)
        //println("Datoteka: " + file + " -> " + newFile)
      })
      Files.delete(tempDir)
    }
    catch {
      case x: AccessDeniedException => println("Nije uspelo kreiranje direktorijuma! " + x.getLocalizedMessage)
      case ex: IOException => ex.printStackTrace()
      case _ => println("Nije uspelo kreiranje direktorijuma! ")
    }
    finally {

    }
    println("Datoteka ima: " + i)
  i
  }

  val OS = {
    val os = System.getProperty("os.name").toLowerCase(Locale.ENGLISH)
    if (os.indexOf("win") >= 0)
      "Windows"
    else if (os.indexOf("nux") >= 0)
      "Linux"
    else "Neither"
  }

  def pdfsSize: Long = {
    var sumOfSizes: Long = 0
    Files.list(Paths.get("./PDFcorpus/")).forEach(path => {
      import java.nio.channels.FileChannel
      val fileChannel = FileChannel.open(path)
      val fileSize = fileChannel.size
      //println("Velicina datoteke " + path + " u bajtovima je: " + fileSize)
      sumOfSizes += fileSize
      fileChannel.close()
    })
    sumOfSizes
  }

  def getMaxWorkers(): Int = {
    val maxMemory = Runtime.getRuntime().maxMemory()
    val totalMemory = Runtime.getRuntime().totalMemory()
    val freeMemory = Runtime.getRuntime().freeMemory()
    val allocatedMemory = totalMemory - freeMemory
    var presumableFreeMemory = maxMemory - allocatedMemory

    val numberOfCores = Runtime.getRuntime.availableProcessors()
    /* npr. maksimalni broj Worker objekata moze da bude za sada (pre analize memorije) 3*numberOfCores */
    var maxWorkers = 3*numberOfCores
    /* sada treba da vidimo da li ima dovoljno memorije za ovoliko Worker objekata */

    var sizes = scala.collection.mutable.ArrayBuffer.empty[Long]

    Files.list(Paths.get("./PDFcorpus/")).forEach(path => {
      import java.nio.channels.FileChannel
      val fileChannel = FileChannel.open(path)
      val fileSize = fileChannel.size
      //println("Velicina datoteke " + path + " u bajtovima je: " + fileSize)
      sizes.append(fileSize)
      fileChannel.close()
    })

    sizes = sizes.sortWith(_<_)
    //println("Sorted sizes: " + sizes.toString())
    var sum = sizes.sum
    //println("Sum before: " + sum)

    /* ukoliko ima vise datoteka od pocetnog maksimalnog broja Worker-a, treba izbaciti njihove velicine iz sume */
    if(sizes.size > numberOfCores*3){
      //println("Ima vise datoteka od procesora")
      val diff: Int = sizes.size - numberOfCores*3
      sizes = sizes.drop(diff)
    }
    /* ukoliko ima manje datoteka od pocetnog maksimalnog broj Worker-a, pocetni maks. broj Worker-a se menja na broj datoteka */
    else if(sizes.size < numberOfCores*3) {
      maxWorkers = sizes.size
      //println("Ima manje datoteka nego numberOfCores*3: " + maxWorkers + " < " + numberOfCores*3)
    }

    sum = sizes.sum
    //println("Sum after : " + sum)
    //println("Sorted sizes: " + sizes.toString())
    while(sum*2 >= presumableFreeMemory && !sizes.isEmpty){
      println(sum*2 + " < " + presumableFreeMemory + " => smanjujemo za 1")
      sizes = sizes.tail
      //println("Sorted sizes: " + sizes.toString())
      maxWorkers -= 1
      sum = sizes.sum
    }
    if(sizes.isEmpty){
      println("Prazna lista: nijedna datoteka ne moze stati u memoriju!")
      return 0
    }

    //println("Maksimalni broj Worker objekata je: " + maxWorkers)
    maxWorkers
  }

  /* svi podaci o izvrsavanju programa se upisuju u datoteku odgovarajuceg naziva */
  def writeToFile(number: Int, out: ArrayBuffer[String], err: ArrayBuffer[String], exitCode : Int, destroyedManually : Boolean) {
    /* naziv datoteke se pravi na osnovu broja Actor-a, i na osnovu toga da li je doslo do greske (postoje podaci u stderr) */
    /* ovo dodajem da ne bi ispisivao rezultate procesa koji nisu uzrokovali gresku pdf citaca */
    val filePath = Paths.get("./fuzzedPDFcorpus/" + number + "_fuzzed.pdf");
     //val path = Paths.get("./prepisivanje.pdf");
    if(destroyedManually /*&& false*/) { //
      //!  println("------------------ destroyedManually " + destroyedManually)
      /* posto nije doslo do greske, brisemo izmenjeni pdf iz foldera fuzzedPDFcorpus */
      import java.io.IOException
      import java.nio.file.DirectoryNotEmptyException
      import java.nio.file.Files
      import java.nio.file.NoSuchFileException
      var succeded = true;
      try {
        succeded = true
        println("Brisem: " + filePath)
        Thread.sleep(500)
        Files.delete(filePath)
      }
      catch {
        case x: NoSuchFileException =>
          System.err.format("%s: no such" + " file or directory%n", filePath)
        case x: DirectoryNotEmptyException =>
          System.err.format("%s not empty%n", filePath)
        case x: IOException =>
          // File permission problems are caught here.
          System.err.println("Ovde:\n\n\n " + x + "   " + x.getLocalizedMessage + "   " + x.getMessage + " \n\n\n")
      }
      finally {
          return;
      }
    }
    //! println("------------------ !destroyedManually: " + destroyedManually)
    /* upisujemo datoteku koja je uzrokovala gresku u direktorijum fuzzedPDFError */
    var fileError = "./fuzzedPDFError/" + number + "_fuzzed.pdf"
    /*  if (!Files.exists(Paths.get(fileError))) {
        val file = new File(fileError)
        file.createNewFile();
      }
      else
        FileChannel.open(Paths.get(fileError), StandardOpenOption.WRITE).truncate(0).close()
    */
    val to = Paths.get(fileError)
    if(Files.exists(to))
      Files.delete(to)
    Files.move(filePath, to);

    var fileName = "./results/";
    if (!err.isEmpty)
      fileName = fileName + "ERR_";
    /* ako se program ugasio sam a nije sacekao da ga unisti glavni program */
    if(!destroyedManually)
      fileName = fileName + "CRASH_";
    fileName = fileName + number + ".txt";
    //println("Filename: " + fileName)
    /* pravimo datoteku za ispis ako vec ne postoji, ako postoji, onda je cistimo da se podaci ne bi nadovezivali */
    if (!Files.exists(Paths.get(fileName))) {
      val file = new File(fileName)
      file.createNewFile();
    }
    else
      FileChannel.open(Paths.get(fileName), StandardOpenOption.WRITE).truncate(0).close()

    /* STDOUT deo */
    Files.write(Paths.get(fileName), "STDOUT:\n\n".getBytes(StandardCharsets.UTF_8), StandardOpenOption.APPEND)
    for (elem <- out) {
      var elem2 = elem + "\n";
      Files.write(Paths.get(fileName), elem2.getBytes(StandardCharsets.UTF_8), StandardOpenOption.APPEND)
    }
    /* STDERR deo */
    Files.write(Paths.get(fileName), "\n\nSTDERR:\n\n".getBytes(StandardCharsets.UTF_8), StandardOpenOption.APPEND)
    for (elem <- err) {
      var elem2 = elem + "\n";
      Files.write(Paths.get(fileName), elem2.getBytes(StandardCharsets.UTF_8), StandardOpenOption.APPEND)
    }
    /* Povratna vrednost se pise na kraju */
    var exitString : String = "\n\nEXIT CODE:\n" + exitCode + "\n";
    Files.write(Paths.get(fileName), exitString.getBytes(StandardCharsets.UTF_8), StandardOpenOption.APPEND)
  }

  /* ako je neki od vracenih podataka null */
  def writeToFileNullValues(number: Int, out: ArrayBuffer[String], err: ArrayBuffer[String], PDFProcess : Process, destroyedManually : Boolean) {
    var fileName = "./results/";
    fileName = fileName + "NULL_";
    if(!destroyedManually)
      fileName = fileName + "CRASH_";
    fileName = fileName + number + ".txt";
    //println("Filename: " + fileName)
    /* pravimo datoteku za ispis ako vec ne postoji, ako postoji, onda je cistimo da se podaci ne bi nadovezivali */
    if (!Files.exists(Paths.get(fileName))) {
      val file = new File(fileName)
      file.createNewFile();
    }
    else
      FileChannel.open(Paths.get(fileName), StandardOpenOption.WRITE).truncate(0).close()

    /* STDOUT deo */
    Files.write(Paths.get(fileName), "STDOUT:\n\n".getBytes(StandardCharsets.UTF_8), StandardOpenOption.APPEND)
    if (out != null) {
      for (elem <- out) {
        var elem2 = elem + "\n";
        Files.write(Paths.get(fileName), elem2.getBytes(StandardCharsets.UTF_8), StandardOpenOption.APPEND)
      }
    }
    else
      Files.write(Paths.get(fileName), "NULL".getBytes(StandardCharsets.UTF_8), StandardOpenOption.APPEND)

    /* STDERR deo */
    Files.write(Paths.get(fileName), "\n\nSTDERR:\n\n".getBytes(StandardCharsets.UTF_8), StandardOpenOption.APPEND)
    if(err != null) {
      for (elem <- err) {
        var elem2 = elem + "\n";
        Files.write(Paths.get(fileName), elem2.getBytes(StandardCharsets.UTF_8), StandardOpenOption.APPEND)
      }
    }
    else
      Files.write(Paths.get(fileName), "NULL".getBytes(StandardCharsets.UTF_8), StandardOpenOption.APPEND)

    /* Povratna vrednost se pise na kraju */
    Files.write(Paths.get(fileName), "\n\nEXIT CODE:\n\n".getBytes(StandardCharsets.UTF_8), StandardOpenOption.APPEND)
    if(PDFProcess != null) {
      val exitCode = PDFProcess.exitValue();
      val exitString : String = exitCode + "\n";
      Files.write(Paths.get(fileName), exitString.getBytes(StandardCharsets.UTF_8), StandardOpenOption.APPEND)
    }
    else
      Files.write(Paths.get(fileName), "Cannot get EXIT CODE because the process is NULL".getBytes(StandardCharsets.UTF_8), StandardOpenOption.APPEND)

  }

  /* u slucaju da je run metod vratio null, onda samo ispisujemo poruku u datoteku */
  def writeToFile(number: Int, msg: String) {
    var fileName = "./results/";
    fileName = fileName + "NULL_";
    fileName = fileName + number + ".txt";
    //println("Filename: " + fileName)
    /* pravimo datoteku za ispis ako vec ne postoji, ako postoji, onda je cistimo da se podaci ne bi nadovezivali */
    if (!Files.exists(Paths.get(fileName))) {
      val file = new File(fileName)
      file.createNewFile();
    }
    else
      FileChannel.open(Paths.get(fileName), StandardOpenOption.WRITE).truncate(0).close()

    Files.write(Paths.get(fileName), msg.getBytes(StandardCharsets.UTF_8), StandardOpenOption.APPEND)
  }

  def readMemory(): Boolean = {
    println("\n---------------- Memorija --------------------")
    val maxMemory = Runtime.getRuntime().maxMemory()
    val totalMemory = Runtime.getRuntime().totalMemory()
    val freeMemory = Runtime.getRuntime().freeMemory()
    val allocatedMemory = totalMemory - freeMemory
    var presumableFreeMemory = maxMemory - allocatedMemory
    println("maxMemory:                     " + maxMemory)
    println("totalMemory:                    " + totalMemory)
    println("freeMemory:                      " + freeMemory)
    println("allocatedMemory:                 " + allocatedMemory)
    println("presumableFreeMemory:          " + presumableFreeMemory)
    println("Velicina svih datoteka:         147615280" + "\n")
    if(presumableFreeMemory < maxMemory/2)
      false
    else true
   /* if(presumableFreeMemory < maxMemory/5){
      println("Cekam")
      Thread.sleep(3000)
      println("Gotovo cekanje")
    }*/

    //println("Kolicina memorije na koju mozemo da racunamo pre izbacivanja izuzetka je: " + presumableFreeMemory + " b")
  }

  def main(args: Array[String]): Unit = {
    /* proveravamo argumente komandne linije */
    if(!args.isEmpty){
      if(args.length != 1) println("Dozvoljen je najvise jedan argument komandne linije.")
      else {
        val arg = args.head
        if(arg != "--help") {
          println("Pogresno ste uneli argument.")
        }
        else {
          println("Birate uputstvo")
          val lines = Files.readAllLines(Paths.get("help.txt"), Charset.forName("UTF-8")).toArray()
          for(line <- lines)
            println(line)
          }
      }
    }

    if(OS == "Neither"){
      println("Program nije prilagodjen Vasem operativnom sistemu!")
      return
    }
    println("Operativni sistem je: " + OS)
    println("Broj jezgara procesora: " + Runtime.getRuntime.availableProcessors())
    println("Datoteke ukupno zauzimaju: " + pdfsSize + "b")
    println("Broj datoteka u korpusu je: " + numberOfPdfs)
    try {
      /* Korisnik bira PDF citac koji ce biti testiran */
      import scala.io.StdIn
      var chosenReader = false;
      while (!chosenReader) {
        println("--------------------------------------------------------------------------------\nOdaberite citac datoteka formata PDF - za odabir unesite broj pod kojim je naveden citac ili putanju do izvrsne datoteke nekog drugog citaca:")
        println("\t\t1) Foxit PDF Reader")
        println("\t\t2) Slim PDF Reader")
        println("\t\t3) Sumatra PDF Reader")
        //        println("\t\t4) Expert PDF Reader")
        //        println("\t\t4) Evince PDF Reader")
        chosenReader = true;
        val input = StdIn.readLine();
        input match {
          case "1" => { println("\t\tOdabrali ste Foxit PDF Reader"); reader = 1 }
          case "2" => { println("\t\tOdabrali ste Slim PDF Reader"); reader = 2 }
          case "3" => { println("\t\tOdabrali ste Sumatra PDF Reader"); reader = 3 }
          //case "4" => { println("Odabrali ste Expert PDF Reader"); reader = 4 }
          //case "5" => { println("Odabrali ste Evince PDF Reader"); reader = 5 }
          case x => {
            //println("Putanja: " + x)
            if(Files.exists(Paths.get(x))) {
              //println("Putanja je validna")
              val listOfExecutables = Array(".bat", ".exe", ".bin", ".cmd", ".com", ".cpl", ".csh", ".gadget", ".inf", ".ins", ".inx", ".inx", ".isu", ".job", ".jse",
                ".ksh", ".lnk", ".msc", ".msi", ".msp", ".mst", ".out", ".paf", ".pif", ".ps1", ".reg", ".rgs", ".run", ".scr", ".sct", ".shb", ".shs", ".u3p", ".vb",
                ".vbe", ".vbs", ".vbscript", ".ws", ".wsf", ".wsh")
              var b = false
              listOfExecutables.foreach(ext => if(x.endsWith(ext)) b=true)
              if(!b) {
                println("\t\tProgram nije prepoznao da je u pitanju izvrsna datoteka. Ukoliko ste sigurni da ste uneli izvrsnu datoteku, nastavite sa radom.")
              }
              path = x;
           }
            else {
              println("\t\tNiste uneli validnu putanju, pokusajte ponovo:");
              chosenReader = false;
            }
          }
        }
      }
      /* ....................... broj iteracija ..................... */
      println("Unesite broj iteracija ili -1 za izvrsavanje programa dok ga rucno ne zaustavite:")
      var numberIter = -2;
      while(numberIter == -2) {
        val numIter = StdIn.readLine()
        if (numIter.matches("""[+-]?\d{1,9}""")){
          numberIter = numIter.toInt;
          numberIter match {
            case -1 => { println("\t\tOdabrali ste da sami zaustavite program") }
            case x if x <= 0 => {println("\t\tPogresno ste uneli; pokusajte ponovo:"); numberIter = -2; }
            case x => { println("\t\tOdabrali ste " + x + " iteracija"); iterations = x; }
          }
        }
        else {
          println("\t\tPogresno ste uneli; pokusajte ponovo:")
        }
      }

      /* ........................ broj worker-a .......................... */
      var maxWorkers = getMaxWorkers()
      println("Unesite broj Izvrsilaca: minimalan broj je 1 a maksimalan broj je "+maxWorkers)
      while(numberOfWorkers == 0){
        var in = StdIn.readLine()
        if(in.matches("""\d{1,9}""")){
          numberOfWorkers = in.toInt
          numberOfWorkers match {
            case x if x>maxWorkers => { println("\t\tUneli ste preveliki broj; pokusajte ponovo:"); numberOfWorkers=0; }
            case x if x<1 => { println("\t\tUneli ste premali broj; pokusajte ponovo:"); numberOfWorkers=0; }
            case x => { println("\t\tOdabrali ste " + x + " Worker objekata") }
          }
        }
        else
          println("\t\tPogresno ste uneli; pokusajte ponovo:")
      }

      /* ........................ broj sekundi .......................... */
      println("Unesite dozvoljen milisekundi za otvaranje jedne datoteke: broj > 0")
      while(millisecs == 0){
        var in = StdIn.readLine()
        if(in.matches("""\d{1,9}""")){
          millisecs = in.toInt
          millisecs match {
            case 0 => { println("\t\tBroj mora biti veci od 0; pokusajte ponovo:"); }
            case x => { println("\t\tOdabrali ste " + x + " milisekundi") }
          }
        }
        else
          println("\t\tPogresno ste uneli; pokusajte ponovo:")
      }
      println("--------------------------------------------------------------------------------");
    }
    catch {
      case x:Exception => println("Uhvacen izuzetak: " + x.getMessage)
    }

    val system = ActorSystem("fuzzerSystem");
    val a = system.actorOf(Props[Controler], name="fuzzer-controler-actor")
    a ! "Pocni!";

    /*
    var input = ""
    while (input != "q") {
      print("Unesite q da biste prekinuli program:")
      input = StdIn.readLine()
      a ! input
    }
*/
    //println("Isprobavanje parsera")
    //val fileName: String = "./survey_on_parallel_computing_and_its_applications_in_dataparallel_problems_using_gpu_architectures.pdf";
    //val fileName: String = "./helloToParseAndChange.pdf";
    //val fileName: String = "./PDFcorpus/10.pdf";
    //val PDFparser = new ParserPDF(fileName, 10);
    // PDFparser.readLines()
    //PDFparser.readFileAsString()

    // println(PDFparser.getAPiece(1293048) + " " + PDFparser.getAPiece(0) + " " + PDFparser.getAPiece(12000) + " " + PDFparser.getAPiece(5435)+ " " + PDFparser.getAPiece(483859673))
    //  PDFparser.readFileBinary()


    /*   var sb = new StringBuilder("a abc fja\n\nrgm aslm 45 abc ab c ");
       println("SB pre: " + sb.toString())
       PDFparser.replaceAllSB(sb, "abc", "CDEFG", 5)
       println("SB posle: " + sb.toString())
      */

    //PDFparser.probam()


    //println(Double.MinValue.toString)
    //    PDFparser.getHeuristicString("./heuristics/strings/traversals.txt")
    /* Ne koristiti CMD jer on ne vraca rezultat!!!!!!!!!!!!!!! */
    // Executes "ls" and sends output to stdout
    /*
    "powershell \"scala primer.scala\"".!

    */
  }
}

/************************
  *  do_something 2>&1 | tee -a some_file
      Ovo stvarno radi!
This is going to redirect stderr to stdout and stdout to some_file and print it to stdout. */