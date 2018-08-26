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
import java.nio.charset.StandardCharsets
import java.nio.file.StandardOpenOption
import java.nio.file.{Files, Paths}

import scala.io.StdIn

object actors {

  var reader: Int = -1;
  var iterations: Int = -1;
/* svi podaci o izvrsavanju programa se upisuju u datoteku odgovarajuceg naziva */
def writeToFile(number: Int, out: List[String], err: List[String], exitCode : Int, destroyedManually : Boolean) {
  /* naziv datoteke se pravi na osnovu broja Actor-a, i na osnovu toga da li je doslo do greske (postoje podaci u stderr) */
  /* ovo dodajem da ne bi ispisivao rezultate procesa koji nisu uzrokovali gresku pdf citaca */

  val path = Paths.get(".\\fuzzedPDFcorpus\\" + number + "_fuzzed.pdf");
  if(destroyedManually){
  //!  println("------------------ destroyedManually " + destroyedManually)
    /* posto nije doslo do greske, brisemo izmenjeni pdf iz foldera fuzzedPDFcorpus */
    import java.io.IOException
    import java.nio.file.DirectoryNotEmptyException
    import java.nio.file.Files
    import java.nio.file.NoSuchFileException
    try
      Files.delete(path)
    catch {
      case x: NoSuchFileException =>
        System.err.format("%s: no such" + " file or directory%n", path)
      case x: DirectoryNotEmptyException =>
        System.err.format("%s not empty%n", path)
      case x: IOException =>
        // File permission problems are caught here.
        System.err.println(x)
    }
    finally {
      return;
    }
  }
 //! println("------------------ !destroyedManually: " + destroyedManually)
  /* upisujemo datoteku koja je uzrokovala gresku u direktorijum fuzzedPDFError */
  var fileError = ".\\fuzzedPDFError\\" + number + "_fuzzed.pdf"
/*  if (!Files.exists(Paths.get(fileError))) {
    val file = new File(fileError)
    file.createNewFile();
  }
  else
    FileChannel.open(Paths.get(fileError), StandardOpenOption.WRITE).truncate(0).close()
*/
  val to = Paths.get(fileError)
  Files.copy(path, to);

  var fileName = ".\\results\\";
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
  def writeToFileNullValues(number: Int, out: List[String], err: List[String], PDFProcess : Process, destroyedManually : Boolean) {
    var fileName = ".\\results\\";
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
    var fileName = ".\\results\\";
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

  /* izvrsavanje komande date stringom */
  def run(in: String): (List[String], List[String], scala.sys.process.Process) = {
    val qb = Process(in) // scala.sys.process.ProcessBuilder
    var out = List[String]()
    var err = List[String]()

    //val PDFprocess: Process = Process("powershell scala .\\primer.scala") run (ProcessLogger((s) => { println("PL: " + s); },/*out ::= s */
     //                                                                   (s) => { err ::= s; println("ER: " + s + "     duzina err: " + err.length);  }));
    val PDFprocess: Process = qb run (ProcessLogger((s) => { /*println("PL: " + s);*/ out ::= s},
                                                    (s) => { err ::= s; /*println("ER: " + s + "     duzina err: " + err.length); */ }));

    /* bez ovog poziva, run se odmah vraca i vraca prazne rezultate! Ovako cekamo kraj izvrsavanja da se dobije povratna vrednost */
    val exit: Int = PDFprocess.exitValue();
    //println("Duzina err: " + err.length)
    (out.reverse, err.reverse, PDFprocess)
  }

  /* kontrolise sve aktore koji generisu pokrecu pdf datoteke */
  class Controler extends Actor {

    /* broj zavrsenih PDF citanja i broj Actor-a */
    var numberOfTerminations = 0;
    val numberOfActors = 5;
    var numberOdPDFsTested = 0;
    var numberOfErrors = 0;
    val workers: Array[ActorRef] = new Array[ActorRef](numberOfActors);

    def pokreni() = {
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
      var fileName = ".\\results\\allTestResults.txt";
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
        case _ => info.append("\tTestiran nepoznati citac\n");
      }
      info.append("\t"+msg)
      info.append("\n--------------------------------------------------\n");
      Files.write(Paths.get(fileName), info.toString().getBytes(StandardCharsets.UTF_8), StandardOpenOption.APPEND)
    }

    def quit(): Unit ={
      /* Upisujemo podatke o testiranju */
      val msg = "............... Ukupno je testirano je: " + numberOdPDFsTested + " datoteka, a pronadjeno je: " + numberOfErrors + " gresaka. ................";
      writeTestResults(msg);
      println(msg)
      println("............... Gasim sistem ................")
      context.system.terminate();
    }

    def receive = {
      case "Pokrenuto" => println("Pokrenuto: " + context.sender().path.name);
      case "Pocni!" => println("\nMain kaze pocni!"); pokreni();
      case "Testirano" => numberOdPDFsTested+=1;
      case "Greska" => numberOfErrors+=1; numberOdPDFsTested+=1;
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
    /* podaci koji se dobijaju pokretanjem PDF citaca metodom run */
    /* proces koji predstavlja pokrenut PDF citac, bice ugasen nakon 5 sekundi od pokretanja */
    var PDFprocess : Process = null;
    var out : List[String] = List[String]();
    var err : List[String] = List[String]();
    var exitCodeFuture : Unit = null;
    var exitCode: Int = 0;
    var tuple : Tuple3[List[String], List[String], Process] = null;
    var destroyedManually: Boolean = true;
    var numberOfIterations = 1;
    var numberComplete = number;

    def receive = {
      case "Pokreni" => {
        try {
         /* pravimo datoteku */
          val fileNumber = numberComplete % 20;
          val fileName: String = ".\\PDFcorpus\\" + fileNumber + ".pdf";
          val PDFparser = new ParserPDF(fileName, numberComplete);
          PDFparser.readFileBinary()
        //!  println("Actor broj " + numberComplete + " je izmenio svoj fajl i sada ga pokrece.")

          val readerPart = new StringBuilder();
          readerPart.append(".\\PDF_citaci\\")
          reader match {
            case 1 => readerPart.append("Foxit\\FoxitReader.exe ");
            case 2 => readerPart.append("slim\\SlimPDFReader.exe ");
            case 3 => readerPart.append("sumatra\\SumatraPDF.exe ");
            case 4 => readerPart.append("visagesoft\\vspdfreader.exe ");
            case 5 => readerPart.append("evince\\Evince.lnk ")
          }

          val command = readerPart.toString + ".\\fuzzedPDFcorpus\\" + numberComplete + "_fuzzed.pdf";
          /* za 8 sekundi saljemo poruku stop sami sebi */
          timers.startSingleTimer("key", "stop", 5000 milliseconds);

          val qb: ProcessBuilder = Process(command) // scala.sys.process.ProcessBuilder
          PDFprocess = qb run (ProcessLogger((s) => {/*println("PL: " + s); */out ::= s},
            (s) => { err ::= s; /*println("ER: " + s + "     duzina err: " + err.length); */ }));
          }
        catch {
          case x => println("-------------- Exception " + x.getMessage + " caught: ---------------\n" + x.printStackTrace() + "\n");
            self ! "stop";
        }
        finally {
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
            context.parent ! "Testirano";
         //!   println("Program " + numberComplete + " je zavrsio sa unistavanjem tj. ugasen je rucno!");
            PDFprocess.destroy();
          }
          /* ako se ugasio sam, to treba zabeleziti */
          else {
            destroyedManually = false;
            context.parent ! "Greska";
          //!  println("********** Program " + numberComplete + " je zavrsio bez unistavanja tj. sam se ugasio!")
          }
          exitCode = PDFprocess.exitValue()
          out = out.reverse
          err = err.reverse
          /* upisivanje rezultata u datoteku */
        //!  println("\n   (" + out + ", " + err + ", " + exitCode + ")");
          if (out == null || err == null || PDFprocess == null) {
            writeToFileNullValues(numberComplete, out, err, PDFprocess, destroyedManually);
          }
          else
            writeToFile(numberComplete, out, err, exitCode, destroyedManually);
        }

        /* pokrecemo novu iteraciju */
        if(numberOfIterations == iterations)
           context.stop(self);
        else{
          numberOfIterations+=1;
          //println("Povecali smo broj iteracija: " + numberOfIterations);
          numberComplete = numberComplete+numberOfActors;
          self ! "Pokreni";
        }
      };
      case x => println("Sta kazes? " + x);
    }//end receive

  }

  def main(args: Array[String]): Unit = {

    try {
      /* Korisnik bira PDF citac koji ce biti testiran */
      import scala.io.StdIn
      var chosenReader = false;
      while (!chosenReader) {
        println("--------------------------------------------------------------------------------\nOdaberite PDF citac - za odabir unesite broj pod kojim je naveden citac:")
        println("\t\t1) Foxit PDF Reader")
        println("\t\t2) Slim PDF Reader")
        println("\t\t3) Sumatra PDF Reader")
        println("\t\t4) Expert PDF Reader")
        println("\t\t4) Evince PDF Reader")
        chosenReader = true;
        val input = StdIn.readLine();
        input match {
          case "1" => { println("Odabrali ste Foxit PDF Reader"); reader = 1 }
          case "2" => { println("Odabrali ste Slim PDF Reader"); reader = 2 }
          case "3" => { println("Odabrali ste Sumatra PDF Reader"); reader = 3 }
          case "4" => { println("Odabrali ste Expert PDF Reader"); reader = 4 }
          case "5" => { println("Odabrali ste Evince PDF Reader"); reader = 5 }
          case _ => {
            println("Niste uneli validnu opciju, pokusajte ponovo:"); chosenReader = false;
          }
        }
      }
      println("Unesite broj iteracija ili -1 za izvrsavanje programa dok ga rucno ne zaustavite:")
      var numberIter = -2;
      while(numberIter == -2) {
        val numIter = StdIn.readLine()
        if (numIter.matches("""[+-]?\d{1,9}""")){
          numberIter = numIter.toInt;
          numberIter match {
            case -1 => { println("Odabrali ste da sami zaustavite program") }
            case x if x <= 0 => {println("Pogresno ste uneli; pokusajte ponovo:"); numberIter = -2; }
            case x => { println("Odabrali ste " + x + " iteracija"); iterations = x; }
          }
        }
        else {
          println("Pogresno ste uneli; pokusajte ponovo:")
        }
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
    //val fileName: String = ".\\survey_on_parallel_computing_and_its_applications_in_dataparallel_problems_using_gpu_architectures.pdf";
    //val fileName: String = ".\\helloToParseAndChange.pdf";
    //val fileName: String = ".\\PDFcorpus\\10.pdf";
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
//    PDFparser.getHeuristicString(".\\heuristics\\strings\\traversals.txt")
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