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

object actors {
/*
  def runIO(in: String): Process = {
    val process = Process(in) // scala.sys.process.ProcessBuilder
    val io = new ProcessIO(
      in => {},
      out => {scala.io.Source.fromInputStream(out).getLines.foreach(println)},
      err => {scala.io.Source.fromInputStream(err).getLines.foreach(println)})

    process run io
  }
*/
  /* svi podaci o izvrsavanju programa se upisuju u datoteku odgovarajuceg naziva */
def writeToFile(number: Int, out: List[String], err: List[String], exitCode : Int, destroyedManually : Boolean) {
  /* naziv datoteke se pravi na osnovu broja Actor-a, i na osnovu toga da li je doslo do greske (postoje podaci u stderr) */
  /* ovo dodajem da ne bi ispisivao rezultate procesa koji nisu uzrokovali gresku pdf citaca */
  if(destroyedManually){
    println("------------------ destroyedManually" + destroyedManually)
    /* posto nije doslo do greske, brisemo izmenjeni pdf iz foldera fuzzedPDFcorpus */
    import java.io.IOException
    import java.nio.file.DirectoryNotEmptyException
    import java.nio.file.Files
    import java.nio.file.NoSuchFileException
    val path = Paths.get(".\\fuzzedPDFcorpus\\" + number + "_fuzzed.pdf");
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
  println("------------------ !destroyedManually: " + destroyedManually)
  var fileName = ".\\results\\";
  if (!err.isEmpty)
    fileName = fileName + "ERR_";
  /* ako se program ugasio sam a nije sacekao da ga unisti glavni program */
  if(!destroyedManually)
    fileName = fileName + "CRASH_";
  fileName = fileName + number + ".txt";
  println("Filename: " + fileName)
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
    println("Filename: " + fileName)
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
    println("Filename: " + fileName)
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
    val PDFprocess: Process = qb run (ProcessLogger((s) => { println("PL: " + s); out ::= s},
                                                    (s) => { err ::= s; /*println("ER: " + s + "     duzina err: " + err.length); */ }));

    /* bez ovog poziva, run se odmah vraca i vraca prazne rezultate! Ovako cekamo kraj izvrsavanja da se dobije povratna vrednost */
    val exit: Int = PDFprocess.exitValue();
    println("Duzina err: " + err.length)
    (out.reverse, err.reverse, PDFprocess)
  }

  /* kontrolise sve aktore koji pokrecu pdf */
  class Controler extends Actor {

    /* broj zavrsenih PDF citanja i broj Actor-a */
    var numberOfTerminations = 0;
    /* za sad je MAX broj actor-a 20 */
    val numberOfActors = 5;
    /* broj zeljenih .pdf fajlova koje zelimo ukupno da testiramo */
    val numberOfFiles = 0;

    def pokreni() = {
      import better.files._
      import better.files.File._

      println("Pokrecem Worker-e");
      var i = 0;
      val workers: Array[ActorRef] = new Array[ActorRef](numberOfActors);
      for(i <- 0 until numberOfActors) {
        /* pravimo Actor-a i saljemo mu poruku za pokretanje */
        workers(i) = context.actorOf(Props(new Worker(i, numberOfActors)), name = "myWorker-" + i);
        context.watch(workers(i));
        workers(i) ! "Pokreni";
      }// end for
    }

    def receive = {
      case "Pokrenuto" => println("Pokrenuto: " + context.sender().path.name);
      case "Ajmo" => println("Main kaze ajmo!"); pokreni();
      case Terminated(x) => {
        println(x + " terminated")
        numberOfTerminations+=1;
        if(numberOfTerminations == numberOfActors) {
          println("............... Gasim sistem ................")
          context.system.terminate();
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
    var numberOfIterations = 0;
    var numberComplete = number;

    def receive = {
      case "Pokreni" => {
        try {
//          do_something 2>&1 | tee -a some_file ---- Ova komanda ispisuje rezultate u datoteku a.txt, ali ispisuje ih i na stdout
          //val hello = "powershell scala .\\primer.scala"//2>&1 | tee -a .\\results\\filename.txt" //".\\FoxitReader.exe .\\hello.pdf"//"FoxitReader.exe hello.pdf";
          //val hello = ".\\inputs\\" + number+".\\FoxitReader.exe .\\inputs\\"+ number + "\\hello.pdf"
          //val hello = ".\\FoxitReader.exe .\\inputs\\"+ number + "\\hello.pdf"
          /* pravimo datoteku */
          val fileNumber = numberComplete % 20;
          val fileName: String = ".\\PDFcorpus\\" + fileNumber + ".pdf";
          val PDFparser = new ParserPDF(fileName, numberComplete);
          // PDFparser.readLines()
          //PDFparser.readFileAsString()

          PDFparser.readFileBinary()
          println("Actor broj " + numberComplete + " je izmenio svoj fajl i sada ga pokrece.")

          val hello = ".\\PDF_citaci\\slim\\SlimPDFReader.exe .\\fuzzedPDFcorpus\\" + numberComplete + "_fuzzed.pdf";
          this.sender() ! "Pokrenuto iz Actora " + numberComplete;
          println("Postavljam tajmer.")
          /* za 8 sekundi saljemo poruku stop sami sebi */
          timers.startSingleTimer("key", "stop", 3000 milliseconds);

          val qb = Process(hello) // scala.sys.process.ProcessBuilder
          PDFprocess = qb run (ProcessLogger((s) => { println("PL: " + s); out ::= s},
            (s) => { err ::= s; /*println("ER: " + s + "     duzina err: " + err.length); */ }));

          }
        catch {
          case x => println("-------------- Exception " + x.getMessage + " caught ---------------");
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
          println("Program je zavrsio jer je uhvacen izuzetak.")
        }
        else {
          /* pri normalnom izvrsavanju programa, proces je i dalje ziv jer se nije sam ugasio zbog greske */
          if (PDFprocess.isAlive()) {
            println("Program je zavrsio sa unistavanjem!");
            PDFprocess.destroy();
          }
          /* ako se ugasio sam, to treba zabeleziti */
          else {
            destroyedManually = false;
            println("********** Program je zavrsio bez unistavanja!")
          }
          exitCode = PDFprocess.exitValue()
          out = out.reverse
          err = err.reverse
          /* upisivanje rezultata u datoteku */
          println("\n   (" + out + ", " + err + ", " + exitCode + ")");
          if (out == null || err == null || PDFprocess == null) {
            writeToFileNullValues(numberComplete, out, err, PDFprocess, destroyedManually);
          }
          else
            writeToFile(numberComplete, out, err, exitCode, destroyedManually);
        }

        /* pokrecemo novu iteraciju */
        if(numberOfIterations == -1)
           context.stop(self);
        else{
          numberOfIterations+=1;
          println("Povecali smo broj iteracija: " + numberOfIterations);
          numberComplete = numberComplete+numberOfActors;
          self ! "Pokreni";
        }
      };
      case x => println("Sta kazes? " + x);
    }//end receive

  }

  def main(args: Array[String]): Unit = {
    val system = ActorSystem("greetings");
    val a = system.actorOf(Props[Controler], name="greetings-actor")
    a ! "Ajmo";

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