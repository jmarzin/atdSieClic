import java.io.File
import org.apache.pdfbox.multipdf.Splitter
import org.apache.pdfbox.pdmodel.{PDDocument, PDPage}
import org.apache.pdfbox.text.PDFTextStripper

/**
  * Created by jmarzin-cp on 23/02/2017.
  */
object atdSieClicApp extends App {

  val verso = PDDocument.load(new File("D:\\Bureau\\jmarzin-cp\\Mes Documents\\atdsieclic\\verso.pdf"))
  val pageVerso = verso.getPage(0)

  val pageBlanche = new PDPage()

  val dicoBulletinsRep = scala.collection.mutable.Map[String, List[PDPage]]()

  val bulletinsRep = PDDocument.load(new File("D:\\Bureau\\jmarzin-cp\\Mes Documents\\atdsieclic\\bulletin réponse.pdf"))
  val pagesBulletinsRep = (new Splitter).split(bulletinsRep).toArray()
  var debiteur = ""
  for(page<-pagesBulletinsRep){
    val texte = (new PDFTextStripper).getText(page.asInstanceOf[PDDocument]).split("montant des impôts dus par :\\r\\n")
    if(texte(0).contains("BULLETIN-REPONSE A L'AVIS A TIERS DETENTEUR")){
      debiteur = texte(1).split("\\(1")(0).replaceAll("\r\n"," ")
      if (dicoBulletinsRep.get(debiteur).isDefined){
        dicoBulletinsRep(debiteur) = dicoBulletinsRep(debiteur) :+ page.asInstanceOf[PDDocument].getPage(0)
      } else {
        dicoBulletinsRep += (debiteur -> List(page.asInstanceOf[PDDocument].getPage(0)))
      }
    } else {
        dicoBulletinsRep(debiteur) = dicoBulletinsRep(debiteur) :+ page.asInstanceOf[PDDocument].getPage(0)
    }
  }

  val atd = PDDocument.load(new File("D:\\Bureau\\jmarzin-cp\\Mes Documents\\atdsieclic\\ATD recto.pdf"))
  val pagesAtd = (new Splitter).split(atd).toArray()
  var titre = ""
  val docsOriginaux = new PDDocument()
  val docCopies = new PDDocument()

  for(page<-pagesAtd){
    val texte = (new PDFTextStripper).getText(page.asInstanceOf[PDDocument]).split("\\r\\n\\(1\\).*?demeurant.*?à  ")
    val debiteur = (texte(0).split("\\r\\n").last.replaceFirst("né\\(.*$","").trim + " " +
      texte(1).split("\\r\\n").head.trim).replaceAll(" +"," ")
    if(texte(0).startsWith("N° 3735 Original")){
      titre = "Original"
      docsOriginaux.addPage(page.asInstanceOf[PDDocument].getPage(0))
      docsOriginaux.addPage(pageVerso)
    } else if(texte(0).startsWith("N° 3735 Ampliation")){
      titre = "Ampliation"
      docCopies.addPage(page.asInstanceOf[PDDocument].getPage(0))
      docCopies.addPage(pageBlanche)
    }
    val adresse = texte(0).split("\\r\\n").last + " " + texte(1).split("\\r\\n").head
    println(adresse)
  }
  //for(i<-0 until docCopies.getNumberOfPages)docsOriginaux.addPage(docCopies.getPage(i))
  docsOriginaux.save("D:\\Bureau\\jmarzin-cp\\Mes Documents\\atdsieclic\\originaux.pdf")
  docCopies.save("D:\\Bureau\\jmarzin-cp\\Mes Documents\\atdsieclic\\copies.pdf")
  //openoffice.org3 "macro:///Standard.Module1.findandreplace" file.doc
}
