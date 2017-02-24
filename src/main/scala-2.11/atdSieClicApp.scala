import java.io.File

import org.apache.pdfbox.multipdf.Splitter
import org.apache.pdfbox.pdmodel.{PDDocument, PDPage}
import org.apache.pdfbox.text.PDFTextStripper

import scala.swing.{Dialog, FileChooser}

/**
  * Created by jmarzin-cp on 23/02/2017.
  */
object atdSieClicApp extends App {

  def getDirectoryListing(title: String = ""): (File, Option[Array[File]]) = {
    val chooser = new FileChooser(null)
    chooser.fileSelectionMode = FileChooser.SelectionMode.DirectoriesOnly
    chooser.title = title
    val result = chooser.showOpenDialog(null)
    if (result == FileChooser.Result.Approve) {
      (chooser.selectedFile, Some(chooser.selectedFile.listFiles().filter(f => f.isFile && f.getName.contains(".pdf"))))
    } else (null, None)
  }

  var retour: (File, Option[Array[File]]) = (null, None)
  do retour = getDirectoryListing("Répertoire des ATD à traiter") while (retour._2.isEmpty)
  val repertoire = retour._1
  val listeFichiers = retour._2

  if(listeFichiers.get.filter(_.getName == "verso.pdf").isEmpty) {
    Dialog.showMessage(null, "Le fichier verso.pdf est absent.", title="Erreur")
    System.exit(0)
  }

  val dicoBulletinsRep = scala.collection.mutable.Map[String, List[PDPage]]()
  val dicoAtd = scala.collection.mutable.Map[String,PDPage]()
  var debiteur = ""
  var pageVerso = new PDPage
  val pageBlanche = new PDPage()
  var titre = ""
  val docsOriginaux = new PDDocument()
  val docsCopies = new PDDocument()
  val alaLigne = sys.props("line.separator")

  for(file <- listeFichiers.get) {
    val fichier = PDDocument.load(file)
    val pages = (new Splitter).split(fichier)
    val textePremierePage = (new PDFTextStripper).getText(pages.get(0))
    if(textePremierePage.contains("BULLETIN-REPONSE A L'AVIS A TIERS DETENTEUR")) {
      println("fichier Bulletin réponse trouvé")
      for(ipage<-0 until pages.size()) {
        val texte = (new PDFTextStripper).getText(pages.get(ipage)).split("montant des impôts dus par :"+alaLigne)
        if(texte(0).contains("BULLETIN-REPONSE A L'AVIS A TIERS DETENTEUR")){
          debiteur = texte(1).split("\\(1")(0).replaceAll(alaLigne," ").trim
          if (dicoBulletinsRep.get(debiteur).isDefined){
            dicoBulletinsRep(debiteur) = dicoBulletinsRep(debiteur) :+ pages.get(ipage).getPage(0)
          } else {
            dicoBulletinsRep += (debiteur -> List(pages.get(ipage).getPage(0)))
          }
        } else {
          dicoBulletinsRep(debiteur) = dicoBulletinsRep(debiteur) :+ pages.get(ipage).getPage(0)
        }
      }
    } else if(textePremierePage.contains("N° 3735 Original")) {
      println("fichier Atd trouvé")
      for(ipage <-0 until pages.size()) {
        val texte = (new PDFTextStripper).getText(pages.get(ipage)).split(alaLigne + "\\(1\\).*?demeurant.*?à  ")
        val debiteur = (texte(0).split(alaLigne).last.replaceFirst("né\\(.*$", "").trim + " " +
          texte(1).split(alaLigne).head.trim).replaceAll(" +", " ")
        if (texte(0).startsWith("N° 3735 Original")) {
          if(dicoAtd.get(debiteur).isEmpty) dicoAtd += (debiteur -> pages.get(ipage).getPage(0))
        } else if (texte(0).startsWith("N° 3735 Ampliation")) {
          docsCopies.addPage(pages.get(ipage).getPage(0))
          docsCopies.addPage(pageBlanche)
        }
      }
    } else if(textePremierePage.contains("N° 3738 Original")) {
      println("fichier Notifications trouvé")
      titre = ""
      for(ipage <- 0 until pages.size()) {
        val texte = (new PDFTextStripper).getText(pages.get(ipage))
        if(texte.contains("N° 3738 Original")) titre = "original"
        else if(texte.contains("N° 3738 Ampliation")) titre = "ampliation"
        titre match {
          case "orginal" =>
            docsOriginaux.addPage(pages.get(ipage).getPage(0))
            docsOriginaux.addPage(pageVerso)
          case _ =>
            docsCopies.addPage(pages.get(ipage).getPage(0))
            docsCopies.addPage(pageBlanche)
        }
      }
    } else if(file.getName == "verso.pdf") {
      pageVerso = pages.get(0).getPage(0)
      println("fichier Verso trouvé")
    }
    fichier.close()
  }

  dicoAtd.foreach(atd => {
    docsOriginaux.addPage(atd._2)
    docsOriginaux.addPage(pageVerso)
    if (dicoBulletinsRep.get(atd._1).isEmpty) println("bulletin réponse non trouvé")
    else {
      docsOriginaux.addPage(dicoBulletinsRep(atd._1).head)
      docsOriginaux.addPage(pageBlanche)
      dicoBulletinsRep(atd._1) = dicoBulletinsRep(atd._1).tail
    }
  })
  dicoBulletinsRep.foreach(listeBulletins => {
    if (listeBulletins._2.nonEmpty) {
      println("des bulletins sont en trop !")
      listeBulletins._2.foreach(bulletin => {
        docsOriginaux.addPage(bulletin)
        docsOriginaux.addPage(pageBlanche)
      })
    }
  })

  //for(i<-0 until docCopies.getNumberOfPages)docsOriginaux.addPage(docCopies.getPage(i))
  docsOriginaux.save("originaux.pdf")
  docsCopies.save("copies.pdf")
  //openoffice.org3 "macro:///Standard.Module1.findandreplace" file.doc
}
