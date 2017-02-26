import java.io.File

import org.pdfclown.documents.Page
import org.pdfclown.files._
import org.pdfclown.tools.TextExtractor
import scala.collection.mutable
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
  val listeFichiers = retour._2.get

  if(listeFichiers.filter(_.getName == "verso.pdf").isEmpty) {
    Dialog.showMessage(null, "Le fichier verso.pdf est absent.", title="Erreur")
    System.exit(0)
  }
  val pageVerso = new org.pdfclown.files.File(listeFichiers.filter(_.getName == "verso.pdf").head.getCanonicalPath).getDocument.getPages.get(0)


  val dicoBulletinsRep = scala.collection.mutable.Map[String, List[Page]]()
  val dicoAtd = scala.collection.mutable.Map[String,Page]()
  var debiteur = ""
  var titre = ""
  val fichierOriginaux = new org.pdfclown.files.File()
  val docsOriginaux = fichierOriginaux.getDocument()
  val fichierCopies = new org.pdfclown.files.File()
  val docsCopies = fichierCopies.getDocument()
  val alaLigne = sys.props("line.separator")
  val listeFichiersLus = new mutable.Stack[org.pdfclown.files.File]()

  for(file <- listeFichiers) {
    val fic = new org.pdfclown.files.File(file.getCanonicalPath)
    val fichier = fic.getDocument
    listeFichiersLus.push(fic)
    val pages = fichier.getPages
    var texte = new TextExtractor().extract(pages.get(0).getContents)
    var chaine = ""
    for(i <- 0 until texte.get(null).size()) chaine += texte.get(null).get(i).getText
    if(chaine.contains("BULLETIN-REPONSE A L'AVIS A TIERS DETENTEUR")) {
      println("fichier Bulletin réponse trouvé")
      for(ipage <- 0 until pages.size) {
        texte = new TextExtractor().extract(pages.get(ipage).getContents)
        chaine = ""
        for(i <- 0 until texte.get(null).size()) chaine += texte.get(null).get(i).getText
          if(chaine.contains("BULLETIN-REPONSE A L'AVIS A TIERS DETENTEUR")) {
            // 15, 14 et 13 mais est-ce tout le temps ?
          debiteur = texte.get(null).get(15).getText + " " +
            texte.get(null).get(14).getText + " " +
            texte.get(null).get(13).getText
          debiteur = debiteur.replaceAll("  ", " ").trim
          if (dicoBulletinsRep.get(debiteur).isDefined) {
            dicoBulletinsRep(debiteur) = dicoBulletinsRep(debiteur) :+ pages.get(ipage)
          } else {
            dicoBulletinsRep += (debiteur -> List(pages.get(ipage)))
          }
        } else {
          dicoBulletinsRep(debiteur) = dicoBulletinsRep(debiteur) :+ pages.get(ipage)
        }
      }
    } else if(chaine.contains("N° 3735 Original")) {
      println("fichier Atd trouvé")
      for(ipage <-0 until pages.size()) {
        var chaine = ""
        texte = new TextExtractor().extract(pages.get(ipage).getContents)
        for(i <- 0 until texte.get(null).size()) chaine += texte.get(null).get(i).getText
        val modele = """.*Madame, Monsieur, (.*)  né.*demeurant  à  (.*) est  redevable.*""".r
        chaine match {
          case modele(nom,adresse) => debiteur = (nom + " " + adresse).replaceAll(" +"," ").trim
          case _ => debiteur = ""
        }
        if (chaine.contains("N° 3735 Original")) {
          if(dicoAtd.get(debiteur).isEmpty) dicoAtd += (debiteur -> pages.get(ipage))
        } else if (chaine.contains("N° 3735 Ampliation")) {
          docsCopies.getPages.add(pages.get(ipage).clone(docsCopies))
          docsCopies.getPages.add(new Page(docsCopies))
        }
      }
    } else if(chaine.contains("N° 3738 Original")) {
      println("fichier Notifications trouvé")
      titre = ""
      for(ipage <- 0 until pages.size()) {
        var chaine = ""
        texte = new TextExtractor().extract(pages.get(ipage).getContents)
        for(i <- 0 until texte.get(null).size()) chaine += texte.get(null).get(i).getText
        if(chaine.contains("N° 3738 Original")) titre = "original"
        else if(chaine.contains("N° 3738 Ampliation")) titre = "ampliation"
        titre match {
          case "original" =>
            docsOriginaux.getPages.add(pages.get(ipage).clone(docsOriginaux))
            docsOriginaux.getPages.add(pageVerso.clone(docsOriginaux))
          case _ =>
            docsCopies.getPages.add(pages.get(ipage).clone(docsCopies))
            docsCopies.getPages.add(new Page(docsCopies))
        }
      }
    } else if(file.getName == "verso.pdf") {
      // déjà traité plus haut
      println("fichier Verso trouvé")
    }
    //fic.close()
  }

  dicoAtd.foreach(atd => {
    docsOriginaux.getPages.add(atd._2.clone(docsOriginaux))
    docsOriginaux.getPages.add(pageVerso.clone(docsOriginaux))
    if (dicoBulletinsRep.get(atd._1).isEmpty) println("bulletin réponse non trouvé")
    else {
      docsOriginaux.getPages.add(dicoBulletinsRep(atd._1).head.clone(docsOriginaux))
      docsOriginaux.getPages.add(new Page(docsOriginaux))
      dicoBulletinsRep(atd._1) = dicoBulletinsRep(atd._1).tail
    }
  })
  dicoBulletinsRep.foreach(listeBulletins => {
    if (listeBulletins._2.nonEmpty) {
      println("des bulletins sont en trop !")
      listeBulletins._2.foreach(bulletin => {
        docsOriginaux.getPages.add(bulletin.clone(docsOriginaux))
        docsOriginaux.getPages.add(new Page(docsOriginaux))
      })
    }
  })

  fichierOriginaux.save(
    new java.io.File("originaux.pdf"),
    SerializationModeEnum.Standard
  )
  fichierOriginaux.close()

  fichierCopies.save(
    new java.io.File("copies.pdf"),
    SerializationModeEnum.Standard
  )
  fichierCopies.close()

  while(listeFichiersLus.nonEmpty)listeFichiersLus.pop.close()

  println("fini")
}
