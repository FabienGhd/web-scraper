package library
import java.io.FileWriter
import java.net.URL


trait AnalysePage {
  /**
   * A partir d'une URL de requête sur le site de référence et d'une expression exp,
   * retourne de pages issues de la requête et satisfaisant l'expression.
   *
   * @param url l'URL de la requête sur le site de référence
   * @param exp l'expression à vérifier sur les pages trouvées
   * @return la liste des couples (titre,ref) où ref est l'URL d'une page
   * satisfaisant l'expression et titre est son titre.
   */
  def resultats(url: String, exp: Expression): List[(String, String)]
}

object ObjetAnalysePage extends AnalysePage {
  val outilWeb = OutilsWebObjet
  val objFiltrageUrls = ObjetFiltrageURLs
  val objFiltrageHtml = ObjectFiltrageHtml

  def resultats(url: String, exp: Expression): List[(String, String)] = {

  //1. Récupère le document Html associé à la page url
	val html = outilWeb.obtenirHtml(url)
	
	//2. On extrait du document html la liste des URL correspondant à des annonces
	val listUrls = objFiltrageUrls.filtreAnnonce(html)


  //3. On obtient tous les documents html qui correspondent aux urls qu'on a obtenu
	val listHtml = (for (str <- listUrls)
  	yield (str, outilWeb.obtenirHtml(str)))
 

  //4. On garde les documents html qui satisfont la requête exp
	val listCouple = (for (str <- listHtml; if objFiltrageHtml.filtreHtml(str._2, exp))
  	yield (str._1, str._2))
  	

  //5. On extrait le titre de chaque page
	val listTitle = (for (couple <- listCouple)
  	yield (recHelper(couple._2 match {
  	case Tag(_, _, c) => c
  	case Texte(_) 	=> Nil
	}), couple._1))

	listTitle

  }
  
  private def recHelper(children: List[Html]): String = {
  	children match {
    	case Nil => ""
    	case x :: xs =>
      	x match {
      	  case Tag("title", List(),List(Texte(title))) => title
        	case Tag(_, _, c)   => recHelper(xs) ++ recHelper(c)//aussi sur les enfants des enfants
        	case Texte(content) => ""
      	}
  	}
  	
  }

}

trait FiltrageURLs {
  /**
   * A partir d'un document Html h, rend la liste des URLs accessibles à partir
   * de h (ces URLs sont des hyperliens h) tels que ces URLs sont tous des URLs
   * d'annonces du site de référence
   *
   * @param h le document Html
   * @return la liste des URLs d'annonces contenues dans h
   */
  def filtreAnnonce(h: Html): List[String]
}

object ObjetFiltrageURLs extends FiltrageURLs {
  /**
   * A partir d'un document Html h, rend la liste des URLs accessibles à partir
   * de h (ces URLs sont des hyperliens h) tels que ces URLs sont tous des URLs
   * d'annonces du site de référence
   *
   * @param h le document Html
   * @return la liste des URLs d'annonces contenues dans h
   * import java.io.FileWriter
   *
   */
  def filtreAnnonce(h: Html): List[String] = {
	  var res: List[String] = List()
	  
	  //Appel à la fonction filtreUrl sur les enfants de h directement, s'ils existent
    filtreUrl(h match{
      case Tag(_,_,children) => children
      case Texte(_) => List()
    },res)

  }

  private def filtreUrl(hlist: List[Html],listOfStrings: List[String]): List[String] = {
    hlist match {
      case x :: xs =>
        
        x match{
          case Tag("a", attribute,_) => 
                  if(isValid(checkAttribute(attribute))) {
                                      

                      val str=checkAttribute(attribute)
                    filtreUrl(xs,str :: listOfStrings)   
                  }
                  else {
                    filtreUrl(xs,listOfStrings)  
                  }
                
         
          case Tag(_,_,c) => { //println("a")
            //le suivant dans les enfants + on check aussi dans les enfants de lenfant x
            filtreUrl(Nil,filtreUrl(xs,listOfStrings) ++ filtreUrl(c,List()))
   
          }
          case Texte(_) => filtreUrl(xs,listOfStrings)
        }
      case Nil=> listOfStrings
    }
  } 
  
  /**
   * Retourne l'url d'un attribut si on le trouve, la chaine vide sinon
   */
   private def checkAttribute(attr: List[(String,String)]): String={
     var res=""
     for(a <- attr){
       if(a._1.equals("href")){
         res=a._2
       }
     }
     res
   }
  

   /**
    * Teste si l'URL passé en argument est valide ou non
    */
  private def isValid(url: String): Boolean = {
  	url.startsWith("https://www.vivastreet.com/") && endIsOnlyDigits(url)
  }

 
/**
 * Teste si les 9 derniers caractères de la string sont des chiffres
 */
  private def endIsOnlyDigits(s: String): Boolean = {

	var res: Boolean = true
	val start: Int = s.length() - 9
	val end: Int = s.length() - 1
	for (i <- start to end) {
  	if (!s.charAt(i).isDigit) res = false
	}
	res
  }
   



 
}

trait FiltrageHtml {
  /**
   * A partir d'un document Html h et d'une requête e, dit si le document
   * satisfait l'expression e
   *
   * @param h le document Html
   * @param e l'expression
   * @return true si le document satisfait l'expression e
   */
  def filtreHtml(h: Html, e: Expression): Boolean
}

object ObjectFiltrageHtml extends FiltrageHtml {

  def filtreHtml(h: Html, e: Expression): Boolean = {
  	val listHtml: Set[String] = htmlToStringText(h)
  
  	e match {
    	case Mot(x)   => listHtml.contains(x)
    	case Et(a, b) => filtreHtml(h, a) && filtreHtml(h, b)
    	case Ou(a, b) => filtreHtml(h, a) || filtreHtml(h, b)
  	}
  }

  /**
   * A partir d'un page web, rend les mots des Texte
   *
   * @param h le document Html
   * @return Set[String] de tout les Strings dans Texte()
   */
  private def htmlToStringText(h: Html): Set[String] = {
  	var textList: Set[String] = Set()
  	h match {
    	case Tag(x, y, z) => for (i <- z) {
      	i match {
        	case Texte(a) => textList ++= a.split(" ")
        	case b: Tag   => textList ++= htmlToStringText(b)
      	}
    	}
    	case Texte(x) => textList += x
    	case _    	=> None
  	}
  	textList
  }
}

trait ProductionResultat {
  /**
   * A partir d'une liste de couples (titre,URL), produit un document Html, qui
   * liste les solutions sous la forme de liens cliquables
   *
   * @param l la liste des couples solution (titre,URL)
   * @return le document Html listant les solutions
   */

  
  /**
   * @param une liste de couples (titre,url)
   * return une page htmp avec les liens correspondant à la liste en paramètre
   */
  def resultatVersHtml(l: List[(String, String)]): Html
}
object ObjetProductionResultat extends ProductionResultat {

  /**
   * return  liste de Tag avec les liens d’une liste de couples (titre, url)
   */
  private def donnelistTag(l: List[(String, String)]): List[Tag] = {
	var listT: List[Tag] = List();
	for ((t, u) <- l) {
  	val n: List[Tag] = List(Tag("a", List(("href", u)), List(Texte(t+"</br>"))))
  	listT = listT ++ n;
	}
	listT;
  }
  /**
   * @param une liste de couples (titre,url)
   * return une page htmp avec les liens correspondant à la liste en paramètre
   */
  def resultatVersHtml(l: List[(String, String)]): Html = {
	val result = Tag("html", List(),
  	List(
    	Tag("head", List(),
      	List(
        	Tag("meta", List(("charset", "utf-8")), List()),
        	Tag("title", List(), List(Texte("My Page"))))),
    	Tag("body", List(), List(
      	Tag("center", List(), donnelistTag(l))))))
	result;
  }
}

trait HtmlVersString {
  /**
   * Produit la chaîne de caractère correspondant à un document Html
   *
   * @param h le document Html
   * @return la chaîne de caractère représentant h
   */
  def traduire(h: Html): String

 
}
object HtmlVersStringObject extends HtmlVersString {
 
  /**
   * @param h le document Html
   * @return la chaîne de caractère représentant h
   */
  def traduire(h: Html): String = {
  	val vide = List("", "")
  	h match {
    	case Texte("")         	=> ""
    	case Texte(txt)        	=> txt
    	case Tag("", vide, List()) => ""
    	case Tag(txt, l, List())   => "<" + txt +toStringHtml(l)  +"/>"
    	case Tag(txt, l, x) => {
      	var s = "<" + txt + toStringHtml(l) + ">"
      	for (html <- x) {
        	s = s +traduire(html) +"\n"
      	}
      	s + "</" + txt + ">"
    	}
  	}
  }
  
  /**
   * 
   */
  private def toStringHtml(l: List[(String, String)]): String = {
  	var s = "";
  	for ((x, y) <- l) {
    	s = s + " " + x  + "\n"+ "=\""+ y + "\""
  	}
  	s
  }
}
