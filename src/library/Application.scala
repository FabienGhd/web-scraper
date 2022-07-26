package library
import java.io.FileWriter
import java.io.File
import java.net.URI
import scala.io.Source.fromFile
import java.awt.Desktop



object Application extends App{

  //1. Lecture de la requete:
  val parser = ParserExpression
  val expr = parser.lireExpression
  
  
  //2. Construction de l'URL à passer au site de référence:
  val strWithUrls= buildAllUrls(expr)
  //On utilise l'espace pour séparer les differents urls
  var arraySeperateUrls= strWithUrls.split(" ")
  //Ajout de la fin de l'URL pour tous les débuts d'URL dans arraySeperateUrls
  for ( i<- 0 until arraySeperateUrls.length) {
	  arraySeperateUrls.update(i, arraySeperateUrls(i) ++"&cat_1=&geosearch_text=&searchGeoId=0")
	  println(arraySeperateUrls(i))
  }
  
  
 //3. Utilisation de AnalysePage:
  val analyse = ObjetAnalysePage 
  val pageList= (for ( elem<-arraySeperateUrls) 
    yield analyse.resultats(elem, expr)).flatten.toList 
    //chaque appel à resultats nous renvoie une List[(String,String)]
    //le yield nous renvoie une List[List[(String,String)]]
    //on utilise donc la fonction flatten pour combiner les sous listes en une seule liste

     
  //4. Pour les paires (titre,url) qu'on a obtenu, on voudrait le document html qui liste les liens:
  val production = ObjetProductionResultat
  val html = production.resultatVersHtml(pageList)
  
   
  //5. Pour le document html obtenu, on le voudrait sous forme de string:
  val htmlToString = HtmlVersStringObject
  val strOut = htmlToString.traduire(html)
  
  //6. On ecrit le résultat dans un fichier nommé texte.txt:
  val f = new File("texte.html");
  val file = new FileWriter(f)
  try {
	file.write(strOut)
  } finally file.close()
  
   if (Desktop.isDesktopSupported) {
       
      Desktop.getDesktop().browse(f.toURI)
    }
 
  /**
   * Créé une String contenant les differents url selon l'expression passée en paramètre
   */
  def buildAllUrls(exp :Expression): String ={
    var isAnd : Boolean=false
    var isOr: Boolean=false
    var str: String = ""
    var entireString : String= "" //Contient la String renvoyée
  
    //Itération sur la List[String] formée par listSearchUrl pour construire les urls
    for (s <- listSearchUrl(exp,List())) {
      //Tous les urls commencent par:
    	str = "https://search.vivastreet.com/annonces/fr?lb=new&search=1&start_field=1&keywords="
    	
    	if (s == "+" && !isAnd) {//On a un +
    	  isAnd=true
    	}else if(s=="|" && !isOr){ //On a un |
    	   isOr=true
    	}else{
    	  
    	  if(isAnd){
    	    //On colle le mot i a la fin du precedent
    	    str=s
      	  entireString +=("+"+str)
      	  isAnd=false
      	  
    	  }else if(isOr){
    	    //On ajoute un autre lien
    	    str+=s	    
    	    entireString +=" "
    	    entireString += str
    	    isOr=false
    	  
    	  }else{
    	    //On a eu aucun and ou or donc on ajoute juste le keyword dans la liste (cas du premier mot)
    	    str+=s
    	    entireString+= str  
    	  }	
    	}  	
    }
    entireString
   
  }
   /**
   * Construit une liste de string avec les mots séparés selon l'expression passée en paramètre
   * par des "+" ou des "|" pour ensuite construire les url à l'aide de la fonction  buildAllUrls(exp :Expression): String 
   */
   def listSearchUrl(expr: Expression, prevExpr : List[String]): List[String] = { 
     //Lorsqu'un "and" est appliqué plus de deux fois, il faut se souvenir de ce qu'on doit rajouter dans chaque url
     //de la même manière qu'on developperait le facteur "and" sur le reste de l'expression
  	expr match {
    	case Mot(x)   => List(x)
    	case Ou(x, y) => listSearchUrl(x,List()) ++ List("|") ++ listSearchUrl(y,List()) 
    	case Et(x, y) => 
    	   x match { //x représente le premier paramètre du "and"
      	case Mot(u)   => 
        	   y match {//y représente le deuxième paramètre du "and"
              	case Mot(a)   => listSearchUrl(x,List()) ++ List("+") ++ listSearchUrl(y,List())
              	case Et(a, b) => listSearchUrl(x,List()) ++ List("+") ++ listSearchUrl(y,prevExpr ++List(u)++ List("+"))
              	case Ou(a, b) =>  listSearchUrl(x,List()) ++ List("+") ++ listSearchUrl(a,prevExpr ++List(u)++ List("+")) ++ List("|")++ prevExpr++ listSearchUrl(x,List()) ++ List("+") ++ listSearchUrl(b,prevExpr ++List(u)++ List("+"))
              	}
      	case Et(u, v) =>  
        	  y match {
              	case Mot(a)   => listSearchUrl(x,List()) ++ List("+") ++ listSearchUrl(y,List())
              	case Et(a, b) => listSearchUrl(x,List()) ++ List("+") ++ listSearchUrl(y,List()) 	 
              	case Ou(a, b) => listSearchUrl(x,List()) ++ List("+") ++ listSearchUrl(a,prevExpr ++listSearchUrl(x,List())++ List("+")) ++ List("|")++ prevExpr ++ listSearchUrl(x,List()) ++ List("+") ++ listSearchUrl(b,prevExpr ++listSearchUrl(x,List())++ List("+"))
              	}
      	case Ou(u, v) =>   
        	  y match {
              	case Mot(a)   => listSearchUrl(u,List()) ++ List("+") ++ listSearchUrl(y,List()) ++ List("|")++ prevExpr++List("+") ++ listSearchUrl(v,List()) ++ List("+") ++ listSearchUrl(y,List())
              	case Et(a, b) => listSearchUrl(u,List()) ++ List("+") ++  listSearchUrl(y,List()) ++List("|") ++listSearchUrl(v,List()) ++ List("+") ++  listSearchUrl(y,List())
              	case Ou(a, b) => listSearchUrl(u,List()) ++ List("+")++ listSearchUrl(a,prevExpr++listSearchUrl(u,List())++ List("+")) ++ List("|")  ++ prevExpr++ listSearchUrl(u,List()) ++ List("+") ++ listSearchUrl(b,prevExpr++listSearchUrl(u,List())++ List("+"))  ++ List("|") ++ prevExpr ++ listSearchUrl(v,List()) ++ List("+") ++ listSearchUrl(a,prevExpr++listSearchUrl(v,List())++ List("+"))++ List("|")  ++ prevExpr++ listSearchUrl(v,List()) ++ List("+") ++ listSearchUrl(b,prevExpr++listSearchUrl(v,List())++ List("+"))    
              	  }
  	  } 
  	}
  	
  }

}

