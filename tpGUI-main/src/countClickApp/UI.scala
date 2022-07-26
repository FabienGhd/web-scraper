package countClickApp
import scala.swing._



//import javax.swing._
import event._


class UI extends MainFrame {
  
  title = "button click"
  preferredSize = new Dimension(300, 300)
  
  val bp=new BoxPanel(Orientation.Vertical)
  
  contents=bp
  
  val b= new Button
  bp.contents += b
  var compteur=0
  b.text = compteur.toString()
  b.reactions += {case ButtonClicked(_) => { compteur +=1; b.text = (compteur).toString()  }}
 
  val reset=new Button
  bp.contents += reset
  reset.text="reset"
  reset.reactions += {case ButtonClicked(_) => { compteur =0; b.text = (compteur).toString();compteur2 =0; b2.text = (compteur2).toString()  }}
  
  
  val b2= new Button
  bp.contents += b2
  var compteur2=0
  b2.text = compteur2.toString()
  b2.reactions += {case ButtonClicked(_) => { compteur2 +=1; b2.text = (compteur2).toString()  }}
  
  
  val b3= new Button
  bp.contents += b3
  var compteur3= new CompteurImpl
  
  compteur3.init(List(1,1,1))
  
  b3.text = compteur3.courant.mkString
  b3.reactions += {case ButtonClicked(_) => { compteur3.suivant; b3.text = (compteur3.courant).mkString  }}
  
  
}