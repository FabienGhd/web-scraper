package reactiveApp

import scala.swing._
import java.awt.Color._
import java.awt.Color
import javax.swing._
import scala.io.StdIn._


/**
 * MainFrame realizing the CopyThat application
 */
class UI extends MainFrame {
  
  // Propriétés de la fenêtre
  title = "My First Reactive Application!"
  preferredSize = new Dimension(500, 100)
  
  // Quelques champs définissant les composants
  val input = new InField
  val output = new ResultText
  val copy = new CopyButton("Copy That!",input,output)
  
  val input2 = new InField
  val output2 = new ResultText
  val copy2 = new CopyButton("Copy That!",input2,output2)
  
  // Ajout des composants à la fenêtre
  contents = new BoxPanel(Orientation.Horizontal) {
    background= Color.BLACK
    
    
    contents += output
    contents += copy
    contents += input
    
    contents += input2
    contents += copy2
    contents += output2
//output2.columns_=(10)
//output2.columns_=(10)
//output2.lineWrap_=(true)
   
    
    output.foreground_=(RED)
    input.foreground_=(BLUE)
  }
    
}