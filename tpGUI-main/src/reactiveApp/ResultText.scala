package reactiveApp

import scala.swing._
import java.awt.Color

/**
 * Customized label receiving the copied text 
 */
/*class ResultText extends Label {
  
  //columns=10
  //rows=50
  //lineWrap=true
  background = Color.BLACK
  foreground = Color.WHITE
  text = "No text!"
  
}*/

class ResultText extends TextArea {
  
  columns=10
  rows=50
  lineWrap=true
  background = Color.BLACK
  foreground = Color.WHITE
  text = "No text!"
  
}