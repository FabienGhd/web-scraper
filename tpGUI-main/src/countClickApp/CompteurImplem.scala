package countClickApp

/** La classe des compteurs paramétrés par une liste non vide de valeurs maximale pour chaque roue.
 *  La liste ne peut être vide et les valeurs sont forcément positives ou nulles */
class CompteurImpl extends Compteur{
  
  
  var roue1:Int=0;
  var roue2:Int=0;
  var roue3:Int=0;
  
  var maxRoue1=0;
  var maxRoue2=0;
  var maxRoue3=0;
  
  var nbRoue=1;
  
  
  def init(l:List[Int]):Unit= { 
    
    l match{
      
      case List(a)=> maxRoue1=a;
      case List(a,b)=> maxRoue2=a; maxRoue1=b; nbRoue=2
      case List(a,b,c)=> maxRoue3=a; maxRoue2=b; maxRoue1=c; nbRoue=3
      
    }
    
  }
  
  
  def courant:List[Int]= { 
    
    if (nbRoue==3) { List(this.roue3,this.roue2,this.roue1) }
    
    else if (nbRoue==2) { List(this.roue2,this.roue1) }
    
    else { List(this.roue1) }
    
  }
  
  
  def suivant:Unit= {
    
    nbRoue match{
    
      case 1 => if(roue1<maxRoue1) { this.roue1 = this.roue1 + 1}
    
      case 2 => if(roue1<maxRoue1) { this.roue1 = this.roue1 + 1 }
                else if((roue1==maxRoue1)&&(roue2<maxRoue2)) {this.roue1=0;this.roue2 = this.roue2 + 1}
      case 3 => if(roue1<maxRoue1) { this.roue1 = this.roue1 + 1}
                else if((roue1==maxRoue1)&&(roue2<maxRoue2)) {this.roue1=0;this.roue2 = this.roue2 + 1;}       
                else if((roue1==maxRoue1)&&(roue2==maxRoue2)&&(roue3<maxRoue3)) {this.roue1=0;this.roue2=0; this.roue3 = this.roue3 + 1}
                
    }
  }
  
  def suivantPossible : Boolean= {
    
    nbRoue match{
      
      case 1 => if(roue1==maxRoue1) {false} else {true}
      case 2 => if((roue1==maxRoue1)&&(roue2==maxRoue2)){ false} else{ true}
      case 3 => if((roue1==maxRoue1)&&(roue2==maxRoue2)&&(roue3==maxRoue3)) {false} else{ true}
      
     }
   }
  
  
  def valPossibles:Int={ 
    
    
    nbRoue match{
      
      case 1 => (maxRoue1 + 1) 
      case 2 => (maxRoue1 + 1)*(maxRoue2 + 1)  
      case 3 => (maxRoue1 + 1)*(maxRoue2 + 1)*(maxRoue3 + 1)
    
    }
   }
  
  
  def valRestantes:Int= { 
    
    
    nbRoue match{
      
      case 1 => maxRoue1 - roue1
      case 2 => ((maxRoue1 + 1)*(maxRoue2 + 1)) - ((roue2)*(maxRoue1+1)+(roue1+1))
      case 3 => (maxRoue1 + 1)*(maxRoue2 + 1)*(maxRoue3 + 1) - ((roue3*(maxRoue2+1)*(maxRoue1+1))+((roue2)*(maxRoue1+1)+(roue1+1)))
    }
  }
  
}
