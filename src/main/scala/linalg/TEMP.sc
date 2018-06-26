val x = 0.333



val (first, last): (String, String) = x.toString.splitAt(x.toString.indexOf("."))

if(last.toDouble == 0){
     if(first.toDouble == 0) "0" //getting rid of minus sign in cases like "-0.0"
     else first
} else {

     x.toString
}

