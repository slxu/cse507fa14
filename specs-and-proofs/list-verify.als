module list 		 

one sig Null {}		 

sig Str {}				

sig Node {
  data: one Str + Null,	 
  next: one Node + Null	 
}

sig List {
  head: one Node + Null	 
}

one sig This extends List {}

pred Inv[next: Node->(Node + Null) ] {
  no ^next & iden
}

pred Pre[This: List, head:  List->(Node + Null), next: Node->(Node + Null)] {
  This.head != Null &&
  This.head.next != Null
}

pred Post[This: List, oldHead, head:  List->(Node + Null), oldNext, next: Node->(Node + Null)] {
  This.head.*next = This.oldHead.*oldNext && 
  let N = This.oldHead.*oldNext - Null |
   next = oldNext ++ ~(oldNext & N->N) ++
               This.oldHead->Null
                
}

fun near0[] : Node+Null { This.head }
fun mid0[] : Node+Null { near0.next }
fun far0[] : Node+Null { mid0.next }

fun next0[] : Node->(Node + Null) { next ++ (near0 -> far0) }
pred guard[] { far0 != Null }
fun next1[] : Node->(Node + Null) { next0 ++ (mid0 -> near0) }
fun near1[] : Node+Null { mid0 }
fun mid1[] : Node+Null { far0 }
fun far1[] : Node+Null { far0.next1 }

fun near2[] : Node+Null { guard => near1 else near0 }
fun mid2[] : Node+Null { guard => mid1 else mid0 }
fun far2[] : Node+Null { guard => far1 else far0 }

fun next2[] : Node->(Node + Null) { guard => next1 else next0 }
fun next3[] : Node->(Node + Null) { next2 ++ (mid2 -> near2) }
fun head0[] : List->(Node + Null)  { head ++ (This -> mid2) }

run {
  far2 = Null &&
  Inv[next] && 
  Pre[This, head, next] &&
  !(Inv[next3] && Post[This, head, head0, next, next3])
} for 1 List, 3 Node, 3 Str, 1 Null
