// Missing frame conditions and parts of the predicate (~4 annotations total).
predicate sorted(a: array<int>,  lo:int, hi:int)
{ 
  forall j,k:: lo<=j<k<hi ==> a[j]<=a[k]
}

// Missing pre/post conditions, frame conditions,
// and loop invariants (~6 annotations total).
method initialize(a: array<int>)
{
  var i: int :=1;
  while (i<a.Length) {
    if (a[i]<a[0]) 
      { a[0],a[i] := a[i],a[0]; }
    i:=i+1; 
  }  
}
  
// Missing pre/post conditions, frame conditions,
// and loop invariants (~12 annotations total, but 
// only ~7 distinct constraints---expect to use the 
// same constraint as, e.g., a precondition and a loop
// invariant).
method insert(a: array<int>, i : int)
  {
    var j : int := i;
    var v : int := a[i];
    
    while(v < a[j-1]) { 
      a[j]:=a[j-1];
      j:=j-1;
    }
     
    a[j] := v;
  }
  
// Missing preconditions, frame conditions,
// and loop invariants (~5 annotations total,
// including the provided postcondition).
method sort(a: array<int>)
  ensures sorted(a, 0, a.Length);
  {
    if (a.Length <= 1) { return; }
    
    initialize(a);
    
    var i : int := 1;
    
    while(i < a.Length)
      {
        insert(a, i);
        i := i + 1;
      }
  }
  
  
