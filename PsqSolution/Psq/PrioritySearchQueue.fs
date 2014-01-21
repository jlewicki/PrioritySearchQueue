namespace Psq

module internal PSQ = 
   // An implementation of a priority search queue as described in the paper "A Simple Implementation Technique for 
   // Priority Search Queues" by R Hinze.

   // The priority search queue is defined in terms of a semi-heap strucure called a pennant.  This pennant is described
   // in relation to a tournament tree, hence the winner and loser nomenclature.
   type Pennant<'K, 'V> = 
       | Empty
      | Winner of winner:('K*'V) * ltree:LoserTree<'K, 'V> * maxKey:'K
   
   and LoserTree<'K, 'V> =
      | Start 
      | Loser of loser:('K*'V) * left:LoserTree<'K, 'V> * splitKey:'K * right:LoserTree<'K, 'V>

   // An empty pennant.
   let empty<'K, 'V> = Empty

   // Returns a pennant containing the specified binding.
   let singleton binding = Winner( binding, Start, (fst binding) )

   // Returns the key of the binding with the maximum value in the pennant.  This is O(1).
   let maxKey = function
      | Empty -> invalidOp "empty pennant"
      | Winner( _, _, max) -> max
//   
//   let insert (k, v) = Pennant (k,v) 
//
//   let ofOrderedList (list: list<'K*'V>) = 
 //      let k, v = list.Head   
//      Pennant (k,v) 
//
//   let (|Empty|Min|) psq = 
