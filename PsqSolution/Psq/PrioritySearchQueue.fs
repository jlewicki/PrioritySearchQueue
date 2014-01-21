namespace Psq
open System.Collections.Generic


module internal PSQ = 
   // An implementation of a priority search queue as described in the paper "A Simple Implementation Technique for 
   // Priority Search Queues" by R. Hinze.

   // The priority search queue is defined in terms of a semi-heap strucure called a pennant.  This pennant is described
   // in relation to a tournament tree, hence the winner and loser nomenclature. Note that winnerKey and winnerValue
   // logically form a tuple/pair, but they are split out into discrete properties for effieciency.
   type Pennant<'K, 'V> = 
      | Empty
      | Winner of winnerKey:'K * winnerValue: 'V * ltree:LoserTree<'K, 'V> * maxKey:'K
   
   and LoserTree<'K, 'V> =
      | Start 
      | Loser of loserKey:'K * loserValue:'V * left:LoserTree<'K, 'V> * splitKey:'K * right:LoserTree<'K, 'V>

   // An empty pennant.
   let empty<'K, 'V> = Empty

   // Returns a pennant containing the specified binding.
   let inline singleton key value = Winner( key, value, Start, key )

   // Returns the key of the binding with the maximum value in the pennant.  This is O(1).
   let maxKey = function
      | Empty -> invalidOp "empty pennant"
      | Winner( _, _, _, max) -> max

   // Merges two pennants into a new pennant, such that keys in the first tree are strictly smaller than keys in the
   // second tree. This is O(1).
   let private merge pennant1 pennant2 = 
      match pennant1, pennant2 with
      | Empty, _ -> pennant2
      | _, Empty -> pennant1
      | Winner( key1, value1, ltree1, max1), Winner( key2, value2, ltree2, max2) ->
         if value1 < value2 then
            Winner( key1, value1, (Loser(key2, value2, ltree1, max1, ltree2)), max2)
         else
            Winner( key2, value2, (Loser(key1, value1, ltree1, max1, ltree2)), max2)
          
//   let fromOrderedList bindings = 
//      bindings
//      |> Seq.fold( fun pennant binding ->
//         let s = singleton binding.Key binding.Value 
//         merge pennant s
//         ) empty

       
       
       
       

//   
//   let insert (k, v) = Pennant (k,v) 
//
//   let ofOrderedList (list: list<'K*'V>) = 
 //      let k, v = list.Head   
//      Pennant (k,v) 
//
//   let (|Empty|Min|) psq = 
