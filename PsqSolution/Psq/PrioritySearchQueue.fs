﻿namespace Psq
open System.Collections.Generic


module internal PSQ = 
   // An implementation of a priority search queue as described in the paper "A Simple Implementation Technique for 
   // Priority Search Queues" by R. Hinze.

   // The priority search queue is defined in terms of a semi-heap strucure called a pennant.  This pennant is described
   // in relation to a tournament tree, hence the winner and loser nomenclature. Note that winnerKey and winnerValue
   // logically form a tuple/pair, but they are split out into discrete properties for effieciency.
   type Pennant<'K, 'V when 'V: comparison> = 
      | Empty
      | Winner of winnerKey:'K * winnerValue: 'V * ltree:LoserTree<'K, 'V> * maxKey:'K
   
   and LoserTree<'K, 'V> =
      | Start 
      | Loser of loserKey:'K * loserValue:'V * left:LoserTree<'K, 'V> * splitKey:'K * right:LoserTree<'K, 'V>

   // An empty pennant.
   let empty : Pennant<'K, 'V> = Empty

   // Returns a pennant containing the specified key and value.
   let inline singleton key value = 
      Winner( key, value, Start, key )

   // Returns a pennant containing the key and value of the specified pair.
   let inline ofPair (pair: KeyValuePair<'K, 'V>) = 
      singleton pair.Key pair.Value

   // Returns the key of the binding with the maximum value in the pennant.  This is O(1).
   let maxKey = function
      | Empty -> invalidOp "empty pennant"
      | Winner( _, _, _, max) -> max

   // Merges two pennants and returns a new pennant, such that keys in the first tree are strictly smaller than keys 
   // in the second tree. This is O(1).
   let private merge pennant1 pennant2 = 
      match pennant1, pennant2 with
      | Empty, _ -> pennant2
      | _, Empty -> pennant1
      | Winner( key1, value1, ltree1, max1), Winner( key2, value2, ltree2, max2) ->
         if value1 < value2 then
            Winner( key1, value1, (Loser(key2, value2, ltree1, max1, ltree2)), max2)
         else
            Winner( key2, value2, (Loser(key1, value1, ltree1, max1, ltree2)), max2)

   // A variation of left and right fold that folds a list in a binary sub-division fashion, producing an almost 
   // balanced tree. The  expression tree  generated by foldm takes the form of a leaf-oriented Braun tree: for any 
   // given subexpression f l r , the left part l has either the same number of leaves as the right part, or one leaf 
   // more. 
   let private foldm f state items =
      match items with
      | [] -> state
      | _ -> 
         let rec recurse length items = 
            match length, items with
            | 1, (x::xs) -> x, xs
            | n, xs ->
               let m = n / 2
               let x1, xs1 = recurse (n - m) xs
               let x2, xs2 = recurse m xs1
               f x1 x2, xs2
         fst (recurse (List.length items) items)

            
   let fromOrderedList (bindings: list<KeyValuePair<'K, 'V>>) : Pennant<'K, 'V> = 
      let asPennants = bindings |> List.map ofPair
      asPennants
      |> foldm( fun pennant singleton ->
         merge pennant singleton ) empty


   //let (|Empty|Min|) 


//   
//   let insert (k, v) = Pennant (k,v) 
//
//   let ofOrderedList (list: list<'K*'V>) = 
 //      let k, v = list.Head   
//      Pennant (k,v) 
//
//   let (|Empty|Min|) psq = 
