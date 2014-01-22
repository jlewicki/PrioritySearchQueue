﻿namespace Psq
open System
open System.Collections
open System.Collections.Generic


// An implementation of a priority search queue as described in the paper "A Simple Implementation Technique for 
// Priority Search Queues" by R. Hinze.
module internal PSQ = 

   // The priority search queue is defined in terms of a semi-heap strucure called a pennant.  This pennant is described
   // in relation to a tournament tree, hence the winner and loser nomenclature. Note that winnerKey and winnerValue
   // logically form a tuple/pair, but they are split out into discrete properties for efficiency. Note additionaly that
   // the pennant its length as a discrete property to ensure constant-time access.
   type Pennant<'K, 'V when 'V: comparison> = 
      | Void
      | Winner of winnerKey:'K * winnerValue: 'V * ltree:LoserTree<'K, 'V> * maxKey:'K * length:int
   and LoserTree<'K, 'V> =
      | Start 
      | Loser of loserKey:'K * loserValue:'V * left:LoserTree<'K, 'V> * splitKey:'K * right:LoserTree<'K, 'V> 


   // An empty pennant.
   let empty : Pennant<'K, 'V> = Void


   // Returns a value indicating if the pennant is empty.  This is O(1).
   let isEmpty = function 
      | Void -> true
      | _ -> false


   // Returns the number of items in the queue. This is O(1).
   let length = function
      | Void -> 0
      | Winner( _, _, _, _, l) -> l


   // Returns a pennant containing the specified key and value.
   let inline singleton key value = 
      Winner( key, value, Start, key, 1 )


   // Returns a pennant containing the key and value of the specified pair.
   let inline ofTuple (key, value) = 
      singleton key value


   // Returns the minimum key and value in the pennant, or throws if pennant is empty.  This is O(1).
   let minBinding = function
      | Void -> invalidOp "empty pennant"
      | Winner( k, v, _, _, _) -> k, v


   // Returns the minimum key and value in the pennan, or None if the pennant is empty.  This is O(1).
   let peekMinBinding = function
      | Void -> None
      | Winner( k, v, _, _, _) -> Some(k, v)
      

   // Returns the key of the binding with the maximum value in the pennant.  This is O(1).
   let maxKey = function
      | Void -> invalidOp "empty pennant"
      | Winner( _, _, _, max, _) -> max

 
   // Merges two pennants and returns a new pennant, such that keys in the first tree are strictly smaller than keys 
   // in the second tree. This is O(1).
   let private merge pennant1 pennant2 = 
      match pennant1, pennant2 with
      | Void, _ -> pennant2
      | _, Void -> pennant1
      | Winner( key1, value1, ltree1, max1, length1), Winner( key2, value2, ltree2, max2, length2) ->
         if value1 < value2 then
            Winner( key1, value1, (Loser(key2, value2, ltree1, max1, ltree2)), max2, length1 + length2)
         else
            Winner( key2, value2, (Loser(key1, value1, ltree1, max1, ltree2)), max2, length1 + length2)


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


   // Returns a pennant containing values from the specified list, which *must* be sorted by key, in ascending order.
   // This is O(N).
   let fromOrderedList (bindings: list<'K *'V>) : Pennant<'K, 'V> = 
      let asPennants = bindings |> List.map ofTuple
      asPennants
      |> foldm( fun pennant singleton ->
         merge pennant singleton ) empty


   // Active pattern for extracting the minumum value from the pennant.
   // If the pennant is not empty, Min is returned and carries the key and value of the minimum entry in the pennant, 
   // and an updated pennant with the minumum entry removed.  Otherwise Empty is returned.
   let (|Empty|Min|) pennant = 

      // Returns the second best entry from the tree, by effectively 'replaying' the tournament without the winner.
      let rec secondBest loserTree key length= 
         match loserTree, key with
         | Start, _ -> Void
         | Loser(loserKey, loserValue, ltree, splitKey, rtree), m ->
            if loserKey <= splitKey then
               merge (Winner(loserKey, loserValue, ltree, splitKey, length)) (secondBest rtree m (length-1))
            else 
               merge (secondBest ltree splitKey (length-1)) (Winner(loserKey, loserValue, rtree, m, length))

      match pennant with
      | Void -> Empty
      | Winner(key, value, ltree, maxKey, length) -> Min( key, value, (secondBest ltree maxKey (length-1)))

   
   // Iterator class for a pennant
   type PennantEnumerator<'K, 'V when 'K: comparison and 'V: comparison> ( pennant : Pennant<'K, 'V> ) =
      let notStarted() = 
         raise <| new InvalidOperationException("The enumerator has not been started by a call to MoveNext")
      let alreadyCompleted() = 
         raise <| new InvalidOperationException("The enumerator has already completed.")

      let mutable currentPennant = pennant
      let mutable isStarted = false

      // Get the current item in the enumerator
      let current() =
         if isStarted then 
            let k, v = minBinding currentPennant
            new KeyValuePair<'K, 'V>(k, v)
         else notStarted()
      
      // Positions the enumerator at the next item in the collection
      let moveNext() =
         if isStarted then 
            match currentPennant with 
            | Empty -> alreadyCompleted()
            | Min( _, _, rest) ->
               currentPennant <- rest
               not(currentPennant |> isEmpty)
         else
             isStarted <- true;  // The first call to MoveNext "starts" the enumeration.
             not (currentPennant |> isEmpty)
 
      interface IEnumerator<KeyValuePair<'K, 'V>> with
         member x.Current = current()
      interface IEnumerator with 
         member x.Current = box (current())
         member x.MoveNext() = moveNext()
         member x.Reset() = currentPennant <- pennant
      interface IDisposable with 
         member x.Dispose() = () 

      

[<Sealed>]
type PrioritySearchQueue<'K, 'V when 'K: comparison and 'V: comparison> internal( pennant: PSQ.Pennant<'K, 'V>  ) = 

   static let collectionIsReadOnly() = new NotSupportedException("The operation is not valid because the collection is read-only")
   
   static let empty = new PrioritySearchQueue<'K, 'V>( PSQ.empty )

   member this.Length = PSQ.length pennant

   member this.IsEmpty = this.Length = 0

   member this.Min = PSQ.minBinding pennant

   member this.PeekMin = PSQ.peekMinBinding pennant
      
   static member Empty : PrioritySearchQueue<'K, 'V> = empty

   interface IEnumerable with
      member x.GetEnumerator() = 
         new PSQ.PennantEnumerator<'K, 'V>( pennant ) :> IEnumerator

   interface IEnumerable<KeyValuePair<'K, 'V>> with
      member x.GetEnumerator() = 
         new PSQ.PennantEnumerator<'K, 'V>( pennant ) :> IEnumerator<KeyValuePair<'K, 'V>>


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PrioritySearchQueue =
   
   let empty<'K, 'V when 'K: comparison and 'V: comparison> = 
      PrioritySearchQueue<'K, 'V>.Empty

   let isEmpty (queue:PrioritySearchQueue<'K, 'V>) = 
      queue.IsEmpty

   let ofOrderedSeq (items:seq<'K*'V>) = 
      new PrioritySearchQueue<'K, 'V>( PSQ.fromOrderedList (List.ofSeq items) )

   let min (queue:PrioritySearchQueue<'K, 'V>) = 
      queue.Min

   let peekMin (queue:PrioritySearchQueue<'K, 'V>) = 
      queue.PeekMin
      