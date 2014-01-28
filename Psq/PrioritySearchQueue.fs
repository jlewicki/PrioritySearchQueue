namespace Psq
open System
open System.Collections
open System.Collections.Generic


// An implementation of a priority search queue as described in the paper "A Simple Implementation Technique for 
// Priority Search Queues" by R. Hinze.
module internal PSQ = 
   // Set to true to assert that tree is always balanced.  
   let private validateBalance = false


   // The priority search queue is defined in terms of a semi-heap strucure called a pennant.  This pennant is described
   // in relation to a tournament tree, hence the winner and loser nomenclature. Note that winnerKey and winnerValue
   // logically form a tuple/pair, but they are split out into discrete properties for efficiency. Also note that
   // the loser tree has its length as a discrete property to ensure constant-time access.
   type Pennant<'K, 'V when 'V: comparison> = 
      | Void
      | Winner of winnerKey:'K * winnerValue: 'V * ltree:LoserTree<'K, 'V> * maxKey:'K
   and LoserTree<'K, 'V> =
      | Lf
      | Nd of loserKey:'K * loserValue:'V * left:LoserTree<'K, 'V> * splitKey:'K * right:LoserTree<'K, 'V> * length:int 


   // Returns the number of items in the tree.  This is O(1).
   let lengthTree = function
      | Lf -> 0
      | Nd(_, _, _, _, _,length) -> length


   // Smart tree constructors that hide length details.
   let node key value leftTree splitKey rightTree = 
      Nd(key, value, leftTree, splitKey, rightTree, 1 + lengthTree leftTree + lengthTree rightTree)      
   let leaf = Lf


   // Smart tree deconstructor.
   let (|Leaf|Node|) tree = 
      match tree with
      | Lf -> Leaf
      | Nd(key, value,left, splitKey, right, length) ->
         Node(key, value,left, splitKey, right)


   // An empty pennant.
   let empty : Pennant<'K, 'V> = Void


   // Returns a value indicating if the pennant is empty.  This is O(1).
   let isEmpty = function 
      | Void -> true
      | _ -> false


   // Returns the number of items in the pennant. This is O(1).
   let length = function
      | Void -> 0
      | Winner( _, _, Lf, _) -> 1
      | Winner( _, _, Nd(_, _, _, _, _,length), _) -> length + 1


   // Returns a pennant containing the specified key and value.
   let inline singleton key value = 
      Winner( key, value, Lf, key)


   // Returns a pennant containing the key and value of the specified pair.
   let inline ofTuple (key, value) = 
      singleton key value


   // Returns the minimum key and value in the pennant, or throws if pennant is empty.  This is O(1).
   let minBinding = function
      | Void -> invalidOp "empty pennant"
      | Winner( k, v, _, _) -> k, v


   // Returns the minimum key and value in the pennan, or None if the pennant is empty.  This is O(1).
   let peekMinBinding = function
      | Void -> None
      | Winner( k, v, _, _) -> Some(k, v)
      

   // Returns the key of the binding with the maximum value in the pennant.  This is O(1).
   let maxKey = function
      | Void -> invalidOp "empty pennant"
      | Winner( _, _, _, max) -> max

  
   // Returns the maximum depth of the tree.  This is O(N)
   let rec private depth = function
      | Leaf -> 1
      | Node( _, _, left, _, right) -> 1 + max (depth left) (depth right) 


   // Returns a tree node that balances the nodes in the left at right trees.
   // See the paper for more details, and more particularly:
   // Stephen Adams. Functional pearls: Efficient sets -- a balancing act
   let private balance key value left splitKey right = 

      let assertBalanced = function 
      | Leaf -> leaf
      | Node( _, _, left, _, right) as node ->
         if validateBalance then 
            let depthl = depth left
            let depthr = depth right
            assert ((abs (depthl - depthr)) <= 1)
         node

      // Rotation functions
      let singleLeft key value left splitKey right = 
         match right with
         | Leaf -> leaf
         | Node(key2, value2, left2, splitKey2, right2) -> 
            if key2 <= splitKey2 && value <= value2 then 
               node key value (node key2 value2 left splitKey left2) splitKey2 right2
            else 
               node key2 value2 (node key value left splitKey left2) splitKey2 right2
      
      let singleRight key value left splitKey right = 
         match left with
         | Leaf -> leaf
         | Node(key2, value2, left2, splitKey2, right2) -> 
            if key2 > splitKey2 && value <= value2 then 
               node key value left2 splitKey2 (node key2 value2 right2 splitKey right)
            else 
               node key2 value2 left2 splitKey2 (node key value right2 splitKey right)
          
      let doubleLeft key value left splitKey right = 
         match right with
         | Leaf -> leaf
         | Node(key2, value2, left2, splitKey2, right2) -> 
            singleLeft key value left splitKey (singleRight key2 value2 left2 splitKey2 right2)

      let doubleRight key value left splitKey right = 
         match left with
         | Leaf -> leaf
         | Node(key2, value2, left2, splitKey2, right2) -> 
            singleRight key value (singleLeft key2 value2 left2 splitKey2 right2) splitKey right

      // Balance functions
      let balanceLeft key value left splitKey right = 
         match right with
         | Leaf -> leaf
         | Node(key2, value2, left2, splitKey2, right2) ->
            if lengthTree left2 < lengthTree right2 then 
               singleLeft key value left splitKey right
            else 
               doubleLeft key value left splitKey right 

      let balanceRight key value left splitKey right = 
         match left with
         | Leaf -> leaf
         | Node(key2, value2, left2, splitKey2, right2) ->
            if lengthTree right2 < lengthTree left2 then 
               singleRight key value left splitKey right
            else 
               doubleRight key value left splitKey right 

      let weightFactor = 4
      let lenl = lengthTree left
      let lenr = lengthTree right

      let tree = 
         if lenl + lenr < 2 then 
            node key value left splitKey right
         elif lenr > weightFactor * lenl then 
            balanceLeft key value left splitKey right
         elif lenl > weightFactor * lenr then 
            balanceRight key value left splitKey right
         else 
            node key value left splitKey right
      assertBalanced tree


   // Merges two pennants and returns a new pennant, such that keys in the first tree are strictly smaller than keys 
   // in the second tree. This is O(1).
   let private merge pennant1 pennant2 = 
      match pennant1, pennant2 with
      | Void, _ -> pennant2
      | _, Void -> pennant1
      | Winner( key1, value1, ltree1, max1), Winner( key2, value2, ltree2, max2) ->
         if value1 < value2 then
            Winner( key1, value1, (balance key2 value2 ltree1 max1 ltree2), max2)
         else
            Winner( key2, value2, (balance key1 value1 ltree1 max1 ltree2), max2)
       

   // Returns a pennant containing values from the specified list, which *must* be sorted by key, in ascending order.
   // This is O(N).
   let fromOrderedList (bindings: list<'K *'V>) : Pennant<'K, 'V> = 
      // A variation of left and right fold that folds a list in a binary sub-division fashion, producing an almost 
      // balanced tree. The  expression tree  generated by foldm takes the form of a leaf-oriented Braun tree: for any 
      // given subexpression f l r , the left part l has either the same number of leaves as the right part, or one leaf 
      // more. 
      let foldm f state items =
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

      let asPennants = bindings |> List.map ofTuple
      asPennants
      |> foldm( fun pennant singleton ->
         merge pennant singleton ) empty


    // Active pattern for extracting the minumum value from the pennant.
   module PriorityQueueView = 
     
      // If the pennant is not empty, Min is returned and carries the key and value of the minimum entry in the pennant, 
      // and an updated pennant with the minumum entry removed.  Otherwise Empty is returned.
      let (|Empty|Min|) pennant = 

         // Returns the second best entry from the tree, by effectively 'replaying' the tournament without the winner.
         let rec secondBest loserTree key  = 
            match loserTree, key with
            | Leaf, _ -> Void
            | Node(loserKey, loserValue, ltree, splitKey, rtree), m ->
               if loserKey <= splitKey then
                  merge (Winner(loserKey, loserValue, ltree, splitKey)) (secondBest rtree m)
               else 
                  merge (secondBest ltree splitKey) (Winner(loserKey, loserValue, rtree, m))

         match pennant with
         | Void -> Empty
         | Winner(key, value, ltree, maxKey) -> Min( key, value, (secondBest ltree maxKey))


   // Active pattern for viewing the pennant as a tournament tree.  That is, a pennant is either
   // - Empty
   // - Singleton: A tree containing single entry
   // - Merged: The result of merging two pennants to determine a winner.  This is effectively the inverse of the
   // merge function.
   module TournamentView = 
      let (|Empty|Singleton|Merged|) pennant = 
         match pennant with
         | Void -> Empty
         | Winner(key, value, Leaf, _) -> Singleton(key, value)
         | Winner(key, value, (Node(lkey, lvalue, leftTree, splitKey, rightTree)), maxKey) ->         
            let pennant1, pennant2 = 
               if lkey <= splitKey then
                  Winner(lkey, lvalue, leftTree, splitKey), Winner(key, value, rightTree, maxKey)
               else
                  Winner(key, value, leftTree, splitKey), Winner(lkey, lvalue, rightTree, maxKey)
            Merged(pennant1, pennant2)


   // Returns the binding with the minimum value in the queue, and the pennant with that binding removed. This is
   // O(lgN).  
   let removeMin pennant = 
      match pennant with
      | PriorityQueueView.Empty -> None
      | PriorityQueueView.Min(k, v, rest) -> Some(k, v, rest)


   // Returns the value associated with the specified key in the pennant, or None if there is no such entry.  This is
   // O(lgN) on average.  
   let rec lookup key pennant = 
      match pennant with
      | PriorityQueueView.Min(k, v, _) when k = key -> Some(v)
      | TournamentView.Empty -> None
      | TournamentView.Singleton(k, v) -> None // k = key handled by Min case
      | TournamentView.Merged(pennant1, pennant2) ->
         if key <= maxKey pennant1 then lookup key pennant1
         else lookup key pennant2

   
   // Returns pennant, with the value of the specifiedd key adjusted by applying the specied function to the current 
   // value. This is O(lgN) on average.
   let rec adjust f key pennant = 
      match pennant with 
      | TournamentView.Empty -> 
         pennant
      | TournamentView.Singleton(k, v) -> 
         if k = key then singleton k (f v) else pennant
      | TournamentView.Merged(pennant1, pennant2) -> 
         if key <= maxKey pennant1 then 
            merge (adjust f key pennant1) pennant2
         else 
            merge pennant1 (adjust f key pennant2) 


   // Inserts the specified key and value into pennant, and returns an updated pennant.  If the pennant already 
   // contains the key, the corresponding value is replaced.  This is O(lgN) on average. 
   let rec insert key value pennant = 
      match pennant with 
      | TournamentView.Empty -> 
         singleton key value
      | TournamentView.Singleton(k, _) -> 
         if key < k then merge (singleton key value) pennant
         elif key = k then singleton key value  // Update existing value
         else merge pennant (singleton key value) 
      | TournamentView.Merged(pennant1, pennant2) -> 
         if key <= maxKey pennant1 then 
            merge (insert key value pennant1) pennant2
         else 
            merge pennant1 (insert key value pennant2) 


   // Removes the entry with the specified key from the pennant, and returns an updated pennant, and a flag indicating 
   // if an item was removed.
   let rec delete key pennant = 
      match pennant with 
      | TournamentView.Empty -> 
         pennant
      | TournamentView.Singleton(k, _) -> 
        if key = k then empty else pennant
      | TournamentView.Merged(pennant1, pennant2) -> 
         if key <= maxKey pennant1 then 
            merge (delete key pennant1) pennant2
         else 
            merge pennant1 (delete key pennant2)

   
   // Returns an ordered list of the keys in the pennant.
   let rec toOrderedList pennant =
      match pennant with
      | TournamentView.Empty -> []
      | TournamentView.Singleton(k, v) -> [k]
      | TournamentView.Merged(pennant1, pennant2) ->
        List.append (toOrderedList pennant1) (toOrderedList pennant2)


   // Returns a list of entries, ordered by key, that contain values from the specified pennant that are less than or 
   // equal to the specified value.
   let rec atMost value pennant = 
      match pennant with
      | PriorityQueueView.Min(_, v, _) when v > value -> []
      | TournamentView.Empty -> []
      | TournamentView.Singleton(k, v) -> [(k, v)] // Since we know v <= value
      | TournamentView.Merged(pennant1, pennant2) ->
         List.append (atMost value pennant1) (atMost value pennant2) 


   // Iterator class for a pennant that iterates bindings in order of increasing priority.  Complete iteration
   // is O(NlgN).
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
            | PriorityQueueView.Empty -> alreadyCompleted()
            | PriorityQueueView.Min( _, _, rest) ->
               currentPennant <- rest
               not(currentPennant |> isEmpty)
         else
             isStarted <- true;
             not (currentPennant |> isEmpty)
 
      interface IEnumerator<KeyValuePair<'K, 'V>> with
         member x.Current = current()
      interface IEnumerator with 
         member x.Current = box (current())
         member x.MoveNext() = moveNext()
         member x.Reset() = currentPennant <- pennant
      interface IDisposable with 
         member x.Dispose() = () 

      
// Documention in signature file
[<Sealed>]
type PrioritySearchQueue<'K, 'V when 'K: comparison and 'V: comparison> 
   internal( pennant: PSQ.Pennant<'K, 'V>  ) as this = 

   static let collectionIsReadOnly() = 
      new InvalidOperationException("The operation is not valid because the collection is read-only.")
   
   static let empty = 
      new PrioritySearchQueue<'K, 'V>( PSQ.empty )

   let lazyHashCode = lazy (
      let mutable hash = 1
      for x in this do
         hash <- 31 * hash + Unchecked.hash x
      hash )

   new (items: seq<KeyValuePair<'K, 'V>>) = 
      let pennant = 
         items 
         |> Seq.map (fun pair -> pair.Key, pair.Value ) 
         |> List.ofSeq 
         |> List.sortBy fst 
         |> PSQ.fromOrderedList
      new PrioritySearchQueue<'K, 'V>( pennant )

   override this.GetHashCode() = 
      lazyHashCode.Value 

   override this.Equals obj =
       match obj with
        | :? PrioritySearchQueue<'K, 'V> as other -> 
            if this.Length <> other.Length then false 
            elif this.GetHashCode() <> other.GetHashCode() then false
            else Seq.forall2 (Unchecked.equals) this other
        | _ -> false

   member this.Length = 
      PSQ.length pennant

   member this.IsEmpty = 
      this.Length = 0

   member this.Min = 
      PSQ.minBinding pennant

   member this.Keys = 
      PSQ.toOrderedList pennant
   
   member this.TryMin = 
      PSQ.peekMinBinding pennant

   member this.TryRemoveMin = 
      match PSQ.removeMin pennant with
      | Some(k, v, rest) -> Some(k, v, new PrioritySearchQueue<'K, 'V>( rest ))
      | None -> None

   member this.RemoveMin = 
      match PSQ.removeMin pennant with
      | Some(k, v, rest) -> k, v, new PrioritySearchQueue<'K, 'V>( rest )
      | None -> invalidOp "The queue is empty" 

   member this.Find key = 
      match PSQ.lookup key pennant with
      | Some(value) -> value
      | None -> raise (KeyNotFoundException(sprintf "%A" key))

   member this.TryFind key = 
      PSQ.lookup key pennant 
      
   member this.Item 
      with get(key:'K) = this.Find(key)

   member this.Add(key, value) =
      new PrioritySearchQueue<'K, 'V>( PSQ.insert key value pennant )

   member this.Remove key =
      new PrioritySearchQueue<'K, 'V>( PSQ.delete key pennant  )

   member this.AtMost value =
      PSQ.atMost value pennant 

   static member Empty : PrioritySearchQueue<'K, 'V> = 
      empty

   interface IEnumerable with
      member x.GetEnumerator() = 
         new PSQ.PennantEnumerator<'K, 'V>( pennant ) :> IEnumerator

   interface IEnumerable<KeyValuePair<'K, 'V>> with
      member x.GetEnumerator() = 
         new PSQ.PennantEnumerator<'K, 'V>( pennant ) :> IEnumerator<KeyValuePair<'K, 'V>>

   interface ICollection<KeyValuePair<'K, 'V>> with
      member this.Count = this.Length
      member this.IsReadOnly = true
      member this.Add item = raise <| collectionIsReadOnly()
      member this.Remove item = raise <| collectionIsReadOnly()
      member this.Clear() = raise <| collectionIsReadOnly()
      member this.Contains item =  
         (this.TryFind item.Key ).IsSome
      member this.CopyTo( array, i ) =
         let j = ref i 
         Seq.iter (fun x -> array.[!j] <- x; j := !j + 1) this

// Documention in signature file
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PrioritySearchQueue =
   
   let empty<'K, 'V when 'K: comparison and 'V: comparison> = 
      PrioritySearchQueue<'K, 'V>.Empty

   let isEmpty (queue:PrioritySearchQueue<'K, 'V>) = 
      queue.IsEmpty

   let ofOrderedSeq (items:seq<'K*'V>) = 
      new PrioritySearchQueue<'K, 'V>( PSQ.fromOrderedList (List.ofSeq items) )

   let ofSeq (items:seq<'K*'V>) = 
      items |> Seq.sortBy fst |> ofOrderedSeq

   let (|Empty|Min|) (queue:PrioritySearchQueue<'K, 'V>) =
      if queue.IsEmpty then Empty
      else Min(queue.RemoveMin)

   let min (queue:PrioritySearchQueue<'K, 'V>) = 
      queue.Min

   let tryMin (queue:PrioritySearchQueue<'K, 'V>) = 
      queue.TryMin

   let removeMin (queue:PrioritySearchQueue<'K, 'V>) = 
      queue.RemoveMin

   let tryRemoveMin (queue:PrioritySearchQueue<'K, 'V>) = 
      queue.TryRemoveMin

   let toSeq (queue:PrioritySearchQueue<'K, 'V>) = 
      queue
      |> Seq.map( fun pair -> pair.Key, pair.Value )

   let find (key:'K) (queue:PrioritySearchQueue<'K, 'V>) = 
      queue.Find key

   let tryFind (key:'K) (queue:PrioritySearchQueue<'K, 'V>) = 
      queue.TryFind key
      
   let add (key:'K) (value:'V) (queue:PrioritySearchQueue<'K, 'V>) = 
      queue.Add(key, value)

   let remove (key:'K) (queue:PrioritySearchQueue<'K, 'V>) = 
      queue.Remove key

   let keys (queue:PrioritySearchQueue<'K, 'V>) = 
      queue.Keys

   let atMost (value:'V) (queue:PrioritySearchQueue<'K, 'V>) = 
      queue.AtMost value

   let map (f:'K -> 'V -> 'V2) (queue:PrioritySearchQueue<'K, 'V>) =
      queue
      |> toSeq
      |> Seq.map (fun (k, v) -> k, (f k v))
      |> ofSeq
      