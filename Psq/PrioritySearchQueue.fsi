﻿namespace Psq
open System.Collections
open System.Collections.Generic


/// A priority search queue is an immutable collection that combines the behavior of a priority queue and a associative 
/// map. Traditional priority queue operations such as constant time access to a minimum value, and dictionary operations
/// such as logarithmic access to a binding for a given key, are both supported.
[<Class>]
[<Sealed>]
type PrioritySearchQueue<'K, 'V when 'K: comparison and 'V: comparison> = 
    
    interface IEnumerable
    interface IEnumerable<KeyValuePair<'K, 'V>>
    interface ICollection<KeyValuePair<'K, 'V>>

    /// O(N^2), O(NlgN) on average. Creates a new queue containing the entries in the specified collection.
    new: items:seq<KeyValuePair<'K, 'V>> -> PrioritySearchQueue<'K, 'V>

    /// O(1). Returns the number if items in this queue.
    member Length: int

    /// O(1). Returns true if this queue has no elements.
    member IsEmpty: bool

    /// O(1). Returns the entry with the minimum value in this queue. Throws an exception if the queue is empty.
    member Min: 'K*'V

    /// O(1). Returns the entry with the minimum value in this queue. Returns None if the queue is empty.
    member TryMin: option<'K*'V>

    /// O(lgN). Returns the entry with the minimum value in this queue, and a queue with that entry removed.  Throws
    /// an exception if the queue is empty.
    member RemoveMin: 'K * 'V * PrioritySearchQueue<'K, 'V>

    /// O(lgN). Returns the entry with the minimum value in this queue, and the queue with that entry removed. Returns
    /// None if the queue is empty.
    member TryRemoveMin: option<'K * 'V * PrioritySearchQueue<'K, 'V>>

    /// O(lgN). Returns the value associated with the specified key. Throws an exception if the queue does not contain 
    /// an entry with the key.
    member Find: key:'K -> 'V

    /// O(lgN). Returns the value associated with the specified key. Returns None if the queue does not contain an 
    /// entry with the key.
    member TryFind: key:'K -> option<'V>

    /// O(lgN). Indexer that returns the value associated with the specified key. Throws an exception if the queue 
    /// does not contain an entry with the key.
    member Item : key:'K -> 'V with get

    /// O(lgN). Adds the specified key and value to this queue, replacing an existing entry if necessary.
    member Add: key:'K * value:'V -> PrioritySearchQueue<'K, 'V>

    /// O(lgN). Removes the entry with the specified key to this queue, and returns an updated queue.  This queue is 
    /// returned unchanged if there is no matching entry.
    member Remove: key:'K -> PrioritySearchQueue<'K, 'V>

    /// O(N) worst case. Returns a list of entries, in ascending order by key, that contain values from this queue that
    /// are less than or equal to the specified value.
    member AtMost: value:'V -> list<'K*'V>
 
    /// O(N). Returns a queue containing entries from this queue for which the specified predicate
    /// function returns true.
    member Filter: pred:('K -> 'V -> bool) -> PrioritySearchQueue<'K, 'V>

    /// O(N). Applies a function to each entry in this queue, threading an accumulator argument through the computation. 
    /// This function takes the second argument, and applies the function to it and the first entry in this queue (in 
    /// order of ascending keys). Then, it passes this result into the function along with the second element, and so 
    /// on.
    member Fold: f:('A -> 'K -> 'V -> 'A) -> 'A -> 'A


/// Functional operators for <c>PrioritySearchQueue<_, _></c> type.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PrioritySearchQueue =   
   
   /// O(1). Returns a empty queue.
   [<GeneralizableValue>]
   val empty<'K, 'V when 'K: comparison and 'V: comparison> : PrioritySearchQueue<'K, 'V>

   /// O(1). Returns a value indicating if the queue is empty.
   val isEmpty: queue:PrioritySearchQueue<'K, 'V> -> bool

   /// O(1). Returns the number of entries in the queue.
   val length: queue:PrioritySearchQueue<'K, 'V> -> int

   /// O(N^2), O(NlgN) on average.  Returns a new queue containing the items in the specified sequence
   val ofSeq: items:seq<'K*'V> -> PrioritySearchQueue<'K, 'V>

   /// O(N): Returns a new queue containing the items in the specified sequence, which must be ordered by key, in 
   /// ascending order.
   val ofOrderedSeq: items:seq<'K*'V> -> PrioritySearchQueue<'K, 'V>

   /// O(1). Returns the entry with the minimum value in the queue. Throws an exception if the queue is empty.
   val min: queue:PrioritySearchQueue<'K, 'V> -> 'K*'V

   /// O(1). Returns the entry with the minimum value in the queue. Returns None if the queue is empty.
   val tryMin: queue:PrioritySearchQueue<'K, 'V> -> option<'K*'V>

   /// O(lgN). Returns the entry with the minimum value in the queue, and the queue with that entry removed. Throws
   /// an exception if the queue is empty.
   val removeMin: queue:PrioritySearchQueue<'K, 'V> -> 'K * 'V * PrioritySearchQueue<'K, 'V>

   /// O(lgN). Returns the entry with the minimum value in the queue, and the queue with that entry removed. Returns
   /// None if the queue is empty.
   val tryRemoveMin: queue:PrioritySearchQueue<'K, 'V> -> option<'K * 'V * PrioritySearchQueue<'K, 'V>>

   /// O(1), iteration is O(NlgN). Returns a sequence that iterates the entries in the queue in order of increasing value.
   val toSeq: queue:PrioritySearchQueue<'K, 'V> -> seq<'K*'V>

   /// O(lgN). Returns the value associated with the specified key in the queue. Throws an exception if the queue does 
   /// not contain an entry with the key.
   val find: key:'K -> queue:PrioritySearchQueue<'K, 'V> -> 'V 

   /// O(lgN). Returns the value associated with the specified key in the queue. Returns None if the queue does not 
   /// contain an entry with the key.
   val tryFind: key:'K -> queue:PrioritySearchQueue<'K, 'V> -> option<'V>

   /// O(lgN). Adds the specified key and value to the queue, replacing an existing entry if necessary.
   val add: key:'K -> value:'V -> PrioritySearchQueue<'K, 'V> -> PrioritySearchQueue<'K, 'V>

   /// O(lgN). Removes the entry with the specified key to the queue, and returns an updated queue.  The queue is returned
   /// unchanged if there is no matching entry.
   val remove: key:'K -> PrioritySearchQueue<'K, 'V> -> PrioritySearchQueue<'K, 'V>

   /// O(N). Returns a list of the keys in the specified queue, in ascending order.
   val keys: PrioritySearchQueue<'K, 'V> -> list<'K>

   /// O(N) worst case. Returns a list of entries, in ascending order by key, that contain values from the specified 
   /// queue that are less than or equal to the specified value.
   val atMost: value:'V -> PrioritySearchQueue<'K, 'V> -> list<'K*'V>

   /// O(NlgN). Returns a new queue whose values are the results of applying the given function to each of the 
   /// elements of the queue. The key passed to the function indicates the key of element being transformed. 
   val map: f:('K -> 'V -> 'V2) -> PrioritySearchQueue<'K, 'V> -> PrioritySearchQueue<'K, 'V2>

   /// O(N). Applies the function to each entry in the queue, in order of ascending key value.
   val iter: f:('K -> 'V -> unit) -> PrioritySearchQueue<'K, 'V> -> unit

   /// O(N). Returns a queue containing entries from the specified queue for which the specified predicate
   /// function returns true.
   val filter: pred:('K -> 'V -> bool) -> PrioritySearchQueue<'K, 'V> -> PrioritySearchQueue<'K, 'V>

   /// O(N). Applies a function to each entry in the queue, threading an accumulator argument through the computation. 
   /// This function takes the second argument, and applies the function to it and the first entry in the queue (in 
   /// order of ascending keys). Then, it passes this result into the function along with the second element, and so 
   /// on.
   val fold: f:('A -> 'K -> 'V -> 'A) -> 'A -> PrioritySearchQueue<'K, 'V> -> 'A

