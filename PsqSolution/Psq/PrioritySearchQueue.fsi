﻿namespace Psq
open System.Collections
open System.Collections.Generic


/// A priority search queue is a immutable collection that combines the behavior of a priority queue and a associative 
/// map. Traditional priority queue operations such as constant time access to a minimum value, and dictionary operations
/// such as logarithmic access to a binding for a given key, are both supported.
[<Class>]
[<Sealed>]
type PrioritySearchQueue<'K, 'V when 'K: comparison and 'V: comparison> = 
    
    interface IEnumerable
    interface IEnumerable<KeyValuePair<'K, 'V>>

    /// O(1). Returns the number if items in this queue.
    member Length: int

    /// O(1). Returns true if this queue has no elements.
    member IsEmpty: bool

    /// O(1). Returns the entry with the minimum value in this queue. Throws an exception if the queue is empty.
    member Min: 'K*'V

    /// O(1). Returns the entry with the minimum value in this queue. Returns None if the queue is empty.
    member PeekMin: option<'K*'V>

    /// O(lgN). Returns the value associated with the specified key. Throws an exception if the queue does not contain 
    /// an entry with the key.
    member Find: key:'K -> 'V

    /// O(lgN). Returns the value associated with the specified key. Returns None if the queue does not contain an entry with 
    /// the key.
    member TryFind: key:'K -> option<'V>

    /// O(lgN). Adds the specified key and value to this queue, replacing an existing entry if necessary.
    member Add: key:'K * value:'V -> PrioritySearchQueue<'K, 'V>

    /// O(lgN). Removes the entry with the specified key to this queue, and returns an updated queue.  This queue is returned
    /// unchanged if there is no matching entry.
    member Remove: key:'K -> PrioritySearchQueue<'K, 'V>
 


/// Functional operators for <c>PrioritySearchQueue<_, _></c> type.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PrioritySearchQueue =   
   
   /// O(1). Returns a empty queue.
   [<GeneralizableValue>]
   val empty<'K, 'V when 'K: comparison and 'V: comparison> : PrioritySearchQueue<'K, 'V>

   /// O(1). Returns a value indicating if the queue is empty.
   val isEmpty: queue:PrioritySearchQueue<'K, 'V> -> bool

   /// O(N): Returns a new queue contanining the items in the specified sequence, which must be ordered by key, in 
   /// ascending order.
   val ofOrderedSeq: items:seq<'K*'V> -> PrioritySearchQueue<'K, 'V>

   /// O(1). Returns the entry with the minimum value in the queue. Throws an exception if the queue is empty.
   val min: queue:PrioritySearchQueue<'K, 'V> -> 'K*'V

   /// O(1). Returns the entry with the minimum value in the queue. Returns None if the queue is empty.
   val peekMin: queue:PrioritySearchQueue<'K, 'V> -> option<'K*'V>

   /// O(N). Returns a sequence that iterates the items in the queue.
   val toSeq: queue:PrioritySearchQueue<'K, 'V> -> seq<'K*'V>

   /// O(lgN). Returns the value associated with the specified key in the queue. Throws an exception if the queue does not 
   /// contain an entry with the key.
   val find: key:'K -> queue:PrioritySearchQueue<'K, 'V> -> 'V 

   /// O(lgN). Returns the value associated with the specified key in the queue. Returns None if the queue does not 
   /// contain an entry with the key.
   val tryFind: key:'K -> queue:PrioritySearchQueue<'K, 'V> -> option<'V>

   /// O(lgN). Adds the specified key and value to the queue, replacing an existing entry if necessary.
   val add: key:'K -> value:'V -> PrioritySearchQueue<'K, 'V> -> PrioritySearchQueue<'K, 'V>

   /// O(lgN). Removes the entry with the specified key to the queue, and returns an updated queue.  The queue is returned
   /// unchanged if there is no matching entry.
   val remove: key:'K -> PrioritySearchQueue<'K, 'V> -> PrioritySearchQueue<'K, 'V>

