namespace Psq
open System.Collections
open System.Collections.Generic


/// A priority search queue is a immutable collection that combines the behavior of a priority queue and a map, so that 
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



