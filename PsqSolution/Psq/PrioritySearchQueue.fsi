namespace Psq
open System.Collections
open System.Collections.Generic


/// A priority search queue is a immutable collection that combines the behavior of a priority queue and a map, so that 
[<Class>]
[<Sealed>]
type PrioritySearchQueue<'K, 'V when 'K: comparison and 'V: comparison> = 
    
    interface IEnumerable
    interface IEnumerable<KeyValuePair<'K, 'V>>

    /// O(1). Returns the number if items in the queue.
    member Length: int

    /// O(1). Returns true if the heap has no elements.
    member IsEmpty : bool


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PrioritySearchQueue =   
   
   /// O(1). Returns a empty queue.
   [<GeneralizableValue>]
   val empty<'K, 'V when 'K: comparison and 'V: comparison> : PrioritySearchQueue<'K, 'V>



