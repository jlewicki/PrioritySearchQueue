namespace Psq.Test.PrioritySearchQueue
open System
open System.Collections.Generic
open Psq
open Xunit
module Q = Psq.PrioritySearchQueue

type Empty() =
   [<Fact>]
   member x.Should_Be_Empty() =
      Assert.Equal( 0, Q.empty.Length )
      Assert.True( Q.empty |> Q.isEmpty )


type OfOrderedSeq() =
   [<Fact>]
   member x.Should_Create_Empty() =
      let psq = Q.ofOrderedSeq []
      Assert.True (psq.IsEmpty)

   [<Fact>]
   member x.Should_Create_Queue() =
      let ordered = [("A", 3); ("B", 5); ("C", 1); ("D", 2); ("E", 2)]
      let psq = Q.ofOrderedSeq ordered
      Assert.Equal(5, psq.Length )
      let k, v = Q.min psq
      Assert.Equal<string>( "C", k )
      Assert.Equal( 1, v )


type OfSeq() =
   [<Fact>]
   member x.Should_Create_Empty() =
      let psq = Q.ofSeq []
      Assert.True (psq.IsEmpty)

   [<Fact>]
   member x.Should_Create_Queue() =
      let ordered = [("A", 3); ("E", 2); ("C", 1); ("D", 2); ("B", 5); ]
      let psq = Q.ofSeq ordered
      Assert.Equal(5, psq.Length )
      let k, v = Q.min psq
      Assert.Equal<string>( "C", k )
      Assert.Equal( 1, v )


 type Min() = 
   [<Fact>]
   member x.Should_Return_Binding_With_Min_Value() =
      let psq = 
         [("A", 3); ("B", 5); ("C", 1); ("D", 2); ("E", 2)] 
         |> Q.ofOrderedSeq
      let k, v = Q.min psq
      Assert.Equal<string>( "C", k )
      Assert.Equal( 1, v )

   [<Fact>]
   member x.Should_Throw_For_Empty_Queue() = 
      Assert.Throws<InvalidOperationException>( fun() ->
         Q.empty |> Q.min |> ignore )


type RemoveMin() = 
   [<Fact>]
   member x.Should_Return_Binding_With_Min_Value_And_Rest_Of_Queue() =
      let psq = 
         [("A", 3); ("B", 5); ("C", 1); ("D", 2); ("E", 2)] 
         |> Q.ofOrderedSeq
      let k, v, rest = Q.removeMin psq
      Assert.Equal<string>( "C", k )
      Assert.Equal( 1, v )
      Assert.Equal( 4, rest.Length )
      rest 
      |> Q.toSeq
      |> Seq.zip [("A", 3); ("B", 5); ("D", 2); ("E", 2)] 
      |> Seq.iter Assert.Equal

   [<Fact>]
   member x.Should_Throw_For_Empty_Queue() = 
      Assert.Throws<InvalidOperationException>( fun() ->
         Q.empty |> Q.removeMin |> ignore )


type ToSeq() = 
   [<Fact>]
   member x.Should_Return_Empty_Sequence_If_Queue_Is_Empty() = 
      Assert.True( Q.empty |> Q.toSeq |> Seq.isEmpty )

   [<Fact>]
   member x.Should_Return_Sequence_Containing_Same_Elements_As_Queue__In_Ascending_Key_Order() =
      let items =  [("C", 3); ("B", 5); ("E", 1); ("D", 4); ("A", 2)] 
      let q = Q.ofSeq items
      let seq = q |> Q.toSeq
      Assert.Equal( q.Length, seq |> Seq.length )
      seq
      |> Seq.zip [("A", 2); ("B", 5); ("C", 3); ("D", 4); ("E", 1)] 
      |> Seq.iter Assert.Equal


type TryFind() = 
   [<Fact>]
   member x.Should_Find_Value_For_Existing_Key() = 
      let items = [("A", 3); ("B", 5); ("C", 1); ("D", 2); ("E", 2)] 
      let q = Q.ofOrderedSeq items
      items
      |> List.iter (fun (k, v) -> 
         let optValue = q.TryFind(k)
         Assert.True( optValue.IsSome )
         Assert.Equal( v, optValue.Value ) )

   [<Fact>]
   member x.Should_Return_None_For_Missing_Key() = 
     let items = [("A", 3); ("B", 5); ("C", 1);] 
     let q = Q.ofOrderedSeq items

     let optValue = q.TryFind("D")
     Assert.True( optValue.IsNone )


type Find() = 
   [<Fact>]
   member x.Should_Find_Value_For_Existing_Key() = 
      let items = [("A", 3); ("B", 5); ("C", 1); ("D", 2); ("E", 2)] 
      let q = Q.ofOrderedSeq items
      items
      |> List.iter (fun (k, v) -> 
         let value = q.Find(k)
         Assert.Equal( v, value ) )

   [<Fact>]
   member x.Should_Throw_For_Missing_Key() = 
     let items = [("A", 3); ("B", 5); ("C", 1);] 
     let q = Q.ofOrderedSeq items
      
     Assert.Throws<KeyNotFoundException>( fun() ->
         q.Find "D" |> ignore )


 type Add() = 
   [<Fact>]
   member x.Should_Add_Entry_To_Queue() =
      let items = [("A", 3); ("B", 5); ("C", 2);] 
      let q = Q.ofOrderedSeq items

      let newQ = q |> Q.add "AA" 1
      let minKey, minVal = newQ.Min
      Assert.Equal( 4, newQ.Length )
      Assert.Equal<string>( "AA", minKey )
      Assert.Equal( 1, minVal )
      Assert.Equal( 1, newQ.Find "AA")

   [<Fact>]
   member x.Should_Update_Existing_Entry() =
      let items = [("A", 3); ("B", 5); ("C", 2);] 
      let q = Q.ofOrderedSeq items

      let newQ = q |> Q.add "A" 1
      let minKey, minVal = newQ.Min
      Assert.Equal( 3, newQ.Length )
      Assert.Equal<string>( "A", minKey )
      Assert.Equal( 1, minVal )
      Assert.Equal( 1, newQ.Find "A")


type Remove() = 
   [<Fact>]
   member x.Should_Remove_Entry_From_Queue() =
      let items = [("A", 3); ("B", 5); ("C", 2);] 
      let q = Q.ofOrderedSeq items

      let newQ = q |> Q.remove "C"
      let minKey, minVal = newQ.Min
      Assert.Equal( 2, newQ.Length )
      Assert.Equal<string>( "A", minKey )
      Assert.Equal( 3, minVal )
      Assert.True( (newQ.TryFind "C").IsNone )

   [<Fact>]
   member x.Should_Return_Queue_Unchanged_If_Key_Not_Found() =
      let items = [("A", 3); ("B", 5); ("C", 2);] 
      let q = Q.ofOrderedSeq items

      let newQ = q |> Q.remove "D"
   
      Assert.Equal( 3, newQ.Length )
      let minKey, minVal = newQ.Min
      Assert.Equal<string>( "C", minKey )
      Assert.Equal( 2, minVal )
      Assert.True( (newQ.TryFind "A").IsSome )
      Assert.True( (newQ.TryFind "B").IsSome )
      Assert.True( (newQ.TryFind "C").IsSome )


type AtMost() =
   [<Fact>]
   member x.Should_Return_List_With_Values_LTEQ_Value() =
      let items = [("A", 3); ("B", 5); ("C", 1); ("D", 2); ("E", 2)] 
      let q = Q.ofOrderedSeq items

      let entries = Q.atMost 3 q
      Assert.Equal( 4, entries.Length )
      entries
      |> List.zip [("A", 3); ("C", 1); ("D", 2); ("E", 2)] 
      |> List.iter (fun ((expKey, expValue), (key, value)) ->
         Assert.Equal<string>(expKey, key)
         Assert.Equal(expValue, value) )


   [<Fact>]
   member x.Should_Return_Empty_List_If_Value_Is_LT_Min() =
      let items = [("A", 3); ("B", 5); ("C", 1); ("D", 2); ("E", 2)] 
      let q = Q.ofOrderedSeq items

      let entries = Q.atMost 0 q
      Assert.Equal( 0, entries.Length )
      

type Keys() = 
   [<Fact>]
   member x.Should_Return_List_Of_Keys_In_Sorted_Order() =
      let items = [("C", 3); ("F", 5); ("E", 1); ("D", 2); ("B", 2); ("A", 2);] 
      let q = Q.ofSeq items

      let keys = Q.keys q

      Assert.Equal(6, keys.Length )
      keys
      |> List.zip ["A"; "B"; "C"; "D"; "E"; "F"] 
      |> List.iter (fun (expKey, key) ->
         Assert.Equal<string>(expKey, key))


type Equals() = 
   [<Fact>]
   member x.Should_Use_Value_Equality_Semantics() =
      let items = [("A", 3); ("B", 5); ("C", 1); ("D", 2); ("E", 2)] 
      let q1 = Q.ofOrderedSeq items
      let q2 = Q.ofSeq items
      Assert.True( Object.Equals(q1, q2))


type GetHashCode() = 
   [<Fact>]
   member x.Should_Use_Value_Equality_Semantics() =
      let items = [("A", 3); ("B", 5); ("C", 1); ("D", 2); ("E", 2)] 
      let q1 = Q.ofOrderedSeq items
      let q2 = Q.ofSeq items
      Assert.True( Object.Equals(q1.GetHashCode(), q2.GetHashCode()))


type Iter() =
   [<Fact>]
   member x.Should_F_To_Entries_In_Ascending_Key_Order() = 
      let items = [("C", 3); ("F", 5); ("E", 1); ("B", 2); ("D", 2); ("A", 2);] 
      let q = Q.ofSeq items
      let expected = ref [("A", 2); ("B", 2); ("C", 3); ("D", 2); ("E", 1); ("F", 5);] 
      q
      |> Q.iter (fun k v ->
         match !expected with
         | (expKey, expVal)::rest ->
            Assert.Equal<string>(expKey, k)
            Assert.Equal(expVal, v)
            expected := rest
         | _ -> invalidOp "" )


type Contains() = 
   [<Fact>]
   member x.Should_Return_True_If_Entry_Exists() = 
      let items = [("A", 3); ("B", 5); ("C", 1); ("D", 2); ("E", 2)] 
      let q = Q.ofOrderedSeq items :> ICollection<KeyValuePair<string, int>>
      Assert.True( q.Contains( new KeyValuePair<string, int>("A", 3)))

   [<Fact>]
   member x.Should_Return_False_If_Entry_Does_Not_Exist() = 
      let items = [("A", 3); ("B", 5); ("C", 1); ("D", 2); ("E", 2)] 
      let q = Q.ofOrderedSeq items :> ICollection<KeyValuePair<string, int>>
      Assert.False( q.Contains( new KeyValuePair<string, int>("A", 4)))
      Assert.False( q.Contains( new KeyValuePair<string, int>("F", 1)))
