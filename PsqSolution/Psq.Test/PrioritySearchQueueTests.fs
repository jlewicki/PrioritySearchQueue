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
   member x.Should_Create_Queue() =
      let ordered = [("A", 3); ("B", 5); ("C", 1); ("D", 2); ("E", 2)]
      let psq = Q.ofOrderedSeq ordered
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


type ToSeq() = 
   [<Fact>]
   member x.Should_Return_Empty_Sequence_If_Queue_Is_Empty() = 
      Assert.True( Q.empty |> Q.toSeq |> Seq.isEmpty )

   [<Fact>]
   member x.Should_Return_Sequence_Containing_Same_Elements_As_Queue() =
      let items =  [("A", 3); ("B", 5); ("C", 1); ("D", 2); ("E", 2)] 
      let q = Q.ofOrderedSeq items
      let seq = q |> Q.toSeq
      Assert.Equal( q.Length, seq |> Seq.length )
      seq
      // Note that relative ordering of D/E is undefined.  This happens to be the ordering that is producded
      |> Seq.zip [("C", 1); ("E", 2); ("D", 2); ("A", 3); ("B", 5)] 
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




