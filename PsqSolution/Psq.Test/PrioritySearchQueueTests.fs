namespace Psq.Test.PrioritySearchQueue
open System
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
