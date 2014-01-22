namespace Psq.Test.PrioritySearchQueue
open Psq
open Xunit

module Q = Psq.PrioritySearchQueue

type Empty() =
   [<Fact>]
   member x.Should_Be_Empty() =
      Assert.Equal( 0, Q.empty.Length )
      Assert.True( Q.empty |> Q.isEmpty )
