let pout p1 p2 = printfn "%A %A" p1 p2    
type Enum =
    | First = 0
    | Second = 1 

type Product =
    | OwnProduct of string
    | SupplierReference of int

let p1 = OwnProduct("bread")
let p2 = SupplierReference(53)
pout p1 p2
type Count = int

type StockBooking =
    | Incoming of Product * Count
    | Outgoing of Product * Count

let bookings =
    [
        Incoming(OwnProduct("rubber bands"), 50);
        Incoming(SupplierReference(112), 18);
        Outgoing(OwnProduct("pulleys"), 6);
        Outgoing(SupplierReference(37), 40);
    ]
let output bookings = printfn "%A" bookings
output bookings

//Sanity check to ensure 5 != 0
type System.Int32 with
    member x.IsZero = x = 0

let i = 5
printfn "%A" i.IsZero

let booking = Incoming(SupplierReference(63), 20)
//printfn "%A" (booking.IsIncomingBooking())
type StockBooking with
    member x.IsIncomingBooking() =
        match x with 
        | Incoming(_, _) -> true
        | _ -> false

printfn "%A" (booking.IsIncomingBooking())

// A Linked List implmentation based on a DU:
type 'a List = E | L of 'a * 'a List

let ints = L(10, L(12, L(15, E)))
printfn "%A" ints

let rec listSum = function
    | E -> 0
    | L(x, xs) -> x + (listSum xs)
printfn "%A" (listSum ints)

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code

