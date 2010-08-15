#light

namespace Langexplr
open System.IO



type ParseResult<'a> =
   | Success of 'a * BinaryReader
   | Failure of int64 * string

type BinParser<'a> =
   | BinParser of (BinaryReader -> ParseResult<'a>)   
with
   member this.Function = 
            match this with
                 BinParser pFunc -> pFunc                 

end


type BinParserBuilder() =
    member this.Bind(p:BinParser<'a>,rest:'a -> BinParser<'b>) : BinParser<'b> = 
        BinParser(fun i -> match p.Function(i) with
                           | Success(r:'a,i2) -> ((rest r).Function i2) 
                           | Failure(offset,description) -> Failure(offset,description)
                   )
                                                                      
    member this.Return(x) = BinParser(fun i -> Success(x,i))

module BinParserModule =

 let IOExceptionHandlingWrapper(f:BinaryReader -> ParseResult<'a>) =
   fun i -> try 
              f(i)
            with
              (e & :? IOException ) -> Failure(i.BaseStream.Position,e.Message)


    
 let RByte =
    BinParser(IOExceptionHandlingWrapper(
                fun (i:BinaryReader) -> Success(i.ReadByte(),i)))
                   
 let RShort =
    BinParser(IOExceptionHandlingWrapper(
               fun (i:BinaryReader) -> Success(i.ReadInt16(),i)))

 let RInt =
    BinParser(IOExceptionHandlingWrapper(
               fun (i:BinaryReader) -> Success(i.ReadInt32(),i)))

 let AByte(b:byte) =
    BinParser(IOExceptionHandlingWrapper(
               fun (i:BinaryReader) ->
                  let rB = i.ReadByte() in
                      if (rB = b) then
                          Success(byte(rB),i)
                      else
                          Failure(i.BaseStream.Position,
                               System.String.Format("Expecting {0}, got {1}",b,rB))))
   

 let RByteBlock(length) =
    BinParser(IOExceptionHandlingWrapper(
                fun (i:BinaryReader) -> Success(i.ReadBytes(length),i)))


 let RZString =
   BinParser(IOExceptionHandlingWrapper(
                fun (i:BinaryReader) -> 
                   let mutable resultList = []
                   let mutable next = i.ReadByte()
                   while (next <> byte(0)) do
                      resultList <- next :: resultList
                      next <- i.ReadByte()
                   Success(resultList |> List.rev |> Array.ofList ,i)))

 let RZString2 =
   BinParser(IOExceptionHandlingWrapper(
                fun (i:BinaryReader) -> 
                   let mutable resultList = []
                   printf "-- %O\n" (i.BaseStream.Position)
                   let mutable next = i.ReadChar()
                   printf "-%O\n" next 
                   printf "-- %O\n" (i.BaseStream.Position)
                   while (int(next) <> 0) do
                      resultList <- next :: resultList
                      next <- i.ReadChar()
                      printf "-%O\n" next 
                   Success(resultList |> List.rev |> Array.ofList ,i)))


 let wrap(parser : BinParser<'a>,wrappingFunction : 'a -> 'b) =
     BinParser (
         fun (i:BinaryReader) ->
             match parser with
             | BinParser(parsingFunc) -> 
                    match (parsingFunc i) with 
                    | Success(x,y)-> Success((wrappingFunction x),y)
                    | Failure(x,y) -> Failure(x,y))

 let ParsingStep (func:'a -> BinParser<'b>) (accumulatedResult:ParseResult<'b list>) currentSeqItem =
   match accumulatedResult with
    | Success(result,inp) ->
            match ((func currentSeqItem).Function inp) with
              | Success(result2,inp2) -> Success(result2::result,inp2)
              | Failure(offset,description) -> Failure(offset,description)
    | Failure(offset,description) -> Failure(offset,description) 

   
 let FixedSequence (s:seq<'b>,parser:BinParser<'a>) =   
    BinParser(fun i ->
               match  (Seq.fold (ParsingStep (fun _ -> parser)) (Success([],i)) s) with
                   | Success(result,input) -> Success(List.rev(result),input)
                   | Failure(offset,description) -> Failure(offset,description))
                            
                      
