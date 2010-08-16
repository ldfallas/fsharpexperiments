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
    member this.Bind(p : BinParser<'a>,
                     rest : 'a -> BinParser<'b>) : BinParser<'b> = 
        BinParser(fun i -> match p.Function(i) with
                           | Success(r:'a, i2) -> ((rest r).Function i2) 
                           | Failure(offset, description) ->
                                Failure(offset, description)
                   )
                                                                      
    member this.Return(x) = BinParser(fun i -> Success(x,i))

module BinParserModule =

 let IOExceptionHandlingWrapper(f : BinaryReader -> ParseResult<'a>) =
   fun reader ->
            try 
              f(reader)
            with
              (e & :? IOException ) -> Failure(reader.BaseStream.Position,
                                               e.Message)


    
 let RByte =
    BinParser(IOExceptionHandlingWrapper(
                fun (reader : BinaryReader) -> Success(reader.ReadByte(),
                                                       reader)))
                   
 let RShort =
    BinParser(IOExceptionHandlingWrapper(
               fun (reader : BinaryReader) -> Success(reader.ReadInt16(),
                                                      reader)))

 let RInt =
    BinParser(IOExceptionHandlingWrapper(
               fun (reader : BinaryReader) -> Success(reader.ReadInt32(),
                                                      reader)))

 let AByte(expected : byte) =
    BinParser(IOExceptionHandlingWrapper(
               fun (reader : BinaryReader) ->
                  let actual = reader.ReadByte() in
                      if (actual = expected) then
                          Success(byte(actual), reader)
                      else
                          Failure(reader.BaseStream.Position,
                                  System.String.Format(
                                            "Expecting {0}, got {1}",
                                            expected,
                                            actual))))
   

 let RByteBlock(length) =
    BinParser(IOExceptionHandlingWrapper(
                fun (reader:BinaryReader) -> Success(reader.ReadBytes(length),
                                                     reader)))


 let RZString =
   BinParser(IOExceptionHandlingWrapper(
                fun (reader:BinaryReader) -> 
                   let mutable resultList = []
                   let mutable next = reader.ReadChar()
                   while (int(next) <> 0) do
                      resultList <- next :: resultList
                      next <- reader.ReadChar()
                   Success(resultList |> List.rev |> Array.ofList ,reader)))

 let RDWordAlignData(blockSize) =
     if ( (blockSize &&& 3)  <> 0) then
        RByteBlock(4 - (blockSize &&& 3) )
     else
        BinParser(fun reader -> Success([||], reader)) 


 let wrap(parser : BinParser<'a>,wrappingFunction : 'a -> 'b) =
     BinParser (
         fun (reader:BinaryReader) ->
             match parser with
             | BinParser(parsingFunc) -> 
                    match (parsingFunc reader) with 
                    | Success(x,y)-> Success((wrappingFunction x),y)
                    | Failure(x,y) -> Failure(x,y))

 let ParsingStep (func:'a -> BinParser<'b>)
                 (accumulatedResult : ParseResult<'b list>) currentSeqItem =
   match accumulatedResult with
    | Success(result, inp) ->
            match ((func currentSeqItem).Function inp) with
              | Success(result2, inp2) -> Success(result2::result, inp2)
              | Failure(offset, description) -> Failure(offset, description)
    | Failure(offset, description) -> Failure(offset, description) 

   
 let FixedSequence (s : seq<'b>, parser:BinParser<'a>) =   
    BinParser(fun reader ->
               match  (Seq.fold (ParsingStep (fun _ -> parser))
                                (Success([],reader)) s) with
                   | Success(result, input) ->
                          Success(List.rev(result), input)
                   | Failure(offset, description) ->
                          Failure(offset,description))
                            
                      
