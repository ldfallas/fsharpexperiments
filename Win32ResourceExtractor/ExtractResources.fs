open System.IO

open Langexplr
open Langexplr.BinParserModule
open System.IO

type ResourceId =
     | Numeric of int16
     | Name of char[]

let sizeOfResourceId resId = 
   match resId with
   | Numeric(_) -> 2 
   | Name(dat) -> dat.Length*2 + 2

//let fileName = "resCursorImages.RES" 
//let fileName = "myres.RES" 
let fileName = "res.res"

let pBuilder = new BinParserBuilder()


let resourceParser = pBuilder {
     let! dataSize = RInt
     let! _ = BinParser(fun i -> 
                         printf "1Voy %O\n" (i.BaseStream.Position)
                         Success([||],i))
     printf "1--- %O\n" dataSize
     let! headerSize = RInt
     let! _ = BinParser(fun i -> 
                         printf "2Voy %O\n" (i.BaseStream.Position)
                         Success([||],i))
     printf "2--- %O\n" headerSize
     let! typePrefix = RShort   
     let! resourceType =
            if typePrefix = int16(-1) then
                wrap(RShort, Numeric)
            else
                wrap(pBuilder { let! res = RZString2
                                return res
                              }, Name)
     let! namePrefix = RShort   
     let! resourceName =
            if namePrefix = int16(-1) then
                wrap(RShort, Numeric)
            else
                wrap(pBuilder { let! res = RZString2
                                return res
                              }, Name)
     let typeAndNameLength = sizeOfResourceId resourceType +
                             sizeOfResourceId resourceName +
                             4
     let! _ = if ( (typeAndNameLength &&& 3)  <> 0) then
                 printf "2Skipping %d\n" (4 - (dataSize &&& 3) )
                 RByteBlock(4 - (typeAndNameLength &&& 3) )
              else
                 printf "2allowqing %d\n" (typeAndNameLength)
                 BinParser(fun i -> Success([||],i))
     let! dataVersion = RInt
     let! memoryFlags = RShort
     let! languageId = RShort
     let! version = RInt
     let! characteristics = RInt
     let! contents = RByteBlock(dataSize)
     let! _ = if (dataSize &&& 3) <> 0 then
                printf "Skipping %d\n" (4 - (dataSize &&& 3) )
                RByteBlock(4 - (dataSize &&& 3) )
              else
                BinParser(fun i -> Success([||],i))
     return (resourceName,resourceType,contents)
   }


let cursorResEntry = pBuilder {
    let! reserved = RShort
    let! restype  = RShort
    let! rescount = RShort
    let! cursorWidth = RShort
    let! cursorHeight = RShort
    let! planes = RShort 
    let! bitCount = RShort 
    let! bytesInRes = RInt
    let! iconCursorId = RShort
    return (reserved,restype,rescount),
           (cursorWidth,cursorHeight),
           planes,bitCount,bytesInRes,iconCursorId
}

let getCursorInfo (data : byte array) =
  use str = new MemoryStream(data)
  use bReader = new BinaryReader(str,System.Text.Encoding.Unicode)
  cursorResEntry.Function bReader


let iconResEntry = pBuilder {
    let! reserved = RShort
    let! restype  = RShort
    let! rescount = RShort
    let! iconWidth = RByte
    let! iconHeight = RByte
    let! colorCount = RByte
    let! reserved = RByte
    let! planes = RShort 
    let! bitCount = RShort 
    let! bytesInRes = RInt
    let! iconCursorId = RShort
    return (reserved,restype,rescount),
           (iconWidth,iconHeight,colorCount),
           planes,bitCount,bytesInRes,iconCursorId
}



let getIconInfo (data : byte array) =
  use str = new MemoryStream(data)
  use bReader = new BinaryReader(str)
  iconResEntry.Function bReader


let filterMap f = 
    Seq.fold (fun total current ->  
                  match (f current) with
                  | Some result -> result::total
                  | _ -> total)
             []

let isEntryWithId cursorId entry =
    match entry with 
    | Some(Numeric(v),_,_) when v = cursorId -> true
    | _ -> false

let writeIcon(index:int16,(width:byte,height:byte,bitcount:byte),bpp:int16,planes:int16,contents:byte array) =
    use writer = new FileStream(sprintf "icon%d.ico" index,FileMode.Create)
    use bwriter = new BinaryWriter(writer)
    bwriter.Write(0s)
    bwriter.Write(1s)
    bwriter.Write(1s)
    bwriter.Write(byte(width))
    bwriter.Write(byte(height))
    bwriter.Write(byte(bitcount))
    bwriter.Write(byte(0))
    bwriter.Write(planes) 
    bwriter.Write(bpp)
    bwriter.Write(contents.Length)
    bwriter.Write(int32(writer.Position) + 4)
    bwriter.Write(contents )

let writeBmp(name,data:byte array) =
    use writer = new FileStream(name,FileMode.Create)
    use bwriter = new BinaryWriter(writer)
    bwriter.Write(0x42uy)
    bwriter.Write(0x4Duy)
    bwriter.Write(14  + data.Length)
    bwriter.Write(5s)
    bwriter.Write(5s)
    bwriter.Write(14 + 40 + int((2.0 ** float(data.[14])) * 4.0))
    bwriter.Write(data)


let file = new FileStream(fileName,FileMode.Open)
let binaryReader = new BinaryReader(file,System.Text.Encoding.Unicode)
let resources = 
  seq {
     while(file.Position < file.Length) do
       match (resourceParser.Function binaryReader) with
       | Success (t,_) -> yield (Some t)
       | Failure _ -> printf "Error!"
                      yield None
  } |>  List.ofSeq

resources |> 
   filterMap (fun current -> 
                   match current with
                   | Some(Numeric(id),Numeric(12s),data)  -> 
                         match (getCursorInfo data) with
                         | Success(t,_) -> Some(id,t)
                         | _ ->
                             printf "WARNING: Skipping resource: %d\n" id
                             None
                   | _ -> None) |>
   filterMap (fun (id,(_,(w,h),_,bitCount,_,cursorId)) -> 
             let cursorResourceGroup =
                List.find 
                     (isEntryWithId cursorId)
                     resources
             match cursorResourceGroup with
             | Some(_,_,theData) -> 
                 Some(id,min w h,bitCount,theData)
             | _ -> 
                 printf "Could not file cursor resource group for %d\n" cursorId
                 None
             )  |>
   Seq.iter ( fun (index,size,bitcount,contents) ->
                 use writer = new FileStream(sprintf "cur%d.cur" index,FileMode.Create)
                 use bwriter = new BinaryWriter(writer)
                 bwriter.Write(0s)
                 bwriter.Write(2s)
                 bwriter.Write(1s)
                 //Assume 32x32
                 bwriter.Write(byte(32))
                 bwriter.Write(byte(32))
                 bwriter.Write(byte(bitcount))
                 bwriter.Write(byte(0))
                 let v1 = uint16(contents.[0]) ||| (uint16(contents.[1] <<< 16))
                 let v2 = uint16(contents.[2]) ||| (uint16(contents.[3] <<< 16))
                 bwriter.Write(v1) 
                 bwriter.Write(v2)
                 bwriter.Write(contents.Length - 4)
                 bwriter.Write(int32(writer.Position) + 4)
                 bwriter.Write(contents,4,contents.Length - 4 ))
// Write Icons

resources |> 
     filterMap id |> 
     filterMap (fun (id,ttype,z) -> 
                  match id,ttype with 
                  | Numeric(id),Numeric(14s)-> Some(id,z) 
                  | _ -> None) |>
     List.map (fun (id,resentry) -> id,(getIconInfo resentry)) |> 
     filterMap (fun (id,e) -> match e with                
                              | Success(x,_) -> Some(id,x)
                              | _ -> None) |> 
     filterMap (fun (originalid,(_,i,planes,bitcount,bytes,id)) -> 
             match (List.find (isEntryWithId id) resources) with
             | Some(_,_,data) -> Some(originalid,i,planes,bitcount,data)
             | _ -> None) |>
     Seq.iter (fun (id,(w,h,colorcount),planes,bitcount,contents) -> 
                  writeIcon(id,(w,h,colorcount),bitcount,planes,contents))

resources |> 
   filterMap (fun x -> match x with 
                       | Some(Numeric(id),Numeric(2s),data) -> 
                                 Some(sprintf "bmp%d.bmp" (int(id)),data) 
                       | Some(Name(namea),Numeric(2s),data) -> 
                                 Some(new System.String(namea)+".bmp",data) 
                       | _ -> None) |> 
   Seq.iter (fun (name,data) -> writeBmp(name,data))


binaryReader.Close()
file.Close()


