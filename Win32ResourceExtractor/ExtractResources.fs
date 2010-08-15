open System

open Langexplr
open Langexplr.BinParserModule
open System.IO

type ResourceId =
     | Numeric of int16
     | Name of char[]

let sizeOfResourceId resId = 
   match resId with
   | Numeric(_) -> 2 + 2
   | Name(dat) -> dat.Length*2 + 2 

 
let fileName =  Environment.GetCommandLineArgs().[1]

let pBuilder = new BinParserBuilder()

let resourceParser = pBuilder {
     let! dataSize = RInt
     let! headerSize = RInt
     let! typePrefix = RShort   
     let! resourceType =
            if typePrefix = int16(-1) then
                wrap(RShort, Numeric)
            else
                wrap(RZString,
                     (fun data -> Name(Array.concat
                                          [[|char(typePrefix)|];
                                           data] )))
     let! namePrefix = RShort   
     let! resourceName =
            if namePrefix = int16(-1) then
                wrap(RShort, Numeric)
            else
                wrap(RZString,
                     (fun data -> Name(Array.concat
                                          [[|char(namePrefix)|];
                                           data] )))
     let typeAndNameLength = sizeOfResourceId resourceType +
                             sizeOfResourceId resourceName 
                             
     let! _ = RDWordAlignData(typeAndNameLength)
     
     let! dataVersion = RInt
     let! memoryFlags = RShort
     let! languageId = RShort
     let! version = RInt
     let! characteristics = RInt
     let! contents = RByteBlock(dataSize)

     let! _ = RDWordAlignData(dataSize)

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
    let paletteSize =
         if data.[14] < 24uy then
            int((2.0 ** float(data.[14])) * 4.0)
         else
            0         
    bwriter.Write(14 + 40 + paletteSize)
    bwriter.Write(data)

let writeCursor(fileName, bitcount : byte, contents : byte array) =
    use writer = new FileStream(fileName, FileMode.Create)
    use bwriter = new BinaryWriter(writer)
    bwriter.Write(0s)
    bwriter.Write(2s)
    bwriter.Write(1s)
    //Assume 32x32
    bwriter.Write(byte(32))
    bwriter.Write(byte(32))
    bwriter.Write(bitcount)
    bwriter.Write(byte(0))
    let v1 = uint16(contents.[0]) ||| (uint16(contents.[1] <<< 16))
    let v2 = uint16(contents.[2]) ||| (uint16(contents.[3] <<< 16))
    bwriter.Write(v1) 
    bwriter.Write(v2)
    bwriter.Write(contents.Length - 4)
    bwriter.Write(int32(writer.Position) + 4)
    bwriter.Write(contents,4,contents.Length - 4 )
    
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

  
// Write Cursors
resources |> 
   filterMap (fun current -> 
                   match current with
                   | Some(resName,Numeric(12s),data)  -> 
                         match resName,(getCursorInfo data) with
                         | Numeric(id), Success(t,_) ->
                                     Some(sprintf "cur%O.cur" id,t)
                         | Name(nameChars), Success(t,_) -> 
                                     Some(new String(nameChars) + ".cur",t)
                         | _,_ ->
                             printf "WARNING: Skipping resource: %O\n" id
                             None
                   | _ -> None) |>
   filterMap (fun (name,(_,(w,h),_,bitCount,_,cursorId)) -> 
             let cursorResourceGroup =
                List.find 
                     (isEntryWithId cursorId)
                     resources
             match cursorResourceGroup with
             | Some(_,_,theData) -> 
                 Some(name, min w h, bitCount, theData)
             | _ -> 
                 printf "Could not file cursor resource group for %d\n" cursorId
                 None
             )  |>
   Seq.iter ( fun (name,size,bitcount,contents) ->
                 writeCursor(name,byte(bitcount),contents))
// Write Icons
resources |> 
     filterMap id |> 
     filterMap (fun (id, ttype, z) -> 
                  match id, ttype with 
                  | Numeric id, Numeric 14s -> Some(id,z) 
                  | _ -> None) |>
     List.map (fun (id,resentry) -> id,(getIconInfo resentry)) |> 
     filterMap (fun (id,e) -> match e with                
                              | Success(x, _) -> Some(id,x)
                              | _ -> None) |> 
     filterMap (fun (originalid, (_, i, planes, bitcount, bytes, id)) -> 
             match (List.find (isEntryWithId id) resources) with
             | Some(_,_,data) -> Some(originalid,i,planes,bitcount,data)
             | _ -> None) |>
     Seq.iter (fun (id,(w,h,colorcount),planes,bitcount,contents) -> 
                  writeIcon(id,(w,h,colorcount),bitcount,planes,contents))
     
// Write Bitmaps
resources |> 
   filterMap (fun res -> match res with 
                         | Some(Numeric id, Numeric 2s, data) -> 
                                 Some(sprintf "bmp%O.bmp" id, data) 
                         | Some(Name nameChars, Numeric 2s, data) -> 
                                 Some(new String(nameChars)+".bmp", data) 
                         | _ -> None) |> 
   Seq.iter (fun (name,data) -> writeBmp(name,data))


binaryReader.Close()
file.Close()


