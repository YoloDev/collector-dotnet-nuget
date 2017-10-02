module DependenciesIO.Json

open Aether

(* Types

   Simple AST for JSON, with included isomorphisms and lenses in Aether form for
   lens/isomorphism based modification of complex JSON structures. *)

type Json =
  | Array of Json list
  | Bool of bool
  | Null of unit
  | Number of double
  | Object of Map<string, Json>
  | String of string

  (* Epimorphisms *)
  
  static member internal Array__ =
    (function | Array x -> Some x
              | _ -> None), Array
  
  static member internal Bool__ =
    (function | Bool x -> Some x
              | _ -> None), Bool
  
  static member internal Null__ =
    (function | Null () -> Some ()
              | _ -> None), Null
  
  static member internal Number__ =
    (function | Number x -> Some x
              | _ -> None), Number
  
  static member internal Object__ =
    (function | Object x -> Some x
              | _ -> None), Object
  
  static member internal String__ =
    (function | String x -> Some x
              | _ -> None), String
  
  (* Prisms *)

  static member Array_ =
    Prism.ofEpimorphism Json.Array__
  
  static member Bool_ =
    Prism.ofEpimorphism Json.Bool__

  static member Null_ =
    Prism.ofEpimorphism Json.Null__

  static member Number_ =
    Prism.ofEpimorphism Json.Number__

  static member Object_ =
    Prism.ofEpimorphism Json.Object__

  static member String_ =
    Prism.ofEpimorphism Json.String__

(* Functional

   Functional signatures for working with Json types, implying a monadic
   approach to working with Json where appropriate.

   Additionally includes common functions for combining and creating
   functions of type Json<'a> which may be used via operator based
   combinators or a computation expression (both provided later). *)

[<AutoOpen>]
module Functional =
  type JsonResult<'a> = Result<'a, string>

  type Json<'a> = Json -> JsonResult<'a> * Json

  (* Functions

     Common functions for combining Json<'a> functions in to new
     forms, and for creating new Json<'a> functions given suitable
     initial data. *)

  [<RequireQualifiedAccess>]
  module Json =
    let inline init (a: 'a) : Json<'a> = 
      fun json ->
        Ok a, json

    let inline error (e: string) : Json<'a> =
      fun json ->
        Error e, json
    
    let inline internal ofResult result =
      fun json ->
        result, json
    
    let inline bind (m: Json<'a>) (f: 'a -> Json<'b>) : Json<'b> =
      fun json ->
        match m json with
        | Ok a, json    -> (f a) json
        | Error e, json -> Error e, json
    
    let inline apply (f: Json<'a -> 'b>) (m: Json<'a>) : Json<'b> =
      bind f (fun f' ->
        bind m (f' >> init))
    
    let inline map (f: 'a -> 'b) (m: Json<'a>) : Json<'b> =
      bind m (f >> init)
    
    let inline map2 (f: 'a -> 'b -> 'c) (m1: Json<'a>) (m2: Json<'b>) : Json<'c> =
      apply (apply (init f) m1) m2

(* Operators

   Symbolic operators for working with Json<'a> functions, providing
   an operator based concise alternative to the primitive Json<'a> combinators
   given as part of Functional.
   
   This module is not opened by default, as symbolic operators are a matter
   of taste and may also clash with other operators from other libraries. *)

module Operators =
  let inline (>>=) m f =
    Json.bind m f
  
  let inline (=<<) f m =
    Json.bind m f
  
  let inline (<*>) f m =
    Json.apply f m
  
  let inline (<!>) f m =
    Json.map f m
  
  let inline (>>.) m f =
    Json.bind m (fun _ -> f)
  
  let inline (.>>) m f =
    Json.bind (fun _ -> m) f
  
  let inline ( *>) m1 m2 =
    Json.map2 (fun _ x -> x) m1 m2
  
  let inline (<* ) m1 m2 =
    Json.map2 (fun x _ -> x) m1 m2
  
  let inline (>=>) m1 m2 =
    Json.bind (fun x -> m1 x) m2
  
  let inline (<=<) m1 m2 =
    Json.bind (fun x -> m2 x) m1

(* Builder

   Computation expression (builder) for working with JSON structures in a
   simple way, including lensing, morphisms, etc. using the Aether
   library. *)

module Builder =
  type JsonBuilder () =
    member __.Bind (m1, m2) : Json<_> =
      Json.bind m1 m2
    
    member __.Combine (m1, m2) : Json<_> =
      Json.bind m1 (fun () -> m2)
    
    member __.Delay (f) : Json<_> =
      Json.bind (Json.init ()) f
    
    member __.Return (x) : Json<_> =
      Json.init x
    
    member __.ReturnFrom (f) : Json<_> =
      f
    
    member __.Zero () : Json<_> =
      Json.init ()

let json = Builder.JsonBuilder ()

(* Optics

   Functional optics based access to nested Json data structures,
   using Aether format lenses/prisms/etc. Uses Json<'a> based functions, so
   can be used monadically. *)

[<AutoOpen>]
module Optics =
  
  (* Functions *)

  [<RequireQualifiedAccess>]
  module Json =

    [<RequireQualifiedAccess>]
    module Optic =

      type Get =
        | Get with

        static member (^.) (Get, l: Lens<Json,'b>) : Json<_> =
          fun json ->
            Ok (Optic.get l json), json
        
        static member (^.) (Get, p: Prism<Json,'b>) : Json<_> =
          fun json ->
            match Optic.get p json with
            | Some x -> Ok x, json
            | _      -> Error (sprintf "Couldn't use Prism %A on JSON: '%A'" p json), json
      
      let inline get o : Json<_> =
        (Get ^. o)
      
      type TryGet =
        | TryGet with

        static member (^.) (TryGet, l: Lens<Json,'b>) : Json<_> =
          fun json ->
            Ok (Some (Optic.get l json)), json
        
        static member (^.) (TryGet, p: Prism<Json,'b>) : Json<_> =
          fun json ->
            Ok (Optic.get p json), json
        
      let inline tryGet o : Json<_> =
        (TryGet ^. o)
      
      let inline set o v : Json<_> =
        fun json ->
          Ok (), Optic.set o v json
      
      let inline map o f : Json<_> =
        fun json ->
          Ok (), Optic.map o f json

(* Conversion *)
module internal Conversion =
  open Newtonsoft.Json.Linq

  let nullValue = JValue.CreateNull ()

  let rec toToken (json: Json) : JToken =
    match json with
    | Array xs  -> JArray (xs |> Seq.map toToken |> Seq.toArray) :> JToken
    | Bool b    -> JValue b :> JToken
    | Null _    -> nullValue :> JToken
    | Number n  -> JValue n :> JToken
    | String s  -> JValue s :> JToken
    | Object ps ->
      ps
      |> Map.toSeq
      |> Seq.map (fun (k, v) -> JProperty (k, toToken v))
      |> Seq.toArray
      |> JObject
      :> JToken
  
  let rec fromToken (json: JToken) : Json =
    match json with
    | :? JArray as a -> a.Values () |> Seq.map fromToken |> Seq.toList |> Array
    | :? JValue as v ->
      match v.Value with
      | null           -> Null ()
      | :? double as n -> Number n
      | :? string as s -> String s
      | _              -> failwithf "Invalid token type %s - token: %O" ((v.Value.GetType ()).Name) v
    | :? JObject as o ->
      o.Properties ()
      |> Seq.map (fun p -> p.Name, fromToken p.Value)
      |> Map.ofSeq
      |> Object
    | _ -> failwithf "Invalid token type %s - token: %O" ((json.GetType ()).Name) json
      
(* Parsing *)

[<AutoOpen>]
module Parsing =
  open System
  open Newtonsoft.Json.Linq

  [<RequireQualifiedAccess>]
  module Json =
    let internal parseJson s =
      if String.IsNullOrWhiteSpace s then
        Error "Input is null or whitespace"
      else
        try
          JToken.Parse s
          |> Conversion.fromToken
          |> Ok
        with e -> Error e.Message
    
    let tryParse =
        parseJson
      >> function | Ok json -> Choice1Of2 json
                  | Error e -> Choice2Of2 e
    
    let parse =
        parseJson
      >> function | Ok json -> json
                  | Error e -> failwith e
    
    let import s =
      fun json ->
        match parseJson s with
        | Ok json -> Ok (), json
        | Error e -> Error e, json

(* Formatting *)

[<AutoOpen>]
module Formatting =
  open Newtonsoft.Json

  type Format = Compact | Pretty

  [<RequireQualifiedAccess>]
  module Json =
    let format json =
      (Conversion.toToken json).ToString ()
    
    let formatWith format json =
      let fmt =
        match format with
        | Compact -> Formatting.None
        | Pretty  -> Formatting.Indented
        
      (Conversion.toToken json).ToString fmt
  
  (* Error Message Formatters *)

  [<RequireQualifiedAccess>]
  module Errors =

    let missingMember key =
       sprintf "Error deserializing JSON object; Missing required member '%s'" key

    let missingMemberWithJson key =
      function | Some format -> Json.formatWith format >> (+) (missingMember key + ": ")
               | None        -> fun _ -> missingMember key   

(* Mapping

   Functional mapping between Json and native F# data structures,
   through statically inferred types. Types providing FromJson and
   ToJson static members with appropriate signatures can be
   seamlessly serialized and deserialized.

   This approach is the same as that taken by the Fleece library,
   credit for which is due to Mauricio Scheffer. *)

[<AutoOpen>]
module Mapping =
  open Operators

  (* From
     
     Default conversion functions (static members on FromJsonDefaults)
     and statically inferred inline conversion functions for conversion
     from Json to F# data structures. *)

  (* Defaults *)

  type FromJsonDefaults = FromJsonDefaults with
    
    (* Basic Types *)

    static member inline FromJson (_: unit) =
      Json.Optic.get Json.Null_
    
    static member inline FromJson (_: bool) =
      Json.Optic.get Json.Bool_
    
    static member inline FromJson (_: double) =
      id <!> Json.Optic.get Json.Number_
    
    static member inline FromJson (_: decimal) =
      decimal <!> Json.Optic.get Json.Number_
    
    static member inline FromJson (_: int) =
      int <!> Json.Optic.get Json.Number_
    
    static member inline FromJson (_: int16) =
      int16 <!> Json.Optic.get Json.Number_
    
    static member inline FromJson (_: int64) =
      int64 <!> Json.Optic.get Json.Number_
    
    static member inline FromJson (_: single) =
      single <!> Json.Optic.get Json.Number_
    
    static member inline FromJson (_: string) =
      Json.Optic.get Json.String_
    
    static member inline FromJson (_: uint16) =
      uint16 <!> Json.Optic.get Json.Number_
    
    static member inline FromJson (_: uint32) =
      uint32 <!> Json.Optic.get Json.Number_
    
    static member inline FromJson (_: uint64) =
      uint64 <!> Json.Optic.get Json.Number_

    (* Json Type *)

    static member inline FromJson (_: Json) =
      Json.Optic.get id_
    
  (* Mapping Functions
  
     Functions for applying the FromJson function to Json to produce
     new instances of 'a where possible, including folding the FromJson
     function across a list of Json objects. *)
  
  let inline internal fromJsonDefaults (a: ^a, _: ^b) =
    ((^a or ^b) : (static member FromJson: ^a -> ^a Json) a)
  
  let inline internal fromJson x =
    fst (fromJsonDefaults (Unchecked.defaultof<'a>, FromJsonDefaults) x)
  
  let inline internal fromJsonFold init fold xs =
    List.fold (fun r x ->
      match r with
      | Error e -> Error e
      | Ok xs   ->
        match fromJson x with
        | Ok x    -> Ok (fold x xs)
        | Error e -> Error e) (Ok init) (List.rev xs)
  
  (* Defaults *)

  type FromJsonDefaults with

    (* Arrays *)

    static member inline FromJson (_: 'a array) : Json<'a array> =
        fromJsonFold [||] (fun x xs -> Array.append [| x |] xs) >> Json.ofResult
      =<< Json.Optic.get Json.Array_
    
    (* Lists *)

    static member inline FromJson (_: 'a list) : Json<'a list> =
        fromJsonFold [] (fun x xs -> x :: xs) >> Json.ofResult
      =<< Json.Optic.get Json.Array_

    (* Maps *)

    static member inline FromJson (_: Map<string,'a>) : Json<Map<string,'a>> =
          fun x ->
            let k, v = (Map.toList >> List.unzip) x
            List.zip k >> Map.ofList <!> Json.ofResult (fromJsonFold [] (fun x xs -> x :: xs) v)
      =<< Json.Optic.get Json.Object_
    
    (* Sets *)

    static member inline FromJson (_: Set<'a>) : Json<Set<'a>> =
        fromJsonFold Set.empty Set.add >> Json.ofResult
       =<< Json.Optic.get Json.Array_

    (* Options *)

    static member inline FromJson (_: 'a option) : Json<'a option> =
        function | Null _ -> Json.init None
                 | x -> Some <!> Json.ofResult (fromJson x)
      =<< Json.Optic.get id_

    (* Tuples *)

    static member inline FromJson (_: 'a * 'b) : Json<'a * 'b> =
          function | [a; b] ->
                          fun a b -> a, b
                      <!> Json.ofResult (fromJson a)
                      <*> Json.ofResult (fromJson b)
                   | _ -> Json.error "tuple2"
      =<< Json.Optic.get Json.Array_
    
    static member inline FromJson (_: 'a * 'b * 'c) : Json<'a * 'b * 'c> =
          function | [a; b; c] ->
                          fun a b c -> a, b, c
                      <!> Json.ofResult (fromJson a)
                      <*> Json.ofResult (fromJson b)
                      <*> Json.ofResult (fromJson c)
                   | _ -> Json.error "tuple3"
      =<< Json.Optic.get Json.Array_
  
  (* To JSON *)

  (* Defaults *)

  type ToJsonDefaults = ToJsonDefaults with

    (* Basic Types *)

    static member inline ToJson (x: unit) =
      Json.Optic.set Json.Null_ x
    
    static member inline ToJson (x: bool) =
      Json.Optic.set Json.Bool_ x
    
    static member inline ToJson (x: double) =
      match x with
      | x when System.Double.IsInfinity x -> failwith "Serialization of Infinite Numbers Invalid."
      | x when System.Double.IsNaN x -> failwith "Serialization of NaN Invalid."
      | x -> Json.Optic.set Json.Number_ x
    
    static member inline ToJson (x: decimal) =
      Json.Optic.set Json.Number_ (double x)
    
    static member inline ToJson (x: int) =
      Json.Optic.set Json.Number_ (double x)
    
    static member inline ToJson (x: int16) =
      Json.Optic.set Json.Number_ (double x)
    
    static member inline ToJson (x: int64) =
      Json.Optic.set Json.Number_ (double x)
    
    static member inline ToJson (x: single) =
      match x with
      | x when System.Single.IsInfinity x -> failwith "Serialization of Infinite Numbers Invalid."
      | x when System.Single.IsNaN x -> failwith "Serialization of NaN Invalid."
      | x -> Json.Optic.set Json.Number_ (double x)
    
    static member inline ToJson (x: string) =
      Json.Optic.set Json.String_ x
    
    static member inline ToJson (x: uint16) =
      Json.Optic.set Json.Number_ (double x)
    
    static member inline ToJson (x: uint32) =
      Json.Optic.set Json.Number_ (double x)
    
    static member inline ToJson (x: uint64) =
      Json.Optic.set Json.Number_ (double x)
    
    (* Json Type *)
    
    static member inline ToJson (x: Json) =
      Json.Optic.set id_ x
    
  (* Mapping Functions

     Functions for applying the ToJson function to data structures to produce
     new Json instances. *)

  let inline internal toJsonDefaults (a: ^a, _: ^b) =
    ((^a or ^b) : (static member ToJson: ^a -> unit Json) a)

  let inline internal toJson (x: 'a) =
    snd (toJsonDefaults (x, ToJsonDefaults) (Object (Map.empty)))
  
  let inline internal toJsonWith (f:'a -> unit Json) (x: 'a) = 
    snd (f x (Object (Map.empty)))
  
  (* Defaults *)

  type ToJsonDefaults with

    (* Arrays *)

    static member inline ToJson (x: 'a array) =
      Json.Optic.set id_ (Array ((Array.toList >> List.map toJson) x))
    
    (* Lists *)

    static member inline ToJson (x: 'a list) =
      Json.Optic.set id_ (Array (List.map toJson x))
    
    (* Maps *)

    static member inline ToJson (x: Map<string,'a>) =
      Json.Optic.set id_ (Object (Map.map (fun _ a -> toJson a) x))
    
    (* Options *)

    static member inline ToJson (x: 'a option) =
      Json.Optic.set id_ ((function | Some a -> toJson a 
                                    | _ -> Null ()) x)
    
    (* Sets *)

    static member inline ToJson (x: Set<'a>) =
      Json.Optic.set id_ (Array ((Set.toList >> List.map toJson) x))
    
    (* Tuples *)

    static member inline ToJson ((a, b)) =
      Json.Optic.set id_ (Array [ toJson a; toJson b ])
    
    static member inline ToJson ((a, b, c)) =
      Json.Optic.set id_ (Array [ toJson a; toJson b; toJson c ])
  
  (* Functions *)

  [<RequireQualifiedAccess>]
  module Json =

    (* Read/Write *)

    let missingMember key =
      fun json ->
        Errors.missingMemberWithJson key (Some Compact) json
        |> fun e -> Error e, json
    
    let readMemberWith fromJson key onMissing =
        Json.Optic.tryGet (Json.Object_ >?> Map.key_ key)
      >>= function | Some json -> Json.ofResult (fromJson json)
                   | None -> onMissing ()
    
    let inline readWith fromJson key =
      readMemberWith fromJson key <| fun () -> missingMember key
    
    let inline read key =
      readWith fromJson key
    
    let inline readWithOrDefault fromJson key def =
      readMemberWith fromJson key <| fun () -> Json.init def

    let inline readOrDefault key def =
      readWithOrDefault fromJson key def

    let inline tryReadWith fromJson key =
      readMemberWith fromJson key <| fun () -> Json.init None

    let inline tryRead key =
      tryReadWith fromJson key

    let writeWith toJson key value =
      Json.Optic.set (Json.Object_ >?> Map.value_ key) (Some (toJson value))

    let inline write key value =
      writeWith toJson key value

    let writeWithUnlessDefault toJson key def value =
      match value with
      | v when v = def -> Json.ofResult <| Ok ()
      | _ -> writeWith toJson key value
    
    let inline writeUnlessDefault key def value =
      writeWithUnlessDefault toJson key def value

    let writeNone key =
      Json.Optic.set (Json.Object_ >?> Map.value_ key) (Some (Json.Null ()))

    (* Serialization/Deserialization *)

    let inline deserialize json =
      fromJson json
      |> function | Ok a    -> a
                  | Error e -> failwith e    

    let inline tryDeserialize json =
      fromJson json
      |> function | Ok a    -> Choice1Of2 a
                  | Error e -> Choice2Of2 e
    
    let inline serialize a =
      toJson a
    
    let inline serializeWith f a = 
      toJsonWith f a

(* Patterns

   Active patterns for working with Json data structures, making it
   easier to write code for matching against unions, etc. *)

[<AutoOpen>]
module Patterns =
  open Aether.Operators

  /// Parse a Property from a Json Object token using a supplied fromJson,
  /// and try to deserialize it to the inferred type.
  let inline (|PropertyWith|) fromJson key =
       Optic.get (Json.Object_ >?> Map.key_ key)
    >> Option.bind (fromJson >> function | Ok a, _ -> Some a
                                         | _       -> None)

  /// Parse a Property from a Json Object token, and try to deserialize it to the
  /// inferred type.
  let inline (|Property|_|) key =
       Optic.get (Json.Object_ >?> Map.key_ key)
    >> Option.bind (Json.tryDeserialize >> function | Choice1Of2 a -> Some a
                                                    | _            -> None)

