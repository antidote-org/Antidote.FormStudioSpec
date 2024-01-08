module Antidote.FormSpec.ResultExtract

open Antidote.Core.FormProcessor.Values.v2_0_1

open Thoth.Json
// open SimpleJson


let dynamicFormResultDataToList (dynamicFormResultData:DynamicFormResultData): FieldDetails list =
    dynamicFormResultData.ResultSteps
    |> Map.fold( fun acc (StepOrder step) dynamicStepValues ->
        dynamicStepValues
        |> Map.fold(fun acc (FieldKey fieldKey) fieldValue ->
            fieldValue :: acc
        ) acc
    ) []
    |> List.sortBy (fun f-> f.FieldOrder)

type User =
    {
        Name: string
        Age: int
        KnownLangs : string list
    }

module User =

    let encoder (user : User) =
        Encode.object [
            "name", Encode.string user.Name
            "age", Encode.int user.Age
            "known-langs",
                user.KnownLangs
                |> List.map Encode.string
                |> Encode.list
        ]

module FieldDetails =
    let toCamelCase (str:string) =
        let firstChar = str.[0]
        let rest = str.[1..]
        let firstCharUpper = System.Char.ToUpper firstChar |> string
        firstCharUpper + rest

    let removeSpaces (str:string) =
        str
        |> (fun x -> x.Split ' ')
        |> Array.map (fun (s:string) -> s.ToLower())
        |> String.concat ""
        |> toCamelCase

    let removeSpacesAndConvertToCamelCase = removeSpaces >> toCamelCase

    // let gen (str) =
    //     "[{\"key\":\"dc610c9e-4620-4aab-84b7-eb57e1bf59c0\",\"Untitled Text\":\"HHHHHH\"}]"
    //     |> SimpleJson.parse
    //     |> SimpleJson.mapKeys (function
    //         | key -> key |> removeSpacesAndConvertToCamelCase
    //     )
    //     |> SimpleJson.
    //     |> SimpleJson.toString
    //     // |> Json.convertFromJsonAs<Person>
    //     // { FirstName = "John"; LastName = "Doe" }



    let encode (fieldDetails :FieldDetails) =
        let key:string = fieldDetails.Key |> function | FieldKey key -> key

        Encode.object [
            fieldDetails.Label,
                match fieldDetails.FieldValue with
                | Single answer -> Encode.string answer.Value
                | Multiple answers ->
                    answers
                    |> Set.toList
                    |> List.map (fun a -> Encode.string a.Value)
                    |> Encode.list
        ]

let discoverableDataGraph (fieldList: FieldDetails list) =
    let encodedFields =
        fieldList
        |> List.map (fun f ->
            f.Label,
            match f.FieldValue with
            | Single answer -> Encode.string answer.Value
            | Multiple answers ->
                answers
                |> Set.toList
                |> List.map (fun a -> Encode.string a.Value)
                |> Encode.list
        )
        |> Encode.object
    let str = Thoth.Json.Encode.toString 0 encodedFields
    str


let discoverableDataGraphOrig (fieldList: FieldDetails list) =
    let encodedFields =
        fieldList
        |> List.map (fun f -> FieldDetails.encode f )
        |> Encode.list
    let str = Thoth.Json.Encode.toString 0 encodedFields
    str
