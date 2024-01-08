module Antidote.Core.FormProcessor.JSON.Values.v2_0_1

open Thoth.Json
open Antidote.Core.FormProcessor.Values.v2_0_1

module Helper =
    let sanitize (str:string) =
        let regex = System.Text.RegularExpressions.Regex("[^a-zA-Z0-9 ]")
        regex.Replace(str, "")

    let jsonSanitize (str:string) =
        str
        |> sanitize
        |> (fun x -> x.Split ' ')
        |> Array.filter (fun (s:string) -> s <> "")
        |> Array.map (fun (s:string) -> s.ToLower())
        |> Array.mapi (fun i x ->
            if i > 0
            then string (x.[0] |> System.Char.ToUpper) + (string x.[1..])
            else x
        )
        |> String.concat ""

module FieldDetails =
    let encode (FieldKey fieldKey,  fieldDetails: FieldDetails) =
        let key:string = fieldDetails.Label |> Helper.jsonSanitize

        match fieldDetails.FieldValue with
        | Single answer ->
            key,
                Encode.string answer.Value

        | Multiple answers ->
            key,
                answers
                |> Set.toList
                |> List.map (fun a -> Encode.string a.Value)
                |> Encode.list

module DynamicStepValues =
    let encode (StepOrder step , dynamicStepValues:DynamicStepValues) =
        string step,            
            dynamicStepValues
            |> Map.toList
            |> List.map FieldDetails.encode
            |> Encode.object

module DynamicFormResultData =
    let encode (dynamicFormResultData: DynamicFormResultData) =
        Encode.object
            [
                "FormSpecId", Encode.guid dynamicFormResultData.ResultFormSpecDetails.FormSpecId
                "code", dynamicFormResultData.ResultFormSpecDetails.FormSpecCode |> Option.map Encode.string |> Option.toObj
                "title", Encode.string dynamicFormResultData.ResultFormSpecDetails.FormSpecTitle
                "abstract", Encode.string dynamicFormResultData.ResultFormSpecDetails.FormSpecAbstract
                "formSpecVersion", Encode.string dynamicFormResultData.ResultFormSpecDetails.FormSpecVersion
                "dynamicVersion", Encode.string dynamicFormResultData.ResultFormSpecDetails.DynamicVersion
                "steps",
                    dynamicFormResultData.ResultSteps
                    |> Map.toList
                    |> List.map DynamicStepValues.encode
                    |> Encode.object
            ]
