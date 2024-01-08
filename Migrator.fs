module Antidote.Core.FormProcessor.Migrator

open System
open Antidote.Core.FormProcessor.Spec
open Antidote.Core.FormProcessor.Values
open Antidote.Core.FormProcessor.Values.v1_0_1
// open Antidote.Core.FormProcessor.Values.v2_0_0
open Antidote.Core.FormProcessor.Values.v2_0_1
open Antidote.Core.FormProcessor.Helpers.v2_0_1

#if !FABLE_COMPILER
open Thoth.Json.Net
#else
open Thoth.Json
#endif

module FormSpec =
    [<RequireQualifiedAccess>]
    type FormSpecInput =
        | V1_0_0_FormSpec
        | V1_0_1_FormSpec
        | V2_0_0_FormSpec
        | V2_0_1_FormSpec
        | Latest
        | Unknown

    [<RequireQualifiedAccess>]
    type FormSpecOutput =
        | V1_0_0_FormSpec
        | V1_0_1_FormSpec
        | V2_0_0_FormSpec
        | V2_0_1_FormSpec
        | Latest

    type FormSpecVersion =
        | V1_0_0_FormSpec of v1_0_0.FormSpec
        | V1_0_1_FormSpec of v1_0_1.FormSpec
        | V2_0_0_FormSpec of v2_0_0.FormSpec
        | V2_0_1_FormSpec of v2_0_1.FormSpec

    let decodeFormSpec (inputVersion: FormSpecInput) (formSpecJson: string) =
        match inputVersion with
        | FormSpecInput.V1_0_0_FormSpec ->
            match Decode.Auto.fromString<v1_0_0.FormSpec>(formSpecJson) with
            | Ok formSpec ->
                formSpec
                |> V1_0_0_FormSpec

            | Error err -> failwith "The input format does not match 1.0.0"
        | FormSpecInput.V1_0_1_FormSpec ->
            match Decode.Auto.fromString<v1_0_1.FormSpec>(formSpecJson) with
            | Ok formSpec ->
                formSpec |> V1_0_1_FormSpec
            | Error err -> failwith "The input format does not match 1.0.1"

        | FormSpecInput.V2_0_0_FormSpec ->
            match Decode.Auto.fromString<v2_0_0.FormSpec>(formSpecJson) with
            | Ok formSpec ->
                formSpec|> V2_0_0_FormSpec
            | Error err -> failwith "The input format does not match 2.0.1"

        | FormSpecInput.Latest
        | FormSpecInput.V2_0_1_FormSpec ->
            match Decode.Auto.fromString<v2_0_1.FormSpec>(formSpecJson) with
            | Ok formSpec -> formSpec |> V2_0_1_FormSpec
            | Error err -> failwith "The input format does not match 2.0.1"

        | FormSpecInput.Unknown ->
            match Decode.Auto.fromString<v2_0_1.FormSpec>(formSpecJson) with
            | Ok formSpec -> formSpec |> V2_0_1_FormSpec
            | Error err ->
                match Decode.Auto.fromString<v2_0_0.FormSpec>(formSpecJson) with
                | Ok formSpec -> formSpec |> V2_0_0_FormSpec
                | Error err ->
                    match Decode.Auto.fromString<v1_0_1.FormSpec>(formSpecJson) with
                    | Ok formSpec ->
                        formSpec |> V1_0_1_FormSpec
                    | Error err ->
                        match Decode.Auto.fromString<v1_0_0.FormSpec>(formSpecJson) with
                        | Ok formSpec ->
                            formSpec
                            |> V1_0_0_FormSpec

                        | Error err -> failwith "Could not decode form spec with any supported versions of the decoders"

    let migrateTo (outputVersion: FormSpecOutput) (formSpec: FormSpecVersion) =
        // printfn $"Antidote.Core.FormProcessor.Migrator: migrateTo {outputVersion}"

        let latestFormSpec =
            match formSpec, outputVersion with
            // | V1_0_0_FormSpec formSpec, FormSpecOutput.V1_0_0_FormSpec ->
            //     printfn "Antidote.Core.FormProcessor.Migrator: From v1.0.0 migrateTo: v1.0.0"
            //     formSpec
            //     |> V1_0_0_FormSpec

            // | V1_0_0_FormSpec formSpec, FormSpecOutput.V1_0_1_FormSpec ->
            //     printfn "Antidote.Core.FormProcessor.Migrator: From v1.0.0 migrateTo: v1.0.1"
            //     formSpec
            //     |> v1_0_1.migrate
            //     |> V1_0_1_FormSpec

            // | V1_0_0_FormSpec formSpec, FormSpecOutput.V2_0_0_FormSpec
            // | V1_0_0_FormSpec formSpec, FormSpecOutput.Latest ->
            //     printfn "Antidote.Core.FormProcessor.Migrator: From v1.0.0 migrateTo: v2.0.0 - Latest"
            //     formSpec
            //     |> v1_0_1.migrate
            //     |> v2_0_0.SpecUtil.migrate
            //     |> V2_0_0_FormSpec

            // | V1_0_1_FormSpec formSpec , FormSpecOutput.V1_0_1_FormSpec ->
            //     printfn "Antidote.Core.FormProcessor.Migrator: From v1.0.1 migrateTo: v1.0.1 - Latest"
            //     formSpec
            //     |> V1_0_1_FormSpec

            // | V1_0_1_FormSpec formSpec, FormSpecOutput.V2_0_0_FormSpec
            // | V1_0_1_FormSpec formSpec, FormSpecOutput.Latest ->
            //     printfn "Antidote.Core.FormProcessor.Migrator: From v1.0.1 migrateTo: v2.0.0 - Latest"
            //     formSpec
            //     |> v2_0_0.SpecUtil.migrate
            //     |> V2_0_0_FormSpec

            // | V2_0_1_FormSpec formSpec, FormSpecOutput.V2_0_1_FormSpec ->
            //     printfn "Antidote.Core.FormProcessor.Migrator: From v2.0.1 migrateTo: v2.0.1"
            //     formSpec
            //     |> V2_0_1_FormSpec

            | V1_0_1_FormSpec _, FormSpecOutput.V1_0_0_FormSpec
            | V2_0_0_FormSpec _, FormSpecOutput.V1_0_0_FormSpec
            | V2_0_0_FormSpec _, FormSpecOutput.V1_0_1_FormSpec
            | V2_0_1_FormSpec _, FormSpecOutput.V1_0_0_FormSpec
            | V2_0_1_FormSpec _, FormSpecOutput.V1_0_1_FormSpec ->
                failwithf "Downgrades are not supported"

            | V1_0_1_FormSpec formSpec, FormSpecOutput.V2_0_1_FormSpec
            | V1_0_1_FormSpec formSpec, FormSpecOutput.Latest ->
                // printfn "Antidote.Core.FormProcessor.Migrator: From v1.0.1 migrateTo: Latest"
                formSpec
                |> v2_0_0.SpecUtil.migrate
                |> v2_0_1.SpecUtil.migrate

            | V2_0_0_FormSpec formSpec, FormSpecOutput.V2_0_1_FormSpec
            | V2_0_0_FormSpec formSpec, FormSpecOutput.Latest ->
                // printfn "Antidote.Core.FormProcessor.Migrator: From v2.0.0 migrateTo: Latest"
                formSpec
                |> v2_0_1.SpecUtil.migrate

            | V2_0_1_FormSpec formSpec, FormSpecOutput.V2_0_1_FormSpec
            | V2_0_1_FormSpec formSpec, FormSpecOutput.Latest ->
                // printfn "Antidote.Core.FormProcessor.Migrator: From v2.0.1 migrateTo: Latest"
                formSpec

            | A, B -> failwithf "Antidote.Core.FormProcessor.Migrator: migrateTo: Unsupported migration from %A to %A" A B

        latestFormSpec |> V2_0_1_FormSpec

module Values =
    open Antidote.Core.FormProcessor.Values.v2_0_0

    [<RequireQualifiedAccess>]
    type FormValuesInput =
        | V1_0_1_FormValues
        | V2_0_0_FormValues
        | Latest
        | Unknown

    [<RequireQualifiedAccess>]
    type FormValuesOutput =
        | V1_0_1_FormValues
        | V2_0_0_FormValues
        | Latest

    [<RequireQualifiedAccess>]
    type FormValuesVersion =
        | V1_0_1 of v1_0_1.FormValues
        | V2_0_0 of v2_0_0.FormValues
        | V2_0_1 of v2_0_1.DynamicStepValues

    // let decodeValues (inputVersion: FormValuesInput) jsonFormValues =
    //     match inputVersion with
    //     | FormValuesInput.Unknown ->
    //         match Decode.Auto.fromString<v2_0_0.FormValues>(jsonFormValues) with
    //         | Ok formValues ->
    //             formValues
    //             |> FormValuesVersion.V2_0_0
    //             |> Ok
    //         | Error err ->
    //             match Decode.Auto.fromString<v1_0_1.FormValues>(jsonFormValues) with
    //             | Ok formValues ->
    //                 formValues
    //                 |> FormValuesVersion.V1_0_1
    //                 |> Ok
    //             | Error err -> Error "Could not decode form values with any supported versions of the decoders"

    //     | FormValuesInput.V1_0_1_FormValues ->
    //         match Decode.Auto.fromString<v1_0_1.FormValues>(jsonFormValues) with
    //         | Ok formValues ->
    //             formValues
    //             |> FormValuesVersion.V1_0_1
    //             |> Ok
    //         | Error err -> failwith "The input format does not match 1.0.1"

    //     | FormValuesInput.V2_0_0_FormValues
    //     | FormValuesInput.Latest ->
    //         match Decode.Auto.fromString<v2_0_0.FormValues>(jsonFormValues) with
    //         | Ok formValues ->
    //             formValues
    //             |> FormValuesVersion.V2_0_0
    //             |> Ok
    //         | Error err -> Error "The input format does not match 2.0.0"

    let from_v1_0_1_to_v2_0_0 (formSpec:v2_0_0.FormSpec) (previousVersion:v1_0_1.FormValues) : v2_0_0.FormValues =
        let (v1_0_1.FormValues previousValues) = previousVersion
        let outValue =
            previousValues
            |> Seq.map (fun x ->

                let formSpecField = v2_0_0.SpecUtil.tryFindField formSpec x.Key

                match formSpecField with
                | None -> failwithf "FormSpec Migrator: migrate: Field with key %s not found in form spec" x.Key
                | Some specField ->
                    v2_0_0.FieldKey specField.FieldKey,
                    {
                        v2_0_0.FieldOrder = specField.FieldOrder
                        v2_0_0.Key = specField.FieldKey |> v2_0_0.FieldKey
                        v2_0_0.Label = specField.Label
                        v2_0_0.Value =
                            match x.Value with
                            | v1_0_1.FieldValues.One value -> v2_0_0.Single value
                            | v1_0_1.FieldValues.Many values -> v2_0_0.Multiple values
                        v2_0_0.Options =
                            specField
                            |> Spec.optionsFromSpecField_V2_0_0
                    }
            )
            |> Map.ofSeq
            |> v2_0_0.FormValues
        outValue

    // let from_v2_0_0_to_v2_0_1 (formSpec: v2_0_0.FormSpec) (previousVersion: v2_0_1.FormValues)

    // let from_v2_0_0_to_v2_0_1 (formSpec: v2_0_0.FormSpec) (previousVersion: v2_0_0.FormValues) : v2_0_1.DynamicStepValues =
    //     let (v2_0_0.FormValues previousValues) = previousVersion

    //     let outValue =
    //         previousValues
    //         |> Map.map ( fun (v2_0_0.FieldKey fieldKeyString) fieldDetails ->
    //                 let newFieldKey:v2_0_1.FieldKey = v2_0_1.FieldKey fieldKeyString
    //                 {
    //                     v2_0_1.FieldOrder = fieldDetails.FieldOrder
    //                     v2_0_1.Key = v2_0_1.FieldKey ""
    //                     v2_0_1.Label = fieldDetails.Label
    //                     v2_0_1.FieldValue =
    //                         match fieldDetails.Value with
    //                         | v2_0_0.Single value ->
    //                             v2_0_1.Single {
    //                                 v2_0_1.Value = value
    //                                 v2_0_1.Description =
    //                                     let formSpecField = v2_0_0.SpecUtil.tryFindField formSpec value
    //                                     match formSpecField with
    //                                     | None -> (failwithf "FormSpec Migrator: migrate: Field with key %s not found in form spec" value)
    //                                     | Some specField ->
    //                                         match specField.FieldType with
    //                                         | v2_0_0.FieldType.MultiChoice multi ->
    //                                             let options = specField |> v2_0_0.optionsFromSpecField
    //                                             options
    //                                             |> Seq.tryFind (fun option -> option.Value = value)
    //                                             |> fun option ->
    //                                                 option
    //                                                 |> Option.map (fun option -> option.Description)
    //                                                 |> Option.defaultValue value

    //                                         | v2_0_0.FieldType.SingleChoice single ->
    //                                             let options = specField |> v2_0_0.optionsFromSpecField
    //                                             options
    //                                             |> Seq.tryFind (fun option -> option.Value = value)
    //                                             |> fun option ->
    //                                                 option
    //                                                 |> Option.map (fun option -> option.Description)
    //                                                 |> Option.defaultValue value
    //                                         | _ -> value


    //                             }
    //                         | v2_0_0.Multiple values ->
    //                             v2_0_1.Multiple (
    //                                 values
    //                                 |> Seq.map (fun value -> { v2_0_1.Value = value; v2_0_1.Description = value})
    //                                 |> Set.ofSeq
    //                             )
    //                     v2_0_1.Options = fieldDetails.Options
    //                 }
    //         )
    //         |> Seq.map ( fun formValues ->
    //             formValues.Value.Key, formValues.Value
    //         )
    //         |> Map.ofSeq
    //         |> v2_0_1.DynamicStepValues

    //     outValue

    // let migrate (formSpec: v2_0_0.FormSpec) (previousVersion: FormValuesVersion) : v2_0_1.DynamicStepValues =
    //     printfn "Migrating form values"
    //     printfn "Form spec: %A" formSpec
    //     printfn "Old values: %A" previousVersion

    //     match previousVersion with
    //     | FormValuesVersion.V1_0_1 values ->
    //         values
    //         |> from_v1_0_1_to_v2_0_0 formSpec
    //         |> from_v2_0_0_to_v2_0_1 formSpec
    //     | FormValuesVersion.v2_0_1 values -> values
    //     | FormValuesVersion.V2_0_0 values -> values |> from_v2_0_0_to_v2_0_1 formSpec




        // FormValues (migrateFormValues formValues)

// let fieldValuesWithKey =
//     """
//     [[1,{"Values":["FormValues",{"S1F1":["One","1"]}],"State":"Idle","ErrorTracking":["ErrorTracking",{"ShowAllErrors":false,"ShowFieldError":[]}]}],[2,{"Values":["FormValues",{"S2F1":["One","0"]}],"State":"Idle","ErrorTracking":["ErrorTracking",{"ShowAllErrors":false,"ShowFieldError":[]}]}],[3,{"Values":["FormValues",{"S3F1":["One","0"]}],"State":"Idle","ErrorTracking":["ErrorTracking",{"ShowAllErrors":false,"ShowFieldError":[]}]}],[4,{"Values":["FormValues",{"Condition History:":["Many",["chf_1"]]}],"State":"Idle","ErrorTracking":["ErrorTracking",{"ShowAllErrors":false,"ShowFieldError":[]}]}],[5,{"Values":["FormValues",{"S5F1":["One","2"]}],"State":"Idle","ErrorTracking":["ErrorTracking",{"ShowAllErrors":false,"ShowFieldError":[]}]}]]
//     [[1,{"Values":["FormValues",{"S1F1":["One","1"]}],"State":"Idle","ErrorTracking":["ErrorTracking",{"ShowAllErrors":false,"ShowFieldError":[]}]}],[2,{"Values":["FormValues",{"S2F1":["One","0"]}],"State":"Idle","ErrorTracking":["ErrorTracking",{"ShowAllErrors":false,"ShowFieldError":[]}]}],[3,{"Values":["FormValues",{"S3F1":["One","0"]}],"State":"Idle","ErrorTracking":["ErrorTracking",{"ShowAllErrors":false,"ShowFieldError":[]}]}],[4,{"Values":["FormValues",{"Condition History:":["Many",["chf_1"]]}],"State":"Idle","ErrorTracking":["ErrorTracking",{"ShowAllErrors":false,"ShowFieldError":[]}]}],[5,{"Values":["FormValues",{"S5F1":["One","2"]}],"State":"Idle","ErrorTracking":["ErrorTracking",{"ShowAllErrors":false,"ShowFieldError":[]}]}]]
//     """"
