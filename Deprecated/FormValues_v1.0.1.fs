module Antidote.Core.FormProcessor.Values.v1_0_1

[<RequireQualifiedAccess>]
type FieldValues =
    | One of string
    | Many of Set<string>

type FormValues = FormValues of Map<string, FieldValues>

// let migrate (formValues:FormValues) =
//     let migrateFieldValues (fieldValues:FieldValues) =
//         match fieldValues with
//         | One value -> Many (Set.singleton