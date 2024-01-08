module Antidote.Core.FormProcessor.Helpers.v2_0_1.Spec

open Antidote.Core.FormProcessor.Spec.v2_0_1
open Antidote.Core.FormProcessor.Values.v2_0_1

let specFieldOptionsToValueOptions (options:Antidote.Core.FormProcessor.Spec.v2_0_1.FieldOption list) : Antidote.Core.FormProcessor.Values.v2_0_1.FieldOption list =
    options
    |> List.map (fun o ->
        {
            OptionKey = o.OptionKey
            Description = o.Description
            Value = o.Value
        }
    )

let optionsFromSpecField_V2_0_0 (field: Antidote.Core.FormProcessor.Spec.v2_0_0.FormField)  =
    match field.FieldType with
    | Antidote.Core.FormProcessor.Spec.v2_0_0.Checkbox _
    | Antidote.Core.FormProcessor.Spec.v2_0_0.Switch _
    | Antidote.Core.FormProcessor.Spec.v2_0_0.TrueFalse _
    | Antidote.Core.FormProcessor.Spec.v2_0_0.YesNo _

    | Antidote.Core.FormProcessor.Spec.v2_0_0.Date _
    | Antidote.Core.FormProcessor.Spec.v2_0_0.EPrescribe _
    | Antidote.Core.FormProcessor.Spec.v2_0_0.Image _
    | Antidote.Core.FormProcessor.Spec.v2_0_0.Number _
    | Antidote.Core.FormProcessor.Spec.v2_0_0.Signature _
    | Antidote.Core.FormProcessor.Spec.v2_0_0.StateSelectorUSA _
    | Antidote.Core.FormProcessor.Spec.v2_0_0.Tel _
    | Antidote.Core.FormProcessor.Spec.v2_0_0.Text _
    | Antidote.Core.FormProcessor.Spec.v2_0_0.TextArea _
    | Antidote.Core.FormProcessor.Spec.v2_0_0.Time _ -> []

    | Antidote.Core.FormProcessor.Spec.v2_0_0.CheckboxList info -> info.Options
    | Antidote.Core.FormProcessor.Spec.v2_0_0.MultiChoice info -> info.Options
    | Antidote.Core.FormProcessor.Spec.v2_0_0.TagList info -> info.Options

    | Antidote.Core.FormProcessor.Spec.v2_0_0.Dropdown info -> info.Options
    | Antidote.Core.FormProcessor.Spec.v2_0_0.Radio info -> info.Options
    | Antidote.Core.FormProcessor.Spec.v2_0_0.SingleChoice info -> info.Options
    | Antidote.Core.FormProcessor.Spec.v2_0_0.TextAutoComplete info -> info.Options

    | Antidote.Core.FormProcessor.Spec.v2_0_0.Message _ -> []

let optionsFromSpecField_V2_0_1 (field: Antidote.Core.FormProcessor.Spec.v2_0_1.FormField)  =
    match field.FieldType with
    | Antidote.Core.FormProcessor.Spec.v2_0_1.Checkbox _
    | Antidote.Core.FormProcessor.Spec.v2_0_1.Switch _
    | Antidote.Core.FormProcessor.Spec.v2_0_1.TrueFalse _
    | Antidote.Core.FormProcessor.Spec.v2_0_1.YesNo _

    | Antidote.Core.FormProcessor.Spec.v2_0_1.Date _
    | Antidote.Core.FormProcessor.Spec.v2_0_1.DrugFinder _
    | Antidote.Core.FormProcessor.Spec.v2_0_1.DrugFinderWithFrequency _
    // TODO: Unify these controls after extending the
    // FormSpec to include properties for determining the appropriate 'finder'
    | Antidote.Core.FormProcessor.Spec.v2_0_1.AllergyFinder _
    | Antidote.Core.FormProcessor.Spec.v2_0_1.CPTFinder _
    | Antidote.Core.FormProcessor.Spec.v2_0_1.ICD10Finder _
    | Antidote.Core.FormProcessor.Spec.v2_0_1.EPrescribe _
    | Antidote.Core.FormProcessor.Spec.v2_0_1.Image _
    | Antidote.Core.FormProcessor.Spec.v2_0_1.Number _
    | Antidote.Core.FormProcessor.Spec.v2_0_1.Signature _
    | Antidote.Core.FormProcessor.Spec.v2_0_1.SpeechToText _
    | Antidote.Core.FormProcessor.Spec.v2_0_1.StateSelectorUSA _
    | Antidote.Core.FormProcessor.Spec.v2_0_1.Tel _
    | Antidote.Core.FormProcessor.Spec.v2_0_1.Text _
    | Antidote.Core.FormProcessor.Spec.v2_0_1.TextArea _
    | Antidote.Core.FormProcessor.Spec.v2_0_1.Time _ -> []

    | Antidote.Core.FormProcessor.Spec.v2_0_1.CheckboxList info -> info.Options
    | Antidote.Core.FormProcessor.Spec.v2_0_1.MultiChoice info -> info.Options
    | Antidote.Core.FormProcessor.Spec.v2_0_1.TagList info -> info.Options

    | Antidote.Core.FormProcessor.Spec.v2_0_1.Dropdown info -> info.Options
    | Antidote.Core.FormProcessor.Spec.v2_0_1.Radio info -> info.Options
    | Antidote.Core.FormProcessor.Spec.v2_0_1.SingleChoice info -> info.Options
    | Antidote.Core.FormProcessor.Spec.v2_0_1.TextAutoComplete info -> info.Options

    | Antidote.Core.FormProcessor.Spec.v2_0_1.Message _ -> []


let readValue (field:FormField) (values:DynamicStepValues) : string =
    //read the value from the values map
    match values.Keys |> Seq.tryFind (fun k -> k = (FieldKey field.FieldKey)) with
    | None -> ""
    | Some key ->
        let fv = values.Item key
        match fv.FieldValue with
        | Single v -> v.Value
        | _ -> "" //Should never happen

let readManyValue fieldKey (values:DynamicStepValues) =
    match values.Keys |> Seq.tryFind (fun k -> k = fieldKey) with
    | None -> Set.empty
    | Some key ->
        let fieldDetails = values.Item key
        match fieldDetails.FieldValue with
        | Multiple v -> v |> Set.map (fun v -> v.Value)
        | _ -> Set.empty //Should never happen


let updateSingleFunc formatter (specField:Antidote.Core.FormProcessor.Spec.v2_0_1.FormField) (newValue:string) (values: DynamicStepValues) : DynamicStepValues =
    let fieldOptions = optionsFromSpecField_V2_0_1 specField
    let newFieldDetails =
        {
            FieldOrder = specField.FieldOrder
            Key = FieldKey specField.FieldKey
            // Value = Single (formatter newValue)
            FieldValue = Single {
                // FieldType = specField.FieldType
                FieldKey = specField.FieldKey
                Value = formatter newValue
                Description =
                    match  fieldOptions |> List.tryFind (fun o -> o.Value = newValue) with
                    | Some o -> o.Description
                    | None -> newValue
            }
            Label = specField.Label
            Options =
                fieldOptions
                |> specFieldOptionsToValueOptions

        }

    match values.Keys |> Seq.tryFind (fun k -> k = newFieldDetails.Key) with
    | None ->
        values.Add(newFieldDetails.Key, newFieldDetails)
    | Some key ->
        values.Remove key
        |> ( fun f -> values.Add(key, newFieldDetails) )

let updateManyFunc formatter (specField:FormField) (newValue:Set<string>) (values: DynamicStepValues) : DynamicStepValues =
    let newFieldDetails =
        {
            FieldOrder = specField.FieldOrder
            Key = FieldKey specField.FieldKey
            FieldValue =
                Multiple (
                    newValue
                    |> Set.map (fun v ->
                        {
                            // FieldType = specField.FieldType
                            FieldKey = specField.FieldKey
                            Value = formatter v
                            Description =
                                match optionsFromSpecField_V2_0_1 specField |> List.tryFind (fun o -> o.Value = v) with
                                | Some o -> o.Description
                                | None -> v
                        }
                    )
                )
            Label = specField.Label
            Options =
                optionsFromSpecField_V2_0_1 specField
                |> specFieldOptionsToValueOptions
        }
    match values.Keys |> Seq.tryFind (fun k -> k = newFieldDetails.Key) with
    | None ->
        values.Add(newFieldDetails.Key, newFieldDetails)
    | Some key ->
        values.Remove(newFieldDetails.Key)
        |> (fun f -> f.Add(newFieldDetails.Key, newFieldDetails) )
