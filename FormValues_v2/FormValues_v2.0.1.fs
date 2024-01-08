module Antidote.Core.FormProcessor.Values.v2_0_1

open v1_0_1

open Antidote.Core.FormProcessor.Spec.v2_0_1
open System


type FieldAnswer = {
    FieldKey: string
    Description: string
    Value: string
}

type FieldOption = {
    OptionKey: string
    Description: string
    Value: string
}

type FieldValue =
    | Single of FieldAnswer
    | Multiple of Set<FieldAnswer>

type StepOrder = StepOrder of int
type FieldKey = FieldKey of string

type FieldDetails = {
    FieldOrder: int
    Key: FieldKey
    Label: string
    FieldValue: FieldValue
    Options: FieldOption list
}

type DynamicStepValues = Map<FieldKey, FieldDetails>

type DynamicFormSpecDetails = {
    FormSpecId: Guid
    FormSpecCode: string option
    FormSpecTitle: string
    FormSpecAbstract: string
    FormSpecVersion: string
    DynamicVersion: string
    MaxScore: Score option
}

type DynamicForm<'FableFormModel> = {
    DynamicFormSpecDetails: DynamicFormSpecDetails
    Steps: Map<StepOrder, 'FableFormModel>
}


type DynamicFormResultData = {
    ResultFormSpecDetails: DynamicFormSpecDetails
    ResultSteps: Map<StepOrder, DynamicStepValues>
}


type SimplifiedResultData = {
    ResultFormSpecDetails: DynamicFormSpecDetails
    Data: Map<int, FieldDetails array>
}

let mapDynamicFormResultDataToSimplifiedResultData (dynamicFormResultData: DynamicFormResultData) :  SimplifiedResultData =
    let resultFormSpecDetails = dynamicFormResultData.ResultFormSpecDetails
    let resultSteps = dynamicFormResultData.ResultSteps

    let simplifiedResultData =
        resultSteps
        |> Seq.map (fun kv -> //(stepOrder, stepValues) ->
            let (StepOrder stepOrder) = kv.Key
            let stepValues = kv.Value

            let simplifiedStepValues =
                stepValues
                |> Seq.map (fun fieldKey_FieldDetails ->
                    let (FieldKey fieldKey) = fieldKey_FieldDetails.Key
                    let fieldDetails = fieldKey_FieldDetails.Value

                    let simplifiedFieldDetails =
                        {
                            FieldOrder = fieldDetails.FieldOrder
                            Key = fieldDetails.Key
                            Label = fieldDetails.Label
                            FieldValue = fieldDetails.FieldValue
                            Options = fieldDetails.Options
                        }
                    simplifiedFieldDetails
                )
                |> Array.ofSeq
            stepOrder, simplifiedStepValues
        )
        |> Map.ofSeq
    {
        ResultFormSpecDetails = resultFormSpecDetails
        Data = simplifiedResultData
    }
