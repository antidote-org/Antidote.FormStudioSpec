module Antidote.Core.FormProcessor.Values.v2_0_0

open v1_0_1

open Antidote.Core.FormProcessor.Spec.v2_0_0

type FieldValue =
    | Single of string
    | Multiple of Set<string>

type FieldKey = FieldKey of string

type FieldDetails = {
    FieldOrder: int
    Key: FieldKey
    Label: string
    Value: FieldValue
    Options: FieldOption list
}

type FormValues = FormValues of Map<FieldKey, FieldDetails>
