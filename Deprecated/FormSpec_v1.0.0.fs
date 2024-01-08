module Antidote.Core.FormProcessor.Spec.v1_0_0

open System

type FieldOption = {
    Description: string
    Value: string
}

type FieldResult = {
    Patient: string
    Order: int
    Question: string
    Answer: string
    Value: string
}

type MinimalField = {
    FieldOrder: int
    Options: FieldOption list
}

type DependsOn = {
    FieldName: string
    FieldValue: string
}

type SingleChoiceInfo =
    {
        FieldOrder: int
        Label: string
        Options: FieldOption list
        DependsOn: DependsOn option
        IsOptional: bool
    }

type MultiChoiceInfo =
    {
        FieldOrder: int
        Label: string
        Options: FieldOption list
        DependsOn: DependsOn option
        IsOptional: bool
    }

type BooleanInfo =
    {
        FieldOrder: int
        Label: string
        DefaultValue: bool option
        Selection: bool option
        DependsOn: DependsOn option
        IsOptional: bool
    }


type TextInfo =
    {
        FieldOrder: int
        Label: string
        Value: string option
        DependsOn: DependsOn option
        IsOptional: bool
    }

[<RequireQualifiedAccess>]
type MessageType =
    | Warning
    | Success
    | Error
    | Info
    | Danger

type MessageInfo = {
    FieldOrder: int
    Label: string
    MessageType: MessageType
    Message: string
    Footer: string option
    DependsOn: DependsOn option
    IsOptional: bool
}

type FieldType =
    | Message of MessageInfo
    | Signature of TextInfo
    | SingleChoice of SingleChoiceInfo
    | MultiChoice of MultiChoiceInfo
    | Text of TextInfo
    | Tel of TextInfo
    | Date of TextInfo
    | Time of TextInfo // actually a different type?
    | Number of TextInfo
    | YesNo of BooleanInfo
    | TrueFalse of BooleanInfo

type FormStep =
    {
        StepOrder: int
        StepLabel: string
        Fields: FieldType list
    }

type FormSpec =
    {
        Id: Guid
        Code: string
        Title: string
        Abstract: string
        Version: string
        Steps: FormStep list
    }