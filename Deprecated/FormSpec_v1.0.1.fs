module Antidote.Core.FormProcessor.Spec.v1_0_1

open v1_0_0

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


[<RequireQualifiedAccess>]
type MessageType =
    | Warning
    | Success
    | Error
    | Info
    | Danger

type TextInfo = {
    FieldOrder: int
    Label: string
    Value: string option
    DependsOn: DependsOn option
    IsOptional: bool
}

type BooleanInfo = {
    FieldOrder: int
    Label: string
    DefaultValue: bool option
    Selection: bool option
    DependsOn: DependsOn option
    IsOptional: bool
}

type MessageInfo = {
    FieldOrder: int
    Label: string
    DependsOn: DependsOn option
    IsOptional: bool
    MessageType: MessageType
    Message: string
    Footer: string option
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

type FormStep = {
    StepOrder: int
    StepLabel: string
    Fields: FieldType list
}

type FormSpec =
    {
        Id: System.Guid
        Code: string
        Title: string
        Abstract: string
        Version: string
        FormSpecVersion: string
        Steps: FormStep list
    }

let migrateToLatestDependsOn (oldDependsOn: v1_0_0.DependsOn option) =
    match oldDependsOn with
    | None -> None
    | Some d ->
        Some {
            FieldName = d.FieldName
            FieldValue = d.FieldValue
        }

let migrateToLatestMessageType ( oldMessageType: v1_0_0.MessageType ) =
    match oldMessageType with
    | v1_0_0.MessageType.Warning -> MessageType.Warning
    | v1_0_0.MessageType.Success -> MessageType.Success
    | v1_0_0.MessageType.Error -> MessageType.Error
    | v1_0_0.MessageType.Info -> MessageType.Info
    | v1_0_0.MessageType.Danger -> MessageType.Danger

let migrateToLatestOptions (oldOptions: v1_0_0.FieldOption list) =
    oldOptions
    |> List.map (fun o ->
        {
            Description = o.Description
            Value = o.Value
        }
    )

let migrateToLatestFields (oldFields: v1_0_0.FieldType list ) : FieldType list =
    oldFields
    |> List.map (fun f ->
        match f with
        | v1_0_0.Message m ->
            Message
                {
                    FieldOrder = m.FieldOrder
                    Label = m.Label
                    MessageType = m.MessageType |> migrateToLatestMessageType
                    Message = m.Message
                    Footer = m.Footer
                    DependsOn = m.DependsOn |> migrateToLatestDependsOn
                    IsOptional = m.IsOptional
                } 
        | v1_0_0.Signature s ->
            Signature
                {
                    FieldOrder = s.FieldOrder
                    Label = s.Label
                    Value = s.Value
                    DependsOn = s.DependsOn |> migrateToLatestDependsOn
                    IsOptional = s.IsOptional
                }
        | v1_0_0.SingleChoice s ->
            SingleChoice
                {
                    FieldOrder = s.FieldOrder
                    Label = s.Label
                    Options = s.Options |> migrateToLatestOptions
                    DependsOn = s.DependsOn |> migrateToLatestDependsOn
                    IsOptional = s.IsOptional
                }
        | v1_0_0.MultiChoice m ->
            MultiChoice
                {
                    FieldOrder = m.FieldOrder
                    Label = m.Label
                    Options = m.Options |> migrateToLatestOptions
                    DependsOn = m.DependsOn |> migrateToLatestDependsOn
                    IsOptional = m.IsOptional
                }
        | v1_0_0.Text t ->
            Text
                {
                    FieldOrder = t.FieldOrder
                    Label = t.Label
                    Value = t.Value
                    DependsOn = t.DependsOn |> migrateToLatestDependsOn
                    IsOptional = t.IsOptional
                }
        | v1_0_0.Tel t ->
            Tel
                {
                    FieldOrder = t.FieldOrder
                    Label = t.Label
                    Value = t.Value
                    DependsOn = t.DependsOn |> migrateToLatestDependsOn
                    IsOptional = t.IsOptional
                }
        | v1_0_0.Date d ->
            Date
                {
                    FieldOrder = d.FieldOrder
                    Label = d.Label
                    Value = d.Value
                    DependsOn = d.DependsOn |> migrateToLatestDependsOn
                    IsOptional = d.IsOptional
                }
        | v1_0_0.Time t ->
            Time
                {
                    FieldOrder = t.FieldOrder
                    Label = t.Label
                    Value = t.Value
                    DependsOn = t.DependsOn |> migrateToLatestDependsOn
                    IsOptional = t.IsOptional
                }
        | v1_0_0.Number n ->
            Number
                {
                    FieldOrder = n.FieldOrder
                    Label = n.Label
                    Value = n.Value
                    DependsOn = n.DependsOn |> migrateToLatestDependsOn
                    IsOptional = n.IsOptional
                }
        | v1_0_0.YesNo y ->
            YesNo
                {
                    FieldOrder = y.FieldOrder
                    Label = y.Label
                    DefaultValue = y.DefaultValue
                    Selection = y.Selection
                    DependsOn = y.DependsOn |> migrateToLatestDependsOn
                    IsOptional = y.IsOptional
                }
        | v1_0_0.TrueFalse t ->
            TrueFalse
                {
                    FieldOrder = t.FieldOrder
                    Label = t.Label
                    DefaultValue = t.DefaultValue
                    Selection = t.Selection
                    DependsOn = t.DependsOn |> migrateToLatestDependsOn
                    IsOptional = t.IsOptional
                }
    )

let migrateToLatestSteps (oldSteps: v1_0_0.FormStep list) : FormStep list =
    oldSteps
    |> List.map (fun s ->
        {
            StepOrder = s.StepOrder
            StepLabel = s.StepLabel
            Fields = s.Fields |> migrateToLatestFields
        } : FormStep
    )

let migrate (oldSpec: v1_0_0.FormSpec) : FormSpec =
    {
        Id = oldSpec.Id
        Code = oldSpec.Code
        Title = oldSpec.Title
        Abstract = oldSpec.Abstract
        Version = oldSpec.Version
        FormSpecVersion = "1.0.1"
        Steps =
            oldSpec.Steps
            |> migrateToLatestSteps
    }