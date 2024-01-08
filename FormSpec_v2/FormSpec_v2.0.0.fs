module Antidote.Core.FormProcessor.Spec.v2_0_0

open v1_0_1
open Types

type FieldOption = {
    Description: string
    Value: string
    OptionKey: string
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

[<RequireQualifiedAccess>]
type Evaluator =
    | Equals
    | NotEquals
    | GreaterThan
    | GreaterThanOrEquals
    | LessThan
    | LessThanOrEquals
    | Exists
    | IsEmpty

    member x.Key =
        match x with
        | Equals -> "Equals"
        | NotEquals -> "Not Equals"
        | GreaterThan -> "Greater Than"
        | GreaterThanOrEquals -> "Greater Than Or Equals"
        | LessThan -> "Less Than"
        | LessThanOrEquals -> "Less Than Or Equals"
        | Exists -> "Exists"
        | IsEmpty -> "Is Empty"

let tryEvaluationKeyToEvaluation str: Evaluator option =
    match str with
    | "Equals" -> Some Evaluator.Equals
    | "Not Equals" -> Some Evaluator.NotEquals
    | "Greater Than" -> Some Evaluator.GreaterThan
    | "Greater Than Or Equals" -> Some Evaluator.GreaterThanOrEquals
    | "Less Than" -> Some Evaluator.LessThan
    | "Less Than Or Equals" -> Some Evaluator.LessThanOrEquals
    | "Exists" -> Some Evaluator.Exists
    | "Is Empty" -> Some Evaluator.IsEmpty
    | _ -> None

let evaluators =
    [
        Evaluator.Equals
        Evaluator.NotEquals
        Evaluator.GreaterThan
        Evaluator.GreaterThanOrEquals
        Evaluator.LessThan
        Evaluator.LessThanOrEquals
        Evaluator.Exists
        Evaluator.IsEmpty
    ]

type DependsOn = {
    FieldKey: string
    FieldValue: string
    Evaluator: Evaluator
}

type TextInfo = { Value: string option }

type SingleChoiceInfo = { Options: FieldOption list }

type MultiChoiceInfo = { Options: FieldOption list }

type BooleanInfo =
    {
        DefaultValue: bool option
        Selection: bool option
    }



[<RequireQualifiedAccess>]
type MessageType =
    | Warning
    | Success
    | Error
    | Info
    | Danger

type MessageInfo = {
    MessageType: MessageType
    Message: string
    Footer: string option
}

type FieldType =
    | Image of TextInfo
    | Message of MessageInfo
    | Signature of TextInfo
    | SingleChoice of SingleChoiceInfo
    | MultiChoice of MultiChoiceInfo
    | CheckboxList of MultiChoiceInfo
    | Text of TextInfo
    | TextArea of TextInfo
    | Tel of TextInfo
    | Date of TextInfo
    | Time of TextInfo // actually a different type?
    | Number of TextInfo
    | StateSelectorUSA of TextInfo
    | YesNo of BooleanInfo
    | TrueFalse of BooleanInfo
    | Checkbox of BooleanInfo
    | Radio of SingleChoiceInfo
    | Dropdown of SingleChoiceInfo
    | TagList of MultiChoiceInfo
    | Switch of BooleanInfo
    | TextAutoComplete of SingleChoiceInfo
    | EPrescribe of TextInfo

type FieldAnswerFlag = {
    OptionKey: string // dragged field option - option key
    Label: string // what flag you want to generate
}

type FormField = {
    FieldOrder: int
    FieldKey: string
    Label: string
    DependsOn: DependsOn option
    IsOptional: bool
    IsDeprecated: bool
    // Flags: FieldAnswerFlag list
    FieldType: FieldType
}

type FormStep = {
    StepOrder: int
    StepLabel: string
    Fields: FormField list
}

type ScoreColor =
    // | White
    // | Black
    // | Light
    // | Dark
    | Unspecified
    | Primary
    | Link
    | Info
    | Success
    | Warning
    | Danger

type ScoreRange = {
    Id: System.Guid
    Min: int
    Max: int
    Label: string
    Tag: ScoreColor
}

type Score = {
    MaxScore: int
    ScoreRanges: ScoreRange list
}

type FormSpec = {
    Id: System.Guid
    Code: string option
    Title: string
    Abstract: string
    Version: string
    FormSpecVersion: string
    Steps: FormStep list
    CategoryTags: CategoryTag list
    Score: Score option
}

module SpecUtil =

    #if FABLE_COMPILER
    //STUB TO MAKE FABLE NOT COMPLAIN. THIS FUNCTION IS ONLY FOR MIGRATIONS. MOVE IT?
    let generateGuidFromStringSeed seed = id
    #else
    let generateGuidFromStringSeed (seed:string) =
        let md5 = System.Security.Cryptography.MD5.Create()
        let hash = md5.ComputeHash(System.Text.Encoding.UTF8.GetBytes(seed))
        System.Guid(hash)
    #endif

    let tryFindField (formSpec:FormSpec) (key:string) =
        let tryFindKeyInFields (list: FormField list) =
            list
            |> List.tryFind (fun item -> item.FieldKey = key)

        let tryFindLabelInFields (list: FormField list) =
            list
            |> List.tryFind (fun item -> item.Label = key)

        formSpec.Steps
        |> List.map (fun a -> tryFindKeyInFields a.Fields)
        |> List.tryFind (fun opt -> opt.IsSome)
        |> Option.orElseWith (fun _ ->
            formSpec.Steps
            |> List.map (fun a -> tryFindLabelInFields a.Fields)
            |> List.tryFind (fun opt -> opt.IsSome)
        )
        |> Option.bind id


    let migrateToLatestDependsOn (oldDependsOn: v1_0_1.DependsOn option) =
        match oldDependsOn with
        | None -> None
        | Some d ->
            Some {
                FieldKey = d.FieldName
                FieldValue = d.FieldValue
                Evaluator = Evaluator.Equals
            }

    let migrateToLatestMessageType ( oldMessageType: v1_0_1.MessageType ) =
        match oldMessageType with
        | v1_0_1.MessageType.Warning -> MessageType.Warning
        | v1_0_1.MessageType.Success -> MessageType.Success
        | v1_0_1.MessageType.Error -> MessageType.Error
        | v1_0_1.MessageType.Info -> MessageType.Info
        | v1_0_1.MessageType.Danger -> MessageType.Danger



    let migrateToLatestOptions (oldOptions: v1_0_1.FieldOption list) =
        oldOptions
        |> List.map (fun o ->
            {
                Description = o.Description
                Value = o.Value
                OptionKey = (generateGuidFromStringSeed (o.Description + o.Value)).ToString()
            }
        )

    let migrateToLatestFields (oldFields: v1_0_1.FieldType list ) : FormField list =
        oldFields
        |> List.map (fun f ->
            match f with
            | v1_0_1.Message m ->
                {
                    FieldOrder = m.FieldOrder
                    FieldKey = (id (m.Label)).ToString()
                    Label = m.Label
                    DependsOn = m.DependsOn |> migrateToLatestDependsOn
                    IsOptional = m.IsOptional
                    IsDeprecated = false
                    // Flags = []
                    FieldType = Message
                        {
                            MessageType = m.MessageType |> migrateToLatestMessageType
                            Message = m.Message
                            Footer = m.Footer
                        }
                } : FormField
            | v1_0_1.Signature s ->
                {
                    FieldOrder = s.FieldOrder
                    FieldKey = (id (s.Label)).ToString()
                    Label = s.Label
                    DependsOn = s.DependsOn |> migrateToLatestDependsOn
                    IsOptional = s.IsOptional
                    IsDeprecated = false
                    // Flags = []
                    FieldType = Signature
                        {
                            Value = s.Value
                        }
                } : FormField
            | v1_0_1.SingleChoice s ->
                {
                    FieldOrder = s.FieldOrder
                    FieldKey = (id (s.Label)).ToString()
                    Label = s.Label
                    DependsOn = s.DependsOn |> migrateToLatestDependsOn
                    IsOptional = s.IsOptional
                    IsDeprecated = false
                    // Flags = []
                    FieldType = SingleChoice
                        {
                            Options = s.Options |> migrateToLatestOptions
                        }
                }
            | v1_0_1.MultiChoice m ->
                {
                    FieldOrder = m.FieldOrder
                    FieldKey = (id (m.Label)).ToString()
                    Label = m.Label
                    DependsOn = m.DependsOn |> migrateToLatestDependsOn
                    IsOptional = m.IsOptional
                    IsDeprecated = false
                    // Flags = []
                    FieldType = MultiChoice
                        {
                            Options = m.Options |> migrateToLatestOptions
                        }
                }
            | v1_0_1.Text t ->
                {
                    FieldOrder = t.FieldOrder
                    FieldKey = (id (t.Label)).ToString()
                    Label = t.Label
                    DependsOn = t.DependsOn |> migrateToLatestDependsOn
                    IsOptional = t.IsOptional
                    IsDeprecated = false
                    // Flags = []
                    FieldType = Text
                        {
                            Value = t.Value
                        }
                }
            | v1_0_1.Tel t ->
                {
                    FieldOrder = t.FieldOrder
                    FieldKey = (id (t.Label)).ToString()
                    Label = t.Label
                    DependsOn = t.DependsOn |> migrateToLatestDependsOn
                    IsOptional = t.IsOptional
                    IsDeprecated = false
                    // Flags = []
                    FieldType = Tel
                        {
                            Value = t.Value
                        }
                }
            | v1_0_1.Date d ->
                {
                    FieldOrder = d.FieldOrder
                    FieldKey = (id (d.Label)).ToString()
                    Label = d.Label
                    DependsOn = d.DependsOn |> migrateToLatestDependsOn
                    IsOptional = d.IsOptional
                    IsDeprecated = false
                    // Flags = []
                    FieldType = Date
                        {
                            Value = d.Value
                        }
                }
            | v1_0_1.Time t ->
                {
                    FieldOrder = t.FieldOrder
                    FieldKey = (id (t.Label)).ToString()
                    Label = t.Label
                    DependsOn = t.DependsOn |> migrateToLatestDependsOn
                    IsOptional = t.IsOptional
                    IsDeprecated = false
                    // Flags = []
                    FieldType = Time
                        {
                            Value = t.Value
                        }
                }
            | v1_0_1.Number n ->
                {
                    FieldOrder = n.FieldOrder
                    FieldKey = (id (n.Label)).ToString()
                    Label = n.Label
                    DependsOn = n.DependsOn |> migrateToLatestDependsOn
                    IsOptional = n.IsOptional
                    IsDeprecated = false
                    // Flags = []
                    FieldType = Number
                        {
                            Value = n.Value
                        }
                }
            | v1_0_1.YesNo y ->
                {
                    FieldOrder = y.FieldOrder
                    FieldKey = (id (y.Label)).ToString()
                    Label = y.Label
                    DependsOn = y.DependsOn |> migrateToLatestDependsOn
                    IsOptional = y.IsOptional
                    IsDeprecated = false
                    // Flags = []
                    FieldType = YesNo
                        {
                            DefaultValue = y.DefaultValue
                            Selection = y.Selection
                        }
                }
            | v1_0_1.TrueFalse t ->
                {
                    FieldOrder = t.FieldOrder
                    Label = t.Label
                    FieldKey = (id (t.Label)).ToString()
                    DependsOn = t.DependsOn |> migrateToLatestDependsOn
                    IsOptional = t.IsOptional
                    IsDeprecated = false
                    // Flags = []
                    FieldType = TrueFalse
                        {
                            DefaultValue = t.DefaultValue
                            Selection = t.Selection
                        }
                }
        )

    let migrateToLatestSteps (oldSteps: v1_0_1.FormStep list) =
        oldSteps
        |> List.map (fun s ->
            {
                StepOrder = s.StepOrder
                StepLabel = s.StepLabel
                Fields =
                    s.Fields
                    |> migrateToLatestFields
            } : FormStep
        )

    let migrate (oldSpec: v1_0_1.FormSpec) =
        {
            Id = oldSpec.Id
            Code = Some oldSpec.Code
            Title = oldSpec.Title
            Abstract = oldSpec.Abstract
            Version = oldSpec.Version
            FormSpecVersion = "2.0.0"
            Steps =
                oldSpec.Steps
                |> migrateToLatestSteps
            CategoryTags = []
            Score = None
        }
