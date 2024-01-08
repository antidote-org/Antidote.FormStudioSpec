module Antidote.Core.FormProcessor.Spec.v2_0_1

open v1_0_1
open v2_0_0
open Types
open System

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

type TextToSpeechInfo = { Value: string option }
type DrugFinderInfo = { Value: string option }
type DrugFinderWithFrequencyInfo = { Value: string option }
type AllergyFinderInfo = { Value: string option }
type ICD10FinderInfo = { Value: string option }
type CPTFinderInfo = { Value: string option }

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
    | Date of TextInfo
    | DrugFinder of DrugFinderInfo
    | DrugFinderWithFrequency of DrugFinderWithFrequencyInfo
    // TODO: Unify these controls after extending the
    // FormSpec to include properties for determining the appropriate 'finder'
    | AllergyFinder of AllergyFinderInfo
    | CPTFinder of CPTFinderInfo
    | ICD10Finder of ICD10FinderInfo
    | EPrescribe of TextInfo
    | Image of TextInfo
    | Number of TextInfo
    | Signature of TextInfo
    | SpeechToText of TextInfo
    | StateSelectorUSA of TextInfo
    | Tel of TextInfo
    | Text of TextInfo
    | TextArea of TextInfo
    | Time of TextInfo

    | Checkbox of BooleanInfo
    | Switch of BooleanInfo
    | TrueFalse of BooleanInfo
    | YesNo of BooleanInfo

    | CheckboxList of MultiChoiceInfo
    | MultiChoice of MultiChoiceInfo
    | TagList of MultiChoiceInfo

    | Dropdown of SingleChoiceInfo
    | Radio of SingleChoiceInfo
    | SingleChoice of SingleChoiceInfo
    | TextAutoComplete of SingleChoiceInfo

    | Message of MessageInfo

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
    AssociatedCodes: string list
}

module SpecUtil =

    #if FABLE_COMPILER
    //STUB TO MAKE FABLE NOT COMPLAIN. THIS FUNCTION IS ONLY FOR MIGRATIONS. MOVE IT?
    let generateGuidFromStringSeed seed = id
        // printfn "FABLE COMPILER STUB: generateGuidFromStringSeed"
        // const md5 = require('crypto').createHash('md5');
        // const hash = md5.update(seed, 'utf8').digest();
        // const guid = Array.from(hash).map(b => b.toString(16).padStart(2, '0')).join('');
        // const guidWithHyphens = `${guid.slice(0, 8)}-${guid.slice(8, 12)}-${guid.slice(12, 16)}-${guid.slice(16, 20)}-${guid.slice(20)}`;

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


    let migrateFromPreviousDependsOn (oldDependsOn: v2_0_0.DependsOn option) =
        match oldDependsOn with
        | None -> None
        | Some d ->
            Some {
                FieldKey = d.FieldKey
                    // match System.Guid.TryParse(d.FieldKey) with
                    // | true, k -> d.FieldKey
                    // | false, _ ->
                    //     (generateGuidFromStringSeed (d.FieldKey)).ToString()
                FieldValue = d.FieldValue
                Evaluator = Evaluator.Equals
            }

    let migrateToLatestMessageType ( oldMessageType: v2_0_0.MessageType ) =
        match oldMessageType with
        | v2_0_0.MessageType.Warning -> MessageType.Warning
        | v2_0_0.MessageType.Success -> MessageType.Success
        | v2_0_0.MessageType.Error -> MessageType.Error
        | v2_0_0.MessageType.Info -> MessageType.Info
        | v2_0_0.MessageType.Danger -> MessageType.Danger



    let migrateFromPreviousOptions (oldOptions: v2_0_0.FieldOption list) =
        oldOptions
        |> List.map (fun o ->
            {
                Description = o.Description
                Value = o.Value
                OptionKey = o.OptionKey
                    // match System.Guid.TryParse(o.OptionKey) with
                    // | true, k -> o.OptionKey
                    // | false, _ -> (generateGuidFromStringSeed (o.Description + o.Value)).ToString()
            }
        )

    let migrateFromPreviousFields (oldFields: v2_0_0.FormField list ) : FormField list =
        oldFields
        |> List.map (fun f ->
            {
                FieldOrder = f.FieldOrder
                FieldKey = f.FieldKey
                Label = f.Label
                DependsOn = migrateFromPreviousDependsOn f.DependsOn
                IsOptional = f.IsOptional
                IsDeprecated = f.IsDeprecated
                // Flags: FieldAnswerFlag list
                FieldType =
                    match f.FieldType with
                    | v2_0_0.FieldType.Image t -> Image { Value = t.Value }
                    | v2_0_0.FieldType.Message t ->
                        Message {
                            MessageType = migrateToLatestMessageType t.MessageType
                            Message = t.Message
                            Footer = t.Footer
                        }
                    | v2_0_0.FieldType.Signature t -> Signature { Value = t.Value }
                    | v2_0_0.FieldType.SingleChoice t -> SingleChoice { Options = migrateFromPreviousOptions t.Options }
                    | v2_0_0.FieldType.MultiChoice t -> MultiChoice { Options = migrateFromPreviousOptions t.Options }
                    | v2_0_0.FieldType.CheckboxList t -> CheckboxList { Options = migrateFromPreviousOptions t.Options }
                    | v2_0_0.FieldType.Text t -> Text { Value = t.Value }
                    | v2_0_0.FieldType.TextArea t -> TextArea { Value = t.Value }
                    | v2_0_0.FieldType.Tel t -> Tel { Value = t.Value }
                    | v2_0_0.FieldType.Date t -> Date { Value = t.Value }
                    | v2_0_0.FieldType.Time t -> Time { Value = t.Value }
                    | v2_0_0.FieldType.Number t -> Number { Value = t.Value }
                    | v2_0_0.FieldType.StateSelectorUSA t -> StateSelectorUSA { Value = t.Value }
                    | v2_0_0.FieldType.YesNo t ->
                        YesNo {
                            DefaultValue = t.DefaultValue
                            Selection = t.Selection
                        }
                    | v2_0_0.FieldType.TrueFalse t ->
                        TrueFalse {
                            DefaultValue = t.DefaultValue
                            Selection = t.Selection
                        }
                    | v2_0_0.FieldType.Checkbox t ->
                        Checkbox {
                            DefaultValue = t.DefaultValue
                            Selection = t.Selection
                        }
                    | v2_0_0.FieldType.Radio t -> Radio { Options = migrateFromPreviousOptions t.Options }
                    | v2_0_0.FieldType.Dropdown t -> Dropdown { Options = migrateFromPreviousOptions t.Options }
                    | v2_0_0.FieldType.TagList t -> TagList { Options = migrateFromPreviousOptions t.Options }
                    | v2_0_0.FieldType.Switch t ->
                        Switch {
                            DefaultValue = t.DefaultValue
                            Selection = t.Selection
                        }
                    | v2_0_0.FieldType.TextAutoComplete t -> TextAutoComplete { Options = migrateFromPreviousOptions t.Options }
                    | v2_0_0.FieldType.EPrescribe t -> EPrescribe { Value = t.Value }

            }
        )

    let migrateFromPreviousSteps (oldSteps: v2_0_0.FormStep list) =
        oldSteps
        |> List.map (fun s ->
            {
                StepOrder = s.StepOrder
                StepLabel = s.StepLabel
                Fields =
                    s.Fields
                    |> migrateFromPreviousFields
            } : FormStep
        )

    let migrate (oldSpec: v2_0_0.FormSpec) : FormSpec =
        {
            Id = oldSpec.Id
            Code = oldSpec.Code
            Title = oldSpec.Title
            Abstract = oldSpec.Abstract
            Version = oldSpec.Version
            FormSpecVersion = "2.0.1"
            Steps =
                oldSpec.Steps
                |> migrateFromPreviousSteps
            CategoryTags = oldSpec.CategoryTags
            Score =
                match oldSpec.Score with
                | None -> None
                | Some s ->
                    Some {
                        MaxScore = s.MaxScore
                        ScoreRanges =
                            s.ScoreRanges
                            |> List.map (fun r ->
                                {
                                    Id = r.Id
                                    Min = r.Min
                                    Max = r.Max
                                    Label = r.Label
                                    Tag =
                                        match r.Tag with
                                        | v2_0_0.ScoreColor.Unspecified -> ScoreColor.Unspecified
                                        | v2_0_0.ScoreColor.Primary -> ScoreColor.Primary
                                        | v2_0_0.ScoreColor.Link -> ScoreColor.Link
                                        | v2_0_0.ScoreColor.Info -> ScoreColor.Info
                                        | v2_0_0.ScoreColor.Success -> ScoreColor.Success
                                        | v2_0_0.ScoreColor.Warning -> ScoreColor.Warning
                                        | v2_0_0.ScoreColor.Danger -> ScoreColor.Danger
                                }
                            )
                    }
            AssociatedCodes = []
        }
