module Antidote.Core.FormProcessor.Spec.Types

type CategoryTag  =
    | MentalHealth
    | IllicitDrugs
    | Alcohol
    | RiskScore
    | COPD
    | HRA
    | HeartDisease
    | GeneralWellness
    | KidneyDisease
    | Diabetes
    | Hospital
    | MedAdherance

    member x.toString =
        match x with
        | MentalHealth -> "Mental Health"
        | IllicitDrugs -> "Illicit Drugs"
        | Alcohol -> "Alcohol"
        | RiskScore -> "Risk Score"
        | COPD -> "COPD"
        | HRA -> "HRA"
        | HeartDisease -> "Heart Disease"
        | GeneralWellness -> "General Wellness"
        | KidneyDisease -> "Kidney Disease"
        | Diabetes -> "Diabetes"
        | Hospital -> "Hospital"
        | MedAdherance -> "Med Adherance"

    static member fromString (s: string) =
        match s with
        | "Mental Health" -> MentalHealth
        | "Illicit Drugs" -> IllicitDrugs
        | "Alcohol" -> Alcohol
        | "Risk Score" -> RiskScore
        | "COPD" -> COPD
        | "HRA" -> HRA
        | "Heart Disease" -> HeartDisease
        | "General Wellness" -> GeneralWellness
        | "Kidney Disease" -> KidneyDisease
        | "Diabetes" -> Diabetes
        | "Hospital" -> Hospital
        | "Med Adherance" -> MedAdherance
        | _ -> failwithf "Unknown category tag: %s" s


    member x.toIcon =
        match x with
        | MentalHealth -> "/images/brain2.svg"
        | IllicitDrugs -> "/images/pills.svg"
        | Alcohol -> "/images/alcohol.svg"
        | RiskScore -> "/images/speedometer.svg"
        | COPD -> "/images/lungs.svg"
        | HRA -> "/images/assessment.svg"
        | HeartDisease -> "/images/heart.svg"
        | GeneralWellness -> "/images/healthcare.svg"
        | KidneyDisease -> "/images/kidney.svg"
        | Diabetes -> "/images/insulin-pen.svg"
        | Hospital -> "/images/medical-kit.svg"
        | MedAdherance -> "/images/tablet.svg"

    member x.toIndex =
        match x with
        | MentalHealth -> 0
        | IllicitDrugs -> 1
        | Alcohol -> 2
        | RiskScore -> 3
        | COPD -> 4
        | HRA -> 5
        | HeartDisease -> 6
        | GeneralWellness -> 7
        | KidneyDisease -> 8
        | Diabetes -> 9
        | Hospital -> 10
        | MedAdherance -> 11

    member x.fromIndex (categoryTagIndex: int) =
        match categoryTagIndex with
        | 0 -> MentalHealth
        | 1 -> IllicitDrugs
        | 2 -> Alcohol
        | 3 -> RiskScore
        | 4 -> COPD
        | 5 -> HRA
        | 6 -> HeartDisease
        | 7 -> GeneralWellness
        | 8 -> KidneyDisease
        | 9 -> Diabetes
        | 10 -> Hospital
        | 11 -> MedAdherance
        | _ -> failwith $"{categoryTagIndex} is not a valid Category Tag index"


    static member validate (categoryTagIndex) =
        match categoryTagIndex with
        | 0
        | 1
        | 2
        | 3
        | 4
        | 5
        | 6
        | 7
        | 8
        | 9
        | 10
        | 11 -> Ok ()
        | _ -> Error "Invalid value for AccountType index"

let categoryTagToString (c: CategoryTag) =
    match c with
    | MentalHealth -> "Mental Health"
    | IllicitDrugs -> "Illicit Drugs"
    | Alcohol -> "Alcohol"
    | RiskScore -> "Risk Score"
    | COPD -> "COPD"
    | HRA -> "HRA"
    | HeartDisease -> "Heart Disease"
    | GeneralWellness -> "General Wellness"
    | KidneyDisease -> "Kidney Disease"
    | Diabetes -> "Diabetes"
    | Hospital -> "Hospital"
    | MedAdherance -> "Med Adherance"

let availableCategoryTags =
    [ MentalHealth; IllicitDrugs; Alcohol; RiskScore; COPD; HRA; HeartDisease; GeneralWellness; KidneyDisease; Diabetes; Hospital; MedAdherance; ]
    |> List.sort

module CategoryTag =
    let getList () =
        [ MentalHealth; IllicitDrugs; Alcohol; RiskScore; COPD; HRA; HeartDisease; GeneralWellness; KidneyDisease; Diabetes; Hospital; MedAdherance; ]
        |> List.sort
