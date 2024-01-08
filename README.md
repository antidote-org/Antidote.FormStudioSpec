# Antidote.FormStudioSpec
Github Repo [link](https://github.com/antidote-org/Antidote.FormStudioSpec)

## Overview

An F# form specifically designed for Antidote.FormStudio for producing dynamically generated Fable.Form forms. While it was designed in conjuction with FormStudio, it is project agnostic and could be used on any project for giving a strong specification for forms that support steps and a wide range of fields.


## Installation

```
## using nuget
dotnet add package Antidote.FormStudioSpec
```

## or with paket

```
paket add Antidote.FormStudioSpec --project /path/to/project.fsproj
```


## To publish

*For maintainers only*

```ps1
cd Antidote.uFuzzy
dotnet pack -c Release
dotnet nuget push .\bin\Release\Antidote.FormStudioSpec.X.X.X.snupkg -s nuget.org -k <nuget_key>
dotnet nuget push .\bin\Release\Antidote.FormStudioSpec.X.X.X.nupkg -s nuget.org -k <nuget_key>
```
