// Copyright (c) IxMilia.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

namespace ExpressSchemaParser

type Expression =
    | Number of float
    | String of string
    | Identifier of string
    | Array of Expression array
    | Function of string * Expression array
    | Index of string * Expression array
    | Less of Expression * Expression
    | LessEqual of Expression * Expression
    | Greater of Expression * Expression
    | GreaterEqual of Expression * Expression
    | Equal of Expression * Expression
    | And of Expression * Expression
    | Or of Expression * Expression
    | In of Expression * Expression
    | Add of Expression * Expression
    | Sub of Expression * Expression
    | Mul of Expression * Expression
    | Div of Expression * Expression
    | Pow of Expression * Expression

type PropertyType (optional: bool, typeName: string, bounds: (int option * int option) option) =
    member this.TypeName = typeName
    member this.IsOptional = optional
    member this.Bounds = bounds

type Property (name: string, typ: PropertyType) =
    member this.Name = name
    member this.Type = typ

type DerivedProperty (prop: Property, expr: Expression) =
    member this.Property = prop
    member this.Expression = expr

type TypeList =
    | SimpleList of string array
    | EnumerationList of string array

type TypeRestriction =
    | TypeRestriction of string * Expression
    with
        member this.Name =
            match this with
            | TypeRestriction(name, _) -> name
        member this.Expression =
            match this with
            | TypeRestriction(_, expr) -> expr

type Type =
    | SimpleType of Name : string * Types : string array * Restrictions : TypeRestriction array
    | Enumeration of Name : string * Values : string array
    with
        member this.Name =
            match this with
            | SimpleType(name, _, _) -> name
            | Enumeration(name, _) -> name
        member this.Types =
            match this with
            | SimpleType(_, types, _) -> types
            | _ -> failwith "enumerations don't have types"
        member this.Restrictions =
            match this with
            | SimpleType(_, _, restrictions) -> restrictions
            | _ -> failwith "enumerations don't have restrictions"
        member this.Values =
            match this with
            | Enumeration(_, values) -> values
            | _ -> failwith "simple types don't have values"

type Entity (name: string, subtype: string, supertypes: string array, properties: Property array, derived: DerivedProperty array, restrictions: TypeRestriction array) =
    member this.Name = name
    member this.SubType = subtype
    member this.SuperTypes = supertypes
    member this.Properties = properties
    member this.DerivedProperties = derived
    member this.Restrictions = restrictions

type ExpressSchema (name : string, types : Type array, entities : Entity array) =
    member this.Name = name
    member this.Types = types
    member this.Entities = entities
