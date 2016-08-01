// Copyright (c) IxMilia.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

namespace ExpressSchemaParser

open FParsec

module ExpressParser =
    let parser : Parser<ExpressSchema, unit> =
        let str = pstring
        let pcomment = str "--" >>. many1Satisfy ((<>) '\n')
        let pspaces = spaces >>. many (spaces >>. pcomment >>. spaces)
        let pmlcomment = pstring "(*" >>. skipCharsTillString "*)" true System.Int32.MaxValue
        let ws = pspaces >>. many (pspaces >>. pmlcomment >>. pspaces) |>> ignore
        let str_ws s = ws >>. str s .>> ws
        let colon = str_ws ":"
        let semicolon = str_ws ";"
        let equals = str_ws "="
        let colonEquals = str_ws ":="
        let comma = str_ws ","
        let leftParen = str_ws "("
        let rightParen = str_ws ")"
        let leftBracket = str_ws "["
        let rightBracket = str_ws "]"

        let isIdentifierCharStart c = isLower c || c = '_'
        let isIdentifierCharContinue c = isIdentifierCharStart c || isDigit c
        let identifier = many1Satisfy2 isIdentifierCharStart isIdentifierCharContinue
        let isKeywordChar c = isUpper c || c = '_'
        let keyword = many1Satisfy isKeywordChar
        let delimitedParameterizedKeyword command = str command >>. ws >>. identifier .>> semicolon
        let delimitedKeyword command = str_ws command .>> semicolon

        // parse expressions
        let opp = new OperatorPrecedenceParser<Expression, unit, unit>()
        let pexpression = opp.ExpressionParser
        let pterm =
            let strExpr = pchar '\'' >>. manySatisfy ((<>) '\'') .>> pchar '\''
            let qualifiedIdentifierPart = identifier <|> keyword .>> ws
            let qualifiedIdentifierDotPart = str_ws "." >>. qualifiedIdentifierPart |>> (fun p -> "." + p)
            let qualifiedIdentifierBackslashPart = str_ws "\\" >>. qualifiedIdentifierPart |>> (fun p -> "\\" + p)
            let qualifiedIdentifierTail = many (qualifiedIdentifierDotPart <|> qualifiedIdentifierBackslashPart) |>> List.fold (+) ""
            let qualifiedIdentifier = (qualifiedIdentifierPart .>>. qualifiedIdentifierTail) |>> (fun (a, b) -> a + b)
            let identOrFunction =
                let arguments = sepBy pexpression comma |>> List.toArray
                let argumentList = between leftParen rightParen arguments |>> (fun x -> ("func", x))
                let indexList = between leftBracket rightBracket arguments |>> (fun x -> ("index", x))
                ((qualifiedIdentifier <|> keyword) .>> ws) .>>. (opt (argumentList <|> indexList))
                |>> fun x ->
                    match x with
                    | (id, Some(args)) ->
                        match args with
                        | "func", a -> Function(id, a)
                        | "index", a -> Index(id, a)
                        | _ -> failwith "impossible"
                    | (id, None) -> Identifier(id)
            let parray = leftBracket >>. sepBy pexpression comma .>> rightBracket |>> List.toArray |>> Array
            choice [
                (pfloat .>> ws |>> Number)
                (strExpr .>> ws |>> String)
                parray
                identOrFunction
                between leftParen rightParen pexpression
            ]
        opp.TermParser <- pterm
        opp.AddOperator(InfixOperator("<", ws, 1, Associativity.Left, fun l r -> Less(l, r)))
        opp.AddOperator(InfixOperator("<*", ws, 1, Associativity.Left, fun l r -> Less(l, r)))
        opp.AddOperator(InfixOperator("<=", ws, 1, Associativity.Left, fun l r -> LessEqual(l, r)))
        opp.AddOperator(InfixOperator(">", ws, 1, Associativity.Left, fun l r -> Greater(l, r)))
        opp.AddOperator(InfixOperator(">=", ws, 1, Associativity.Left, fun l r -> GreaterEqual(l, r)))
        opp.AddOperator(InfixOperator("=", ws, 1, Associativity.Left, fun l r -> Equal(l, r)))
        opp.AddOperator(InfixOperator("AND", ws, 2, Associativity.Left, fun l r -> And(l, r)))
        opp.AddOperator(InfixOperator("OR", ws, 2, Associativity.Left, fun l r -> Or(l, r)))
        opp.AddOperator(InfixOperator("|", ws, 2, Associativity.Left, fun l r -> Or(l, r)))
        opp.AddOperator(InfixOperator("IN", ws, 2, Associativity.Left, fun l r -> In(l, r)))
        opp.AddOperator(InfixOperator("+", ws, 3, Associativity.Left, fun l r -> Add(l, r)))
        opp.AddOperator(InfixOperator("-", ws, 3, Associativity.Left, fun l r -> Sub(l, r)))
        opp.AddOperator(InfixOperator("*", ws, 4, Associativity.Left, fun l r -> Mul(l, r)))
        opp.AddOperator(InfixOperator("/", ws, 4, Associativity.Left, fun l r -> Div(l, r)))
        opp.AddOperator(InfixOperator("**", ws, 5, Associativity.Left, fun l r -> Pow(l, r)))

        // parse types
        let typeKind = keyword <|> identifier .>> ws
        let separatedValueList = leftParen >>. sepBy typeKind comma .>> rightParen |>> List.toArray
        let typeKindOfMany = str_ws "SELECT" >>. separatedValueList
        let singleTypeKind = typeKind |>> (fun k -> [|k|])
        let namedTypeBody = typeKindOfMany <|> singleTypeKind |>> SimpleList
        let enumerationOfMany = str_ws "ENUMERATION" >>. str_ws "OF" >>. separatedValueList |>> EnumerationList
        let resultSatisfies predicate msg (p: Parser<_,_>) : Parser<_,_> =
            let error = messageError msg
            fun stream ->
                let state = stream.State
                let reply = p stream
                if reply.Status <> Ok || predicate reply.Result then reply
                else
                    stream.BacktrackTo(state) // backtrack to beginning
                    Reply(Error, error)
        let restrictionName =
            resultSatisfies
                (fun s ->
                    match s with
                    | "END_ENTITY" | "END_TYPE" -> false
                    | _ -> true)
                "invalid restriction name"
                (identifier <|> keyword) .>> colon
        let restrictionCondition = pexpression .>> semicolon
        let restriction = restrictionName .>>. restrictionCondition
        let typeRestrictions = str_ws "WHERE" >>. many1 (restriction .>> ws |>> TypeRestriction) |>> List.toArray
        let optionalRestrictions = opt (ws >>. typeRestrictions) |>> (fun x -> match x with | Some(r) -> r | _ -> [||])
        let parseType =
            pipe4
                (str_ws "TYPE" >>. identifier .>> equals)
                ((enumerationOfMany <|> namedTypeBody) .>> semicolon)
                (optionalRestrictions .>> ws)
                (delimitedKeyword "END_TYPE")
                (fun name x restrictions _ ->
                    match x with
                    | SimpleList(types) -> SimpleType(name, types, restrictions)
                    | EnumerationList(values) -> Enumeration(name, values))

        // parse properties
        let propertyType =
            let isOptional = str "OPTIONAL" |> opt |>> (fun x -> match x with | Some(_) -> true | _ -> false)
            let setCountValue = (stringReturn "?" None) <|> (pint32 |>> Some)
            let setCounts = str_ws "SET" >>. leftBracket >>. setCountValue .>> colon .>>. setCountValue .>> rightBracket .>> str_ws "OF"
            pipe3
                (isOptional .>> ws)
                (opt setCounts)
                (keyword <|> identifier)
                (fun o b n -> PropertyType(o, n, b))
        let property = (identifier .>> colon) .>>. (ws >>. propertyType .>> ws) |>> Property
        let delimitedProperty = property .>> semicolon

        let derivedPropertyExpression = pexpression
        let derivedProperty = property .>> colonEquals .>>. derivedPropertyExpression .>> semicolon |>> DerivedProperty
        let derivedProperties = str_ws "DERIVE" >>. many1 (derivedProperty .>> ws) |>> List.toArray

        // parse entities
        let entity =
            let nameSuperSubType =
                let supertype =
                    let typeList = str_ws "ONEOF" >>. leftParen >>. sepBy (identifier .>> ws) comma .>> rightParen |>> List.toArray
                    let typeListOrSingleType = typeList <|> (identifier |>> (fun i -> [| i |] ))
                    opt (str_ws "SUPERTYPE" >>. str_ws "OF" >>. leftParen >>. typeListOrSingleType .>> rightParen)
                    |>> fun so ->
                        match so with
                        | Some(s) -> s
                        | None -> [||]
                let subtype =
                    opt (str_ws "SUBTYPE" >>. str_ws "OF" >>. leftParen >>. identifier .>> rightParen)
                    |>> fun so ->
                        match so with
                        | Some(s) -> s
                        | None -> ""
                pipe3
                    (str_ws "ENTITY" >>. identifier .>> ws)
                    supertype
                    (subtype .>> semicolon)
                    (fun a b c -> (a, b, c))
            pipe4
                nameSuperSubType
                (ws >>. many (delimitedProperty .>> ws) |>> List.toArray) // properties
                (ws >>. (derivedProperties |> opt) |>> (fun x -> match x with | Some(i) -> i | _ -> [||])) // derived properties
                (optionalRestrictions .>> ws .>> delimitedKeyword "END_ENTITY") // entity restrictions
                (fun (n, ss, s) p d r -> Entity(n, s, ss, p, d, r))

        // parse the schema
        let schema =
            pipe4
                (delimitedParameterizedKeyword "SCHEMA") // schema name
                (ws >>. many (parseType .>> ws) |>> List.toArray) // types
                (ws >>. many (entity .>> ws) |>> List.toArray) // entities
                (delimitedKeyword "END_SCHEMA")
                (fun n t e _ -> ExpressSchema(n, t, e))

        ws >>. schema .>> ws .>> eof
