module Compiler where

import Data.Map (Map)
import qualified Data.Map as Map
import PazLexer (
    ASTLexicalToken,
    LexicalToken(..),
    ASTCharacterString,
    ASTIdentifier,
    ASTUnsignedInteger,
    ASTUnsignedReal,
    printTokenLeftBrace,
    printTokenRightBrace,
    printTokenLeftParenthesis,
    printTokenRightParenthesis,
    printTokenTimes,
    printTokenPlus,
    printTokenComma,
    printTokenMinus,
    printTokenEllipsis,
    printTokenDot,
    printTokenDivideBy,
    printTokenAssign,
    printTokenColon,
    printTokenSemicolon,
    printTokenLessThanOrEqual,
    printTokenNotEqual,
    printTokenLessThan,
    printTokenEqual,
    printTokenGreaterThanOrEqual,
    printTokenGreaterThan,
    printTokenLeftBracket,
    printTokenRightBracket,
    printTokenAnd,
    printTokenArray,
    printTokenBegin,
    printTokenBoolean,
    printTokenDiv,
    printTokenDo,
    printTokenDownTo,
    printTokenElse,
    printTokenEnd,
    printTokenFor,
    printTokenFunction,
    printTokenIf,
    printTokenInteger,
    printTokenNot,
    printTokenOf,
    printTokenOr,
    printTokenProcedure,
    printTokenProgram,
    printTokenReal,
    printTokenThen,
    printTokenTo,
    printTokenVar,
    printTokenWhile,
    printCharacterString,
    printTokenSingleQuote,
    printStringElement,
    printIdentifier,
    printUnsignedInteger,
    printDigitSequence,
    printUnsignedReal
    )
import PazParser (
    ASTStartSymbol,
    ASTProgram,
    ASTProcedureDeclarationPart,
    ASTStatement,
    Statement(..),
    ASTEmptyStatement,
    ASTAssignmentStatement,
    ASTReadStatement,
    ASTWriteStatement,
    ASTWriteStringStatement,
    ASTWritelnStatement,
    ASTProcedureStatement,
    ASTActualParameterList,
    ASTCompoundStatement,
    ASTStatementSequence,
    ASTIfStatement,
    ASTWhileStatement,
    ASTForStatement,
    ForDirection(..),
    BinaryOperator(..),
    Expression(..),
    ASTExpression,
    ASTRelationalOperator,
    ASTSimpleExpression,
    ASTAddingOperator,
    ASTTerm,
    ASTMultiplyingOperator,
    ASTFactor,
    ASTUnsignedConstant,
    UnsignedConstant(..),
    ASTBooleanConstant,
    ASTVariableAccess,
    VariableAccess(..),
    ASTIndexedVariable,
    ASTProcedureDeclaration,
    ASTFormalParameterList,
    ASTFormalParameterSection,
    ASTIdentifierList,
    ASTVariableDeclarationPart,
    ASTVariableDeclaration,
    ASTTypeDenoter,
    TypeDenoter(..),
    ASTTypeIdentifier,
    TypeIdentifier(..),
    ASTArrayType,
    ASTSubrangeType,
    ASTConstant,
    ASTSign,
    Sign(..),
    generateSpaces,
    printStartSymbol,
    printProgram,
    printProcedureDeclarationPart,
    printStatement,
    printEmptyStatement,
    printAssignmentStatement,
    printReadStatement,
    printWriteStatement,
    printWriteStringStatement,
    printWritelnStatement,
    printProcedureStatement,
    printActualParameterList,
    printCompoundStatement,
    printStatementSequence,
    printIfStatement,
    printWhileStatement,
    printForStatement,
    printBinaryOperator,
    printExpression,
    printSimpleExpression,
    printTerm,
    printFactor,
    printUnsignedConstant,
    printBooleanConstant,
    printVariableAccess,
    printIndexedVariable,
    printProcedureDeclaration,
    printFormalParameterList,
    printFormalParameterSection,
    printIdentifierList,
    printVariableDeclarationPart,
    printVariableDeclaration,
    printTypeDenoter,
    printTypeIdentifier,
    printArrayType,
    printSubrangeType,
    printConstant,
    printSign
    )

-- this is the entry point to the compiler from the Paz.hs driver module
compileStartSymbol :: ASTStartSymbol -> String
compileStartSymbol =
    compileProgram

-- the following is a suggestion for how you can maintain the symbol table,
-- the symbol information for procedures and for variables is kept separate
-- (although this isn't a requirement, they can share the same name space
-- if you wish), and put in a pair to make it easy to pass around everywhere
type Symbols =
    (
        -- for each procedure, for each formal parameter, its varness and type
        Map String [(Bool, ASTTypeDenoter)],

        -- for each variable, its varness, type, and starting slot number
        Map String (Bool, ASTTypeDenoter, Int)
        )

-- the following is a suggestion for how your compiler can be structured,
-- there is no requirement to follow this template but it shows how the
-- important information (such as current label number) can be threaded
-- through the functions that implement the various parts of the compiler
-- (the more advanced students might wish to use a state monad for this)
compileProgram :: ASTProgram -> String
compileProgram (name, varDecls, procDecls, bodyStatement) =
    let
        slot = 0
        symbols = (Map.empty, Map.empty)
        label = 0

        -- pre-compile the procedure declarations to create a map from
        -- procedure name to formal parameter list, then really compile
        symbols' = precompileProcedureDeclarationPart symbols procDecls

        -- the following is a bit crusty due to the awkward order of declaring
        -- variables vs. procedures (because variables are meant to be global,
        -- but aren't in Paz), hence why we use symbols' twice, not symbols''
        (slot', symbols'') =
            compileVariableDeclarationPart slot symbols' varDecls
        (label', procText) =
            compileProcedureDeclarationPart label symbols' procDecls
        (label'', bodyText) =
            compileCompoundStatement label' symbols'' bodyStatement
    in
        "# " ++
            printTokenProgram ++
            " " ++
            printIdentifier name ++
            "\n    call main\n    halt\n" ++
            procText ++
            "main:\n# prologue\n    push_stack_frame " ++
            show slot' ++
            "\n" ++
            bodyText ++
            "# epilogue\n    pop_stack_frame " ++
            show slot' ++
            "\n    return\n"

-- the following pre-compilation functions are intended as an example of
-- how you can walk through the AST gathering information into a symbol
-- table (by adding additional state such as the label or slot number and
-- changing the return type, you can easily modify this to compile things)
precompileProcedureDeclarationPart ::
    Symbols -> ASTProcedureDeclarationPart -> Symbols
precompileProcedureDeclarationPart symbols [] =
    symbols
precompileProcedureDeclarationPart symbols (x : xs) =
    -- the following doesn't actually require a let statement, but when
    -- you start adding additional parameters and return values it might
    let
        symbols' = precompileProcedureDeclaration symbols x
        symbols'' = precompileProcedureDeclarationPart symbols' xs
    in
        symbols''

precompileProcedureDeclaration ::
    Symbols -> ASTProcedureDeclaration -> Symbols
precompileProcedureDeclaration (procSymbols, varSymbols) (x0, x1, _, _) =
    case Map.lookup x0 procSymbols of
        Nothing ->
            (
                Map.insert x0 (precompileFormalParameterList x1) procSymbols,
                varSymbols
                )
        otherwise ->
            error ("multiply defined procedure: " ++ x0)

precompileFormalParameterList ::
    ASTFormalParameterList -> [(Bool, ASTTypeDenoter)]
precompileFormalParameterList [] =
    []
precompileFormalParameterList (x : xs) =
    precompileFormalParameterSection x ++ precompileFormalParameterList xs

precompileFormalParameterSection ::
    ASTFormalParameterSection -> [(Bool, ASTTypeDenoter)]
precompileFormalParameterSection (isVar, [], typeDenoter) =
    []
precompileFormalParameterSection (isVar, (x : xs), typeDenoter) =
    (isVar, typeDenoter) :
        precompileFormalParameterSection (isVar, xs, typeDenoter)

-- the following is code that you have to implement
-- we've provided signatures and comments corresponding to the calls made
-- above, there is no requirement that you follow these signatures at all!

-- compile a list of variable declarations
-- takes a slot number, a symbol table and an AST fragment
-- returns the advanced slot number and the updated symbol table
compileVariableDeclarationPart ::
    Int -> Symbols -> ASTVariableDeclarationPart -> (Int, Symbols)
compileVariableDeclarationPart slot symbols [] =
    (slot, symbols)
compileVariableDeclarationPart slot symbols ((idl, td) : xs) =
    let
        (slot', symbols') = compileIdentifierList slot symbols (False, idl, td)
    in
        compileVariableDeclarationPart slot' symbols' xs

compileIdentifierList ::
    Int -> Symbols -> (Bool, ASTIdentifierList, ASTTypeDenoter)
        -> (Int, Symbols)
compileIdentifierList slot symbols (_, [], _) =
    (slot, symbols)
compileIdentifierList slot symbols (var, (x:xs), td) =
    let
        (ptable, table) = symbols
    in
        compileIdentifierList (slot + 1)
            (ptable, Map.insert x (var, td, slot) table)
            (var, xs, td)

-- compile a list of procedures
-- takes a label number, a symbol table and an AST fragment
-- returns the advanced label number and the generated code
compileProcedureDeclarationPart ::
    Int -> Symbols -> ASTProcedureDeclarationPart -> (Int, String)
compileProcedureDeclarationPart label symbols [] =
    (label, "")
compileProcedureDeclarationPart label symbols (x : xs) =
    let
        (label', text1) = compileProcedureDeclaration label symbols x
        (label'', text2) = compileProcedureDeclarationPart label' symbols xs
    in
        (label'', text1 ++ text2)

compileProcedureDeclaration ::
    Int -> Symbols -> ASTProcedureDeclaration -> (Int, String)
compileProcedureDeclaration label symbols (pid, fpl, vdp, cs) =
    let
        -- since the program variables are isolated from the procedure variables
        (ptable, _) = symbols
        symbols' = (ptable, Map.empty)

        (slot, symbols'', text1) = compileFormalParameterList 0 symbols' fpl
        (slot', symbols''') = compileVariableDeclarationPart slot symbols'' vdp
        (label', text2) = compileCompoundStatement label symbols''' cs
    in
        (
            label',
            "# procedure " ++
                pid ++
                "\n" ++
                pid ++
                ":\n    push_stack_frame " ++
                show slot' ++
                "\n" ++
                text1 ++ "" ++
                text2 ++
                "    pop_stack_frame " ++
                show slot' ++
                "\n    return\n"
            )

compileFormalParameterList ::
    Int -> Symbols -> ASTFormalParameterList -> (Int, Symbols, String)
compileFormalParameterList slot symbols [] =
    (slot, symbols, "")
compileFormalParameterList slot symbols (x:xs) =
    let
        (slot', symbols') =
            compileIdentifierList slot symbols x
        (slot'', symbols'', text) = compileFormalParameterList slot' symbols' xs
    in
        (
            slot'',
            symbols'',
            "    store " ++
                show slot ++
                ", r" ++
                show slot ++
                "\n" ++ text
            )

-- compile a list of statements
-- takes a label number, a symbol table and an AST fragment
-- returns the advanced label number and the generated code
compileCompoundStatement ::
    Int -> Symbols -> ASTCompoundStatement -> (Int, String)
compileCompoundStatement label symbols [] =
    (label, "")
compileCompoundStatement label symbols (x:xs) =
    let
        (label', text) = compileStatement label symbols x
        (label'', text') = compileCompoundStatement label' symbols xs
    in
        (label'', text ++ text')

compileStatement ::
    Int -> Symbols -> ASTStatement -> (Int, String)
compileStatement label symbols s =
    case s of
        AssignmentStatement (va, e) ->
            let
                (tid, text) = compileExpression 0 symbols False e
                (_, table) = symbols
            in
                (
                    label,
                    "# assignment\n" ++ text ++ case va of
                        IndexedVariableAccess iva ->
                            undefined
                        OrdinaryVariableAccess vid ->
                            let
                                (var, td, slot) = case Map.lookup vid table of
                                    Nothing ->
                                        error (
                                            "variable " ++ vid ++ " undeclared"
                                            )
                                    Just i ->
                                        i
                                text = if var then
                                    "    load r1, " ++
                                        show slot ++
                                        "\n    store_indirect r1, r0\n"
                                    else "    store " ++
                                        show slot ++
                                        ", r0\n"
                            in
                                case td of
                                    ArrayTypeDenoter _ ->
                                        error "cannot put value into array"
                                    OrdinaryTypeDenoter tid' ->
                                        case (tid', tid) of
                                            (
                                                IntegerTypeIdentifier,
                                                IntegerTypeIdentifier
                                                ) ->
                                                    text
                                            (
                                                RealTypeIdentifier,
                                                RealTypeIdentifier
                                                ) ->
                                                    text
                                            (
                                                BooleanTypeIdentifier,
                                                BooleanTypeIdentifier
                                                ) ->
                                                    text
                                            (
                                                RealTypeIdentifier,
                                                IntegerTypeIdentifier
                                                ) ->
                                                    "    int_to_real r1, r1" ++
                                                        text
                                            otherwise ->
                                                error "unmatched type"
                    )
        ReadStatement rs ->
            (
                label,
                let
                    (_, table) = symbols
                in
                    "# read\n    call_builtin " ++ case rs of
                        IndexedVariableAccess iva ->
                            undefined
                        OrdinaryVariableAccess vid ->
                            let
                                (var, td, slot) = case Map.lookup vid table of
                                    Nothing ->
                                        error (
                                            "variable " ++ vid ++ " undeclared"
                                            )
                                    Just i ->
                                        i
                                t = case td of
                                    ArrayTypeDenoter _ ->
                                        error "cannot put value into array"
                                    OrdinaryTypeDenoter tid ->
                                        case tid of
                                            IntegerTypeIdentifier ->
                                                "read_int"
                                            RealTypeIdentifier ->
                                                "read_real"
                                            BooleanTypeIdentifier ->
                                                "read_bool"
                            in
                                if var then
                                    undefined
                                    else t ++ "\n    store " ++
                                        show slot ++
                                        ", r0\n"
                )
        WriteStatement ws ->
            (
                label,
                let
                    (tid, text) = compileExpression 0 symbols False ws
                in
                    "# write\n" ++
                        text ++
                        "    call_builtin " ++ case tid of
                            IntegerTypeIdentifier ->
                                "print_int"
                            RealTypeIdentifier ->
                                "print_real"
                            BooleanTypeIdentifier ->
                                "print_bool"
                        ++ "\n"
            )
        WriteStringStatement wss ->
            (
                label,
                "# write\n    string_const r0, '" ++ wss ++
                    "'\n    call_builtin print_string\n"
                )
        WritelnStatement ->
            (label, "# writeln\n    call_builtin print_newline\n")
        ProcedureStatement (pid, apl) ->
            (
                label,
                let
                    (ptable, _) = symbols
                    fpl = case Map.lookup pid ptable of
                        Nothing ->
                            error ("procedure" ++ pid ++ "undeclared")
                        Just fpl ->
                            fpl
                    (_, text1) = compileActualParameterList 0 symbols fpl apl
                in
                    "# get actual parameters\n" ++
                        text1 ++
                        "# call procedure\n    call " ++
                        pid ++
                        "\n"
                )
        CompoundStatement cs ->
            compileCompoundStatement label symbols cs
        IfStatement (e, s, ms) ->
            let
                (td, text1) = compileExpression 0 symbols False e
                (label', text2) = if td == BooleanTypeIdentifier then
                    compileStatement label symbols s
                    else error "if guard should be boolean expression"
                text = "# if\n" ++
                    text1 ++
                    "    branch_on_false r0, label" ++
                    show label ++
                    "\n" ++
                    text2
            in
                case ms of
                    Nothing ->
                        (
                            label' + 1,
                            text ++
                                "label" ++
                                show label ++
                                ":\n"
                            )
                    -- have a else branch
                    Just s ->
                        let
                            (label'', text3) =
                                compileStatement (label' + 1) symbols s
                        in
                            (
                                label'' + 1,
                                text ++
                                    "    branch_uncond label" ++
                                    show label'' ++
                                    "\nlabel" ++
                                    show label ++
                                    ":\n" ++
                                    text3 ++
                                    "label" ++
                                    show label'' ++
                                    ":\n"
                                )
        WhileStatement (e, s) ->
            (
                label + 2,
                let
                    label' = label + 1
                    (td, text1) = compileExpression 0 symbols False e
                    (_, text2) = if td == BooleanTypeIdentifier then
                        compileStatement label' symbols s
                        else error "while guard should be boolean expression"
                in
                    "# while\nlabel" ++
                        show label ++
                        ":\n" ++
                        text1 ++
                        "    branch_on_false r0, label" ++
                        show label' ++
                        "\n" ++
                        text2 ++
                        "    branch_uncond label" ++
                        show label ++
                        "\nlabel" ++
                        show label' ++
                        ":\n"
            )
        ForStatement fs ->
            undefined
        EmptyStatement ->
            (label, "")

compileActualParameterList ::
    Int -> Symbols -> [(Bool, ASTTypeDenoter)] -> ASTActualParameterList
        -> (Int, String)
compileActualParameterList slot _ [] [] =
    (slot, "")
compileActualParameterList _ _ _ [] =
    error "parameters supplied less than expected"
compileActualParameterList _ _ [] _ =
    error "parameters supplied more than expected"
compileActualParameterList slot symbols ((var, td):xs) (y:ys) =
    let
        (atid, text) = compileExpression slot symbols var y
        (slot', text1) = case td of
            ArrayTypeDenoter at ->
                undefined
            OrdinaryTypeDenoter ftid ->
                if ftid == atid then
                    compileActualParameterList (slot + 1) symbols xs ys
                    else error "parameter type mismatch"
    in
        (slot', text ++ text1)

compileExpression ::
    Int -> Symbols -> Bool -> ASTExpression -> (ASTTypeIdentifier, String)
compileExpression reg symbols var e = case e of
    EqualExpression se1 se2 ->
        let
            reg' = reg + 1
            (td1, text1) = compileSimpleExpression reg symbols var se1
            (td2, text2) = compileSimpleExpression reg' symbols var se2
            r1 = case td1 of
                IntegerTypeIdentifier ->
                    False
                RealTypeIdentifier ->
                    True
                BooleanTypeIdentifier ->
                    error "cannot compare boolean value"
            r2 = case td2 of
                IntegerTypeIdentifier ->
                    False
                RealTypeIdentifier ->
                    True
                BooleanTypeIdentifier ->
                    error "cannot compare boolean value"
            r = if r1 || r2 then True else False
        in
            (
                BooleanTypeIdentifier,
                text1 ++ text2 ++
                    if r then "    cmp_eq_real r" else "    cmp_eq_int r" ++
                    show reg ++ ", r" ++ show reg ++ ", r" ++ show reg' ++ "\n"
                )
    NotEqualExpression se1 se2 ->
        undefined
    LessThanExpression se1 se2 ->
        let
            reg' = reg + 1
            (td1, text1) = compileSimpleExpression reg symbols var se1
            (td2, text2) = compileSimpleExpression reg' symbols var se2
            r1 = case td1 of
                IntegerTypeIdentifier ->
                    False
                RealTypeIdentifier ->
                    True
                BooleanTypeIdentifier ->
                    error "cannot compare boolean value"
            r2 = case td2 of
                IntegerTypeIdentifier ->
                    False
                RealTypeIdentifier ->
                    True
                BooleanTypeIdentifier ->
                    error "cannot compare boolean value"
            r = if r1 || r2 then True else False
        in
            (
                BooleanTypeIdentifier,
                text1 ++ text2 ++
                    if r then "    cmp_lt_real r" else "    cmp_lt_int r" ++
                    show reg ++ ", r" ++ show reg ++ ", r" ++ show reg' ++ "\n"
                )
    GreaterThanExpression se1 se2 ->
        let
            reg' = reg + 1
            (td1, text1) = compileSimpleExpression reg symbols var se1
            (td2, text2) = compileSimpleExpression reg' symbols var se2
            r1 = case td1 of
                IntegerTypeIdentifier ->
                    False
                RealTypeIdentifier ->
                    True
                BooleanTypeIdentifier ->
                    error "cannot compare boolean value"
            r2 = case td2 of
                IntegerTypeIdentifier ->
                    False
                RealTypeIdentifier ->
                    True
                BooleanTypeIdentifier ->
                    error "cannot compare boolean value"
            r = if r1 || r2 then True else False
        in
            (
                BooleanTypeIdentifier,
                text1 ++ text2 ++
                    if r then "    cmp_gt_real r" else "    cmp_gt_int r" ++
                    show reg ++ ", r" ++ show reg ++ ", r" ++ show reg' ++ "\n"
                )
    LessThanOrEqualExpression se1 se2 ->
        undefined
    GreaterThanOrEqualExpression se1 se2 ->
        undefined
    otherwise ->
        compileSimpleExpression reg symbols var e

compileSimpleExpression ::
    Int -> Symbols -> Bool -> ASTSimpleExpression -> (ASTTypeIdentifier, String)
compileSimpleExpression reg symbols var se = case se of
    PlusExpression se t ->
        let
            reg' = reg + 1
            (tid1, text1) = compileSimpleExpression reg symbols var se
            (tid2, text2) = if tid1 == BooleanTypeIdentifier then
                error "cannot be boolean value"
                else compileTerm reg' symbols var t
        in
            if tid2 == BooleanTypeIdentifier then
                error "cannot be boolean value"
                else case (tid1, tid2) of
                    (IntegerTypeIdentifier, IntegerTypeIdentifier) ->
                        (
                            IntegerTypeIdentifier,
                            text1 ++
                                text2 ++
                                "    add_int r" ++
                                show reg ++
                                ", r" ++
                                show reg ++
                                ", r" ++
                                show reg' ++
                                "\n"
                            )
                    (RealTypeIdentifier, RealTypeIdentifier) ->
                        (
                            RealTypeIdentifier,
                            text1 ++
                                text2 ++
                                "    add_real r" ++
                                show reg ++
                                ", r" ++
                                show reg ++
                                ", r" ++
                                show reg' ++
                                "\n"
                            )
                    (IntegerTypeIdentifier, RealTypeIdentifier) ->
                        (
                            RealTypeIdentifier,
                            text1 ++
                                "    int_to_real r" ++
                                show reg ++
                                ", r" ++
                                show reg ++
                                "\n" ++
                                text2 ++
                                "    add_real r" ++
                                show reg ++
                                ", r" ++
                                show reg ++
                                ", r" ++
                                show reg' ++
                                "\n"
                            )
                    (RealTypeIdentifier, IntegerTypeIdentifier) ->
                        (
                            RealTypeIdentifier,
                            text1 ++
                                text2 ++
                                "    int_to_real r" ++
                                show reg' ++
                                ", r" ++
                                show reg' ++
                                "\n" ++
                                "    add_real r" ++
                                show reg ++
                                ", r" ++
                                show reg ++
                                ", r" ++
                                show reg' ++
                                "\n"
                            )
    MinusExpression se t ->
        let
            reg' = reg + 1
            (tid1, text1) = compileSimpleExpression reg symbols var se
            (tid2, text2) = if tid1 == BooleanTypeIdentifier then
                error "cannot be boolean value"
                else compileTerm reg' symbols var t
        in
            if tid2 == BooleanTypeIdentifier then
                error "cannot be boolean value"
                else case (tid1, tid2) of
                    (IntegerTypeIdentifier, IntegerTypeIdentifier) ->
                        (
                            IntegerTypeIdentifier,
                            text1 ++
                                text2 ++
                                "    sub_int r" ++
                                show reg ++
                                ", r" ++
                                show reg ++
                                ", r" ++
                                show reg' ++
                                "\n"
                            )
                    (RealTypeIdentifier, RealTypeIdentifier) ->
                        (
                            RealTypeIdentifier,
                            text1 ++
                                text2 ++
                                "    sub_real r" ++
                                show reg ++
                                ", r" ++
                                show reg ++
                                ", r" ++
                                show reg' ++
                                "\n"
                            )
                    (IntegerTypeIdentifier, RealTypeIdentifier) ->
                        (
                            RealTypeIdentifier,
                            text1 ++
                                "    int_to_real r" ++
                                show reg ++
                                ", r" ++
                                show reg ++
                                "\n" ++
                                text2 ++
                                "    sub_real r" ++
                                show reg ++
                                ", r" ++
                                show reg ++
                                ", r" ++
                                show reg' ++
                                "\n"
                            )
                    (RealTypeIdentifier, IntegerTypeIdentifier) ->
                        (
                            RealTypeIdentifier,
                            text1 ++
                                text2 ++
                                "    int_to_real r" ++
                                show reg' ++
                                ", r" ++
                                show reg' ++
                                "\n" ++
                                "    sub_real r" ++
                                show reg ++
                                ", r" ++
                                show reg ++
                                ", r" ++
                                show reg' ++
                                "\n"
                            )
    OrExpression se t ->
        undefined
    PosExpression t ->
        undefined
    NegExpression t ->
        undefined
    otherwise ->
        compileTerm reg symbols var se

compileTerm ::
    Int -> Symbols -> Bool -> ASTTerm -> (ASTTypeIdentifier, String)
compileTerm reg symbols var t = case t of
    TimesExpression t f ->
        let
            reg' = reg + 1
            (tid1, text1) = compileTerm reg symbols var t
            (tid2, text2) = if tid1 == BooleanTypeIdentifier then
                error "cannot be boolean value"
                else compileFactor reg' symbols var f
        in
            if tid2 == BooleanTypeIdentifier then
                error "cannot be boolean value"
                else case (tid1, tid2) of
                    (IntegerTypeIdentifier, IntegerTypeIdentifier) ->
                        (
                            IntegerTypeIdentifier,
                            text1 ++
                                text2 ++
                                "    mul_int r" ++
                                show reg ++
                                ", r" ++
                                show reg ++
                                ", r" ++
                                show reg' ++
                                "\n"
                            )
                    (RealTypeIdentifier, RealTypeIdentifier) ->
                        (
                            RealTypeIdentifier,
                            text1 ++
                                text2 ++
                                "    mul_real r" ++
                                show reg ++
                                ", r" ++
                                show reg ++
                                ", r" ++
                                show reg' ++
                                "\n"
                            )
                    (IntegerTypeIdentifier, RealTypeIdentifier) ->
                        (
                            RealTypeIdentifier,
                            text1 ++
                                "    int_to_real r" ++
                                show reg ++
                                ", r" ++
                                show reg ++
                                "\n" ++
                                text2 ++
                                "    mul_real r" ++
                                show reg ++
                                ", r" ++
                                show reg ++
                                ", r" ++
                                show reg' ++
                                "\n"
                            )
                    (RealTypeIdentifier, IntegerTypeIdentifier) ->
                        (
                            RealTypeIdentifier,
                            text1 ++
                                text2 ++
                                "    int_to_real r" ++
                                show reg' ++
                                ", r" ++
                                show reg' ++
                                "\n" ++
                                "    mul_real r" ++
                                show reg ++
                                ", r" ++
                                show reg ++
                                ", r" ++
                                show reg' ++
                                "\n"
                            )
                    otherwise ->
                        error "invalid type"
    DivideByExpression t f ->
        undefined
    DivExpression t f ->
        (
            IntegerTypeIdentifier,
            let
                reg' = reg + 1
                (tid1, text1) = compileTerm reg symbols var t
                (tid2, text2) = if tid1 == IntegerTypeIdentifier then
                    compileFactor reg' symbols var f
                    else error "both side should be integers"
            in
                if tid2 == IntegerTypeIdentifier then
                    text1 ++
                        text2 ++
                        "    div_int r" ++
                        show reg ++
                        ", r" ++
                        show reg ++
                        ", r" ++
                        show reg' ++
                        "\n"
                    else error "both side should be integers"
            )
    AndExpression t f ->
        undefined
    otherwise ->
        compileFactor reg symbols var t

compileFactor ::
    Int -> Symbols -> Bool -> ASTFactor -> (ASTTypeIdentifier, String)
compileFactor reg symbols var f = case f of
    UnsignedConstantExpression uc -> if var then
        error "rvalue cannot be referenced"
        else case uc of
            UnsignedInteger i ->
                (
                    IntegerTypeIdentifier,
                    "    int_const r" ++
                        show reg ++
                        ", " ++
                        show i ++
                        "\n"
                    )
            UnsignedReal r ->
                (
                    RealTypeIdentifier,
                    "    real_const r" ++
                        show reg ++
                        ", " ++
                        show r ++
                        "\n"
                    )
            BooleanConstant b ->
                "    int_const r" ++
                    show reg ++
                    ", " ++
                    if b then "1" else "0" ++
                    "\n"
    VariableAccessExpression va ->
        let
            (_, table) = symbols
        in
            case va of
                IndexedVariableAccess iva ->
                    undefined
                OrdinaryVariableAccess vid ->
                    let
                        (_, td, slot) = case Map.lookup vid table of
                            Nothing ->
                                error (
                                    "variable " ++ vid ++ " undeclared"
                                    )
                            Just i ->
                                i
                        tid = case td of
                            ArrayTypeDenoter at ->
                                undefined
                            OrdinaryTypeDenoter tid ->
                                tid
                    in
                        if var then
                            (
                                tid,
                                "    load_address r" ++
                                    show reg ++
                                    ", " ++
                                    show slot ++
                                    "\n"
                                )
                            else (
                                tid,
                                "    load r" ++
                                    show reg ++
                                    ", " ++
                                    show slot ++
                                    "\n"
                                )
    NotExpression f ->
        undefined
    otherwise ->
        compileExpression reg symbols var f
