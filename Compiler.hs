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
        Map String (ASTTypeDenoter, Int)
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
            "\n    pop_stack_frame " ++
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
precompileProcedureDeclaration (procSymbols, varSymbols) (x0, x1, x2, x3) =
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
compileVariableDeclarationPart slot symbols (x : xs) =
    let
        (slot', symbols') = compileVariableDeclaration slot symbols x
    in
        compileVariableDeclarationPart slot' symbols' xs

compileVariableDeclaration ::
    Int -> Symbols -> ASTVariableDeclaration -> (Int, Symbols)
compileVariableDeclaration slot symbols vd =
    let
        (ptable, table) = symbols
        (idl, td) = vd

        compile slot table [] =
            (slot, table)
        compile slot table (x:xs) =
            compile (slot + 1) (Map.insert x (td, slot) table) xs

        (slot', table') = compile slot table idl
    in
        (slot', (ptable, table'))

-- compile a list of procedures
-- takes a label number, a symbol table and an AST fragment
-- returns the advanced label number and the generated code
compileProcedureDeclarationPart ::
    Int -> Symbols -> ASTProcedureDeclarationPart -> (Int, String)
compileProcedureDeclarationPart label symbols _ =
    (label, "")
-- compileProcedureDeclarationPart label symbols (x : xs) =
--     error "compiling procedure declarations is not yet implemented"

-- compile a list of statements
-- takes a label number, a symbol table and an AST fragment
-- returns the advanced label number and the generated code
compileCompoundStatement ::
    Int -> Symbols -> ASTCompoundStatement -> (Int, String)
compileCompoundStatement label symbols cs =
    compileCompoundStatement' label "" symbols cs

compileCompoundStatement' ::
    Int -> String -> Symbols -> ASTCompoundStatement -> (Int, String)
compileCompoundStatement' label text symbols [] =
    (label, text)
compileCompoundStatement' label text symbols (x : xs) =
    let
        (label', text') = compileStatement label symbols x
        text'' = text ++ text'
    in
        compileCompoundStatement' label' text'' symbols xs

compileStatement ::
    Int -> Symbols -> ASTStatement -> (Int, String)
compileStatement label symbols s =
    case s of
        AssignmentStatement (va, e) ->
            let
                (tid, text) = compileExpression 0 symbols e
                (_, table) = symbols
            in
                (
                    label,
                    "# assignment\n" ++ text ++ case va of
                        IndexedVariableAccess iva ->
                            undefined
                        OrdinaryVariableAccess vid ->
                            let
                                (td, slot) = case Map.lookup vid table of
                                    Nothing ->
                                        error (
                                            "variable " ++ vid ++ " undeclared"
                                        )
                                    Just i ->
                                        i
                            in
                                case td of
                                    ArrayTypeDenoter _ ->
                                        error "cannot put value into array"
                                    OrdinaryTypeDenoter tid' ->
                                        if tid' == tid then
                                            "    store " ++
                                                show slot ++
                                                ", r0\n"
                                            else error "unmatched type"
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
                                (td, slot) = case Map.lookup vid table of
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
                                t ++ "\n    store " ++
                                show slot ++
                                ", r0\n"
            )
        WriteStatement ws ->
            (
                label,
                let
                    (tid, text) = compileExpression 0 symbols ws
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
        ProcedureStatement ps ->
            undefined
        CompoundStatement cs ->
            compileCompoundStatement label symbols cs
        IfStatement (e, s, ms) -> case ms of
            Nothing ->
                let
                    (_, text1) = compileExpression 0 symbols e
                    (label', text2) = compileStatement label symbols s
                in
                    (
                        label' + 1,
                        "# if\n" ++
                            text1 ++
                            "    branch_on_false r0, label" ++
                            show label ++
                            "\n" ++
                            text2 ++
                            "label" ++
                            show label ++
                            ":\n"
                    )
            -- have a else branch
            Just s ->
                (
                    label + 2,
                    undefined
                )
        WhileStatement whs ->
            undefined
        ForStatement fs ->
            undefined
        EmptyStatement ->
            (label, "")

compileExpression ::
    Int -> Symbols -> ASTExpression -> (ASTTypeIdentifier, String)
compileExpression reg symbols e = case e of
    EqualExpression se1 se2 ->
        undefined
    NotEqualExpression se1 se2 ->
        undefined
    LessThanExpression se1 se2 ->
        let
            (td1, text1) = compileSimpleExpression reg symbols se1
            reg' = reg + 1
            (td2, text2) = compileSimpleExpression reg' symbols se2
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
        undefined
    LessThanOrEqualExpression se1 se2 ->
        undefined
    GreaterThanOrEqualExpression se1 se2 ->
        undefined
    otherwise ->
        compileSimpleExpression reg symbols e

compileSimpleExpression ::
    Int -> Symbols -> ASTSimpleExpression -> (ASTTypeIdentifier, String)
compileSimpleExpression reg symbols se = case se of
    PlusExpression se t ->
        undefined
    MinusExpression se t ->
        let
            reg' = reg + 1
            (tid1, text1) = compileSimpleExpression reg symbols se
            (tid2, text2) = if tid1 == BooleanTypeIdentifier then
                error "cannot be boolean value"
                else compileTerm reg' symbols t
        in
            undefined
    OrExpression se t ->
        undefined
    PosExpression t ->
        undefined
    NegExpression t ->
        undefined
    otherwise ->
        compileTerm reg symbols se

compileTerm ::
    Int -> Symbols -> ASTTerm -> (ASTTypeIdentifier, String)
compileTerm reg symbols t = case t of
    TimesExpression t f ->
        undefined
    DivideByExpression t f ->
        undefined
    DivExpression t f ->
        (
            IntegerTypeIdentifier,
            let
                reg' = reg + 1
                (tid1, text1) = compileTerm reg symbols t
                (tid2, text2) = if tid1 == IntegerTypeIdentifier then
                    compileFactor reg' symbols f
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
        compileFactor reg symbols t

compileFactor ::
    Int -> Symbols -> ASTFactor -> (ASTTypeIdentifier, String)
compileFactor reg symbols f = case f of
    UnsignedConstantExpression uc ->
        undefined
    VariableAccessExpression va ->
        let
            (_, table) = symbols
        in
            case va of
                IndexedVariableAccess iva ->
                    undefined
                OrdinaryVariableAccess vid ->
                    let
                        (td, slot) = case Map.lookup vid table of
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
                        (
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
        compileExpression reg symbols f
