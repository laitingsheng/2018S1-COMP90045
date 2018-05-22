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
import Text.Printf

-- for ease of printing
printIndent :: String
printIndent = "    "

printAction :: String -> String
printAction a = printf "%s%s\n" printIndent a

printActionInt :: String -> Int -> String
printActionInt a d = printf "%s%s %d\n" printIndent a d

printActionString :: String -> String -> String
printActionString a s = printf "%s%s %s\n" printIndent a s

printActionReg2 :: String -> Int -> Int -> String
printActionReg2 a r0 r1 = printf "%s%s r%d, r%d\n" printIndent a r0 r1

printActionReg3 :: String -> Int -> Int -> Int -> String
printActionReg3 a r0 r1 r2 =
    printf "%s%s r%d, r%d, r%d\n" printIndent a r0 r1 r2

printActionRegString :: String -> Int -> String -> String
printActionRegString a reg s = printf "%s%s r%d, %s\n" printIndent a reg s

printIntLabel :: Int -> String
printIntLabel label = printf "label%d:\n" label

printProcedureName :: String -> String
printProcedureName name = printf "%s:\n" name

printComment :: String -> String
printComment comment = printf "# %s\n" comment

-- all instructions
printPushStackFrame :: Int -> String
printPushStackFrame = printActionInt "push_stack_frame"
printPopStackFrame :: Int -> String
printPopStackFrame = printActionInt "pop_stack_frame"

printLoad :: Int -> Int -> String
printLoad reg slot = printActionRegString "load" reg (show slot)
printStore :: Int -> Int -> String
printStore slot reg = printActionString "store" (printf "%d, r%d" slot reg)
printLoadAddress :: Int -> Int -> String
printLoadAddress reg slot = printActionRegString "load_address" reg (show slot)
printLoadIndirect :: Int -> Int -> String
printLoadIndirect = printActionReg2 "load_indirect"
printStoreIndirect :: Int -> Int -> String
printStoreIndirect = printActionReg2 "store_indirect"

printIntConst :: Int -> Int -> String
printIntConst reg c = printActionRegString "int_const" reg (show c)
printRealConst :: Int -> Double -> String
printRealConst reg c = printActionRegString "real_const" reg (show c)
printStringConst :: Int -> String -> String
printStringConst reg c =
    printActionRegString "string_const" reg (printCharacterString c)

printBinary :: String -> Maybe String -> Int -> Int -> Int -> String
printBinary a Nothing = printActionReg3 (printf "%s" a)
printBinary a (Just t) = printActionReg3 (printf "%s_%s" a t)
printUnary :: String -> Maybe String -> Int -> Int -> String
printUnary a Nothing = printActionReg2 (printf "%s" a)
printUnary a (Just t) = printActionReg2 (printf "%s_%s" a t)

printCmp :: String -> String -> Int -> Int -> Int -> String
printCmp c t = printActionReg3 (printf "cmp_%s_%s" c t)

printInt2Real :: Int -> Int -> String
printInt2Real = printActionReg2 "int_to_real"
printMove :: Int -> Int -> String
printMove = printActionReg2 "move"

printBranchTrue :: Int -> Int -> String
printBranchTrue reg label =
    printActionRegString "branch_on_true" reg (printf "label%d" label)
printBranchFalse :: Int -> Int -> String
printBranchFalse reg label =
    printActionRegString "branch_on_false" reg (printf "label%d" label)
printBranchUncond :: Int -> String
printBranchUncond label =
    printActionString "branch_uncond" (printf "label%d" label)

printCall :: String -> String
printCall = printActionString "call"
printCallBuiltin :: String -> String
printCallBuiltin = printActionString "call_builtin"
printReturn :: String
printReturn = printAction "return"
printHalt :: String
printHalt = printAction "halt"

printDebugReg :: Int -> String
printDebugReg reg = printActionString "debug_reg" (printf "r%d" reg)
printDebugSlot :: Int -> String
printDebugSlot = printActionInt "debug_slot"
printDebugStack :: String
printDebugStack = printAction "debug_stack"


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
            "\n" ++
            printCall "main" ++
            printHalt ++
            procText ++
            printProcedureName "main" ++
            printComment "prologue" ++
            printPushStackFrame slot' ++
            bodyText ++
            printComment "epilogue" ++
            printPopStackFrame slot' ++
            printReturn ++
            -- handle runtime error
            printComment "error handling" ++
            printProcedureName "__Exception" ++
            printStringConst 0 "an exception was thrown" ++
            printCallBuiltin "print_string" ++
            printHalt

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
        -- compile each line of declration, local variable don't have varness
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
        -- array occupies a continuous block of positions in the current stack
        -- frame with a size of (hi - lo + 1)
        (ptable, table) = symbols
        slot' = case td of
            ArrayTypeDenoter ((lo, hi), _) ->
                slot + hi - lo + 1
            OrdinaryTypeDenoter _ ->
                slot + 1
    in
        -- compile each declaration in the list
        compileIdentifierList slot'
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
        -- the program variables are isolated from the procedure variables
        (ptable, _) = symbols
        symbols' = (ptable, Map.empty)

        (slot, symbols'', text1) = compileFormalParameterList 0 symbols' fpl
        (slot', symbols''') = compileVariableDeclarationPart slot symbols'' vdp
        (label', text2) = compileCompoundStatement label symbols''' cs
    in
        (
            label',
            printComment ("procedure " ++ pid) ++
                printProcedureName pid ++
                -- push a new stack frame for the current procedure
                printPushStackFrame slot' ++
                -- first parse the parameter list
                printComment "store parameters" ++
                text1 ++
                -- parse all statements
                text2 ++
                -- pop the current stack frame
                printPopStackFrame slot' ++
                printReturn
            )

compileFormalParameterList ::
    Int -> Symbols -> ASTFormalParameterList -> (Int, Symbols, String)
compileFormalParameterList slot symbols [] =
    (slot, symbols, "")
compileFormalParameterList slot symbols (x:xs) =
    let
        (slot', symbols') =
            compileIdentifierList slot symbols x
        (slot'', symbols'', r) = compileFormalParameterList slot' symbols' xs
    in
        (
            -- for n parameters, the value is stored in registers numbering 0 to
            -- n - 1 and the parameters are treated as local variable (the
            -- referenced variable is also a local variable holding the address)
            -- so we need an extra n slots
            slot'',
            -- store the values from registers to the stack
            symbols'',
            -- proceed to next parameter
            printStore slot slot ++ r
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
        -- compile each statement
        (label', text) = compileStatement label symbols x
        (label'', text') = compileCompoundStatement label' symbols xs
    in
        -- proceed to next statement
        (label'', text ++ text')

compileStatement ::
    Int -> Symbols -> ASTStatement -> (Int, String)
compileStatement label symbols s =
    case s of
        AssignmentStatement (va, e) ->
            let
                (tid, text) = compileExpression 0 symbols False e
                (_, table) = symbols
                text' = case va of
                    IndexedVariableAccess (aid, e) ->
                        let
                            (tid', text) =
                                compileExpression 1 symbols False e
                            (var, td, slot) =
                                if tid' == IntegerTypeIdentifier then
                                    case Map.lookup aid table of
                                        Nothing ->
                                            error (
                                                "variable " ++
                                                    aid ++
                                                    " undeclared"
                                                )
                                        Just i ->
                                            i
                                    else
                                        error "index must be integer"
                            text' = if var then
                                undefined
                                else "# check boundaries\n" ++
                                    "    cmp_lt_int r4, r1, r2\n" ++
                                    -- runtime lower bound check
                                    "    branch_on_true r4, __Exception\n" ++
                                    "    cmp_gt_int r4, r1, r3\n" ++
                                    -- runtime upper bound check
                                    "    branch_on_true r4, __Exception\n" ++
                                    "# get offset\n" ++
                                    "    sub_int r1, r1, r2\n" ++
                                    -- load the address and add offset
                                    "    load_address r2, " ++ show slot ++
                                    "\n    add_offset r2, r2, r1\n" ++
                                    "    store_indirect r2, r0\n"
                        in
                            case td of
                                ArrayTypeDenoter (st, tid') ->
                                    let
                                        text'' = text ++
                                            compileSubrangeType 2 st ++
                                            text'
                                    in
                                        if tid == tid' then text''
                                            else if tid == RealTypeIdentifier &&
                                                tid' == IntegerTypeIdentifier
                                                then "    int_to_real r1, r1" ++
                                                    text''
                                                else error "unmatched type"
                                OrdinaryTypeDenoter _ ->
                                    error (aid ++ " cannot be indexed")
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
                                    if tid == tid' then text
                                        else if tid == RealTypeIdentifier &&
                                            tid' == IntegerTypeIdentifier then
                                                "    int_to_real r1, r1" ++
                                                    text
                                                else error "unmatched type"
            in
                (
                    label,
                    "# assignment\n" ++ text ++ text'
                    )
        ReadStatement rs ->
            let
                (_, table) = symbols
                text = case rs of
                    IndexedVariableAccess (aid, e) ->
                        let
                            (tid, text) =
                                compileExpression 1 symbols False e
                            (var, td, slot) =
                                if tid == IntegerTypeIdentifier then
                                    case Map.lookup aid table of
                                        Nothing ->
                                            error (
                                                "variable " ++
                                                    aid ++
                                                    " undeclared"
                                                )
                                        Just i ->
                                            i
                                    else
                                        error "index must be integer"
                            t = case td of
                                ArrayTypeDenoter (st, tid) ->
                                    case tid of
                                        IntegerTypeIdentifier ->
                                            "read_int"
                                        RealTypeIdentifier ->
                                            "read_real"
                                        BooleanTypeIdentifier ->
                                            "read_bool"
                                    ++ "\n" ++ compileSubrangeType 1 st
                                OrdinaryTypeDenoter _ ->
                                    error (aid ++ " cannot be indexed")
                            text' = if var then
                                undefined
                                else undefined
                        in
                            t ++ text'
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
            in
                (label, "# read\n    call_builtin " ++ text)
        WriteStatement ws ->
            (
                label,
                let
                    (tid, text) = compileExpression 0 symbols False ws
                in
                    printComment "write" ++
                        text ++
                        printCallBuiltin (case tid of
                            IntegerTypeIdentifier ->
                                "print_int"
                            RealTypeIdentifier ->
                                "print_real"
                            BooleanTypeIdentifier ->
                                "print_bool"
                            )
            )
        WriteStringStatement wss ->
            (
                label,
                printComment "write" ++
                    printStringConst 0 wss ++
                    printCallBuiltin "print_string"
                )
        WritelnStatement ->
            (label, printComment "writeln" ++ printCallBuiltin "print_newline")
        ProcedureStatement (pid, apl) ->
            (
                label,
                let
                    (ptable, _) = symbols
                    fpl = case Map.lookup pid ptable of
                        Nothing ->
                            error (printf "procedure %s undeclared" pid)
                        Just fpl ->
                            fpl
                    (_, text1) = compileActualParameterList 0 symbols fpl apl
                in
                    printComment "get actual parameters" ++
                        text1 ++
                        printComment "call procedure" ++
                        printCall pid
                )
        CompoundStatement cs ->
            compileCompoundStatement label symbols cs
        IfStatement (e, s, ms) ->
            let
                (td, text1) = compileExpression 0 symbols False e
                (label', text2) = if td == RealTypeIdentifier then
                    error "if guard should be boolean expression"
                    -- int value can be considered as boolean value
                    else compileStatement label symbols s
                text = printComment "if" ++
                    text1 ++
                    printBranchFalse 0 label ++
                    text2
            in
                case ms of
                    Nothing ->
                        (
                            label' + 1,
                            text ++ printIntLabel label
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
                                    printBranchUncond label'' ++
                                    printIntLabel label ++
                                    text3 ++
                                    printIntLabel label''
                                )
        WhileStatement (e, s) ->
            let
                label' = label + 1
                (tid, text1) = compileExpression 0 symbols False e
                (label'', text2) = if tid == RealTypeIdentifier then
                    error "while guard should be boolean expression"
                    -- int value can be considered as boolean value
                    else compileStatement label' symbols s
            in
                (
                    label'' + 1,
                    printComment "while" ++
                        text1 ++
                        printBranchFalse 0 label' ++
                        printIntLabel label ++
                        text2 ++
                        text1 ++
                        printBranchTrue 0 label ++
                        printIntLabel label'
                    )
        ForStatement (fid, e1, d, e2, s) ->
            let
                (tid1, text1) = compileExpression 0 symbols False e1
                (tid2, text2) = if tid1 == IntegerTypeIdentifier then
                    compileExpression 1 symbols False e2
                    else error "the lower bound of for loop must be integer"
                label' = label + 1
                (label'', text3) = compileStatement label' symbols s
            in
                (
                    label'' + 1,
                    "# for\n" ++
                        text1 ++
                        text2 ++
                        "    move r2, r0" ++
                        "    cmp_gt_int r3, r2, r1\n" ++
                        "    branch_on_false r3, l" ++ show label' ++ "\n" ++
                        "l" ++ show label ++ ":\n" ++
                        text3 ++
                        "    int_const r3, 1\n" ++
                        "    add_int r2, r2, r3\n" ++
                        "    cmp_gt_int r3, r2, r1\n" ++
                        "    branch_on_true r3, l" ++ show label ++ "\n" ++
                        "l" ++ show label' ++ ":\n"
                    )
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

compileArrayType ::
    Int -> Int -> Symbols -> ASTExpression -> ASTArrayType
        -> (ASTTypeIdentifier, String)
compileArrayType slot reg symbols e (st, tid) =
    let
        r0 = reg
        r1 = reg + 1
        r2 = reg + 2
        r3 = reg + 3
        r4 = reg + 4
        -- expression as the index
        (tid', text) = compileExpression r0 symbols False e
    in
        if tid' == IntegerTypeIdentifier then
            (
                tid,
                "# array\n" ++
                    text ++
                    compileSubrangeType r1 st ++
                    "# check boundaries\n" ++
                    -- runtime lower bound check
                    printf "    cmp_lt_int r%d, r%d, r%d\n" r4 r1 r2 ++
                    printf "    branch_on_true r%d, __Exception\n" r4 ++
                    -- runtime upper bound check
                    printf "    cmp_gt_int r%d, r%d, r%d\n" r4 r1 r3 ++
                    printf "    branch_on_true r%d, __Exception\n" r4 ++
                    "# get offset\n" ++
                    printf "    sub_int r%d, r%d, r%d\n" r1 r1 r2 ++
                    -- load the address and add offset
                    "    load_address r2, " ++ show slot ++
                    "\n    add_offset r2, r2, r1\n" ++
                    "    store_indirect r2, r0\n"
                )
            else error "array index must be integer"

compileSubrangeType :: Int -> ASTSubrangeType -> String
compileSubrangeType reg (lo, hi) =
    "    int_const r" ++
        show reg ++
        ", " ++
        show lo ++
        "\n    int_const r" ++
        show (reg + 1) ++
        ", " ++
        show hi ++
        "\n"

compileExpression ::
    Int -> Symbols -> Bool -> ASTExpression -> (ASTTypeIdentifier, String)
compileExpression reg symbols ref e =
    let
        msesc = case e of
            EqualExpression se1 se2 ->
                Just (se1, se2, "eq")
            NotEqualExpression se1 se2 ->
                Just (se1, se2, "ne")
            LessThanExpression se1 se2 ->
                Just (se1, se2, "lt")
            GreaterThanExpression se1 se2 ->
                Just (se1, se2, "gt")
            LessThanOrEqualExpression se1 se2 ->
                Just (se1, se2, "le")
            GreaterThanOrEqualExpression se1 se2 ->
                Just (se1, se2, "ge")
            otherwise ->
                Nothing
    in
        case msesc of
            Nothing ->
                -- transfer to higher precedence
                compileSimpleExpression reg symbols ref e
            Just (se1, se2, c) ->
                let
                    reg' = reg + 1
                    (td1, text1) = compileSimpleExpression reg symbols ref se1
                    (td2, text2) = compileSimpleExpression reg' symbols ref se2
                    -- allow expression such as (1 < 2) > (3 < 2) or
                    -- True = 1
                    r1 = case td1 of
                        RealTypeIdentifier ->
                            True
                        otherwise ->
                            False
                    r2 = case td2 of
                        RealTypeIdentifier ->
                            True
                        otherwise ->
                            False
                    (r, t) = if r1 || r2 then (True, "real") else (False, "int")

                    -- convert if necessary
                    text1' = text1 ++
                        if r && not r1 then printInt2Real reg reg else ""
                    text2' = text2 ++
                        if r && not r2 then printInt2Real reg' reg' else ""
                in
                    (
                        BooleanTypeIdentifier,
                        text1' ++ text2' ++
                            printCmp c t reg reg reg'
                        )

compileSimpleExpression ::
    Int -> Symbols -> Bool -> ASTSimpleExpression -> (ASTTypeIdentifier, String)
compileSimpleExpression reg symbols ref se =
    let
        msetop = case se of
            PlusExpression se t ->
                Just (Just se ,t, "add")
            MinusExpression se t ->
                Just (Just se, t, "sub")
            OrExpression se t ->
                Just (Just se, t, "or")
            PosExpression t ->
                Just (Nothing, t, "pos")
            NegExpression t ->
                Just (Nothing, t, "neg")
            otherwise ->
                Nothing
    in
        case msetop of
            Nothing ->
                compileTerm reg symbols ref se
            Just (mse, t, op) ->
                case mse of
                    Nothing ->
                        let
                            (tid, text) = compileTerm reg symbols ref t
                            t' = case tid of
                                RealTypeIdentifier ->
                                    Just "real"
                                otherwise ->
                                    Just "int"
                        in
                            (
                                tid,
                                text ++ case op of
                                    "pos" ->
                                        ""
                                    "neg" ->
                                        printUnary op t' reg reg
                                )
                    Just se ->
                        let
                            reg' = reg + 1
                            (tid1, text1) =
                                compileSimpleExpression reg symbols ref se
                            (tid2, text2) = compileTerm reg' symbols ref t
                            r1 = case tid1 of
                                RealTypeIdentifier ->
                                    True
                                otherwise ->
                                    False
                            r2 = case tid2 of
                                RealTypeIdentifier ->
                                    True
                                otherwise ->
                                    False
                            (r, t') = if r1 || r2 then (True, "real")
                                else (False, "int")
                            -- convert if necessary
                            text1' = text1 ++ if r && not r1 then
                                printInt2Real reg reg
                                else ""
                            text2' = text2 ++ if r && not r2 then
                                printInt2Real reg' reg'
                                else ""
                        in
                            (
                                if r then RealTypeIdentifier
                                    else IntegerTypeIdentifier,
                                text1' ++ text2' ++ case op of
                                    "or" ->
                                        printBinary op Nothing reg reg reg'
                                    otherwise ->
                                        printBinary op (Just t') reg reg reg'
                                )


compileTerm ::
    Int -> Symbols -> Bool -> ASTTerm -> (ASTTypeIdentifier, String)
compileTerm reg symbols ref t = case t of
    TimesExpression t f ->
        let
            reg' = reg + 1
            (tid1, text1) = compileTerm reg symbols ref t
            (tid2, text2) = if tid1 == BooleanTypeIdentifier then
                error "cannot be boolean value"
                else compileFactor reg' symbols ref f
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
                (tid1, text1) = compileTerm reg symbols ref t
                (tid2, text2) = if tid1 == IntegerTypeIdentifier then
                    compileFactor reg' symbols ref f
                    else error "both side should be integer"
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
                    else error "both side should be integer"
            )
    AndExpression t f ->
        let
            (tid1, text1) = compileTerm reg symbols ref t
            (tid2, text2) = if tid1 == BooleanTypeIdentifier then
                compileFactor (reg + 1) symbols ref f
                else error "both side should be boolean"
        in
            if tid2 == BooleanTypeIdentifier then
                (
                    BooleanTypeIdentifier,
                    text1 ++
                        text2 ++
                        "    and r" ++
                        show reg ++
                        ", r" ++
                        show reg ++
                        ", r" ++
                        show (reg + 1) ++
                        "\n"
                    )
                else error "both side should be boolean"
    otherwise ->
        compileFactor reg symbols ref t

compileFactor ::
    Int -> Symbols -> Bool -> ASTFactor -> (ASTTypeIdentifier, String)
compileFactor reg symbols ref f = case f of
    UnsignedConstantExpression uc ->
        if ref then
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
                    (
                        BooleanTypeIdentifier,
                        "    int_const r" ++
                            show reg ++
                            ", " ++
                            if b then "1" else "0" ++
                            "\n"
                        )
    VariableAccessExpression va ->
        let
            (_, table) = symbols
        in
            case va of
                IndexedVariableAccess (aid, e) ->
                    let
                        (tid, text) =
                            compileExpression 1 symbols False e
                        (var, td, slot) =
                            if tid == IntegerTypeIdentifier then
                                case Map.lookup aid table of
                                    Nothing ->
                                        error (
                                            "variable " ++
                                                aid ++
                                                " undeclared"
                                            )
                                    Just i ->
                                        i
                                else
                                    error "index must be integer"
                        text' = if var then
                            undefined
                            else "# check boundaries\n" ++
                                "    cmp_lt_int r4, r1, r2\n" ++
                                "    branch_on_true r4, __Exception\n" ++
                                "    cmp_gt_int r4, r1, r3\n" ++
                                "    branch_on_true r4, __Exception\n" ++
                                "# get offset\n" ++
                                "    sub_int r1, r1, r2\n" ++
                                "    int_const r2, " ++ show slot ++ "\n" ++
                                "    add_int r1, r1, r2\n"
                    in
                        case td of
                            ArrayTypeDenoter (st, tid) ->
                                (
                                    tid,
                                    text ++
                                        compileSubrangeType 1 st ++
                                        text' ++
                                        if ref then
                                            "    load_address r0, r1\n"
                                            else "    load r0, r1\n"
                                    )
                            OrdinaryTypeDenoter _ ->
                                error (aid ++ " cannot be indexed")
                OrdinaryVariableAccess vid ->
                    let
                        -- varness of the variable is unrelated to variable
                        -- access
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
                        -- decide if it needs the address
                        if ref then
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
        compileExpression reg symbols ref f
