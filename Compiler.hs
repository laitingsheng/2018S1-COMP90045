-- Compiler for Paz, a subset of programming language Pascal
-- Tingsheng Lai 781319
--     zero division detection added
--     index out of bound detection added

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

-- for ease of printing and formatting
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

-- for exception handling
printToException :: String -> Int -> String -> String
printToException cond reg label =
    printActionRegString (printf "branch_%s" cond) reg label


-- this is the entry point to the compiler from the Paz.hs driver module
compileStartSymbol :: ASTStartSymbol -> String
compileStartSymbol =
    compileProgram

-- the first element is the variables in current scope, and the second is a list
-- of lists of formal parameters of different procedures
type Symbols =
    (
        -- for each procedure, for each formal parameter, its varness and type
        Map String [(Bool, ASTTypeDenoter)],

        -- for each variable, its varness, type, and starting slot number
        Map String (Bool, ASTTypeDenoter, Int)
        )

-- the entry point
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
        printComment (printf "%s %s" printTokenProgram name) ++
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
            printComment "index" ++
            printProcedureName "__IndexException" ++
            printStringConst 0 "index out of bound" ++
            printCallBuiltin "print_string" ++
            printHalt ++
            -- zero division
            printComment "zero division" ++
            printProcedureName "__ZeroDivException" ++
            printStringConst 0 "0 cannot be the denominator" ++
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
        (slot, symbols', text1) = compileFormalParameterList 0 symbols fpl
        (slot', symbols'') = compileVariableDeclarationPart slot symbols' vdp
        (label', text2) = compileCompoundStatement label symbols'' cs
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
        (slot'', symbols'', re) = compileFormalParameterList slot' symbols' xs
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
            printStore slot slot ++ re
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
                (ref, tid1, (n, t)) = compileVariableAccess 0 symbols va
                -- determine if r0 is occupied by an address
                reg = if ref then 1 else 0
                (tid2, text2) = compileExpression reg symbols False e

                -- may not be evaluated, apply conversion if appropriate
                conversion = if tid1 == tid2 then ""
                    else if tid1 == RealTypeIdentifier &&
                        tid2 == IntegerTypeIdentifier then printInt2Real reg reg
                        else error "type mismatch"
            in
                (
                    label,
                    printComment "assign" ++
                        t ++
                        text2 ++
                        conversion ++
                        (if ref then printStoreIndirect
                            else printStore) n reg
                    )
        ReadStatement va ->
            let
                (ref, tid, (n, t)) = compileVariableAccess 1 symbols va
            in
                (
                    label,
                    printComment "read" ++
                        printCallBuiltin (
                            case tid of
                                IntegerTypeIdentifier ->
                                    "read_int"
                                RealTypeIdentifier ->
                                    "read_real"
                                BooleanTypeIdentifier ->
                                    "read_bool"
                            ) ++
                        t ++
                        (if ref then printStoreIndirect
                            else printStore) n 0
                    )
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
                (label', text2) = if td /= BooleanTypeIdentifier then
                    error "if guard should be boolean expression"
                    -- int value can be considered as boolean value
                    else compileStatement label symbols s
                text = printComment "if branch" ++ text1
            in
                -- determine if there is an else statement
                case ms of
                    Nothing ->
                        (
                            label' + 1,
                            text ++
                                printBranchFalse 0 label' ++
                                text2 ++
                                printIntLabel label'
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
                                    printBranchFalse 0 label' ++
                                    text2 ++
                                    printBranchUncond label'' ++
                                    printIntLabel label' ++
                                    text3 ++
                                    printIntLabel label''
                                )
        WhileStatement (e, s) ->
            let
                (tid, text1) = compileExpression 0 symbols False e
                (label', text2) = if tid /= BooleanTypeIdentifier then
                    error "while guard should be boolean expression"
                    -- int value can be considered as boolean value
                    else compileStatement (label + 1) symbols s
            in
                (
                    label' + 1,
                    printComment "while loop" ++
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
                (tid2, text2) = compileExpression 1 symbols False e2
                (label'', text3) = compileStatement (label + 1) symbols s
                (ref, tid, (n, t)) =
                    compileVariableAccess 2 symbols (OrdinaryVariableAccess fid)
                (g, ac) = case d of
                    ForDirectionUp ->
                        (printCmp "gt" "int" 0 0 1, "add")
                    ForDirectionDown ->
                        (printCmp "lt" "int" 0 0 1, "sub")
            in
                if tid == IntegerTypeIdentifier &&
                    tid1 == IntegerTypeIdentifier &&
                    tid2 == IntegerTypeIdentifier then
                        (
                            label'' + 1,
                            printComment "for loop" ++
                                text1 ++
                                t ++
                                (if ref then printStoreIndirect else printStore)
                                    n 0 ++
                                text2 ++
                                g ++
                                printBranchTrue 0 label'' ++
                                printIntLabel label ++
                                text3 ++
                                t ++
                                (if ref then printLoadIndirect else printLoad)
                                    0 n ++
                                printIntConst 1 1 ++
                                printBinary ac (Just "int") 0 0 1 ++
                                t ++
                                (if ref then printStoreIndirect else printStore)
                                    n 0 ++
                                text2 ++
                                g ++
                                printBranchFalse 0 label ++
                                printIntLabel label''
                            )
                    else error "boundaries and variable must be integer type"
        EmptyStatement ->
            (label, "")

compileVariableAccess ::
    Int -> Symbols -> ASTVariableAccess
        -> (Bool, ASTTypeIdentifier, (Int, String))
compileVariableAccess reg symbols va =
    let
        (_, table) = symbols
    in
        case va of
            IndexedVariableAccess (aid, e) ->
                let
                    (var, td, slot) = case Map.lookup aid table of
                        Nothing ->
                            error (printf "variable %s undeclared" aid)
                        Just i ->
                            i
                    (tid', t) = compileExpression reg symbols False e
                    (tid, text) = case td of
                        OrdinaryTypeDenoter _ ->
                            error (printf "%s is not an array" aid)
                        ArrayTypeDenoter at ->
                            compileArrayType slot var reg symbols e at
                in
                    (True, tid, (reg, text))
            OrdinaryVariableAccess vid ->
                let
                    (var, td, slot) = case Map.lookup vid table of
                        Nothing ->
                            error (printf "variable %s undeclared" vid)
                        Just i ->
                            i
                in
                    case td of
                        ArrayTypeDenoter _ ->
                            error "cannot assign to the whole array"
                        OrdinaryTypeDenoter tid ->
                            (
                                var,
                                tid,
                                if var then (reg, printLoad reg slot)
                                    else (slot, "")
                                )

compileArrayType ::
    Int -> Bool -> Int -> Symbols -> ASTExpression -> ASTArrayType
        -> (ASTTypeIdentifier, String)
compileArrayType slot ref reg symbols e ((lo, hi), tid) =
    let
        r0 = reg
        r1 = reg + 1
        r2 = reg + 2
        r3 = reg + 3
        -- expression as the index
        (tid', text) = compileExpression r0 symbols False e
    in
        if tid' == IntegerTypeIdentifier then
            (
                tid,
                printComment "array" ++
                    text ++
                    -- put boundaries into registers
                    printIntConst r1 lo ++
                    printIntConst r2 hi ++
                    printComment "check boundaries" ++
                    -- runtime lower bound check
                    printCmp "lt" "int" r3 r0 r1 ++
                    printToException "on_true" r3 "__IndexException" ++
                    -- runtime upper bound check
                    printCmp "gt" "int" r3 r0 r2 ++
                    printToException "on_true" r3 "__IndexException" ++
                    printComment "get offset" ++
                    printBinary "sub" (Just "int") r0 r0 r1 ++
                    (if ref then printLoad else printLoadAddress) r1 slot ++
                    printBinary "sub" (Just "offset") r0 r1 r0
                )
            else error "array index must be integer"

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
        (slot', text1) = compileActualParameterList (slot + 1) symbols xs ys
    in
        (
            slot',
            text ++
                case td of
                    ArrayTypeDenoter at ->
                        undefined
                    OrdinaryTypeDenoter ftid ->
                        if ftid == atid then ""
                            else if ftid == RealTypeIdentifier &&
                                atid == IntegerTypeIdentifier then
                                    printInt2Real slot slot
                                    else error "parameter type mismatch"
                ++ text1
            )

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
                    (tid1, text1) = compileSimpleExpression reg symbols ref se1
                    (tid2, text2) = compileSimpleExpression reg' symbols ref se2
                    -- determine the final type
                    r1 = case tid1 of
                        RealTypeIdentifier ->
                            True
                        IntegerTypeIdentifier ->
                            False
                        BooleanTypeIdentifier ->
                            error "boolean type cannot be compared"
                    r2 = case tid2 of
                        RealTypeIdentifier ->
                            True
                        IntegerTypeIdentifier ->
                            False
                        BooleanTypeIdentifier ->
                            error "boolean type cannot be compared"
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
                                IntegerTypeIdentifier ->
                                    Just "int"
                                BooleanTypeIdentifier ->
                                    error (
                                        "must use not to negate a boolean value"
                                        )
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

                            -- may not be evaluated, for expression other than
                            -- boolean expression only
                            r1 = case tid1 of
                                RealTypeIdentifier ->
                                    True
                                IntegerTypeIdentifier ->
                                    False
                                BooleanTypeIdentifier ->
                                    error "boolean type cannot be compared"
                            r2 = case tid2 of
                                RealTypeIdentifier ->
                                    True
                                IntegerTypeIdentifier ->
                                    False
                                BooleanTypeIdentifier ->
                                    error "boolean type cannot be compared"
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
                            case op of
                                "or" ->
                                    (
                                        BooleanTypeIdentifier,
                                        if tid1 == BooleanTypeIdentifier &&
                                            tid2 == BooleanTypeIdentifier then
                                                text1 ++
                                                    text2 ++
                                                    printBinary op Nothing
                                                        reg reg reg'
                                            else error (
                                                "both side must be boolean"
                                                )
                                        )
                                otherwise ->
                                    (
                                        if r then RealTypeIdentifier
                                            else IntegerTypeIdentifier,
                                        text1' ++
                                            text2' ++
                                            printBinary op (Just t')
                                                reg reg reg'
                                        )


compileTerm ::
    Int -> Symbols -> Bool -> ASTTerm -> (ASTTypeIdentifier, String)
compileTerm reg symbols ref t =
    let
        mtfop = case t of
            TimesExpression t f ->
                Just (t, f, "mul")
            DivideByExpression t f ->
                Just (t, f, "div_real")
            DivExpression t f ->
                Just (t, f, "div_int")
            AndExpression t f ->
                Just (t, f, "and")
            otherwise ->
                Nothing
    in
        case mtfop of
            Nothing ->
                compileFactor reg symbols ref t
            Just (t, f, op) ->
                let
                    reg' = reg + 1
                    reg'' = reg + 2
                    (tid1, text1) = compileTerm reg symbols ref t
                    (tid2, text2) = compileFactor reg' symbols ref f

                    -- may not be evaluated
                    r1 = case tid1 of
                        RealTypeIdentifier ->
                            True
                        IntegerTypeIdentifier ->
                            False
                        BooleanTypeIdentifier ->
                            error "boolean type cannot be compared"
                    r2 = case tid2 of
                        RealTypeIdentifier ->
                            True
                        IntegerTypeIdentifier ->
                            False
                        BooleanTypeIdentifier ->
                            error "boolean type cannot be compared"
                    (r, t') = if r1 || r2 then (True, "real")
                        else (False, "int")
                    -- convert if necessary
                    text1' = text1 ++ if (op == "div_real" || r) && not r1 then
                        printInt2Real reg reg
                        else ""
                    text2' = text2 ++ if (op == "div_real" || r) && not r2 then
                        printInt2Real reg' reg'
                        else ""
                in
                    case op of
                        "and" ->
                            if tid1 == BooleanTypeIdentifier &&
                                tid2 == BooleanTypeIdentifier then
                                    (
                                        BooleanTypeIdentifier,
                                        text1 ++
                                            text2 ++
                                            printBinary op Nothing reg reg reg'
                                        )
                                else error "both side must be boolean"
                        "div_int" ->
                            if tid1 == IntegerTypeIdentifier ||
                                tid2 == IntegerTypeIdentifier then
                                    (
                                        IntegerTypeIdentifier,
                                        text1 ++
                                            text2 ++
                                            printIntConst reg'' 0 ++
                                            printCmp "eq" "int" reg'' reg'
                                                reg'' ++
                                            printToException "on_true" reg''
                                                "__ZeroDivException" ++
                                            printBinary op Nothing reg reg reg'
                                        )
                                    else  error "both sides must be integer"
                        "div_real" ->
                            (
                                RealTypeIdentifier,
                                text1' ++
                                    text2' ++
                                    printRealConst reg'' 0 ++
                                    printCmp "eq" "real" reg'' reg' reg'' ++
                                    printToException "on_true" reg''
                                        "__ZeroDivException" ++
                                    printBinary op Nothing reg reg reg'
                                )
                        "mul" ->
                            (
                                if r then RealTypeIdentifier
                                    else IntegerTypeIdentifier,
                                text1' ++ text2' ++
                                    printBinary op (Just t') reg reg reg'
                                )

compileFactor ::
    Int -> Symbols -> Bool -> ASTFactor -> (ASTTypeIdentifier, String)
compileFactor reg symbols ref f = case f of
    UnsignedConstantExpression uc ->
        if ref then
            -- these are rvalues
            error "rvalue cannot be referenced"
            else case uc of
                UnsignedInteger i ->
                    (IntegerTypeIdentifier, printIntConst reg i)
                UnsignedReal r ->
                    (RealTypeIdentifier, printRealConst reg r)
                BooleanConstant b ->
                    (
                        BooleanTypeIdentifier,
                        -- cast boolean value to integer
                        printIntConst reg (if b then 1 else 0)
                        )
    VariableAccessExpression va ->
        let
            (ref', tid, (n, t)) = compileVariableAccess reg symbols va
        in
            (
                tid,
                if ref then
                    if ref' then t
                        else printLoadAddress reg n
                    else if ref' then t ++ printLoadIndirect reg reg
                        else printLoad reg n
                )
    NotExpression f ->
        let
            (tid, text) = compileFactor reg symbols ref f
        in
            (
                BooleanTypeIdentifier,
                if tid == BooleanTypeIdentifier then text ++
                    printUnary "not" Nothing reg reg
                    else error "negating a non-boolean value"
                )
    otherwise ->
        compileExpression reg symbols ref f
