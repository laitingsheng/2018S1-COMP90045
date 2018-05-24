-- Compiler for Paz, a subset of programming language Pascal
-- Tingsheng Lai 781319
--     zero division detection added
--     index out of bound detection added
-- Note:
-- * Initially I thought that integer and boolean value can be mutually
--   converted in an implicit way, i.e. allow expressions such as
--   [False < True, True + 2], and that's how the Taz emulator works (in fact
--   this is how C handles bool), but since the spec explicitly states that we
--   need to type match, so I still add the type checking for integer and
--   boolean
-- * for ease of reading the Taz code, I use an XML-like syntax within the
--   comment in the output

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
        printComment (printf "<%s %s>" printTokenProgram name) ++
            printCall "main" ++
            printHalt ++
            procText ++
            printProcedureName "main" ++
            printComment "<prologue>" ++
            printPushStackFrame slot' ++
            bodyText ++
            printComment "</prologue>" ++
            printComment "<epilogue>" ++
            printPopStackFrame slot' ++
            printReturn ++
            printComment "</epilogue>" ++
            -- handle runtime error
            printComment "<Exception>" ++
            printComment "<index>" ++
            printProcedureName "__IndexException" ++
            printStringConst 0 "index out of bound" ++
            printCallBuiltin "print_string" ++
            printHalt ++
            printComment "</index>" ++
            -- zero division
            printComment "<zero division>" ++
            printProcedureName "__ZeroDivException" ++
            printStringConst 0 "0 cannot be the denominator" ++
            printCallBuiltin "print_string" ++
            printHalt ++
            printComment "</Exception>" ++
            printComment (printf "</%s %s>" printTokenProgram name)

-- the following pre-compilation functions are intended as an example of
-- how you can walk through the AST gathering information into a symbol
-- table (by adding additional state such as the label or slot number and
-- changing the return type, you can easily modify this to compile things)
precompileProcedureDeclarationPart ::
    Symbols -> ASTProcedureDeclarationPart -> Symbols
precompileProcedureDeclarationPart symbols [] =
    symbols
precompileProcedureDeclarationPart symbols (x : xs) =
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

-- compile a list of variable declarations
-- takes a slot number, a symbol table and an AST fragment
-- returns the advanced slot number and the updated symbol table
compileVariableDeclarationPart ::
    Int -> Symbols -> ASTVariableDeclarationPart -> (Int, Symbols)
compileVariableDeclarationPart slot symbols [] =
    (slot, symbols)
compileVariableDeclarationPart slot symbols ((idl, td) : xs) =
    compileVariableDeclarationPart slot' symbols' xs
    where
        -- compile each line of declration, local variable don't have varness
        (slot', symbols') = compileIdentifierList slot symbols (False, idl, td)


compileIdentifierList ::
    Int -> Symbols -> (Bool, ASTIdentifierList, ASTTypeDenoter)
        -> (Int, Symbols)
compileIdentifierList slot symbols (_, [], _) =
    (slot, symbols)
compileIdentifierList slot symbols (var, (x:xs), td) =
    -- compile each declaration in the list
    compileIdentifierList slot' (ptable, Map.insert x (var, td, slot) table)
        (var, xs, td)
    where
        -- array occupies a continuous block of positions in the current stack
        -- frame with a size of (hi - lo + 1)
        (ptable, table) = symbols
        slot' = case td of
            ArrayTypeDenoter ((lo, hi), _) ->
                if lo <= hi then
                    -- the variable containing a referenced array only needs to
                    -- hold the address, this would allow to pass a whole array
                    -- to a procedure by value or by reference
                    if var then slot + 1
                        else slot + hi - lo + 1
                    else error (printf "the interval %d..%d is invalid" lo hi)
            OrdinaryTypeDenoter _ ->
                slot + 1


-- compile a list of procedures
-- takes a label number, a symbol table and an AST fragment
-- returns the advanced label number and the generated code
compileProcedureDeclarationPart ::
    Int -> Symbols -> ASTProcedureDeclarationPart -> (Int, String)
compileProcedureDeclarationPart label symbols [] =
    (label, "")
compileProcedureDeclarationPart label symbols (x : xs) =
    (label'', text1 ++ text2)
    where
        -- compile each of the procedures
        (label', text1) = compileProcedureDeclaration label symbols x
        (label'', text2) = compileProcedureDeclarationPart label' symbols xs


compileProcedureDeclaration ::
    Int -> Symbols -> ASTProcedureDeclaration -> (Int, String)
compileProcedureDeclaration label symbols (pid, fpl, vdp, cs) =
    (
        label',
        printComment (printf "<%s %s>" printTokenProcedure pid) ++
            printProcedureName pid ++
            -- push a new stack frame for the current procedure
            printPushStackFrame slot' ++
            -- first parse the parameter list
            printComment "<store parameters>" ++
            text1 ++
            printComment "</store parameters>" ++
            -- parse all statements
            text2 ++
            -- pop the current stack frame
            printPopStackFrame slot' ++
            printReturn ++
            printComment (printf "</%s %s>" printTokenProcedure pid)
        )
    where
        (slot, symbols', text1) = compileFormalParameterList 0 symbols fpl
        (slot', symbols'') = compileVariableDeclarationPart slot symbols' vdp
        (label', text2) = compileCompoundStatement label symbols'' cs


compileFormalParameterList ::
    Int -> Symbols -> ASTFormalParameterList -> (Int, Symbols, String)
compileFormalParameterList slot symbols [] =
    (slot, symbols, "")
compileFormalParameterList slot symbols (x:xs) =
    (
        -- for n parameters, the value is stored in registers numbering 0 to n
        -- - 1 and the parameters are treated as local variable (the referenced
        -- variable is also a local variable holding the address) so we need an
        -- extra n slots, a special case is when an array is passed by value,
        -- which needs to store [slot..slots] variables, that's why I use
        -- concatMap
        slot'',
        -- store the values from registers to the stack
        symbols'',
        -- proceed to next parameter(s)
        concatMap (\i -> printStore i i) (take (slot' - slot) [slot..]) ++ re
        )
    where
        (slot', symbols') =
            compileIdentifierList slot symbols x
        (slot'', symbols'', re) = compileFormalParameterList slot' symbols' xs


-- compile a list of statements
-- takes a label number, a symbol table and an AST fragment
-- returns the advanced label number and the generated code
compileCompoundStatement ::
    Int -> Symbols -> ASTCompoundStatement -> (Int, String)
compileCompoundStatement label symbols [] =
    (label, "")
compileCompoundStatement label symbols (x:xs) =
    -- proceed to next statement
    (label'', text ++ text')
    where
        -- compile each statement
        (label', text) = compileStatement label symbols x
        (label'', text') = compileCompoundStatement label' symbols xs


-- all statements are completed, meaning that all regisiters are available after
-- the execution but it could be useful for variable liveness analysis, but the
-- point is all statements are using regisiters from 0 to perform an action
-- statement doesn't need to use a label and the return value indicates the next
-- avaiable label number
compileStatement ::
    Int -> Symbols -> ASTStatement -> (Int, String)
compileStatement label symbols s =
    case s of
        AssignmentStatement (va, e) ->
            (
                label,
                printComment "<assign>" ++
                    t ++
                    text2 ++
                    conversion ++
                    (if ref then printStoreIndirect else printStore) n reg ++
                    printComment "</assign>"
                )
            where
                (ref, tid1, (n, t)) = compileVariableAccess 0 symbols va
                -- determine if r0 is occupied by an address, i.e. when it is a
                -- indexed variable access
                reg = if ref then 1 else 0
                (tid2, text2) = compileExpression reg symbols False e

                -- may not be evaluated, apply conversion if appropriate
                conversion = if tid1 == tid2 then ""
                    -- do upcast when it needs to
                    else if tid1 == RealTypeIdentifier &&
                        tid2 == IntegerTypeIdentifier then printInt2Real reg reg
                        -- or otherwise the types are incorrectly matched
                        else error "type mismatch"

        ReadStatement va ->
            (
                label,
                printComment "<read>" ++
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
                    (if ref then printStoreIndirect else printStore) n 0 ++
                    printComment "</read>"
                )
            where
                -- r0 is used to store the return value of read
                (ref, tid, (n, t)) = compileVariableAccess 1 symbols va

        WriteStatement ws ->
            (
                label,
                printComment "<write>" ++
                    text ++
                    printCallBuiltin (case tid of
                        IntegerTypeIdentifier ->
                            "print_int"
                        RealTypeIdentifier ->
                            "print_real"
                        BooleanTypeIdentifier ->
                            "print_bool"
                        ) ++
                    printComment "</write>"
            )
            where
                (tid, text) = compileExpression 0 symbols False ws
        WriteStringStatement wss ->
            (
                label,
                printComment "<write>" ++
                    printStringConst 0 wss ++
                    printCallBuiltin "print_string" ++
                    printComment "</write>"
                )
        WritelnStatement ->
            (
                label,
                printComment "<writeln>" ++
                    printCallBuiltin "print_newline" ++
                    printComment "</writeln>"
                )
        ProcedureStatement (pid, apl) ->
            (
                label,
                printComment "<actual parameters>" ++
                    text1 ++
                    printComment "</actual parameters>" ++
                    printCall pid
                )
            where
                (ptable, _) = symbols
                -- determine if the procedure exists
                fpl = case Map.lookup pid ptable of
                    Nothing ->
                        error (printf "procedure %s undeclared" pid)
                    Just fpl ->
                        fpl
                -- first compile the actual parameter list
                (_, text1) = compileActualParameterList 0 symbols fpl apl
        CompoundStatement cs ->
            compileCompoundStatement label symbols cs
        IfStatement (e, s, ms) ->
            -- determine if there is an else statement
            case ms of
                Nothing ->
                    (
                        label' + 1,
                        text ++
                            printBranchFalse 0 label' ++
                            printComment "</cond>" ++ (
                                if tid == BooleanTypeIdentifier then
                                    text2
                                    else error "if guard should be boolean"
                                ) ++
                            printComment "</if>" ++
                            printIntLabel label'
                        )
                -- have an else branch
                Just s ->
                    (
                        label'' + 1,
                        text ++
                            printBranchFalse 0 label' ++
                            printComment "</cond>" ++ (
                                if tid == BooleanTypeIdentifier then
                                    text2
                                    else error (
                                        "if guard should be boolean"
                                        )
                                ) ++
                            printBranchUncond label'' ++
                            printComment "</if>" ++
                            printIntLabel label' ++
                            printComment "<else>" ++
                            text3 ++
                            printComment "</else>" ++
                            printIntLabel label''
                        )
                    where
                        (label'', text3) =
                            compileStatement (label' + 1) symbols s

            where
                (tid, text1) = compileExpression 0 symbols False e
                (label', text2) = compileStatement label symbols s
                text = printComment "<if>" ++ printComment "<cond>" ++ text1

        WhileStatement (e, s) ->
            (
                label' + 1,
                printComment "<while>" ++
                    printComment "<prior cond>" ++
                    text1 ++
                    printBranchFalse 0 label' ++
                    printComment "</prior cond>" ++
                    printIntLabel label ++ (
                        if tid == BooleanTypeIdentifier then text2
                            else
                                error "while guard should be boolean expression"
                        ) ++
                    printComment "<post cond>" ++
                    text1 ++
                    printBranchTrue 0 label ++
                    printComment "</post cond>" ++
                    printComment "</while>" ++
                    printIntLabel label'
                )
            where
                (tid, text1) = compileExpression 0 symbols False e
                -- the statement(s) will use the next label available since
                -- there is one label before for while
                (label', text2) = compileStatement (label + 1) symbols s

        ForStatement (fid, e1, d, e2, s) ->
            if tid == IntegerTypeIdentifier && tid1 == IntegerTypeIdentifier &&
                tid2 == IntegerTypeIdentifier then
                    (
                        label'' + 1,
                        printComment "<for>" ++
                            printComment "<prior cond>" ++
                            text1 ++
                            t ++
                            (if ref then printStoreIndirect else printStore)
                                n 0 ++
                            text2 ++
                            cmptext ++
                            printBranchTrue 0 label'' ++
                            printComment "</prior cond>" ++
                            printIntLabel label ++
                            text3 ++
                            printComment "<post cond>" ++
                            printComment "<update>" ++
                            t ++
                            (if ref then printLoadIndirect else printLoad)
                                0 n ++
                            printIntConst 1 1 ++
                            printBinary ac (Just "int") 0 0 1 ++
                            t ++
                            (if ref then printStoreIndirect else printStore)
                                n 0 ++
                            printComment "</update>" ++
                            -- re-evaluate the upper bound
                            text2 ++
                            cmptext ++
                            printBranchFalse 0 label ++
                            printComment "</post cond>" ++
                            printComment "</for>" ++
                            printIntLabel label''
                        )
                else error "boundaries and variable must be integer type"
            where
                (tid1, text1) = compileExpression 0 symbols False e1
                (tid2, text2) = compileExpression 1 symbols False e2
                -- there is one label for the for loop before the statement(s)
                (label'', text3) = compileStatement (label + 1) symbols s
                -- access the variable
                (ref, tid, (n, t)) =
                    compileVariableAccess 2 symbols (OrdinaryVariableAccess fid)
                (cmptext, ac) = case d of
                    ForDirectionUp ->
                        (printCmp "gt" "int" 0 0 1, "add")
                    ForDirectionDown ->
                        (printCmp "lt" "int" 0 0 1, "sub")

        EmptyStatement ->
            (label, "")

compileVariableAccess ::
    Int -> Symbols -> ASTVariableAccess
        -> (Bool, ASTTypeIdentifier, (Int, String))
compileVariableAccess reg symbols va =
    case va of
        IndexedVariableAccess (aid, e) ->
            (True, tid, (reg, text))
            where
                -- determine if variable exists
                (var, td, slot) = case Map.lookup aid table of
                    Nothing ->
                        error (printf "variable %s undeclared" aid)
                    Just i ->
                        i
                (tid', t) = compileExpression reg symbols False e
                (tid, text) = case td of
                    OrdinaryTypeDenoter _ ->
                        -- variable which is not an array cannot be indexed
                        error (printf "%s is not an array" aid)
                    ArrayTypeDenoter at ->
                        compileArrayType slot var reg symbols e at
        OrdinaryVariableAccess vid ->
            case td of
                -- allow to pass a whole array
                ArrayTypeDenoter _ ->
                    error "getting a whole array is not supported yet"
                OrdinaryTypeDenoter tid ->
                    (
                        var,
                        tid,
                        -- preload the address if necessary
                        if var then (reg, printLoad reg slot) else (slot, "")
                        )
            where
                -- determine if variable exists
                (var, td, slot) = case Map.lookup vid table of
                    Nothing ->
                        error (printf "variable %s undeclared" vid)
                    Just i ->
                        i
    where
        (_, table) = symbols


compileArrayType ::
    Int -> Bool -> Int -> Symbols -> ASTExpression -> ASTArrayType
        -> (ASTTypeIdentifier, String)
compileArrayType slot ref reg symbols e ((lo, hi), tid) =
    if tid' == IntegerTypeIdentifier then
        (
            tid,
            printComment "<array>" ++
                text ++
                -- put boundaries into registers
                printIntConst r1 lo ++
                printIntConst r2 hi ++
                printComment "<boundaries>" ++
                -- runtime lower bound check
                printCmp "lt" "int" r3 r0 r1 ++
                printToException "on_true" r3 "__IndexException" ++
                -- runtime upper bound check
                printCmp "gt" "int" r3 r0 r2 ++
                printToException "on_true" r3 "__IndexException" ++
                printComment "</boundaries>" ++
                printComment "<offset>" ++
                printBinary "sub" (Just "int") r0 r0 r1 ++
                printComment "</offset>" ++
                (if ref then printLoad else printLoadAddress) r1 slot ++
                -- since the top of the stack is the last element
                printBinary "sub" (Just "offset") r0 r1 r0 ++
                printComment "</array>"
            )
        else error "array index must be integer"
    where
        r0 = reg
        r1 = reg + 1
        r2 = reg + 2
        r3 = reg + 3
        -- expression as the index
        (tid', text) = compileExpression r0 symbols False e

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
    (
        slot',
        text ++
            case td of
                ArrayTypeDenoter _ ->
                    error "array type in procedure are not supported yet"
                OrdinaryTypeDenoter ftid ->
                    if ftid == atid then ""
                        else if ftid == RealTypeIdentifier &&
                            atid == IntegerTypeIdentifier then
                                printInt2Real slot slot
                                else error "parameter type mismatch"
            ++ text1
        )
    where
        (atid, text) = compileExpression slot symbols var y
        (slot', text1) = compileActualParameterList (slot + 1) symbols xs ys

compileExpression ::
    Int -> Symbols -> Bool -> ASTExpression -> (ASTTypeIdentifier, String)
compileExpression reg symbols ref e =
    case msesc of
        Nothing ->
            -- transfer to higher precedence
            compileSimpleExpression reg symbols ref e
        Just (se1, se2, c) ->
            (
                BooleanTypeIdentifier,
                text1' ++ text2' ++ printCmp c t reg reg reg'
                )
            where
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
                -- determine the type to be compared
                (r, t) = if r1 || r2 then (True, "real") else (False, "int")
                -- upcast if necessary
                text1' = text1 ++
                    if r && not r1 then printInt2Real reg reg else ""
                text2' = text2 ++
                    if r && not r2 then printInt2Real reg' reg' else ""
    where
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

compileSimpleExpression ::
    Int -> Symbols -> Bool -> ASTSimpleExpression -> (ASTTypeIdentifier, String)
compileSimpleExpression reg symbols ref se =
    case msetop of
        Nothing ->
            -- transfer to higher precedence
            compileTerm reg symbols ref se
        Just (mse, t, op) ->
            case mse of
                Nothing ->
                    (
                        tid,
                        text ++ case op of
                            -- do nothing
                            "pos" ->
                                ""
                            "neg" ->
                                printUnary op t' reg reg
                        )
                    where
                        (tid, text) = compileTerm reg symbols ref t
                        t' = case tid of
                            RealTypeIdentifier ->
                                Just "real"
                            IntegerTypeIdentifier ->
                                Just "int"
                            BooleanTypeIdentifier ->
                                error "must use not to negate a boolean"
                Just se ->
                    case op of
                        "or" ->
                            (
                                BooleanTypeIdentifier,
                                if tid1 == BooleanTypeIdentifier &&
                                    tid2 == BooleanTypeIdentifier then
                                        text1 ++
                                            text2 ++
                                            printBinary op Nothing reg reg reg'
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
                                    printBinary op (Just t') reg reg reg'
                                )
                    where
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
                                error "cannot add/sub boolean"
                        r2 = case tid2 of
                            RealTypeIdentifier ->
                                True
                            IntegerTypeIdentifier ->
                                False
                            BooleanTypeIdentifier ->
                                error "cannot add/sub boolean"
                        -- determine the type to be compared
                        (r, t') = if r1 || r2 then (True, "real")
                            else (False, "int")
                        -- upcast if necessary
                        text1' = text1 ++ if r && not r1 then
                            printInt2Real reg reg
                            else ""
                        text2' = text2 ++ if r && not r2 then
                            printInt2Real reg' reg'
                            else ""
    where
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

compileTerm ::
    Int -> Symbols -> Bool -> ASTTerm -> (ASTTypeIdentifier, String)
compileTerm reg symbols ref t =
    case mtfop of
        Nothing ->
            -- transfer to higher precedence
            compileFactor reg symbols ref t
        Just (t, f, op) ->
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
                                    printComment "<zero division>" ++
                                    printIntConst reg'' 0 ++
                                    printCmp "eq" "int" reg'' reg' reg'' ++
                                    printToException "on_true" reg''
                                        "__ZeroDivException" ++
                                    printComment "</zero division>" ++
                                    printBinary op Nothing reg reg reg'
                                )
                            else  error "both sides must be integer"
                "div_real" ->
                    (
                        RealTypeIdentifier,
                        text1' ++
                            text2' ++
                            printComment "<zero division>" ++
                            printRealConst reg'' 0 ++
                            printCmp "eq" "real" reg'' reg' reg'' ++
                            printToException "on_true" reg''
                                "__ZeroDivException" ++
                            printComment "</zero division>" ++
                            printBinary op Nothing reg reg reg'
                        )
                "mul" ->
                    (
                        if r then RealTypeIdentifier
                            else IntegerTypeIdentifier,
                        text1' ++
                            text2' ++
                            printBinary op (Just t') reg reg reg'
                        )
            where
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
    where
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

compileFactor ::
    Int -> Symbols -> Bool -> ASTFactor -> (ASTTypeIdentifier, String)
compileFactor reg symbols ref f =
    case f of
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
            (
                tid,
                -- if require an address
                if ref then
                    -- if it is already an address
                    if ref' then t
                        -- load the address
                        else printLoadAddress reg n
                    -- load the value from address
                    else if ref' then t ++ printLoadIndirect reg reg
                        -- load the value from slot directly
                        else printLoad reg n
                )
            where
                (ref', tid, (n, t)) = compileVariableAccess reg symbols va
        NotExpression f ->
            (
                BooleanTypeIdentifier,
                if tid == BooleanTypeIdentifier then text ++
                    printUnary "not" Nothing reg reg
                    else error "negating a non-boolean"
                )
            where
                (tid, text) = compileFactor reg symbols ref f
        otherwise ->
            -- possibly parenthesis is used
            compileExpression reg symbols ref f
