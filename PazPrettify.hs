module PazPrettify (prettyPrint) where

import Control.Monad (when)
import Data.Maybe (fromJust, isJust)
import PazLexer
import PazParser

---------- Variable Declaration Part

printIdentifierList :: ASTIdentifierList -> IO ()
printIdentifierList (x, xs) = do
    putStr x
    -- print the rest of ids, adding ", " to separate and ": " for ending
    sequence_ (map (\x -> putStr (", " ++ x)) xs)

printTypeIdentifier :: ASTTypeIdentifier -> IO ()
printTypeIdentifier ti =
    case ti of
        IntegerTypeIdentifier ->
            putStr "integer"
        RealTypeIdentifier ->
            putStr "real"
        BooleanTypeIdentifier ->
            putStr "boolean"

printSubrangeType :: ASTSubrangeType -> IO ()
printSubrangeType (l, r) = do
    putChar '['
    printConstant l
    putStr ".."
    printConstant r
    putChar ']'

printArrayType :: ASTArrayType -> IO ()
printArrayType (st, ti) = do
    putStr "array"
    printSubrangeType st
    putStr " of "
    printTypeIdentifier ti

printTypeDenoter :: ASTTypeDenoter -> IO ()
printTypeDenoter td =
    case td of
        OrdinaryTypeDenoter ti ->
            printTypeIdentifier ti
        ArrayTypeDenoter at ->
            printArrayType at

printVariableDeclaration :: ASTVariableDeclaration -> IO ()
printVariableDeclaration (il, td) = do
    printIdentifierList il
    putStr ": "
    printTypeDenoter td

printVariableDeclarationPart :: ASTVariableDeclarationPart -> IO ()
printVariableDeclarationPart (Just vdp) = do
    putStrLn "var"
    let (hvd, tvds) = vdp
    let spaces = replicate 4 ' '
    printvd hvd
    sequence_ (map printvd tvds)
    where
        spaces = replicate 4 ' '
        -- print the indentation and end by ";"
        printvd vd = do
            putStr spaces
            printVariableDeclaration vd
            putStrLn ";"
printVariableDeclarationPart Nothing =
    return ()

----------

---------- Procedure Declaration Part

printProcedureDeclaration :: ASTProcedureDeclaration -> IO ()
printProcedureDeclaration (pid, mfpl, vdp, cs) = do
    putStr ("procedure " ++ pid)
    -- can have zero parameters
    case mfpl of
        Just fpl ->
            printFormalParameterList fpl
        Nothing ->
            return ()
    putStrLn ";"
    printVariableDeclarationPart vdp
    -- the compound statement needs to align with the procedure
    printCompoundStatement 0 cs

printProcedureDeclarationPart :: ASTProcedureDeclarationPart -> IO ()
printProcedureDeclarationPart pdp = do
    sequence_ (map (\pd -> do
        -- one blank line between two procedures
        putChar '\n'
        printProcedureDeclaration pd
        ) pdp)

----------

---------- Compound Statement

printLexerSign :: PazLexer.ASTSign -> IO ()
printLexerSign s =
    case s of
        PazLexer.SignPlus ->
            putChar '+'
        PazLexer.SignMinus ->
            putChar '-'

printParserSign :: PazParser.ASTSign -> IO ()
printParserSign s =
    case s of
        PazParser.SignPlus ->
            putChar '+'
        PazParser.SignMinus ->
            putChar '-'

printConstant :: ASTConstant -> IO ()
printConstant (ms, ui) = do
    when (isJust ms) (printParserSign (fromJust ms))
    putStr ui

printFormalParameterSection :: ASTFormalParameterSection -> IO ()
printFormalParameterSection (b, il, td) = do
    when b (putStr "var ")
    printVariableDeclaration (il, td)

printFormalParameterList :: ASTFormalParameterList -> IO ()
printFormalParameterList (hfps, tfpss) = do
    putChar '('
    printFormalParameterSection hfps

    -- parameters separated by "; "
    sequence_ (map (\fps -> do
        putStr "; "
        printFormalParameterSection fps
        ) tfpss)
    putChar ')'

printAddingOperator :: ASTAddingOperator -> IO ()
printAddingOperator ao =
    case ao of
        OperatorAdd ->
            putChar '+'
        OperatorMinus ->
            putChar '-'
        OperatorOr ->
            putStr "or"

printMultiplyingOperator :: ASTMultiplyingOperator -> IO ()
printMultiplyingOperator mo =
    case mo of
        OperatorTimes ->
            putChar '*'
        OperatorDivideBy ->
            putChar '/'
        OperatorDiv ->
            putStr "div"
        OperatorAnd ->
            putStr "and"

printScaleFactor :: ASTScaleFactor -> IO ()
printScaleFactor (ms, ds) = do
    when (isJust ms) (printLexerSign (fromJust ms))
    putStr ds

printUnsignedReal :: ASTUnsignedReal -> IO ()
printUnsignedReal (ds, mds, msf) = do
    putStr (ds)
    when (isJust mds) (putStr ("." ++ (fromJust mds)))
    when (isJust msf) (do
        putChar 'e'
        printScaleFactor (fromJust msf)
        )

printUnsignedNumber :: ASTUnsignedNumber -> IO ()
printUnsignedNumber un =
    case un of
        UnsignedInteger ui ->
            putStr ui
        UnsignedReal ur ->
            printUnsignedReal ur

printCharacterString :: ASTCharacterString -> IO ()
printCharacterString cs = do
    let q = if any (== '\'') cs then '"' else '\''
    putChar q
    putStr cs
    putChar q

printUnsignedConstant :: ASTUnsignedConstant -> IO ()
printUnsignedConstant uc =
    case uc of
        UnsignedNumber un ->
            printUnsignedNumber un
        CharacterString cs ->
            printCharacterString cs

printFactor :: ASTFactor -> IO ()
printFactor f =
    case f of
        UnsignedConstant uc ->
            printUnsignedConstant uc
        VariableAccess va ->
            printVariableAccess va
        Expression e ->
            printExpression e
        Factor f ->
            printFactor f

printTerm :: ASTTerm -> IO ()
printTerm (f, mofs) = do
    printFactor f
    sequence_ (map printmof mofs)
    where
        printmof (mo, f) = do
            putChar ' '
            printMultiplyingOperator mo
            putChar ' '
            printFactor f

printSimpleExpression :: ASTSimpleExpression -> IO ()
printSimpleExpression (ms, t, aots) = do
    when (isJust ms) (printParserSign (fromJust ms))
    printTerm t
    sequence_ (map printaot aots)
    where
        printaot (ao, t) = do
            putChar ' '
            printAddingOperator ao
            putChar ' '
            printTerm t

printRelationalOperator :: ASTRelationalOperator -> IO ()
printRelationalOperator ro =
    case ro of
        OperatorEqual ->
            putChar '='
        OperatorNotEqual ->
            putStr "<>"
        OperatorLessThan ->
            putChar '<'
        OperatorGreaterThan ->
            putChar '>'
        OperatorLessThanOrEqual ->
            putStr "<="
        OperatorGreaterThanOrEqual ->
            putStr ">="

printExpression :: ASTExpression -> IO ()
printExpression (se, mrose) = do
    printSimpleExpression se
    when (isJust mrose) (do
        let (ro, se) = fromJust mrose
        putChar ' '
        printRelationalOperator ro
        putChar ' '
        printSimpleExpression se
        )

printIndexedVariableAccess :: ASTIndexedVariable -> IO ()
printIndexedVariableAccess (vid, e) = do
    putStr vid
    putChar '['
    printExpression e
    putChar ']'

printVariableAccess :: ASTVariableAccess -> IO ()
printVariableAccess va =
    case va of
        IndexedVariableAccess iv ->
            printIndexedVariableAccess iv
        OrdinaryVariableAccess vid ->
            putStr vid

printAssignmentStatement :: Int -> ASTAssignmentStatement -> IO ()
printAssignmentStatement ind (va, e) = do
    putStr (replicate ind ' ')
    printVariableAccess va
    putStr " := "
    printExpression e
    putChar '\n'

printActualParameterList :: ASTActualParameterList -> IO ()
printActualParameterList (e, es) = do
    putChar '('
    printExpression e
    sequence_ (map (\e -> do
        putStr ", "
        printExpression e
        ) es)
    putChar ')'

printProcedureStatement :: Int -> ASTProcedureStatement -> IO ()
printProcedureStatement ind (pid, mapl) = do
    putStr (replicate ind ' ' ++ pid)
    when (isJust mapl) (printActualParameterList (fromJust mapl))
    putChar '\n'

printIfStatement :: Int -> ASTIfStatement -> IO ()
printIfStatement ind (e, s, ms) = do
    let spaces = replicate ind ' '
    putStr (spaces ++ "if ")
    printExpression e
    putStrLn " then"
    printStatement (ind + 4) s
    when (isJust ms) (do
        putStrLn (spaces ++ "else")
        printStatement (ind + 4) (fromJust ms)
        )

printWhileStatement :: Int -> ASTWhileStatement -> IO ()
printWhileStatement ind (e, s) = do
    putStr (replicate ind ' ' ++ "while ")
    printExpression e
    putStrLn " do"
    printStatement (ind + 4) s

printForStatement :: Int -> ASTForStatement -> IO ()
printForStatement ind (fid, e1, e2, s) = do
    putStr (replicate ind ' ' ++ "for " ++ fid ++ " := ")
    printExpression e1
    putStr " to "
    printExpression e2
    putStr " do\n"
    printStatement (ind + 4) s

printStatement :: Int -> ASTStatement -> IO ()
printStatement ind s =
    case s of
        AssignmentStatement as ->
            printAssignmentStatement ind as
        ProcedureStatement ps ->
            printProcedureStatement ind ps
        CompoundStatement cs ->
            printCompoundStatement (ind - 4) cs
        IfStatement is ->
            printIfStatement ind is
        WhileStatement ws ->
            printWhileStatement ind ws
        ForStatement fs ->
            printForStatement ind fs
        EmptyStatement ->
            return ()

printStatementSequence :: Int -> ASTStatementSequence -> IO ()
printStatementSequence ind (s, ss) = do
    printStatement ind s
    sequence_ (map (printStatement ind) ss)

printCompoundStatement :: Int -> ASTCompoundStatement -> IO ()
printCompoundStatement ind cs = do
    let spaces = replicate ind ' '
    putStrLn (spaces ++ "begin")
    printStatementSequence (ind + 4) cs
    putStrLn (spaces ++ "end")

---------

--------- main prettify function

prettyPrint :: ASTProgram -> IO ()
prettyPrint (pid, vdp, pdp, cs) = do
    -- program declaration
    putStrLn ("program " ++ pid ++ ";")

    -- variable declarations, global
    printVariableDeclarationPart vdp

    -- procedure declarations
    printProcedureDeclarationPart pdp

    -- compound statement
    putChar '\n'
    printCompoundStatement 0 cs
    putChar '\n'

---------
