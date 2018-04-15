module PazPrettify (prettyPrint) where

import Control.Monad (when)
import Data.Maybe (fromJust, isJust)
import PazLexer
import PazParser

notnull :: [a] -> Bool
notnull = not . null

---------- Variable Declaration Part

printIdentifierList :: ASTIdentifierList -> IO ()
printIdentifierList (x, xs) = do
    putStr x
    -- print the rest of ids, adding ", " to separate and ": " for ending
    sequence_ (map (\x -> do
        putStr ", "
        putStr x
        ) xs)

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
    -- no space for subrange
    printConstant l
    putStr ".."
    printConstant r

printArrayType :: ASTArrayType -> IO ()
printArrayType (st, ti) = do
    putStr "array["
    printSubrangeType st
    putStr "] of "
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

printProcedureDeclaration :: ASTProcedureDeclaration -> IO ()
printProcedureDeclaration (pid, mfpl, vdp, cs) = do
    putStr "procedure "
    putStr pid
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
        putStr ";\n"
        ) pdp)

----------

---------- Compound Statement

----- Expression

printSign :: ASTSign -> IO ()
printSign s =
    case s of
        SignPlus ->
            putChar '+'
        SignMinus ->
            putChar '-'

printConstant :: ASTConstant -> IO ()
printConstant (ms, ui) = do
    when (isJust ms) (printSign (fromJust ms))
    putStr ui

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
    when (isJust ms) (printSign (fromJust ms))
    putStr ds

printUnsignedReal :: ASTUnsignedReal -> IO ()
printUnsignedReal (ds, mds, msf) = do
    putStr (ds)
    when (isJust mds) (do
        putChar '.'
        putStr (fromJust mds)
        )
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
printCharacterString (x:xs) = do
    if x == '\'' then putStr "''" else putChar x
    printCharacterString xs
printCharacterString [] =
    return ()

printUnsignedConstant :: ASTUnsignedConstant -> IO ()
printUnsignedConstant uc =
    case uc of
        UnsignedNumber un ->
            printUnsignedNumber un
        CharacterString cs -> do
            putChar '\''
            printCharacterString cs
            putChar '\''

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

printIndexedVariableAccess :: ASTIndexedVariable -> IO ()
printIndexedVariableAccess (vid, e) = do
    putStr vid
    putChar '['
    -- no need to add parenthesis
    printExpression 0 e
    putChar ']'

printVariableAccess :: ASTVariableAccess -> IO ()
printVariableAccess va =
    case va of
        IndexedVariableAccess iv ->
            printIndexedVariableAccess iv
        OrdinaryVariableAccess vid ->
            putStr vid

printFactor :: Int -> ASTFactor -> IO ()
printFactor level f =
    case f of
        -- no need to add parenthesis, regardless of level
        UnsignedConstant uc ->
            printUnsignedConstant uc
        VariableAccess va ->
            printVariableAccess va

        -- this means a "not" is parsed previously
        Factor f -> do
            putStr "not "
            printFactor 6 f

        Expression e -> do
            printExpression level e

-- for (*, /, div, or) operators
printTerm :: Int -> ASTTerm -> IO ()
printTerm level (f, []) = do
    when pare (putChar '(')
    printFactor level f
    when pare (putChar ')')
    where
        pare = level > 4
printTerm level (f, mofs) = do
    when pare (putChar '(')
    printFactor 4 f
    sequence_ (map (\(mo, f) -> do
        putChar ' '
        printMultiplyingOperator mo
        putChar ' '
        case mo of
            OperatorTimes ->
                printFactor 5 f
            otherwise ->
                printFactor 4 f
        ) mofs)
    when pare (putChar ')')
    where
        pare = level > 4

-- for (+, -, or) operators
printSimpleExpression :: Int -> ASTSimpleExpression -> IO ()
printSimpleExpression level (ms, t, []) = do
    case ms of
        Just s -> do
            when pare (putChar '(')
            printSign s
            -- negative sign has lower precedence accordingly
            printTerm 2 t
            when pare (putChar ')')
        Nothing ->
            printTerm level t
    where
        pare = level >= 2
printSimpleExpression level (ms, t, haots) = do
    when pare (putChar '(')
    case ms of
        Just s -> do
            printSign s
            printTerm 2 t
        Nothing ->
            printTerm 2 t
    sequence_ (map (\(ao, t) -> do
        putChar ' '
        printAddingOperator ao
        putChar ' '
        case ao of
            OperatorAdd ->
                printTerm 2 t
            otherwise ->
                printTerm 3 t
        ) haots)
    when pare (putChar ')')
    where
        pare = level >= 2


-- for relational operators (<, >, <>, <=, >=, =)
printExpression :: Int -> ASTExpression -> IO ()
printExpression level (se, Just rose) = do
    -- allow cases such as (a < b) = (c > d)
    when pare (putChar '(')
    printSimpleExpression 1 se
    putChar ' '
    printRelationalOperator ro
    putChar ' '
    printSimpleExpression 1 nse
    when pare (putChar ')')
    where
        pare = level >= 1
        (ro, nse) = rose
printExpression level (se, Nothing) =
    printSimpleExpression level se

-----

----- Statement

printAssignmentStatement :: Int -> ASTAssignmentStatement -> IO ()
printAssignmentStatement ind (va, e) = do
    putStr (replicate ind ' ')
    printVariableAccess va
    putStr " := "
    printExpression 0 e

printActualParameterList :: ASTActualParameterList -> IO ()
printActualParameterList (e, es) = do
    putChar '('
    printExpression 0 e
    sequence_ (map (\e -> do
        putStr ", "
        printExpression 0 e
        ) es)
    putChar ')'

printProcedureStatement :: Int -> ASTProcedureStatement -> IO ()
printProcedureStatement ind (pid, mapl) = do
    putStr (replicate ind ' ')
    putStr pid
    when (isJust mapl) (printActualParameterList (fromJust mapl))

printIfStatement :: Int -> ASTIfStatement -> IO ()
printIfStatement ind (e, s, ms) = do
    let spaces = replicate ind ' '
    putStr spaces
    putStr "if "
    printExpression 0 e
    putStrLn " then"
    printStatement (ind + 4) s
    when (isJust ms) (do
        putChar '\n'
        putStr spaces
        putStrLn "else"
        printStatement (ind + 4) (fromJust ms)
        )

printWhileStatement :: Int -> ASTWhileStatement -> IO ()
printWhileStatement ind (e, s) = do
    putStr (replicate ind ' ')
    putStr "while "
    printExpression 0 e
    putStrLn " do"
    printStatement (ind + 4) s

printForRangeOperator :: ASTForRangeOperator -> IO ()
printForRangeOperator fro =
    case fro of
        To ->
            putStr "to"
        DownTo ->
            putStr "downto"

printForStatement :: Int -> ASTForStatement -> IO ()
printForStatement ind (fid, e1, fro, e2, s) = do
    putStr (replicate ind ' ')
    putStr "for "
    putStr fid
    putStr " := "
    printExpression 0 e1
    putChar ' '
    printForRangeOperator fro
    putChar ' '
    printExpression 0 e2
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
            -- begin should indent with previous
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
    sequence_ (map (\s -> do
        putStr ";\n"
        printStatement ind s
        ) ss)

printCompoundStatement :: Int -> ASTCompoundStatement -> IO ()
printCompoundStatement ind cs = do
    putStr spaces
    putStrLn "begin"
    printStatementSequence (ind + 4) cs
    putChar '\n'
    putStr spaces
    putStr "end"
    where
        spaces = replicate ind ' '

-----

---------

--------- main prettify function

prettyPrint :: ASTProgram -> IO ()
prettyPrint (pid, vdp, pdp, cs) = do
    -- program declaration
    putStr "program "
    putStr pid
    putStrLn ";"

    -- variable declarations, global
    when (isJust vdp) (putChar '\n')
    printVariableDeclarationPart vdp

    -- procedure declarations
    printProcedureDeclarationPart pdp

    -- compound statement
    putChar '\n'
    printCompoundStatement 0 cs
    putStrLn "."

---------
