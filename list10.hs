import System.Environment
import System.IO hiding (try)
import System.IO.Error
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Monad.Error
import Control.Exception.Base hiding (try)
import qualified Data.Map as Map
import Numeric
import Data.Char hiding (isSymbol, isNumber)
import Data.IORef

main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runOne $ args

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr =  (evalString env expr >>= putStrLn)
                         `catch`
                         (\e -> do let err = show (e :: IOException)
                                   hPutStrLn stderr err)

until_ :: (a -> Bool) -> IO a -> (a -> IO ()) -> IO ()
until_ pred prompt action = do result <- prompt
                               if pred result 
                               then return ()
                               else action result >> until_ pred prompt action

runOne :: [String] -> IO ()
runOne args = do
  env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)] 
  (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)])) 
              >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = (primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint)
          `catch`
          (\e -> if isEOFError e
                 then hPutStrLn stderr ""
                 else do let err = show e
                         hPutStrLn stderr err)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool 
             | Character Char 
             | Array [LispVal] 
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { 
                 params :: [String],
                 vararg :: (Maybe String), 
                 body :: [LispVal],
                 closure :: Env
               }
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle

instance Show LispVal where
    show (String contents) = "\"" ++ contents ++ "\""
    show (Atom name) = name
    show (Number contents) = show contents
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (List contents) = "(" ++ unwordsList contents ++ ")"
    show (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ show tail ++ ")"
    show (PrimitiveFunc _) = "<primitive>"
    show (Func {params = args, vararg = varargs, body = body, closure = env}) = 
        "(lambda (" ++ unwords (map show args) ++ 
                        (case varargs of 
                           Nothing -> ""
                           Just arg -> " . " ++ arg) ++ ") ...)" 
    show (Port _) = "<IO port>"
    show (IOFunc _) = "<IO primitive>"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where
    show (UnboundVar message varname) = message ++ ": " ++ varname
    show (BadSpecialForm message form) = message ++ ": " ++ show form
    show (NotFunction message func) = message ++ ": " ++ show func
    show (NumArgs expected found) = "Expected " ++ show expected 
                                    ++ " args; found values " ++ unwordsList found
    show (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                         ++ ", found " ++ show found
    show (Parser parseErr) = "Parse error at " ++ show parseErr

instance Error LispError where
    noMsg = Default "An error has occurred"
    strMsg = Default

type ThrowsError = Either LispError
type IOThrowsError = ErrorT LispError IO

trapError :: IOThrowsError String -> IOThrowsError String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

-- | Parse input as an Lisp expression.

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
                             Left err -> throwError $ Parser err
                             Right val -> return val

readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)

parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseString   -- begin with "
            <|> do char '#'
                   parseBool <|> parseNumber <|> parseCharacter <|> parseArray
            <|> parseDecNumber -- begin with a digit
            <|> parseQuoted    -- begin with '
            <|> parseBackquoted -- begin with `
            <|> do char '('
                   x <- (try parseList) <|> parseDottedList
                   char ')'
                   return x

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many parseChar
                 char '"'
                 return $ String x

parseChar :: Parser Char
parseChar = do c <- noneOf "\""
               if c == '\\' 
               then parseEscapedChar -- escaped char
               else return c         -- other chars

escapedChars :: Map.Map Char Char
escapedChars = Map.fromList [
                ('\"', '\"'), 
                ('n', '\n'), 
                ('r', '\r'), 
                ('t', '\t'), 
                ('\\', '\\')
               ]

parseEscapedChar :: Parser Char
parseEscapedChar = do c <- satisfy (`Map.member` escapedChars);
                      case Map.lookup c escapedChars of
                        Just x -> return x -- always match

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ Atom atom

parseBool :: Parser LispVal
parseBool = do c <- choice $ map char "tf"
               return $ case c of
                          't' -> Bool True
                          'f' -> Bool False

parseNumber :: Parser LispVal
parseNumber = do base <- choice $ map char "bodx"
                 liftM (Number . fst . head) $
                       case base of
                         'b' -> many1 binDigit >>= return . readBin
                         'o' -> many1 octDigit >>= return . readOct
                         'd' -> many1 digit >>= return . readDec
                         'x' -> many1 hexDigit >>= return . readHex

parseDecNumber :: Parser LispVal
parseDecNumber = liftM (Number . read) $ many1 digit

parseCharacter :: Parser LispVal
parseCharacter = do char '\\'
                    c <- anyChar
                    s <- many letter
                    case map toLower (c:s) of
                      [a] -> return $ Character a
                      "space" -> return $ Character ' '
                      "newline" -> return $ Character '\n'
                      x -> (unexpected $ "Invalid character name: " ++ x) <?> "\"newline\" or \"space\""

parseArray :: Parser LispVal
parseArray = do char '('
                x <- sepBy parseExpr spaces
                char ')'
                return $ Array x

binDigit :: Parser Char
binDigit = satisfy (`elem` "01")

readBin :: ReadS Integer
readBin = readInt 2 (`elem` "01") digitToInt

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do head <- endBy parseExpr spaces
                     tail <- char '.' >> spaces >> parseExpr
                     return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do char '\''
                 x <- parseExpr
                 return $ List [Atom "quote", x]

parseBackquoted :: Parser LispVal
parseBackquoted = do char '`'
                     x <- parseExpr
                     return $ List [Atom "quasiquote", x]

-- | The Lisp Evaluator

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env val@(Character _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) = do result <- eval env pred
                                                    case result of
                                                      Bool False -> eval env alt
                                                      otherwise -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) =
    eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
    eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
    makeVarargs varargs env params body >>= defineVar env var
eval env badform@(List (Atom "define" : _)) = throwError $ BadSpecialForm "Bad define form" badform
eval env (List (Atom "lambda" : List params : body)) =
    makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
    makeVarargs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarargs varargs env [] body
eval env (List [Atom "load", String filename]) = 
    load filename >>= liftM last . mapM (eval env)
eval env (List (function : args)) = do 
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals

makeFunc varargs env params body = return $ Func (map show params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarargs = makeFunc . Just . show

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (IOFunc func) args = func args
apply (Func params varargs body closure) args = 
    if num params /= num args && varargs == Nothing
       then throwError $ NumArgs (num params) args
       else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
    where remainingArgs = drop (length params) args
          num = toInteger . length
          evalBody env = liftM last $ mapM (eval env) body 
          bindVarArgs arg env = case arg of
              Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
              Nothing -> return env 

-- | Primitive functions.

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
                                              ++ map (makeFunc PrimitiveFunc) primitives)
    where makeFunc constructor (var, func) = (var, constructor func)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", isSymbol),
              ("string?", isString),
              ("number?", isNumber),
              ("symbol->string", symbolToString),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2 
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol [Atom _] = return $ Bool True
isSymbol [(List [Atom "quote", Atom _])] = return $ Bool True
isSymbol _ = return $ Bool False

isString :: [LispVal] -> ThrowsError LispVal
isString [String _] = return $ Bool True
isString _ = return $ Bool False

isNumber :: [LispVal] -> ThrowsError LispVal
isNumber [Number _] = return $ Bool True
isNumber _ = return $ Bool False

symbolToString :: [LispVal] -> ThrowsError LispVal
symbolToString emptyVal@[] = throwError $ NumArgs 1 emptyVal
symbolToString [Atom s] = return $ String s
symbolToString [(List [Atom "quote", Atom val])] = return $ String val
symbolToString [notSymbol] = throwError $ TypeMismatch "symbol" notSymbol
symbolToString x@(a:b:bs) = throwError $ NumArgs 1 x

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

-- | Implementing 'car'.
-- Rules:
--   (car (a b c)) = a
--   (car (a)) = a
--   (car (a b . c)) = a
--   (car a) = error (not a list)
--   (car a b) = error (car takes only one argument)
car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

-- | Implementing 'cdr'.
-- Rules:
--   (cdr (a b c)) = (b c)
--   (cdr (a b)) = (b)
--   (cdr (a)) = NIL
--   (cdr (a b . c)) = (b . c)
--   (cdr (a . b)) = b
--   (cdr a) = error (not list)
--   (cdr a b) = error (too many args)
cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] = return $ List xs
cdr [DottedList [xs] x] = return x
cdr [DottedList (_ : xs@(a:as)) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

-- | Implementing 'cons'.
-- Rules:
--   (cons x '()) = (x)
--   (cons x '(a b c ...)) = (x a b c ...)
--   (cons x '(a b c . d)) = (x a b c . d)
--   (cons x y) = (x . y)
--   (cons x) => error
cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ [x] ++ xs
cons [x, DottedList xs xlast] = return $ DottedList (x:xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) && 
                                                    (and $ map eqvPair $ zip arg1 arg2)
    where eqvPair (x1, x2) = case eqv [x1, x2] of
                               Left err -> False
                               Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = 
    do{ unpacked1 <- unpacker arg1;
        unpacked2 <- unpacker arg2;
        return $ unpacked1 == unpacked2
      } `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) 
                     [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine stdin) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename

-- | Environment implementation.

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do env <- liftIO $ readIORef envRef
                       maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                                 (liftIO . readIORef)
                                 (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                                       (liftIO . (flip writeIORef value))
                                       (lookup var env)
                             return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do 
  alreadyDefined <- liftIO $ isBound envRef var 
  if alreadyDefined 
  then setVar envRef var value >> return value
  else liftIO $ do 
    valueRef <- newIORef value
    env <- readIORef envRef
    writeIORef envRef ((var, valueRef) : env)
    return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
          addBinding (var, value) = do ref <- newIORef value
                                       return (var, ref)
