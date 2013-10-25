import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Data.Map as Map hiding (map)
import Numeric
import Data.Char hiding (isSymbol, isNumber)
import Control.Monad.Error

main = do args <- getArgs
          let evaled = liftM show $ readExpr (args!!0) >>= eval
          putStrLn $ extractValue $ trapError evaled

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool 
             | Character Char 
             | Array [LispVal] 

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

trapError :: ThrowsError String -> ThrowsError String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-- | Parse input as an Lisp expression.

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> throwError $ Parser err
                   Right val -> return val

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

instance Show LispVal where
    show (String contents) = "\"" ++ contents ++ "\""
    show (Atom name) = name
    show (Number contents) = show contents
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (List contents) = "(" ++ unwordsList contents ++ ")"
    show (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ show tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

-- | The Lisp Evaluator

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval val@(Character _) = return val
eval (Atom s) = case Prelude.lookup s primitives of
                      Just _ -> return $ Atom "#primitive"
                      Nothing -> throwError $ UnboundVar "Unbound variable" s
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) = do result <- eval pred
                                                case result of
                                                  Bool False -> eval alt
                                                  otherwise -> eval conseq
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func) 
                  ($ args) $ Prelude.lookup func primitives

-- | Primitive functions.
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
              ("eqv?", eqv)]

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
