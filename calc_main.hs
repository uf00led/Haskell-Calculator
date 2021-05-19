--Import
import Data.Char

--Lexing Part
data InputSymbols 
        = PlusToken
        | MinusToken
        | MultToken
        | DivToken
        | PowToken
        | OpenRoundToken
        | CloseRoundToken
        | OpenRectToken
        | CloseRectToken
        | VertBrcktToken
        | IntTypeToken   Int
        deriving (Show)

data Expr
        = IntLiteral    Int
        | BinaryLiteral BinaryOperator Expr Expr

data BinaryOperator
        = AddOperator
        | SubOperator
        | MultOperator
        | DivOperator
        | PowOperator

lexer :: String -> [InputSymbols]
lexer []                 = []
lexer ('+' : restString) = PlusToken       : lexer restString
lexer ('-' : restString) = MinusToken      : lexer restString
lexer ('*' : restString) = MultToken       : lexer restString
lexer ('/' : restString) = DivToken        : lexer restString
lexer ('^' : restString) = PowToken        : lexer restString
lexer ('(' : restString) = OpenRoundToken  : lexer restString
lexer (')' : restString) = CloseRoundToken : lexer restString
lexer ('[' : restString) = OpenRectToken   : lexer restString
lexer (']' : restString) = CloseRectToken  : lexer restString
lexer ('|' : restString) = VertBrcktToken  : lexer restString

lexer (symbol : restString) | isSpace symbol = lexer restString
lexer string @ (symbol : _) | isDigit symbol = IntTypeToken (convertToInt digitalString) : lexer restString
    where
        (digitalString, restString) = break (not . isDigit) string

lexer (_ : restString) = error ("\x1b[31mLexer error: \x1b[0mcatched unexpected symbol before: " ++ show restString ++ "")

convertToInt :: String -> Int
convertToInt = foldl (\acc symbol -> 10 * acc + digitToInt symbol) 0

--Parsing part
parseIntorBrcktExpr :: [InputSymbols] -> Maybe (Expr, [InputSymbols])
parseIntorBrcktExpr (IntTypeToken n : restTokens)        = Just (IntLiteral n, restTokens)
parseIntorBrcktExpr (OpenRoundToken : restTokensInBrckt) = case parseExpr restTokensInBrckt of
    Just (expr, (CloseRoundToken : restTokensBrckt))    -> Just (expr, restTokensBrckt)
    Just _                                              -> Nothing
    Nothing                                             -> Nothing

parseIntorBrcktExpr (OpenRectToken  : restTokensInBrckt) = case parseExpr restTokensInBrckt of
    Just (expr, (CloseRectToken  : restTokensBrckt))    -> Just (expr, restTokensBrckt)
    Just _                                              -> Nothing
    Nothing                                             -> Nothing

parseIntorBrcktExpr (VertBrcktToken : restTokensInBrckt) = case parseExpr restTokensInBrckt of
    Just (expr, (VertBrcktToken  : restTokensBrckt))    -> Just (expr, restTokensBrckt)
    Just _                                              -> Nothing
    Nothing                                             -> Nothing

parseIntorBrcktExpr tokens = Nothing

parseExpr :: [InputSymbols] -> Maybe (Expr, [InputSymbols])
parseExpr tokens = case parseIntorBrcktExpr tokens of
    Just (expr1, (PlusToken  : restTokens1)) -> case parseExpr restTokens1 of
        Just (expr2, restTokens2) -> Just (BinaryLiteral AddOperator  expr1 expr2, restTokens2)
        Nothing                   -> error "\x1b[31mParse error: \x1b[0mCouldn't parse binary operator +"
    Just (expr1, (MinusToken : restTokens1)) -> case parseExpr restTokens1 of
        Just (expr2, restTokens2) -> Just (BinaryLiteral SubOperator  expr1 expr2, restTokens2)
        Nothing                   -> error "\x1b[31mParse error: \x1b[0mCouldn't parse binary operator -"
    Just (expr1, (MultToken  : restTokens1)) -> case parseExpr restTokens1 of
        Just (expr2, restTokens2) -> Just (BinaryLiteral MultOperator expr1 expr2, restTokens2)
        Nothing                   -> error "\x1b[31mParse error: \x1b[0mCouldn't parse binary operator *"
    Just (expr1, (DivToken   : restTokens1)) -> case parseExpr restTokens1 of
        Just (expr2, restTokens2) -> Just (BinaryLiteral DivOperator  expr1 expr2, restTokens2)
        Nothing                   -> error "\x1b[31mParse error: \x1b[0mCouldn't parse binary operator /"
    Just (expr1, (PowToken   : restTokens1)) -> case parseExpr restTokens1 of
        Just (expr2, restTokens2) -> Just (BinaryLiteral PowOperator  expr1 expr2, restTokens2)
        Nothing                   -> error "\x1b[31mParse error: \x1b[0mCouldn't parse binary operator ^"
    result -> result

parse :: [InputSymbols] -> Expr
parse tokens = case parseExpr tokens of
    Just (expr, []) -> expr    
    _               -> error "Couldn't parse input"

eval :: Expr -> Int
eval (IntLiteral x) = x
eval (BinaryLiteral AddOperator  expr1 expr2) = eval expr1   +   eval expr2
eval (BinaryLiteral SubOperator  expr1 expr2) = eval expr1   -   eval expr2
eval (BinaryLiteral MultOperator expr1 expr2) = eval expr1   *   eval expr2
eval (BinaryLiteral DivOperator  expr1 expr2) = eval expr1 `div` eval expr2
eval (BinaryLiteral PowOperator  expr1 expr2) = eval expr1   ^   eval expr2

calculate :: String -> Int
calculate x = eval $ parse $ lexer x