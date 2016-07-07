-- | This module provides a tokenizing parser.
--
-- You provide your own token types and instantiate the 'ParserToken' class, defining the
-- 'nextToken' function, which must produce all possible tokens from the start of the input string
-- using the 'Tokenizer' monad.
--
-- The 'TokenParser' monad is mostly a wrapper around the 'Tokenizer' monad, but it is separate to
-- emphasize the distrinction between the tokenizing step and the parsing step. It is possible to
-- evaluate a 'Tokenizer' monad within the 'TokenParser' simply by wrapping it with the
-- 'TokenParser' coonstructor, but there should be no need to do this.
--
-- Defining the @'nextToken'::'Tokenizer'@ function can be done with combinators such as 'string',
-- 'char', 'satisfy', and 'takeWhile', which all operate similar to the API functions for Attoparsec
-- parsers. Once a tokenizer succeds, it should return the @token@ value, and this value, which will
-- store it into the 'ParserState' where it will be analyzed by the parser combinators during
-- parsing.
--
-- The 'ParserState' does keep track of 'LineNumber's and 'ColumnNumber's, but it is the
-- responsibility of the instantiator of the 'nextToken' method to increment column numbers. This
-- can be done with the 'incColumn' and 'incColumnLen' functions. Line numbers are tracked by use of
-- the 'countLine' function. If line and column numbers are not important, simply not using these
-- functions can help to improve efficiency of the parser.
--
-- The 'token' function is used to analyze the current token, which takes a continuation function.
-- If the continuation backtracks (by calling 'Control.Monad.mzero', or equivalently
-- 'Control.Applicative.empty'), then the current token waiting in the 'ParserState' is not
-- modified. However if the continuation passed to 'token' returns a value successfully, the
-- 'nextToken' function is called and the next token is tokenized from the input stream.
--
-- The 'ParserState' is strict, and state values are never pushed onto the stack as parsers
-- evaluate, unless you should make use of the 'Control.Monad.State.Class.get' function. This makes
-- for efficient parsers.
module Data.TokenParser
  ( -- * Preliminaries
    LineNumber, ColumnNumber, isHorizontalSpace, isEndOfLine,
    CanParse(..), ParserToken(..), yieldToken,
    module Control.Applicative,
    module Control.Monad,
    -- * The Tokenizer and TokenParser monads
    ParserState, Tokenizer, TokenParser, Result(..), Stream(..), parserStream, endOfStream,
    runTokenizer, runParser, feed, runParserOnly, toParser, toTokenizer,
    -- * Primitive Tokenizer Combinators
    string, char, anyChar, satisfy, takeWhile, skipWhile, endOfLine, endOfInput,
    incColumn, incColumnLen, countLine, pushBack,
    -- * Tokenizer Helpers
    TextCollector, liftCtr, collect, runCollector,
    -- * Parser Combinators
    token, isToken, skipTokens, sepBy, sepBy1,
    currentLine, currentColumn, currentToken, (<?>), typeableParser,
    -- ** Common Abstract Syntax Tree Data Types
    -- $CommonASTTypes
    -- * Optional
    Optional(..), parseOptionalWith, parseOptional, foldOptional, ifNot,
    -- * Many1
    Many1(..), many1Length, many1ToList, parseMany1With, parseMany1, sepByMany1, many1_,
    -- * Debugging
    showResult, showParserState, getASTPath, fmapStream, ptrace
  )
  where

import           Prelude                    hiding ((.), id, takeWhile, print)

import           Control.Applicative
import           Control.Arrow
import           Control.Category
import           Control.Monad
import           Control.Monad.State.Strict
import           Control.Monad.Except

--import qualified Data.Attoparsec.Text       as Par
import           Data.Char
import           Data.Foldable              hiding (msum, forM_, sequence_)
import           Data.List                  (intercalate, intersperse)
import           Data.Monoid                hiding ((<>))
import           Data.Semigroup
import           Data.String
import qualified Data.Text                  as Strict
import           Data.Typeable
import qualified Data.Vector                as Vec

import           Debug.Trace

----------------------------------------------------------------------------------------------------

type LineNumber   = Int
type ColumnNumber = Int

-- | A predicate matching either the @' '@ or the @'\t'@ characters.
isHorizontalSpace :: Char -> Bool
isHorizontalSpace c = c==' ' || c=='\t'

-- | A predicate matching either the @'\n'@ or the @'\r'@ characters.
isEndOfLine :: Char -> Bool
isEndOfLine c = c=='\n' || c=='\r'

----------------------------------------------------------------------------------------------------

-- | Kind of like the 'Prelude.Read' class, but defines token parsers, rather that 'Prelude.String'
-- parsers. Every data type of your abstract syntax tree should instantiate this class.
class CanParse token a | a -> token where { parse :: TokenParser token a; }

-- | This class allows you to declare an arbitrary @token@ data type as a @token@ type of a
-- 'TokenParser', that is @token@ is a kind of parser token for a 'TokenParser'.  The name of this
-- class should be "Is a type of TokenParser Token", but it is abbreviated to @ParserToken@.
class ParserToken token where
  -- | The 'nextToken' method should drop the current 'parserNextToken' item in the 'ParserState',
  -- and then parse the next @token@ from the input text stream. _This_function_must_call_the_
  -- 'yieldToken' _function_at_least_once_if_any_characters_from_the_input_stream_are_consumed_.
  nextToken :: Tokenizer token token
  -- | The initial token to be used to instantiate the 'ParserState'.
  initToken :: token

-- | This function will set the current token in the 'ParserState' and return it.
yieldToken :: token -> Tokenizer token token
yieldToken tok = state $ \st -> seq tok (tok, st{ parserNextToken=tok })

----------------------------------------------------------------------------------------------------

data ParserState token
  = ParserState
    { parserLineNumber   :: !LineNumber
    , parserColumnNumber :: !ColumnNumber
    , parserType         :: ![TypeRep]
    , parserStream       :: !Stream
    , parserNextToken    :: !token
    }

showParserState
  :: IsString txt
  => (Int -> txt)
  -> (TypeRep -> txt)
  -> (Strict.Text -> txt)
  -> (token -> txt)
  -> (txt -> [txt] -> txt)
  -> (txt -> txt -> txt)
  -> txt -> ParserState token -> txt
showParserState showInt showTyp showTxt showTok intercalate (<>) nl p =
  "line=" <> showInt (parserLineNumber p) <> ", column=" <> showInt (parserColumnNumber p) <> nl <>
  "stack=[" <> intercalate ", " (showTyp <$> parserType p) <> "]" <> nl <> "input=" <>
  ( case parserStream p of
      EndOfStream    -> "EndOfStream"
      AwaitingInput  -> "AwaitingInput"
      InputPart  txt -> showTxt (Strict.take 20 txt) <>
        (if Strict.length txt > 20 then "..." else "")
  ) <> nl <> "token=" <> showTok (parserNextToken p)

instance Show token => Show (ParserState token) where
  show = showParserState show show show show intercalate (++) "\n"

----------------------------------------------------------------------------------------------------

data Result token a
  = Done      !a
  | Partial   !(TokenParser token a)
  | Backtrack ![TypeRep]
  | Error     ![TypeRep] !Strict.Text

instance Functor (Result token) where
  fmap f a = case a of
    Done      a   -> Done $! f a
    Partial   a   -> Partial $! fmap f a
    Backtrack a   -> Backtrack a
    Error     a b -> Error a b

instance (Show a, Show token) => Show (Result token a) where
  show = showResult show show show Strict.unpack intercalate (++)

showResult
  :: IsString txt
  => (a -> txt) -- convert the result to a textual thing
  -> (TypeRep -> txt) -- convert a 'Data.Typeable.TypeRep' to a textual thing
  -> (ParserState token -> txt) -- convert a 'ParserState' to a textual thing
  -> (Strict.Text -> txt) -- convert a 'Data.Text.Text' to a textual thing
  -> (txt -> [txt] -> txt) -- perform an intercalation
  -> (txt -> txt -> txt) -- join two textual things together
  -> Result token a -> txt
showResult show showTyp showSt unpack intercalate (<>) a = case a of
  Done      a     -> "parser success: "<>show a
  Partial   _     -> "...needs more input..."
  Backtrack t     -> "backtrack [" <> intercalate ", " (showTyp<$>t) <> "]"
  Error     t msg -> "parser error [" <> intercalate ", " (showTyp<$>t) <> "] " <> unpack msg

-- | Pass an empty string to indicate to the parser that the end of the input stream has been
-- reached. This function behaves as 'Prelude.id' if the 'Result' is not 'Partial'.
feed :: Result token a -> ParserState token -> Strict.Text -> (Result token a, ParserState token)
feed a st txt = case a of
  Partial (TokenParser (Tokenizer f)) -> runState f $
    st{ parserStream = if Strict.null txt then EndOfStream else InputPart txt } 
  _ -> (a, st)

----------------------------------------------------------------------------------------------------

-- | This data type can be retrieved from a 'ParserState' using the 'parserStream' function. It
-- indicates the state of the input stream that is being analyzed by the 'Tokenizer'.
data Stream
  = EndOfStream
  | AwaitingInput
  | InputPart !Strict.Text
  deriving (Eq, Ord, Show)

endOfStream :: Stream -> Bool
endOfStream a = case a of { EndOfStream -> True; _ -> False; }

----------------------------------------------------------------------------------------------------

-- | A tokenizing monad that wraps the "Data.Attoparsec" parser extended with line and column
-- information. Tokens are read one at a time with minimal look-ahead, the 'ParserState' only stores
-- one token at a time, once the token is removed, the next is parsed and cannot be returned.
newtype Tokenizer token a = Tokenizer { _runTokenizer :: State (ParserState token) (Result token a) }

instance Functor (Tokenizer token) where
  fmap f (Tokenizer a) = Tokenizer $ a >>= return . fmap f

instance Applicative (Tokenizer token) where { (<*>) = ap; pure = return; }
instance Alternative (Tokenizer token) where { empty = mzero; (<|>) = mplus; }

instance Monad (Tokenizer token) where
  return = Tokenizer . return . Done
  (Tokenizer a) >>= f = Tokenizer $ a >>= \a -> case a of
    Done      a   -> _runTokenizer $ f a
    Partial   a   -> return $ Partial $ a >>= TokenParser . f
    Backtrack a   -> Backtrack <$> gets parserType
    Error     a b -> return $ Error a b
  fail = throwError . Strict.pack

instance MonadPlus (Tokenizer token) where
  mzero = Tokenizer $ Backtrack <$> gets parserType
  mplus (Tokenizer a) (Tokenizer b) =
    Tokenizer $ a >>= \a -> case a of
      Backtrack             _   -> b
      Partial  (TokenParser a)  -> return $ Partial $ TokenParser $ mplus a $ Tokenizer b
      Done                  a   -> return $ Done a
      Error                 a b -> return $ Error a b

instance MonadState (ParserState token) (Tokenizer token) where
  state = Tokenizer . fmap Done . state

instance MonadError Strict.Text (Tokenizer token) where
  throwError msg = Tokenizer $ gets parserType >>= \t -> return $ Error t msg
  catchError (Tokenizer try) catch = Tokenizer $ try >>= \a -> case a of
    Error _ msg -> (\ (Tokenizer a) -> a) $ catch msg
    _           -> return a

instance Monoid a => Monoid (Tokenizer token a) where
  mempty = return mempty
  mappend a b = liftM2 mappend a b

instance IsString (Tokenizer token Strict.Text) where
  fromString = string . Strict.pack

-- | Begin evaluation of a 'Tokenizer' monad, producing a 'Result', which may be continuation
-- requiring more input. If the 'Result' is 'Partial', feed more input into the 'ParserState' using
-- the 'feed' function.
runTokenizer :: Tokenizer token a -> ParserState token -> (Result token a, ParserState token)
runTokenizer (Tokenizer f) = runState f

-- | Apply a 'Data.Text.Text' transformation to the 'Stream'. Since you cannot modify the
-- contents of the 'ParserState', this function is only useful for modifying a copy of the
-- 'Stream' retrieved by the 'parserStream' function.
fmapStream :: (Strict.Text -> Strict.Text) -> Stream -> Stream
fmapStream f a = case a of
  EndOfStream   -> EndOfStream
  AwaitingInput -> AwaitingInput
  InputPart txt -> InputPart $ f txt

----------------------------------------------------------------------------------------------------

newtype TokenParser token a = TokenParser { toTokenizer :: Tokenizer token a }
  deriving (Functor, Typeable)

instance Monad (TokenParser token) where 
  return = TokenParser . return
  (TokenParser a) >>= f = TokenParser $ a >>= toTokenizer . f
  fail = TokenParser . fail

instance MonadPlus (TokenParser token) where
  mzero = TokenParser mzero
  mplus (TokenParser a) (TokenParser b) = TokenParser (mplus a b)

instance Applicative (TokenParser token) where { pure=return; (<*>)=ap; }

instance Alternative (TokenParser token) where { empty=mzero; (<|>)=mplus; }

instance IsString (TokenParser token Strict.Text) where { fromString = TokenParser . fromString; }

instance MonadError Strict.Text (TokenParser token) where
  throwError = TokenParser . throwError
  catchError (TokenParser try) catch = TokenParser $ catchError try ((\ (TokenParser a) -> a) . catch)

instance MonadState (ParserState token) (TokenParser token) where
  state = TokenParser . state

instance Monoid a => Monoid (TokenParser token a) where
  mempty = return mempty
  mappend a b = liftM2 mappend a b

_initParser :: ParserToken token => Strict.Text -> ParserState token
_initParser txt =
  ParserState
  { parserLineNumber   = 1
  , parserColumnNumber = 1
  , parserType         = []
  , parserNextToken    = initToken
  , parserStream       = InputPart txt
  }

toParser :: Tokenizer token a -> TokenParser token a
toParser = TokenParser

-- | Begin running the 'TokenParser' monad, producing a possibly 'Partial' 'Result' value. If the
-- result is 'Partial', use the 'feed' function to continue evaluation. Be aware that the initial
-- token is modified by 'nextToken', you must explicitly call 'nextToken' after evaluating a
-- 'TokenParser' combinator that properly consumes the initial token.
runParser :: ParserToken token => TokenParser token a -> Strict.Text -> (Result token a, ParserState token)
runParser par = runState (_runTokenizer $ toTokenizer par) . _initParser

-- | Run a 'TokenParser' using a single string as the input stream, and force the end of the input
-- stream
runParserOnly :: ParserToken token => TokenParser token a -> Strict.Text -> Either String a
runParserOnly par str = case runParser par str of
  (Done      a, _ ) -> Right a
  (Backtrack t, _ ) -> backtrack t
  (Error t msg, _ ) -> err t msg
  (Partial   f, st) ->
    case runState (_runTokenizer $ toTokenizer f) $ st{ parserStream=EndOfStream } of
      (Done      a, _ ) -> Right a
      (Backtrack t, _ ) -> backtrack t
      (Error t msg, _ ) -> err t msg
      (Partial   _, st) -> Left $ show (parserType st) ++ " parse failed, require more input"
  where
    backtrack t     = Left $ show t ++ " parse failed (mzero)"
    err       t msg = Left $ show t ++ ' ' : Strict.unpack msg

_liftPar :: (a -> a -> a) -> (Strict.Text -> (Optional a, Strict.Text)) -> Tokenizer token a
_liftPar (<>) take = Tokenizer $ state $ \st ->
  let st'      = st{ parserStream=AwaitingInput }
      next rem = st'{ parserStream=InputPart rem }
      pfail    = (Backtrack $! parserType st, st)
  in  case parserStream st of
        EndOfStream                     -> pfail
        AwaitingInput                   -> (Partial $ TokenParser $ _liftPar (<>) take, st)
        InputPart txt | Strict.null txt -> (Partial $ TokenParser $ _liftPar (<>) take, st')
        InputPart txt                   -> case take txt of
          (Defined a, rem) | Strict.null rem ->
            (Partial $! TokenParser $! mplus ((a <>) <$> _liftPar (<>) take) (return a), next rem)
          (Defined a, rem)                   -> (Done a, next rem)
          (Ignored  , rem)                   -> pfail

string :: Strict.Text -> Tokenizer token Strict.Text
string matchText = saveState $ loop matchText where
  saveState f = get >>= \top -> mplus f $ get >>= \st ->
    put (top{ parserStream = parserStream st }) >> mzero
  loop txt = if Strict.null txt then return matchText else Tokenizer $ state $ \st ->
    let st'   = st{ parserStream=AwaitingInput }
        pfail = (Backtrack $! parserType st, st)
    in  case parserStream st of
          AwaitingInput   -> (Partial $ TokenParser $ loop txt, st)
          EndOfStream     -> pfail
          InputPart str | Strict.length txt >= Strict.length str ->
            if Strict.isPrefixOf str txt then
              ( Partial $ TokenParser $ mplus (loop $ Strict.drop (Strict.length str) txt)
                  (pushBack str >> mzero)
              , st'
              )
            else pfail
          InputPart str | Strict.isPrefixOf txt str ->
            ( Done matchText
            , st{ parserStream = InputPart $ Strict.drop (Strict.length txt) str }
            )
          _ -> pfail

-- | Increment the current column number in the parser state.
incColumn :: Int -> Tokenizer token ()
incColumn i = modify $ \st -> st{ parserColumnNumber = parserColumnNumber st + 1 }

-- | Increment the current 'LineNumber' and reset the current 'ColumnNumber'
countLine :: Tokenizer token ()
countLine = modify $ \st ->
  st{ parserColumnNumber = 1, parserLineNumber = parserLineNumber st + 1 }

-- | Increment the current column number in the parser state by the length of
-- the given string.
incColumnLen :: Strict.Text -> Tokenizer token Strict.Text
incColumnLen txt = incColumn (Strict.length txt) >> return txt

-- | Take a minimum number (@i@) of characters that match the given predicate from the front of the
-- input string. If the minimum number given is zero, this 'Tokenizer' will never backtrack, it
-- effectively becomes an optional character 'Tokenizer'. The minimum number of characters given to
-- this function determins the minimum number of characters that must be buffered by the tokenizer.
-- Passing large minimum values of (@i@) result in a lot of backtracking, which requires pushing
-- back up to @i-1@ characters into the buffer. Avoid using values of @i@ larger than 1.
takeWhile :: Int -> (Char -> Bool) -> Tokenizer token Strict.Text
takeWhile i f = do
  let par = _liftPar (<>) $!
        first (\txt -> if Strict.length txt > 0 then Defined txt else Ignored) . Strict.span f
  case i of
    i | i <= 0 -> par <|> return mempty
    i | i == 1 -> Strict.cons <$> satisfy f <*> (par <|> return mempty)
    i          -> par >>= \txt -> if Strict.length txt >= i then return txt else do
      modify $ \st ->
        st{ parserStream = case parserStream st of
              InputPart   rem -> InputPart $! txt <> rem
              _               -> InputPart txt
          }
      mzero

-- | Like 'takeWhile' but does not copy the characters, which results in improved efficiency.
skipWhile :: Int -> (Char -> Bool) -> Tokenizer token ()
skipWhile i f = case i of
  i | i <= 0 -> _liftPar const $ \txt ->
    if Strict.null txt || not (f $! Strict.head txt)
    then (Ignored, txt)
    else (Defined (), Strict.dropWhile f txt)
  i | i == 1 -> satisfy f >> skipWhile 0 f
  i          -> void $ takeWhile i f

-- | Match a single character from the front of the input stream, or backtrack if the charcter does
-- not match.
char :: Char -> Tokenizer token Char
char c = satisfy (== c)

-- | Take any character from the front of the input stream. This combinator will only backtrack if
-- the end of the input stream is reached.
anyChar :: Tokenizer token Char
anyChar = _liftPar const $ \txt ->
  if Strict.null txt then (Ignored, txt) else (Defined $! Strict.head txt, Strict.tail txt)

-- | Take a single character from the front of the input stream and succeed if the given predicate
-- evaluates to @True@, otherwise backtrack.
satisfy :: (Char -> Bool) -> Tokenizer token Char
satisfy f = _liftPar const $ \txt -> let c = Strict.head txt in
  if Strict.null txt || not (f c) then (Ignored, txt) else (Defined c, Strict.tail txt)

-- | Succeeds if the current input matches one of @'string' "\r\n"@, a @'string' "\n\r"@, or a
-- @'char' \'n\'@ . The line number is not incremented.
endOfLine :: Tokenizer token ()
endOfLine = void (string "\r\n") <|> void (string "\n\r") <|> void (char '\n')

-- | Succeeds if the end of the input stream has been reached.
endOfInput :: Tokenizer token ()
endOfInput = (== EndOfStream) <$> gets parserStream >>= guard

-- | Force a string into the front of the tokenizer input. This function should not be used;
-- consider it a "code smell" if you ever use this function, and try to re-design your 'Tokenizer'
-- to avoid using this function.
pushBack :: Strict.Text -> Tokenizer token ()
pushBack txt = modify $ \st ->
  st{ parserStream = case parserStream st of
        InputPart   i -> InputPart $! txt <> i
        EndOfStream   -> InputPart txt
        AwaitingInput -> InputPart txt
    }

----------------------------------------------------------------------------------------------------

-- | There may be times when you wish to evaluae a 'Tokenizer' that collects all of the text it
-- tokenizes into a buffer, for example when defining a looping 'Tokenizer' for tokenizing string
-- literals. This simple 'Control.Monad.State.StateT' data type along with the 'collect' and
-- 'runCollector' functions allows you to do this.
type TextCollector token a = StateT Strict.Text (Tokenizer token) a

-- | Lift a 'Tokenizer' monad into the 'TextCollector' monad.
liftCtr :: Tokenizer token a -> TextCollector token a
liftCtr f = StateT $ liftA2 (,) f . return

-- | Evaluate a 'Tokenizer' that returns a 'Data.Text.Text' value (for example, 'takeWhile'), and
-- store the result into the buffer while also returning the tokenized text.
collect :: Tokenizer token Strict.Text -> TextCollector token Strict.Text
collect f = liftCtr f >>= \txt -> state $ \st -> (txt, st<>txt)

-- | This function is equivalent to 'Control.Monad.State.execStateT', provided here so that you do
-- not need to import the "Control.Monad.State" module.
runCollector :: TextCollector token a -> Tokenizer token Strict.Text
runCollector f = execStateT f mempty

----------------------------------------------------------------------------------------------------

-- | Analyze the current token. Parse look-ahead scans as far as one single token, so you may only
-- analyze one token. If the continuation passed to this function evaluates to
-- 'Control.Applicative.empty' or 'Control.Monad.mzero' or 'Control.Monad.fail', then the token
-- remains unchanged. If the continuation function succeeds, the next token is automatically
-- shifted.
token :: ParserToken token => (token -> TokenParser token a) -> TokenParser token a
token par = (TokenParser (gets parserNextToken) >>= par) <* TokenParser nextToken

-- | Test if the current token is equal to the given token using the 'Prelude.==' operator.
isToken :: (Eq token, ParserToken token) => token -> TokenParser token ()
isToken tok = token $ guard . (== tok)

-- | Starting from the current token, skip all tokens that match the given predicate.
skipTokens :: ParserToken token => (token -> Bool) -> TokenParser token ()
skipTokens p = do
  more <- p <$> gets parserNextToken
  when more $ TokenParser nextToken >> skipTokens p

_sepBy_loop :: (Monad p, MonadPlus p) => p a -> p x -> [a] -> p [a]
_sepBy_loop p sep = loop where
  loop ax = mplus (sep >> mplus p (fail "") >>= loop . (ax ++) . (: [])) (return ax)

-- | Evaluate a 'Tokenizer' or 'TokenParser' where elements of type @a@ are separated by a given
-- parser of type @x@. The @x@ elements are dropped and the @a@ elements are returned. This function
-- never backtracks, it returns at least an empty list if the @a@ parser or tokenizer fails to match
-- any input.
sepBy :: (Monad p, MonadPlus p) => p a -> p x -> p [a]
sepBy p sep = mplus (p >>= _sepBy_loop p sep . (: [])) $ return []

-- | Like 'sepBy' but fails if at least one element of type @a@ cannot be parsed or tokenized.
sepBy1 :: (Monad p, MonadPlus p) => p a -> p x -> p (a, [a])
sepBy1 p sep = liftM2 (,) p $ _sepBy_loop p sep []

-- | Get the current 'LineNumber'. This only returns an accurate 'LineNumber' value if your
-- 'Tokenizer' has properly been accounting for newlines using the 'countLine' function.
currentLine :: TokenParser token LineNumber
currentLine = gets parserLineNumber

-- | Get the current 'ColumnNumber'. This only returns an accurate 'ColumnNumber' value if your
-- 'Tokenizer' has properly been accounting for column numbers using the 'incColumn' or
-- 'incColumnLen' functions.
currentColumn :: TokenParser token LineNumber
currentColumn = TokenParser $ gets parserColumnNumber

-- | Get the current token from the 'ParserState'.
currentToken :: TokenParser token token
currentToken = gets parserNextToken

(<?>) :: TokenParser token a -> Strict.Text -> TokenParser token a
(<?>) (TokenParser (Tokenizer parser)) msg = TokenParser $ Tokenizer $ parser >>= \a -> return $ case a of
  Backtrack t -> Error t msg
  _ -> a
infix 0 <?>

-- | This function should be used to prefix the parser for a node of your abstract syntax tree if
-- you want more descriptive error messages. The 'Data.Typeable.TypeRep' for the type @a@ of the
-- given 'TokenParser' will be stored onto a stack internal to the 'ParserState' allowing you to see
-- full path to the node of the abstract syntax tree. This information becomes available when a
-- parser exception is thrown.
typeableParser :: (Typeable a, Typeable token) => TokenParser token a -> TokenParser token a
typeableParser par = TokenParser $ Tokenizer $ state $ let (TokenParser (Tokenizer f)) = par in
  runState (f <* modify (\st -> st{ parserType = tail $ parserType st })) .
    (\st -> st{ parserType = head (tail $ typeRepArgs $ typeOf par) : parserType st })

----------------------------------------------------------------------------------------------------

-- $CommonASTTypes
-- There are two data types, 'Optional' and 'Many1' which are so commonly seen in Abstract Syntax
-- Trees (ASTs), that it is useful to have them pre-defined in the parser module so that they may be
-- incorporated into your own AST. These data types do not instantiate the 'CanParse' class, because
-- you may wish to provide different 'parse' functions for different specific applications of the
-- polymorphic types, e.g. you may want the 'parse' function for @'Many1' MyInt@ and
-- @'Many1' MyString@ to use the 'sepBy1' combinator.

----------------------------------------------------------------------------------------------------

-- | This data type is a strict version of the 'Data.Maybe.Maybe' data type, however it is defined
-- specifically for use in abstract syntax trees.
data Optional a = Ignored | Defined !a
  deriving (Eq, Ord, Show, Read, Typeable, Functor)

instance Semigroup a => Semigroup (Optional a) where
  (<>) a b = case a of
    Ignored   -> b
    Defined a -> case b of
      Ignored   -> Defined a
      Defined b -> Defined $! a <> b

instance Semigroup a => Monoid (Optional a) where
  mempty = Ignored
  mappend = (<>)

-- | In Parsec and Attoparsec, this function is called @option@, it tries to evaluate the
-- 'Tokenizer' or 'TokenParser', but returns a given default value if evaluation backtracks. This
-- function works well with infix notation.
ifNot :: (Applicative p, Alternative p) => a -> p a -> p a
ifNot a p = p <|> pure a

parseOptionalWith :: TokenParser token a -> TokenParser token (Optional a)
parseOptionalWith parse = ifNot Ignored $ fmap Defined parse

parseOptional :: CanParse token a => TokenParser token (Optional a)
parseOptional = parseOptionalWith parse

-- | Similar to the 'Data.Maybe.maybe' function, folds over the 'Optional' data type, providing a
-- default constant value when the 'Optional' is 'Ignored', or else providing the 'Optional' value
-- when it is 'Defined'.
foldOptional :: b -> (a -> b) -> Optional a -> b
foldOptional b f a = case a of
  Ignored   -> b
  Defined a -> f a

----------------------------------------------------------------------------------------------------

-- | This data type is a strict version of the 'Data.List.NonEmpty.NonEmpty' data type from the
-- @semigroups@ package, however it is defined specifically for use in abstract syntax trees.
data Many1 a = Many1 !a !(Vec.Vector a)
  deriving (Eq, Ord, Show, Read, Typeable)

instance Functor Many1 where
  fmap f (Many1 a b) = Many1 (f a) (fmap f b)

instance Semigroup (Many1 a) where
  (Many1 a1 v1) <> (Many1 a2 v2) = Many1 a1 $ mappend v1 $ Vec.cons a2 v2

-- | Return the number of items stored in the given 'Many1' data structure.
many1Length :: Many1 a -> Int
many1Length (Many1 _ a) = 1 + Vec.length a

-- | Extract all items from the 'Many1' data structure into a list data structure.
many1ToList :: Many1 a -> [a]
many1ToList (Many1 a ax) = a : Vec.toList ax

parseMany1With :: TokenParser token a -> TokenParser token (Many1 a)
parseMany1With f = Many1 <$> f <*> (Vec.fromList <$> many f)

parseMany1 :: CanParse token a => TokenParser token (Many1 a)
parseMany1 = parseMany1With parse

-- | Like 'sepBy1' but evaluates to a 'Many1' data structure.
sepByMany1 :: TokenParser token a -> TokenParser token ig -> TokenParser token (Many1 a)
sepByMany1 f sep = Many1 <$> f <*> (Vec.fromList <$> many (sep >> f))

-- | Evaluate a 'TokenParser' or 'Tokenizer' one or more times, backtrack if at least one evaluation
-- does not backtrack, do not save results. This can be used to skip over items not needed, like
-- whitespace. This function works with any 'Control.Applicative.Applicative' and
-- 'Control.Applicative.Alternative' instance, but is intended to be used with 'Tokenizer's and
-- 'TokenParser's.
many1_ :: (Applicative m, Alternative m) => m a -> m ()
many1_ f = f *> sequenceA_ (repeat $ void f <|> pure ())

----------------------------------------------------------------------------------------------------

-- | Retrieve the path of the abstract syntax tree 
getASTPath :: TokenParser token [TypeRep]
getASTPath = gets parserType

-- | This function makes calls to the 'Debug.Trace.trace' function in the "Debug.Trace" module,
-- pretty-printing the 'ParserState' and whether or not the 'TokenParser' or 'Tokenizer' succeded or
-- backtracked.
ptrace
  :: (Functor m, Applicative m, Alternative m, Monad m, MonadPlus m,
      MonadState (ParserState token) m, Show token)
  => Strict.Text -> m a -> m a
ptrace msg f = do
  let next   = Strict.pack . show . fmapStream (Strict.take 4) <$> gets parserStream
  let gettok = Strict.pack . show <$> gets parserNextToken
  tok <- gettok
  x   <- next
  let un  = Strict.unpack
  let spc = Strict.replicate (max 1 $! 40 - Strict.length msg) " "
  trace    (un $! "evaluate "<>msg<>spc<>x<>" "<>tok) (optional f) >>= maybe
    (do tok <- gettok
        x   <- next
        trace (un $! "   mzero "<>msg<>spc<>x<>" "<>tok) empty
    )
    (\a -> do
        tok <- gettok
        x   <- next
        trace (un $! "  return "<>msg<>spc<>x<>" "<>tok) $ return a
    )

