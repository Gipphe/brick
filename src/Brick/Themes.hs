{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Support for representing attribute themes and loading and saving
-- theme customizations in YAML files.
--
-- Customization files are YAML files with two optional top-level keys:
-- @"default"@ and @"other"@.
--
-- The @"default"@ key's value is a mapping with three optional fields:
--
--  * @"fg"@ - a color specification
--  * @"bg"@ - a color specification
--  * @"style"@ - a style specification
--
-- A color specification can be any of the strings @black@, @red@,
-- @green@, @yellow@, @blue@, @magenta@, @cyan@, @white@, @brightBlack@,
-- @brightRed@, @brightGreen@, @brightYellow@, @brightBlue@,
-- @brightMagenta@, @brightCyan@, @brightWhite@, or @default@.
--
-- We also support color specifications in the common hex format @#RRGGBB@, but
-- note that this specification is lossy: terminals can only display 256 colors,
-- but hex codes can specify @256^3 = 16777216@ colors.
--
-- A style specification can be either one of the following values
-- (without quotes) or a comma-delimited list of one or more of the
-- following values (e.g. @"[bold,underline]"@) indicating that all
-- of the specified styles be used. Valid styles are @standout@,
-- @underline@, @reverseVideo@, @blink@, @dim@, @italic@,
-- @strikethrough@, and @bold@.
--
-- The @other@ key's value is a mapping from attribute names to their
-- @fg@, @bg@, and @style@ fields. Attribute names with multiple
-- components (e.g. @attr1 <> attr2@) are represented by joining the
-- components with a dot. For example, the attribute name
-- @attrName "list" <> attrName "selected"@ is represented as the key
-- @"list.selected"@.
--
-- Any color or style specifications omitted from the file mean that
-- those attribute or style settings will use the theme's default value
-- instead.
--
-- Example customization file:
--
-- > default:
-- >   fg: red
-- >   style: bold
-- >
-- > other:
-- >   list.selected:
-- >     fg: brightWhite
-- >     bg: blue
-- >     style: "[bold,underline]"
module Brick.Themes
  ( CustomAttr(..)
  , customFgL
  , customBgL
  , customStyleL

  , Theme(..)
  , newTheme
  , themeDefaultAttrL
  , themeDefaultMappingL
  , themeCustomMappingL
  , themeCustomDefaultAttrL

  , ThemeDocumentation(..)
  , themeDescriptionsL

  , themeToAttrMap
  , applyCustomizations
  , loadCustomizations
  , saveCustomizations
  , saveTheme
  )
where

import GHC.Generics (Generic)
import Graphics.Vty hiding ((<|>))
import Control.DeepSeq
import Control.Monad (forM, join)
import Control.Applicative ((<|>))
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Map as M
import qualified Data.Semigroup as Sem
import Data.Tuple (swap)
import Data.List (intercalate)
import Data.Bits ((.|.), (.&.))
import Data.Maybe (fromMaybe, isNothing, catMaybes, mapMaybe)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import qualified Data.Foldable as F

import qualified Data.Yaml as Yaml
import Data.Yaml ((.=), object, Value, withObject, (.:?))
import Data.Aeson.Key (fromText)
import Data.Aeson.Types (parseEither, Parser)

import Brick.AttrMap (AttrMap, AttrName, attrMap, attrNameComponents)
import Brick.Types.TH (suffixLenses)

import Text.Printf

-- | An attribute customization can specify which aspects of an
-- attribute to customize.
data CustomAttr =
    CustomAttr { customFg    :: Maybe (MaybeDefault Color)
               -- ^ The customized foreground, if any.
               , customBg    :: Maybe (MaybeDefault Color)
               -- ^ The customized background, if any.
               , customStyle :: Maybe Style
               -- ^ The customized style, if any.
               }
               deriving (Eq, Read, Show, Generic, NFData)

instance Sem.Semigroup CustomAttr where
    a <> b =
        CustomAttr { customFg    = customFg a    <|> customFg b
                   , customBg    = customBg a    <|> customBg b
                   , customStyle = customStyle a <|> customStyle b
                   }

instance Monoid CustomAttr where
    mempty = CustomAttr Nothing Nothing Nothing
    mappend = (Sem.<>)

-- | Documentation for a theme's attributes.
data ThemeDocumentation =
    ThemeDocumentation { themeDescriptions :: M.Map AttrName T.Text
                       -- ^ The per-attribute documentation for a theme
                       -- so e.g. documentation for theme customization
                       -- can be generated mechanically.
                       }
                       deriving (Eq, Read, Show, Generic, NFData)

-- | A theme provides a set of default attribute mappings, a default
-- attribute, and a set of customizations for the default mapping
-- and default attribute. The idea here is that the application will
-- always need to provide a complete specification of its attribute
-- mapping, but if the user wants to customize any aspect of that
-- default mapping, it can be contained here and then built into an
-- 'AttrMap' (see 'themeToAttrMap'). We keep the defaults separate
-- from customizations to permit users to serialize themes and their
-- customizations to, say, disk files.
data Theme =
    Theme { themeDefaultAttr :: Attr
          -- ^ The default attribute to use.
          , themeDefaultMapping :: M.Map AttrName Attr
          -- ^ The default attribute mapping to use.
          , themeCustomDefaultAttr :: Maybe CustomAttr
          -- ^ Customization for the theme's default attribute.
          , themeCustomMapping :: M.Map AttrName CustomAttr
          -- ^ Customizations for individual entries of the default
          -- mapping. Note that this will only affect entries in the
          -- default mapping; any attributes named here that are not
          -- present in the default mapping will not be considered.
          }
          deriving (Eq, Read, Show, Generic, NFData)

suffixLenses ''CustomAttr
suffixLenses ''Theme
suffixLenses ''ThemeDocumentation

-- | Create a new theme with the specified default attribute and
-- attribute mapping. The theme will have no customizations.
newTheme :: Attr -> [(AttrName, Attr)] -> Theme
newTheme def mapping =
    Theme { themeDefaultAttr       = def
          , themeDefaultMapping    = M.fromList mapping
          , themeCustomDefaultAttr = Nothing
          , themeCustomMapping     = mempty
          }

-- | Build an 'AttrMap' from a 'Theme'. This applies all customizations
-- in the returned 'AttrMap'.
themeToAttrMap :: Theme -> AttrMap
themeToAttrMap t =
    attrMap (customizeAttr (themeCustomDefaultAttr t) (themeDefaultAttr t)) customMap
    where
        customMap = F.foldr f [] (M.toList $ themeDefaultMapping t)
        f (aName, attr) mapping =
            let a' = customizeAttr (M.lookup aName (themeCustomMapping t)) attr
            in (aName, a'):mapping

customizeAttr :: Maybe CustomAttr -> Attr -> Attr
customizeAttr Nothing a = a
customizeAttr (Just c) a =
    let fg = fromMaybe (attrForeColor a) (customFg c)
        bg = fromMaybe (attrBackColor a) (customBg c)
        sty = maybe (attrStyle a) SetTo (customStyle c)
    in a { attrForeColor = fg
         , attrBackColor = bg
         , attrStyle = sty
         }

isNullCustomization :: CustomAttr -> Bool
isNullCustomization c =
    isNothing (customFg c) &&
    isNothing (customBg c) &&
    isNothing (customStyle c)

-- |  This function is lossy in the sense that we only internally support 240 colors but
-- the #RRGGBB format supports 16^3 colors.
parseColor :: T.Text -> Either String (MaybeDefault Color)
parseColor s =
    let stripped = T.strip $ T.toLower s
        normalize (t, c) = (T.toLower t, c)
    in if stripped == "default"
          then Right Default
          else case parseRGB stripped of
              Just c  -> Right (SetTo c)
              Nothing -> maybe (Left $ "Invalid color: " <> show stripped) (Right . SetTo) $
                             lookup stripped (normalize <$> swap <$> allColors)
  where
    parseRGB t = if T.head t /= '#'
                    then Nothing
                    else case mapMaybe readHex (T.chunksOf 2 (T.tail t)) of
                            [r,g,b] -> Just (rgbColor r g b)
                            _ -> Nothing

    readHex :: T.Text -> Maybe Int
    readHex t = either (const Nothing) (Just . fst) (T.hexadecimal t)

allColors :: [(Color, T.Text)]
allColors =
    [ (black, "black")
    , (red, "red")
    , (green, "green")
    , (yellow, "yellow")
    , (blue, "blue")
    , (magenta, "magenta")
    , (cyan, "cyan")
    , (white, "white")
    , (brightBlack, "brightBlack")
    , (brightRed, "brightRed")
    , (brightGreen, "brightGreen")
    , (brightYellow, "brightYellow")
    , (brightBlue, "brightBlue")
    , (brightMagenta, "brightMagenta")
    , (brightCyan, "brightCyan")
    , (brightWhite, "brightWhite")
    ]

allStyles :: [(T.Text, Style)]
allStyles =
    [ ("standout", standout)
    , ("underline", underline)
    , ("strikethrough", strikethrough)
    , ("reversevideo", reverseVideo)
    , ("blink", blink)
    , ("dim", dim)
    , ("bold", bold)
    , ("italic", italic)
    ]

parseStyle :: T.Text -> Either String Style
parseStyle s =
    let lookupStyle "" = Right Nothing
        lookupStyle n = case lookup n normalizedStyles of
            Just sty -> Right $ Just sty
            Nothing  -> Left $ T.unpack $ "Invalid style: " <> n
        stripped = T.strip $ T.toLower s
        normalize (n, a) = (T.toLower n, a)
        normalizedStyles = normalize <$> allStyles
        bracketed = "[" `T.isPrefixOf` stripped &&
                    "]" `T.isSuffixOf` stripped
        unbracketed = T.tail $ T.init stripped
        parseStyleList = do
            ss <- mapM lookupStyle $ T.strip <$> T.splitOn "," unbracketed
            return $ foldr (.|.) 0 $ catMaybes ss

    in if bracketed
       then parseStyleList
       else do
           result <- lookupStyle stripped
           case result of
               Nothing -> Left $ "Invalid style: " <> show stripped
               Just sty -> Right sty

parseCustomAttrYaml :: Value -> Parser (Maybe CustomAttr)
parseCustomAttrYaml = withObject "attribute customization" $ \obj -> do
    mFg  <- (obj .:? "fg")    :: Parser (Maybe T.Text)
    mBg  <- (obj .:? "bg")    :: Parser (Maybe T.Text)
    mSty <- (obj .:? "style") :: Parser (Maybe T.Text)
    fg  <- traverse (either fail return . parseColor) mFg
    bg  <- traverse (either fail return . parseColor) mBg
    sty <- traverse (either fail return . parseStyle) mSty
    let c = CustomAttr fg bg sty
    return $ if isNullCustomization c then Nothing else Just c

parseThemeYaml :: Theme -> Value -> Either String (Maybe CustomAttr, M.Map AttrName CustomAttr)
parseThemeYaml t = parseEither $ withObject "theme customization" $ \obj -> do
    mDefVal   <- obj .:? "default"
    mOtherVal <- obj .:? "other"
    defCustom <- join <$> traverse parseCustomAttrYaml mDefVal
    otherMap  <- case mOtherVal of
        Nothing       -> return M.empty
        Just otherVal -> withObject "other section" (\otherObj -> do
            pairs <- forM (M.keys $ themeDefaultMapping t) $ \an -> do
                let key = fromText $ makeFieldName $ attrNameComponents an
                mVal <- otherObj .:? key
                case mVal of
                    Nothing -> return Nothing
                    Just v  -> fmap (an,) <$> parseCustomAttrYaml v
            return $ M.fromList $ catMaybes pairs
            ) otherVal
    return (defCustom, otherMap)

styleToText :: Style -> T.Text
styleToText s =
    let activeStyles = filter (\(_, a) -> a .&. s == a) allStyles
    in case activeStyles of
        [(name, _)] -> name
        many        -> "[" <> T.intercalate ", " (fst <$> many) <> "]"

colorToText :: MaybeDefault Color -> T.Text
colorToText Default      = "default"
colorToText (SetTo c)    = vtyColorName c
colorToText KeepCurrent  = error "colorToText: KeepCurrent not supported"

customAttrToMap :: CustomAttr -> M.Map T.Text T.Text
customAttrToMap c = M.fromList $ catMaybes
    [ fmap ("fg",)    (colorToText <$> customFg c)
    , fmap ("bg",)    (colorToText <$> customBg c)
    , fmap ("style",) (styleToText <$> customStyle c)
    ]

buildThemeYaml :: Maybe CustomAttr -> M.Map AttrName CustomAttr -> Value
buildThemeYaml mDefCustom otherMap =
    let otherMapT = M.fromList
            [ (makeFieldName $ attrNameComponents an, customAttrToMap c)
            | (an, c) <- M.toList otherMap
            ]
    in object $ catMaybes
        [ fmap (\c -> "default" .= customAttrToMap c) mDefCustom
        , Just $ "other" .= otherMapT
        ]

-- | Apply customizations using a custom lookup function. Customizations
-- are obtained for each attribute name in the theme. Any customizations
-- already set are lost.
applyCustomizations :: Maybe CustomAttr
                    -- ^ An optional customization for the theme's
                    -- default attribute.
                    -> (AttrName -> Maybe CustomAttr)
                    -- ^ A function to obtain a customization for the
                    -- specified attribute.
                    -> Theme
                    -- ^ The theme to customize.
                    -> Theme
applyCustomizations customDefAttr lookupAttr t =
    let customMap = foldr nextAttr mempty (M.keys $ themeDefaultMapping t)
        nextAttr an m = case lookupAttr an of
            Nothing     -> m
            Just custom -> M.insert an custom m
    in t { themeCustomDefaultAttr = customDefAttr
         , themeCustomMapping = customMap
         }

-- | Load a YAML file containing theme customizations. Use the specified
-- theme to determine which customizations to load. Return the specified
-- theme with customizations set. See the module documentation for the
-- theme file format.
loadCustomizations :: FilePath -> Theme -> IO (Either String Theme)
loadCustomizations path t = do
    result <- (Yaml.decodeFileEither path :: IO (Either Yaml.ParseException Value))
    return $ case result of
        Left e  -> Left (Yaml.prettyPrintParseException e)
        Right v -> case parseThemeYaml t v of
            Left e               -> Left e
            Right (customDef, customMap) ->
                Right $ applyCustomizations customDef (flip M.lookup customMap) t

vtyColorName :: Color -> T.Text
vtyColorName c@(Color240 n) = case color240CodeToRGB (fromIntegral n) of
    Just (r,g,b) -> T.pack (printf "#%02x%02x%02x" r g b)
    Nothing -> (error $ "Invalid color: " <> show c)
vtyColorName c =
    fromMaybe (error $ "Invalid color: " <> show c)
              (lookup c allColors)

makeFieldName :: [String] -> T.Text
makeFieldName cs = T.pack $ intercalate "." cs

-- | Save a YAML file containing theme customizations. Use the specified
-- theme to determine which customizations to save. See the module
-- documentation for the theme file format.
saveCustomizations :: FilePath -> Theme -> IO ()
saveCustomizations path t =
    Yaml.encodeFile path $ buildThemeYaml
        (themeCustomDefaultAttr t)
        (themeCustomMapping t)

-- | Save a YAML file containing all attributes from the specified
-- theme. Customized attributes are saved, but if an attribute is not
-- customized, its default is saved instead. The file can later be
-- re-loaded as a customization file.
saveTheme :: FilePath -> Theme -> IO ()
saveTheme path t =
    let defCustom = fromMaybe (attrToCustom $ themeDefaultAttr t) (themeCustomDefaultAttr t)
        otherMap  = M.mapWithKey (\an def ->
            fromMaybe (attrToCustom def) (M.lookup an $ themeCustomMapping t))
            (themeDefaultMapping t)
    in Yaml.encodeFile path $ buildThemeYaml (Just defCustom) otherMap

attrToCustom :: Attr -> CustomAttr
attrToCustom a =
    CustomAttr { customFg    = Just $ attrForeColor a
               , customBg    = Just $ attrBackColor a
               , customStyle = case attrStyle a of
                   SetTo s -> Just s
                   _       -> Nothing
               }
