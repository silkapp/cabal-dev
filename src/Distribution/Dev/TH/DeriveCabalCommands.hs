module Distribution.Dev.TH.DeriveCabalCommands (deriveCabalCommands) where

import Control.Applicative ((<$>), (<*>))
import Data.Char (toUpper)
import Language.Haskell.TH

import Distribution.Dev.InterrogateCabalInstall (ArgType (..), CabalCommandStr (..), Option (..), OptionName (..), Program, getCabalCommandHelp, getCabalProgs,
                                                 optParseFlags)

mkLO :: Option -> Exp
mkLO (Option on ty) = optC nE tyE
  where
    (cn, o) = case on of
      Short c      -> ("Short", CharL c)
      LongOption s -> ("LongOption", StringL s)
    tyE = ConE $ mkName $ case ty of
      Req   -> "Req"
      Opt   -> "Opt"
      NoArg -> "NoArg"
    nE = appC cn $ LitE o
    optC n t = AppE (appC "Option" n) t
    appC n = AppE $ ConE $ mkName n

mkSupportedOptionClause :: [Program] -> CabalCommandStr -> String -> Clause
mkSupportedOptionClause progs cStr helpOutput =
  Clause [ConP (commandConsName cStr) []] (NormalB supportedFlags) []
    where
      supportedFlags = ListE . map mkLO $ optParseFlags progs helpOutput

mkGetSupportedOptions :: [Program] -> [(CabalCommandStr, String)] -> [Dec]
mkGetSupportedOptions progs cs
 = [ SigD n $ ccT ~~> AppT ListT (ConT (mkName "Option"))
   , FunD n $ map (uncurry (mkSupportedOptionClause progs)) cs
   ]
 where
   n = mkName "commandOptions"

mkGetSupportedOptionsIO :: [CabalCommandStr] -> IO [Dec]
mkGetSupportedOptionsIO ccs =
    (mkGetSupportedOptions <$> getCabalProgs)
    <*> (zip ccs <$> mapM getCabalCommandHelp ccs)

commandStrs :: [CabalCommandStr]
commandStrs = map CabalCommandStr
  [ "install"
  , "update"
  , "list"
  , "info"
  , "fetch"
  , "freeze"
  , "get"
  , "check"
  , "sdist"
  , "upload"
  , "report"
  , "run"
  , "init"
  , "configure"
  , "build"
  , "repl"
  , "sandbox"
  , "haddock"
  , "exec"
  , "copy"
  , "clean"
  , "hscolour"
  , "register"
  , "test"
  , "bench"
  , "help"
  ]

mkCabalCommandsDef :: IO [Dec]
mkCabalCommandsDef = do
  putStrLn "Interrogating cabal-install executable:"
  fmap concat . mapM ($ commandStrs) $
             [ return . return . cabalCommandsDef
             , return . mkStrToCmd
             , return . mkCmdToStr
             , return . mkAllCommands
             , mkGetSupportedOptionsIO
             ]

deriveCabalCommands :: Q [Dec]
deriveCabalCommands = runIO mkCabalCommandsDef

mkAllCommands :: [CabalCommandStr] -> [Dec]
mkAllCommands cmds =
  [ SigD n $ AppT ListT ccT
  , FunD n
    [ Clause [] (NormalB (ListE $ map (ConE . commandConsName) cmds)) [] ]
  ]
  where
    n = mkName "allCommands"

ccL :: CabalCommandStr -> Lit
ccL = StringL . ccStr

mkCmdToStr :: [CabalCommandStr] -> [Dec]
mkCmdToStr =
  fromCommandClauses "commandToString" (ccT ~~> strT) $ map $ \n ->
  Clause [ConP (commandConsName n) []] (NormalB (LitE $ ccL n)) []

mkStrToCmd :: [CabalCommandStr] -> [Dec]
mkStrToCmd =
  fromCommandClauses "stringToCommand" (strT ~~> maybeT ccT) $ \ccs ->
      map toClause ccs ++ [nothing]
  where
    toClause n = Clause [LitP (ccL n)] (NormalB (justE $ ccE n)) []
    justE = AppE $ ConE $ mkName "Just"
    ccE = ConE . commandConsName
    nothing = Clause [WildP] (NormalB (ConE (mkName "Nothing"))) []

cabalCommandsDef :: [CabalCommandStr] -> Dec
cabalCommandsDef strs = DataD [] ccN [] (map cStrToCon strs) [mkName "Eq", mkName "Show"]

ccN :: Name
ccN = mkName "CabalCommand"

commandConsName :: CabalCommandStr -> Name
commandConsName = mkName . capitalize . ccStr
  where
    capitalize (c:cs) = toUpper c : cs
    capitalize [] = []

cStrToCon :: CabalCommandStr -> Con
cStrToCon c = NormalC (commandConsName c) []

fromCommandClauses :: String -> Type -> ([CabalCommandStr] -> [Clause]) -> [CabalCommandStr] -> [Dec]
fromCommandClauses funName funTy mkClause cmdStrs =
  [ SigD n funTy, FunD n $ mkClause cmdStrs ]
  where
    n = mkName funName

(~~>) :: Type -> Type -> Type
t1 ~~> t2 = AppT (AppT ArrowT t1) t2

strT :: Type
strT = ConT $ mkName "String"

ccT :: Type
ccT = ConT ccN

maybeT :: Type -> Type
maybeT = AppT (ConT $ mkName "Maybe")
