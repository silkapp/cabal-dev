module Distribution.Dev.TH.DeriveCabalCommands
    ( deriveCabalCommands
    , mkCabalCommandsDef
    , optParseFlags
    )
where

import Control.Applicative ( (<$>), (<*>) )
import Debug.Trace
import Data.Char ( toUpper  )
import Language.Haskell.TH
import Distribution.Dev.InterrogateCabalInstall
    ( Option(..), OptionName(..), ArgType(..), CabalCommandStr (..)
    , optParseFlags, getCabalCommandHelp, Program
    , getCabalProgs
    )

mkLO :: Option -> Exp
mkLO (Option on ty) =
    let (cn, o) = case on of
                    Short c -> ("Short", CharL c)
                    LongOption s -> ("LongOption", StringL s)

        tyE = ConE $ mkName $
              case ty of
                Req -> "Req"
                Opt -> "Opt"
                NoArg -> "NoArg"

        nE = appC cn $ LitE o

        optC n t = AppE (appC "Option" n) t

        appC n = AppE $ ConE $ mkName n

    in optC nE tyE

mkSupportedOptionClause :: [Program] -> CabalCommandStr -> String -> Clause
mkSupportedOptionClause progs cStr helpOutput =
    let supportedFlags = ListE . map mkLO $
                         optParseFlags progs helpOutput
    in Clause [ConP (commandConsName cStr) []] (NormalB supportedFlags) []

mkGetSupportedOptions :: [Program] -> [(CabalCommandStr, String)] -> [Dec]
mkGetSupportedOptions progs cs =
    let n = mkName "commandOptions"
    in [ SigD n $ ccT ~~> AppT ListT (ConT (mkName "Option"))
       , FunD n $ map (uncurry (mkSupportedOptionClause progs)) cs
       ]

mkGetSupportedOptionsIO :: [CabalCommandStr] -> IO [Dec]
mkGetSupportedOptionsIO ccs =
    (mkGetSupportedOptions <$> getCabalProgs)
    <*> (zip ccs <$> mapM getCabalCommandHelp ccs)

mkCabalCommandsDef :: IO [Dec]
mkCabalCommandsDef = do
    let strs = map CabalCommandStr $
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
    putStrLn "Interrogating cabal-install executable:"
    fmap concat . mapM ($ strs) $
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
    let n = mkName "allCommands"
    in [ SigD n $ AppT ListT ccT
       , FunD n
         [ Clause [] (NormalB (ListE $ map (ConE . commandConsName) cmds)) []
         ]
       ]

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
      toClause n =
          Clause [LitP (ccL n)] (NormalB (justE $ ccE n)) []
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

fromCommandClauses :: String -> Type -> ([CabalCommandStr] -> [Clause])
                   -> [CabalCommandStr] -> [Dec]
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
