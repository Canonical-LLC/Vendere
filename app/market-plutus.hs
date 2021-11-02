{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Cardano.Api.Shelley
    ( writeFileTextEnvelope,
      Error(displayError),
      PlutusScript,
      PlutusScriptV1,
      ScriptData(ScriptDataNumber),
      toAlonzoData )
import Cardano.Ledger.Alonzo.Data as Alonzo ( Data(Data) )
import qualified Plutus.V1.Ledger.Api as Plutus

import Prelude
import System.Environment ( getArgs )
import qualified Data.ByteString.Short as SBS

import Market.Onchain (apiBuyScript, buyScriptAsShortBs)
import Utility        (companyPkh)


main :: IO ()
main = do
    args <- getArgs
    let nargs = length args
    let scriptnum = if nargs > 0 then read (head args) else 42
    let scriptname = if nargs > 1 then args!!1 else  "market.plutus"
    putStrLn $ "Writing output to: " ++ scriptname
    writePlutusScript scriptnum scriptname (apiBuyScript "eb23924b4ebe67a4e9a86d9c4852a038a244e957cea7cf9c3d1e0614") (buyScriptAsShortBs "eb23924b4ebe67a4e9a86d9c4852a038a244e957cea7cf9c3d1e0614")


writePlutusScript :: Integer -> FilePath -> PlutusScript PlutusScriptV1 -> SBS.ShortByteString -> IO ()
writePlutusScript scriptnum filename scriptSerial scriptSBS =
  do
  case Plutus.defaultCostModelParams of
        Just m ->
          let Alonzo.Data pData = toAlonzoData (ScriptDataNumber scriptnum)
              (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m scriptSBS [pData]
          in do print ("Log output" :: String) >> print logout
                case e of
                  Left evalErr -> print ("Eval Error" :: String) >> print evalErr
                  Right exbudget -> print ("Ex Budget" :: String) >> print exbudget
        Nothing -> error "defaultCostModelParams failed"
  result <- writeFileTextEnvelope filename Nothing scriptSerial
  case result of
    Left err -> print $ displayError err
    Right () -> return ()
