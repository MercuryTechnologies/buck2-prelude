import GHC
import GHC.Utils.Panic
import GHC.Iface.Binary
import GHC.Platform
import GHC.Platform.Profile
import GHC.Types.Name.Cache
import GHC.Unit.Module.Deps
import GHC.Data.FastString
import qualified Data.Set as Set
import System.Environment
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [ifaceFile] -> do
        let profile = Profile { profilePlatform = genericPlatform, profileWays = Set.empty }

            on_usage (UsageFile {usg_file_path, usg_file_nonhs = True}) acc = unpackFS usg_file_path : acc
            on_usage _ acc = acc

        name_cache <- initNameCache 'x' []

        -- Load the interface file
        target <- tryMost $ readBinIface profile name_cache IgnoreHiWay QuietBinIFace ifaceFile

        case target of
          Right iface -> do
            let usages = mi_usages iface
                result = foldr (on_usage) [] usages
            putStrLn $ unlines result
          Left msg -> do
            hPutStrLn stderr $ show msg
            exitFailure

    _ -> do
      putStrLn $ "Usage: usagefiles <interface_file>"
      exitFailure
