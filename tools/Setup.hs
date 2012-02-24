import Distribution.Simple

main = defaultMain

{-
#! /usr/bin/runghc
 
 import Distribution.Simple
 import Distribution.Simple.LocalBuildInfo
 import Distribution.PackageDescription
 import System.Cmd
 import System.Directory
 import Data.List


 main = defaultMainWithHooks (defaultUserHooks { postBuild = buildDll })
   where
   buildDll _ _ pkg info = do putStrLn "Building Dll..."
			      system  "echo hallo"
-}
{-                            setCurrentDirectory (buildDir info)
                              let buildCmd = cmd pkg info
                              putStrLn buildCmd
                              system buildCmd
                              let dll = dllFile pkg
                              let cpDllCmd = "cp " ++ dll ++ " " ++ (name pkg) ++ "\\" ++ dll
                              putStrLn cpDllCmd
                              system $ echo hallo
   ghcExe :: LocalBuildInfo -> String
   ghcExe info = "\"" ++ (compilerPath (compiler info)) ++ "\""
   mainOFile :: PackageDescription -> String
   mainOFile pd = "HS" ++ (name pd) ++ "-" ++ (showVersion (pkgVersion (package pd))) ++ ".o"
   cmd :: PackageDescription -> LocalBuildInfo -> String
   cmd pd i = (ghcExe i) ++ " --mk-dll -o " ++ (dllFile pd) ++ " " ++ (mainOFile pd) ++ " " ++ (packages i)
   packages :: LocalBuildInfo -> String
   packages i = foldl1 (\x y -> x ++ " " ++ y) (map showPackage (packageDeps i))
   showPackage :: PackageIdentifier -> String
   showPackage pi = "-package " ++ showPackageId pi
   name :: PackageDescription -> String
   name = pkgName . package 
   dllFile :: PackageDescription -> String
   dllFile pd = (name pd) ++ ".dll"
-}
