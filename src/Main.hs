{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import           Control.Monad        (void, when)
import           Crypto.Hash
import qualified Data.ByteString.Lazy as LB
import           Data.Maybe
import qualified Data.Text            as T
import           System.FilePath
import           Turtle               (cd, cp, decodeString, empty,
                                       encodeString, mkdir, mv, rmtree, shell)
import qualified Turtle

import           Control.Lens
import           Control.Monad
import           Network.AWS
import           Network.AWS.S3

import           Codec.Archive.Zip
import           Text.Regex

data CFConfig = CFConfig
  { cfcDeployBucket :: String
  , cfcDeployKey    :: String
  , cfcRegion       :: String
  , cfcStackName    :: String
  , cfcParameters   :: [CFParameter]
  } deriving (Show)

data CFParameter = CFParameter
  { pKey   :: String
  , pValue :: String
  } deriving (Show)

mkConfig :: CFConfig
mkConfig = CFConfig bucket key region name params
  where
    bucket = "cf-templates-qgrkfiisxmqc-ap-southeast-2"
    key = "budgeter-deployment"
    region = "ap-southeast-2"
    name = "budgeter"
    params =
      [ CFParameter "Username" "myusername"
      , CFParameter "Password" "mypassword"
      ]

deploy :: CFConfig -> Turtle.FilePath -> IO ()
deploy cfg f = do
  shell (T.pack $ "aws cloudformation deploy --template-file " <> encodeString f <> " --s3-bucket " <> deployBucket <> " --region " <> region <> " --capabilities " <> capabilities <> " --stack-name " <> stackName <> " --parameter-overrides " <> paramOverrides) empty
  return ()
    where
      deployBucket = cfcDeployBucket cfg
      region = cfcRegion cfg
      capabilities = "CAPABILITY_NAMED_IAM"
      stackName = cfcStackName cfg
      paramOverrides = foldl (\a b -> a <> " " <> b) "" $ (\p -> pKey p <> "=" <> pValue p) <$> cfcParameters cfg

package :: CFConfig -> Turtle.FilePath -> IO Turtle.FilePath
package cfg f = do
  cd f
  file <- readFile templateFile
  env <- newEnv Discover
  let uris = (mapMaybe matcher . lines) file
  urisToReplace <- forM uris $ \uri -> do
    let zipDestination = encodeString f <> "output.zip"
    addFilesToArchive [OptRecursive, OptDestination zipDestination] emptyArchive [encodeString f <> uri]
    runResourceT . runAWS env . send $ putObject (BucketName . T.pack $ deployBucket) (ObjectKey . T.pack $ deployKey) (toBody zipDestination)
    return (uri, "s3://" <> deployBucket <> "/" <> deployKey)
  forM_ urisToReplace $ \(oldUri, newUri) -> writeFile outputTemplateFile (unlines . map (replacer oldUri newUri) . lines $ file)
  return $ decodeString outputTemplateFile
    where
      matcher s = matchRegex (mkRegex "CodeUri: (.+)") s >>= listToMaybe
      replacer oldUri newUri s = subRegex (mkRegex "CodeUri: (.+)") s ("CodeUri: " <> newUri)
      templateFile = "template.yml"
      outputTemplateFile = "template.output.yml"
      deployBucket = cfcDeployBucket cfg
      deployKey = cfcDeployKey cfg

shouldPackage :: CFConfig -> Turtle.FilePath -> IO Bool
shouldPackage cfg f = do
  newMd5 <- LB.readFile . encodeString $ f
  env <- newEnv Discover
  obj <- runResourceT . runAWS env . send $ getObject (BucketName bucket) (ObjectKey key)
  let oldMd5Text = obj ^. gorsMetadata ^.at "md5"
  let newMd5Text = Just . T.pack . show $ (hashlazy newMd5 :: Digest MD5)
  return $ oldMd5Text /= newMd5Text
    where
      bucket = T.pack $ cfcDeployBucket cfg
      key = T.pack $ cfcDeployKey cfg

build :: Turtle.FilePath -> IO Turtle.FilePath
build f = do
  let root = f <> decodeString "src/processor"
  cd root
  mkdir $ f <> decodeString ".build"
  shell "npm install" empty
  shell "npm run build" empty
  rmtree $ root <> decodeString "node_modules"
  shell "npm install --prod" empty
  cp (root <> decodeString "index.ts") (root <> decodeString ".build/index.ts")
  return $ root <> decodeString "build"

shouldBuild :: Turtle.FilePath -> IO Bool
shouldBuild f = do
  let root = f <> decodeString "src/processor"
  cd root
  newFileContent <- LB.readFile . encodeString $ root <> decodeString "index.ts"
  oldFileContent <- LB.readFile . encodeString $ root <> decodeString ".build/index.ts"
  let oldmd5 = hashlazy newFileContent :: Digest MD5
  let newmd5 = hashlazy oldFileContent :: Digest MD5
  return $ oldmd5 /= newmd5

data LambdaRuntime = Node610 deriving (Show)

data Lambda = Lambda
  { lambdaPath :: Prelude.FilePath
  , lambdaRuntime      :: LambdaRuntime
  } deriving (Show)

newtype BuiltLambda = BuiltLambda
  { builtLambdaPath :: String
  } deriving (Show)

data PackagedLambda = PackagedLambda
  { packagedLambdaS3Location :: String
  , packagedLambdaCodeUri :: String
  } deriving (Show)

lambdasToBuild :: Prelude.FilePath -> IO [Lambda]
lambdasToBuild f = do
  file <- readFile $ f </> templateFile
  return $ toLambda <$> (mapMaybe matcher . lines) file
    where
      templateFile = "template.yml"
      matcher s = matchRegex (mkRegex "CodeUri: (.+)") s >>= listToMaybe
      toLambda uri = Lambda (f </> uri) Node610

buildLambda :: Lambda -> IO BuiltLambda
buildLambda (Lambda path Node610) = do
  let nodeModulesDir = path </> "node_modules"
  cd $ decodeString path
  shell "npm install" empty
  shell "npm run build" empty
  rmtree $ decodeString nodeModulesDir
  shell "npm install --prod" empty
  return $ BuiltLambda path

packageLambda :: CFConfig -> BuiltLambda -> IO PackagedLambda
packageLambda cfg (BuiltLambda path) = do
  archive <- addFilesToArchive [OptRecursive] emptyArchive [path]
  LB.writeFile zipDestination $ fromArchive archive
  return $ PackagedLambda "s3://" path
    where 
      s3Destination = "s3://" <> bucket <> "/" <> key <> ""
      zipDestination = path </> "package.zip"
      bucket = cfcDeployBucket cfg
      key = cfcDeployKey cfg

-- package :: CFConfig -> Turtle.FilePath -> IO Turtle.FilePath
-- package cfg f = do
--   cd f
--   file <- readFile templateFile
--   env <- newEnv Discover
--   let uris = (mapMaybe matcher . lines) file
--   urisToReplace <- forM uris $ \uri -> do
--     let zipDestination = encodeString f <> "output.zip"
--     addFilesToArchive [OptRecursive, OptDestination zipDestination] emptyArchive [encodeString f <> uri]
--     runResourceT . runAWS env . send $ putObject (BucketName . T.pack $ deployBucket) (ObjectKey . T.pack $ deployKey) (toBody zipDestination)
--     return (uri, "s3://" <> deployBucket <> "/" <> deployKey)
--   forM_ urisToReplace $ \(oldUri, newUri) -> writeFile outputTemplateFile (unlines . map (replacer oldUri newUri) . lines $ file)
--   return $ decodeString outputTemplateFile
--     where
--       matcher s = matchRegex (mkRegex "CodeUri: (.+)") s >>= listToMaybe
--       replacer oldUri newUri s = subRegex (mkRegex "CodeUri: (.+)") s ("CodeUri: " <> newUri)
--       templateFile = "template.yml"
--       outputTemplateFile = "template.output.yml"
--       deployBucket = cfcDeployBucket cfg
--       deployKey = cfcDeployKey cfg

main :: IO ()
main = do
  let f = decodeString "/Users/jordan/Documents/code/budgeter"
  let cfg = mkConfig
  sb <- shouldBuild f
  when sb $ void (build f)
  tpl <- package cfg f
  deploy cfg tpl

