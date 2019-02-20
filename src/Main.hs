{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import           Control.Monad        (void, when)
import           Crypto.Hash
import qualified Data.ByteString.Lazy as LB
import           Data.Maybe
import qualified Data.Text            as T
import           Turtle
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
    bucket = "wc-dl-cloudformation-dev"
    key = "wc-dl-sandbox-stepfn"
    region = "ap-southeast-2"
    name = "wc-dl-sandbox-stepfn-procurement-analytics"
    params =
      [ CFParameter "DataLakeStackName" "wc-dl-base"
      , CFParameter "ReportName" "procurement_analytics"
      , CFParameter "Environment" "dev"
      , CFParameter "DatamartUsername" "admin"
      , CFParameter "VpcId" "vpc-015d49423592b1aac"
      , CFParameter "SecurityGroupId" "sg-0ad15ac7efec366ac"
      , CFParameter "PrivateSubetA" "subnet-0c6a0e8d7ab858f67"
      , CFParameter "PrivateSubetB" "subnet-0fa69ef0d3ef08fac"
      , CFParameter "TempBucket" "wc-dl-base-temp-982209102910-ap-southeast-2"
      , CFParameter "RedshiftConnectionName" "Redshift"
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
      templateFile = "stepfunction.yml"
      outputTemplateFile = "stepfunction.output.yml"
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
  let root = f <> decodeString "lambdas"
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
  let root = f <> decodeString "lambdas"
  cd root
  newFileContent <- LB.readFile . encodeString $ root <> decodeString "index.ts"
  oldFileContent <- LB.readFile . encodeString $ root <> decodeString ".build/index.ts"
  let oldmd5 = hashlazy newFileContent :: Digest MD5
  let newmd5 = hashlazy oldFileContent :: Digest MD5
  return $ oldmd5 /= newmd5

main :: IO ()
main = do
  let f = decodeString "/Users/jordan/Documents/work/water/lake/landing-to-raw/sandboxes/procurement-analytics"
  let cfg = mkConfig
  sb <- shouldBuild f
  when sb $ void (build f)
  tpl <- package cfg f
  deploy cfg tpl

