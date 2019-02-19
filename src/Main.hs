{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import qualified Data.Text            as T
import           Turtle
import qualified Turtle

data CFConfig = CFConfig
  { cfcDeployBucket :: String
  , cfcRegion       :: String
  , cfcStackName    :: String
  , cfcParameters   :: [CFParameter]
  } deriving (Show)

data CFParameter = CFParameter
  { pKey   :: String
  , pValue :: String
  } deriving (Show)

mkConfig :: CFConfig
mkConfig = CFConfig bucket region name params
  where
    bucket = "wc-dl-cloudformation-dev"
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
  shell (T.pack $ "aws cloudformation package --template-file " <> templateFile <> " --output-template-file " <> outputTemplateFile <> " --s3-bucket " <> deployBucket) empty
  return $ f <> decodeString "output.template.yml"
    where
      templateFile = "stepfunction.yml"
      outputTemplateFile = "stepfunction.output.yml"
      deployBucket = cfcDeployBucket cfg

build :: Turtle.FilePath -> IO Turtle.FilePath
build f = do
  let root = f <> decodeString "lambdas"
  cd root
  shell "npm install" empty
  shell "npm run build" empty
  rmtree $ root <> decodeString "node_modules"
  shell "npm install --prod" empty
  return $ root <> decodeString "build"

main :: IO ()
main = do
  let f = decodeString "/Users/jordan/Documents/work/water/lake/landing-to-raw/sandboxes/procurement-analytics"
  let cfg = mkConfig
  build f
  tpl <- package cfg f
  deploy cfg tpl
  