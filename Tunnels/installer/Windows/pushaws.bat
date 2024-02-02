@echo off
setlocal enabledelayedexpansion

REM Default to 'dev' if no argument is provided
set "ENVIRONMENT=%~1"
set "PROFILE=dymium"

REM Determine the S3 bucket based on the environment
if "%ENVIRONMENT%"=="dev" (
    set "S3_BUCKET=dymium-dev-tunneling-clients"
    set "PROFILE=dymium-dev"
) elseif "%ENVIRONMENT%"=="prod" (
    set "S3_BUCKET=dymium-prod-tunneling-clients"
    set "PROFILE=dymium-prod"
) elseif "%ENVIRONMENT%"=="stage" (
    set "S3_BUCKET=dymium-stage-tunneling-clients"
    set "PROFILE=dymium-stage"
) else (
    echo Invalid environment. Please specify 'dev', 'prod', or 'stage'.
    exit /b 1
)

REM Copy the file to S3
aws s3 cp DymiumInstaller.pkg s3://%S3_BUCKET%/windows/ --profile %PROFILE% --region us-west-2

endlocal
