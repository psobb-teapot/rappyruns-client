# Packages the delivered client into dist/EphineaTAClient.zip:
#   EphineaTAClient.exe
#   data/quest-triggers.sexp
# Run after building the exe with deliver.lisp (see README.md).
$ErrorActionPreference = "Stop"

$dist = Join-Path $PSScriptRoot "dist"
$exe = Join-Path $dist "EphineaTAClient.exe"
$triggers = Join-Path $PSScriptRoot "data\quest-triggers.sexp"
$zip = Join-Path $dist "EphineaTAClient.zip"

if (-not (Test-Path $exe)) { throw "Missing $exe - build it first (deliver.lisp)." }
if (-not (Test-Path $triggers)) { throw "Missing $triggers." }

$stage = Join-Path $dist "stage"
if (Test-Path $stage) { Remove-Item -Recurse -Force $stage }
New-Item -ItemType Directory -Force (Join-Path $stage "data") | Out-Null

Copy-Item $exe $stage
Copy-Item $triggers (Join-Path $stage "data")
# If your LispWorks build needs OpenSSL DLLs for https servers, drop them
# in dist/ and add Copy-Item lines here (see README.md, "Building the exe").

if (Test-Path $zip) { Remove-Item -Force $zip }
Compress-Archive -Path (Join-Path $stage "*") -DestinationPath $zip
Remove-Item -Recurse -Force $stage

Write-Host "Created $zip"
