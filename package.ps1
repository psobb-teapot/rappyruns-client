# Packages the delivered client into dist/RappyRunsClient.zip:
#   RappyRunsClient.exe
#   data/quest-triggers.sexp
#   ffmpeg/ffmpeg.exe + LICENSE.txt   (optional, from vendor/ffmpeg/ - see README)
# Run after building the exe with deliver.lisp (see README.md).
$ErrorActionPreference = "Stop"

$dist = Join-Path $PSScriptRoot "dist"
$exe = Join-Path $dist "RappyRunsClient.exe"
$triggers = Join-Path $PSScriptRoot "data\quest-triggers.sexp"
$ffmpegDir = Join-Path $PSScriptRoot "vendor\ffmpeg"

if (-not (Test-Path $exe)) { throw "Missing $exe - build it first (deliver.lisp)." }
if (-not (Test-Path $triggers)) { throw "Missing $triggers." }

$stage = Join-Path $dist "stage"
if (Test-Path $stage) { Remove-Item -Recurse -Force $stage }
New-Item -ItemType Directory -Force (Join-Path $stage "data") | Out-Null

Copy-Item $triggers (Join-Path $stage "data")

# The client looks for ffmpeg/ffmpeg.exe next to its exe for the video
# recording feature. Bundling is optional: without it the zip still works,
# users just have to install ffmpeg themselves to record.
if (Test-Path (Join-Path $ffmpegDir "ffmpeg.exe")) {
    New-Item -ItemType Directory -Force (Join-Path $stage "ffmpeg") | Out-Null
    Copy-Item (Join-Path $ffmpegDir "ffmpeg.exe") (Join-Path $stage "ffmpeg")
    $license = Join-Path $ffmpegDir "LICENSE.txt"
    if (-not (Test-Path $license)) {
        throw "vendor\ffmpeg\ffmpeg.exe is present but LICENSE.txt is missing - a GPL ffmpeg build must ship with its license text."
    }
    Copy-Item $license (Join-Path $stage "ffmpeg")
} else {
    Write-Warning "vendor\ffmpeg\ffmpeg.exe not found - packaging WITHOUT the bundled ffmpeg (recording will need a user-installed ffmpeg). See README.md 'Bundling ffmpeg'."
}

Copy-Item $exe (Join-Path $stage "RappyRunsClient.exe")
$zip = Join-Path $dist "RappyRunsClient.zip"
if (Test-Path $zip) { Remove-Item -Force $zip }
Compress-Archive -Path (Join-Path $stage "*") -DestinationPath $zip
Write-Host "Created $zip"

Remove-Item -Recurse -Force $stage
