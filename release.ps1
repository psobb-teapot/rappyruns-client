# Builds, packages and publishes the client to the public releases repo.
#   .\client\release.ps1 v0.2.0 -NotesFile notes.md   # new release
#   .\client\release.ps1 v0.2.0 -Prerelease           # test build, excluded from `latest`
#   .\client\release.ps1 v0.1.0 -Clobber              # replace the asset on an existing release
# Requires LispWorks (build) and an authenticated `gh` (publish).
param(
    [Parameter(Mandatory = $true)][string]$Version,
    [string]$NotesFile,
    [switch]$Prerelease,
    [switch]$Clobber
)
$ErrorActionPreference = "Stop"

$repo = "psobb-teapot/ephinea-ta-client-releases"
$lispworks = "C:\Program Files\LispWorks\lispworks-8-1-0-x64-windows.exe"

if ($Version -notmatch '^v\d+\.\d+\.\d+$') { throw "Version must look like v1.2.3 (got: $Version)." }
if (-not (Test-Path $lispworks)) { throw "LispWorks not found at $lispworks." }

# The tag must match client/VERSION, which deliver.lisp bakes into the
# exe for the self-updater; a mismatch would ship a binary that thinks
# it is some other release.
$versionFile = Join-Path $PSScriptRoot "VERSION"
if (-not (Test-Path $versionFile)) { throw "client/VERSION not found; create it with the X.Y.Z version." }
$fileVersion = (Get-Content $versionFile -TotalCount 1).Trim()
if ("v$fileVersion" -ne $Version) {
    throw "client/VERSION ($fileVersion) does not match the release tag ($Version); bump and commit client/VERSION first."
}

# The LispWorks image is a GUI-subsystem exe: plain invocation returns
# immediately and never sets $LASTEXITCODE, so wait on it explicitly.
$build = Start-Process -FilePath $lispworks `
  -ArgumentList "-build", (Join-Path $PSScriptRoot "deliver.lisp") `
  -Wait -PassThru -NoNewWindow
if ($build.ExitCode -ne 0) { throw "LispWorks build failed (exit $($build.ExitCode))." }

& (Join-Path $PSScriptRoot "package.ps1")

# Both zips: the legacy asset name keeps pre-rename clients
# (<= v0.15.0) self-updating - see package.ps1 and issue #15.
$zip = Join-Path $PSScriptRoot "dist\RappyRunsClient.zip"
$legacyZip = Join-Path $PSScriptRoot "dist\EphineaTAClient.zip"

if ($Clobber) {
    gh release upload $Version $zip $legacyZip --clobber --repo $repo
} else {
    $ghArgs = @("release", "create", $Version, $zip, $legacyZip,
                "--repo", $repo,
                "--title", "RappyRuns Client $Version")
    if ($NotesFile) { $ghArgs += @("--notes-file", $NotesFile) }
    else { $ghArgs += @("--notes", "RappyRuns Client $Version.") }
    if ($Prerelease) { $ghArgs += "--prerelease" }
    gh @ghArgs
}
if ($LASTEXITCODE -ne 0) { throw "gh failed (exit $LASTEXITCODE)." }

Write-Host "Published $Version to https://github.com/$repo/releases"
