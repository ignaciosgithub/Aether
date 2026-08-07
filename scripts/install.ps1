# One-line Aether installer for Windows 10/11 (no WSL required):
#   irm https://raw.githubusercontent.com/ignaciosgithub/Aether/main/scripts/install.ps1 | iex
#
# Installs: MSYS2 (MinGW64 toolchain incl. NASM), Rust (windows-gnu), Git,
# Python 3 (Tkinter), clones and builds Aether, adds an "Aether Editor"
# Start-menu shortcut and opens it.
#
# Prefer WSL instead? Set $env:AETHER_USE_WSL = 1 before running and the
# installer delegates to the Linux install script inside your WSL distro.

$ErrorActionPreference = "Stop"

function Step($msg) { Write-Host "==> $msg" -ForegroundColor Cyan }

$Msys = "C:\msys64"
$Dest = if ($env:AETHER_HOME) { $env:AETHER_HOME } else { Join-Path $HOME "Aether" }

function Have($cmd) { [bool](Get-Command $cmd -ErrorAction SilentlyContinue) }

function WingetInstall($id, $name) {
    Step "Installing $name"
    winget install --id $id -e --accept-source-agreements --accept-package-agreements --silent
    if ($LASTEXITCODE -ne 0 -and $LASTEXITCODE -ne -1978335189) {
        # -1978335189 = already installed
        throw "winget failed to install $name (exit $LASTEXITCODE)"
    }
}

if ($env:AETHER_USE_WSL) {
    if (-not (Have wsl)) {
        throw "WSL is not installed. Run 'wsl --install' in an elevated PowerShell, reboot, then re-run this script."
    }
    Step "Installing Aether inside WSL"
    wsl -e bash -c "curl -sSf https://raw.githubusercontent.com/ignaciosgithub/Aether/main/scripts/install.sh | bash"
    if ($LASTEXITCODE -ne 0) { throw "WSL install failed (exit $LASTEXITCODE)" }
    Write-Host "Aether installed inside WSL. Open your distro and run: python3 ~/Aether/tools/aether_editor.py" -ForegroundColor Green
    return
}

if (-not (Have winget)) {
    Write-Host "winget is required (comes with Windows 10 1809+/11 via 'App Installer' in the Microsoft Store)." -ForegroundColor Red
    throw "Install 'App Installer' from the Microsoft Store, then re-run this script."
}

if (-not (Have git)) {
    WingetInstall "Git.Git" "Git"
    $env:Path = "$env:Path;$env:ProgramFiles\Git\cmd"
}

if (-not (Have python)) {
    WingetInstall "Python.Python.3.12" "Python 3 (includes Tkinter)"
    $env:Path = "$env:Path;$env:LOCALAPPDATA\Programs\Python\Python312;$env:LOCALAPPDATA\Programs\Python\Python312\Scripts"
}

if (-not (Test-Path "$Msys\usr\bin\bash.exe")) {
    WingetInstall "MSYS2.MSYS2" "MSYS2"
}
$Bash = "$Msys\usr\bin\bash.exe"
if (-not (Test-Path $Bash)) {
    throw "MSYS2 not found at $Msys. Set it up from https://www.msys2.org/ and re-run."
}

Step "Installing MinGW64 toolchain (clang, lld, gcc, make) via pacman"
$env:MSYSTEM = "MINGW64"
& $Bash -lc "pacman -Sy --noconfirm --needed git mingw-w64-x86_64-toolchain mingw-w64-x86_64-clang mingw-w64-x86_64-lld mingw-w64-x86_64-nasm make"
if ($LASTEXITCODE -ne 0) { throw "pacman failed (exit $LASTEXITCODE)" }

$Cargo = Join-Path $HOME ".cargo\bin\cargo.exe"
if (-not (Have cargo) -and -not (Test-Path $Cargo)) {
    Step "Installing Rust (rustup, x86_64-pc-windows-gnu)"
    $rustupInit = Join-Path $env:TEMP "rustup-init.exe"
    Invoke-WebRequest "https://win.rustup.rs/x86_64" -OutFile $rustupInit
    & $rustupInit -y --default-host x86_64-pc-windows-gnu --default-toolchain stable
    if ($LASTEXITCODE -ne 0) { throw "rustup-init failed (exit $LASTEXITCODE)" }
}
$env:Path = "$env:Path;$HOME\.cargo\bin;$Msys\mingw64\bin;$Msys\usr\bin"

if (Test-Path (Join-Path $Dest ".git")) {
    Step "Updating existing checkout at $Dest"
    git -C $Dest pull --ff-only
} else {
    Step "Cloning Aether into $Dest"
    git clone https://github.com/ignaciosgithub/Aether.git $Dest
}

Step "Building the compiler (release)"
cargo build --workspace --release --manifest-path (Join-Path $Dest "Cargo.toml")
if ($LASTEXITCODE -ne 0) { throw "cargo build failed (exit $LASTEXITCODE)" }

Step "Creating Start-menu shortcut"
$pythonw = (Get-Command pythonw -ErrorAction SilentlyContinue).Source
if (-not $pythonw) { $pythonw = (Get-Command python).Source }
$startMenu = Join-Path $env:APPDATA "Microsoft\Windows\Start Menu\Programs"
$shell = New-Object -ComObject WScript.Shell
$lnk = $shell.CreateShortcut((Join-Path $startMenu "Aether Editor.lnk"))
$lnk.TargetPath = $pythonw
$lnk.Arguments = "`"$(Join-Path $Dest 'tools\aether_editor.py')`""
$lnk.WorkingDirectory = $Dest
$lnk.Description = "Multi-tab Aether editor (More with less)"
$lnk.Save()

Write-Host ""
Write-Host "Aether installed." -ForegroundColor Green
Write-Host "  - Editor:  'Aether Editor' in the Start menu, or: python $Dest\tools\aether_editor.py"
Write-Host "  - CLI:     $Dest\target\release\aetherc.exe program.ae --arch x86_64 --os windows -o program.s"
Write-Host "  - Link:    $Bash -lc `"cd '$Dest' && ./scripts/assemble_link.sh x86_64-windows program.s program.exe`""

Step "Launching the editor"
Start-Process $pythonw -ArgumentList "`"$(Join-Path $Dest 'tools\aether_editor.py')`"" -WorkingDirectory $Dest
