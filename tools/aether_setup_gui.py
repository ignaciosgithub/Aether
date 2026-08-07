#!/usr/bin/env python3
"""Aether GUI setup wizard for Linux and Windows.

Checks for the required toolchain and offers a one-button "Install everything
missing" that installs dependencies, builds the aetherc compiler with a live
progress log, adds an editor shortcut, and launches the editor.

On Windows it uses winget + MSYS2/MinGW64 (no WSL needed); on Linux it uses
apt + rustup.

Usage: python3 tools/aether_setup_gui.py
"""

import os
import shutil
import subprocess
import sys
import threading
import tkinter as tk
from tkinter import messagebox, ttk

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
IS_WINDOWS = os.name == "nt"
EXE = ".exe" if IS_WINDOWS else ""
CARGO_BIN = os.path.expanduser("~/.cargo/bin")
APT_PACKAGES = ["git", "clang", "lld", "build-essential", "python3-tk"]
PACMAN_PACKAGES = [
    "git",
    "mingw-w64-x86_64-toolchain",
    "mingw-w64-x86_64-clang",
    "mingw-w64-x86_64-lld",
    "mingw-w64-x86_64-nasm",
    "make",
]
MSYS_ROOT = r"C:\msys64"
MINGW_BIN = os.path.join(MSYS_ROOT, "mingw64", "bin")
DESKTOP_DIR = os.path.expanduser("~/.local/share/applications")
DESKTOP_FILE = os.path.join(DESKTOP_DIR, "aether-editor.desktop")

BG = "#1e1e1e"
FG = "#d4d4d4"
OK = "#4ec9b0"
BAD = "#f48771"


def which(cmd):
    env_path = os.environ.get("PATH", "")
    extra = [CARGO_BIN]
    if IS_WINDOWS:
        extra += [MINGW_BIN, os.path.join(MSYS_ROOT, "usr", "bin")]
    for d in extra:
        if os.path.isdir(d) and d not in env_path.split(os.pathsep):
            env_path += os.pathsep + d
    os.environ["PATH"] = env_path
    return shutil.which(cmd)


def msys_bash():
    p = os.path.join(MSYS_ROOT, "usr", "bin", "bash.exe")
    return p if os.path.isfile(p) else None


def aetherc_path():
    p = os.path.join(REPO_ROOT, "target", "release", "aetherc" + EXE)
    return p if os.path.isfile(p) and os.access(p, os.X_OK) else None


class SetupGUI:
    def __init__(self, root):
        self.root = root
        root.title("Aether Setup — More with less")
        root.geometry("720x560")
        root.configure(bg=BG)

        tk.Label(root, text="Aether Setup", font=("TkDefaultFont", 16, "bold"),
                 bg=BG, fg=FG).pack(pady=(12, 2))
        tk.Label(root, text="This wizard installs the toolchain, builds the compiler, "
                            "and launches the editor.", bg=BG, fg=FG).pack()

        self.checks_frame = tk.Frame(root, bg=BG)
        self.checks_frame.pack(fill="x", padx=16, pady=10)
        self.check_labels = {}

        btns = tk.Frame(root, bg=BG)
        btns.pack(fill="x", padx=16)
        self.all_btn = ttk.Button(btns, text="Install everything missing",
                                  command=self.install_all)
        self.all_btn.pack(side="left", padx=4)
        self.install_btn = ttk.Button(btns, text="1. Install packages",
                                      command=self.install_packages)
        self.install_btn.pack(side="left", padx=4)
        self.build_btn = ttk.Button(btns, text="2. Build compiler",
                                    command=self.build_compiler)
        self.build_btn.pack(side="left", padx=4)
        self.shortcut_btn = ttk.Button(btns, text="3. Add shortcut",
                                       command=self.install_shortcut)
        self.shortcut_btn.pack(side="left", padx=4)
        self.launch_btn = ttk.Button(btns, text="4. Launch editor",
                                     command=self.launch_editor)
        self.launch_btn.pack(side="left", padx=4)

        self.log = tk.Text(root, bg="#111111", fg=FG, insertbackground=FG,
                           height=18, state="disabled", wrap="word")
        self.log.pack(fill="both", expand=True, padx=16, pady=10)
        self.log.tag_configure("ok", foreground=OK)
        self.log.tag_configure("bad", foreground=BAD)

        self.refresh_checks()

    def log_line(self, text, tag=None):
        def append():
            self.log.configure(state="normal")
            self.log.insert("end", text + "\n", tag or ())
            self.log.see("end")
            self.log.configure(state="disabled")
        self.root.after(0, append)

    def check_rows(self):
        if IS_WINDOWS:
            return [
                ("git", bool(which("git"))),
                ("MSYS2 (bash)", bool(msys_bash())),
                ("clang or gcc (MinGW64)", bool(which("clang") or which("gcc"))),
                ("linker (ld.lld or ld)", bool(which("ld.lld") or which("ld"))),
                ("cargo (Rust)", bool(which("cargo"))),
                ("aetherc built", bool(aetherc_path())),
            ]
        return [
            ("git", bool(which("git"))),
            ("clang or gcc", bool(which("clang") or which("gcc"))),
            ("linker (ld.lld or ld)", bool(which("ld.lld") or which("ld"))),
            ("cargo (Rust)", bool(which("cargo"))),
            ("aetherc built", bool(aetherc_path())),
        ]

    def refresh_checks(self):
        for w in self.checks_frame.winfo_children():
            w.destroy()
        for name, present in self.check_rows():
            color = OK if present else BAD
            mark = "[ok]" if present else "[missing]"
            tk.Label(self.checks_frame, text=f"{mark:10s} {name}",
                     bg=BG, fg=color, anchor="w", font=("TkFixedFont", 11)
                     ).pack(fill="x")

    def _run_thread(self, fn):
        threading.Thread(target=fn, daemon=True).start()

    def _stream(self, cmd, cwd=None, env=None):
        self.log_line("$ " + " ".join(cmd))
        proc = subprocess.Popen(cmd, cwd=cwd, env=env,
                                stdout=subprocess.PIPE,
                                stderr=subprocess.STDOUT, text=True)
        for line in proc.stdout:
            self.log_line(line.rstrip())
        proc.wait()
        return proc.returncode

    def _install_packages_work(self):
        """Install missing toolchain pieces. Returns True on success."""
        if IS_WINDOWS:
            return self._install_packages_windows()
        return self._install_packages_linux()

    def _install_packages_linux(self):
        ok = True
        if which("pkexec"):
            rc = self._stream(["pkexec", "apt-get", "install", "-y"] + APT_PACKAGES)
            ok = rc == 0
        else:
            cmd = "sudo apt-get install -y " + " ".join(APT_PACKAGES)
            self.log_line("No graphical sudo found. Run this in a terminal:", "bad")
            self.log_line("  " + cmd)
            ok = False
        if not which("cargo"):
            self.log_line("Installing Rust via rustup...")
            rc2 = self._stream(["bash", "-c",
                                "curl https://sh.rustup.rs -sSf | sh -s -- -y"])
            if rc2 == 0:
                self.log_line("Rust installed.", "ok")
            else:
                ok = False
        if ok:
            self.log_line("Packages installed.", "ok")
        return ok

    def _install_packages_windows(self):
        ok = True
        winget = which("winget")
        if not winget:
            self.log_line("winget not found. Install 'App Installer' from the "
                          "Microsoft Store, then retry.", "bad")
            return False
        if not which("git"):
            rc = self._stream([winget, "install", "--id", "Git.Git", "-e",
                               "--accept-source-agreements",
                               "--accept-package-agreements", "--silent"])
            ok = ok and rc in (0, -1978335189)
        if not msys_bash():
            self.log_line("Installing MSYS2 (this can take a few minutes)...")
            rc = self._stream([winget, "install", "--id", "MSYS2.MSYS2", "-e",
                               "--accept-source-agreements",
                               "--accept-package-agreements", "--silent"])
            ok = ok and rc in (0, -1978335189)
        bash = msys_bash()
        if bash:
            self.log_line("Installing MinGW64 toolchain via pacman...")
            env = dict(os.environ)
            env["MSYSTEM"] = "MINGW64"
            rc = self._stream(
                [bash, "-lc",
                 "pacman -Sy --noconfirm --needed " + " ".join(PACMAN_PACKAGES)],
                env=env)
            ok = ok and rc == 0
        else:
            self.log_line("MSYS2 still not found at " + MSYS_ROOT, "bad")
            ok = False
        if not which("cargo"):
            self.log_line("Installing Rust via rustup (windows-gnu)...")
            import urllib.request
            rustup = os.path.join(os.environ.get("TEMP", "."), "rustup-init.exe")
            try:
                urllib.request.urlretrieve("https://win.rustup.rs/x86_64", rustup)
            except OSError as e:
                self.log_line(f"Download failed: {e}", "bad")
                return False
            rc = self._stream([rustup, "-y",
                               "--default-host", "x86_64-pc-windows-gnu",
                               "--default-toolchain", "stable"])
            ok = ok and rc == 0
        if ok:
            self.log_line("Packages installed.", "ok")
        return ok

    def _build_compiler_work(self):
        cargo = which("cargo")
        if not cargo:
            self.log_line("Rust/cargo is not installed — install packages first.", "bad")
            return False
        self.log_line("Building aetherc (release)... this can take a few minutes.")
        env = dict(os.environ)
        rc = self._stream([cargo, "build", "--workspace", "--release"],
                          cwd=REPO_ROOT, env=env)
        if rc == 0:
            self.log_line("Compiler built: target/release/aetherc" + EXE, "ok")
            return True
        self.log_line("Build failed — see log above.", "bad")
        return False

    def install_all(self):
        def work():
            self.log_line("=== One-button setup: installing everything missing ===")
            needs_packages = any(
                not present for name, present in self.check_rows()
                if name != "aetherc built"
            )
            if needs_packages and not self._install_packages_work():
                self.log_line("Dependency install did not fully succeed; "
                              "see log above.", "bad")
                self.root.after(0, self.refresh_checks)
                return
            self.root.after(0, self.refresh_checks)
            if not aetherc_path() and not self._build_compiler_work():
                self.root.after(0, self.refresh_checks)
                return
            self.root.after(0, self.refresh_checks)
            self.root.after(0, self.install_shortcut)
            self.log_line("Setup complete — launching the editor.", "ok")
            self.root.after(0, self.launch_editor)
        self._run_thread(work)

    def install_packages(self):
        def work():
            self._install_packages_work()
            self.root.after(0, self.refresh_checks)
        self._run_thread(work)

    def build_compiler(self):
        if not which("cargo"):
            messagebox.showerror("Missing cargo",
                                 "Rust/cargo is not installed. Run step 1 first.")
            return
        def work():
            self._build_compiler_work()
            self.root.after(0, self.refresh_checks)
        self._run_thread(work)

    def install_shortcut(self):
        editor = os.path.join(REPO_ROOT, "tools", "aether_editor.py")
        if IS_WINDOWS:
            python = shutil.which("pythonw") or sys.executable or "python"
            start_menu = os.path.join(
                os.environ.get("APPDATA", os.path.expanduser("~")),
                "Microsoft", "Windows", "Start Menu", "Programs")
            lnk = os.path.join(start_menu, "Aether Editor.lnk")
            ps = (
                "$s = (New-Object -ComObject WScript.Shell).CreateShortcut('%s'); "
                "$s.TargetPath = '%s'; $s.Arguments = '\"%s\"'; "
                "$s.WorkingDirectory = '%s'; "
                "$s.Description = 'Multi-tab Aether editor (More with less)'; "
                "$s.Save()" % (lnk, python, editor, REPO_ROOT)
            )
            rc = subprocess.run(["powershell", "-NoProfile", "-Command", ps],
                                capture_output=True).returncode
            if rc == 0:
                self.log_line(f"Start-menu shortcut installed: {lnk}", "ok")
            else:
                self.log_line("Could not create Start-menu shortcut.", "bad")
            return
        python = sys.executable or "python3"
        os.makedirs(DESKTOP_DIR, exist_ok=True)
        with open(DESKTOP_FILE, "w", encoding="utf-8") as f:
            f.write(
                "[Desktop Entry]\n"
                "Type=Application\n"
                "Name=Aether Editor\n"
                "Comment=Multi-tab Aether editor (More with less)\n"
                f"Exec={python} {editor}\n"
                "Terminal=false\n"
                "Categories=Development;IDE;\n"
            )
        self.log_line(f"Menu shortcut installed: {DESKTOP_FILE}", "ok")

    def launch_editor(self):
        editor = os.path.join(REPO_ROOT, "tools", "aether_editor.py")
        subprocess.Popen([sys.executable or "python3", editor])
        self.log_line("Editor launched.", "ok")


def main():
    root = tk.Tk()
    SetupGUI(root)
    root.mainloop()


if __name__ == "__main__":
    main()
