#!/usr/bin/env python3
"""Aether GUI setup wizard for Linux.

Checks for the required toolchain, builds the aetherc compiler with a live
progress log, installs an application-menu shortcut for the editor, and
launches the editor when done.

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
CARGO_BIN = os.path.expanduser("~/.cargo/bin")
APT_PACKAGES = ["git", "clang", "lld", "build-essential", "python3-tk"]
DESKTOP_DIR = os.path.expanduser("~/.local/share/applications")
DESKTOP_FILE = os.path.join(DESKTOP_DIR, "aether-editor.desktop")

BG = "#1e1e1e"
FG = "#d4d4d4"
OK = "#4ec9b0"
BAD = "#f48771"


def which(cmd):
    env_path = os.environ.get("PATH", "")
    if CARGO_BIN not in env_path.split(os.pathsep):
        os.environ["PATH"] = env_path + os.pathsep + CARGO_BIN
    return shutil.which(cmd)


def aetherc_path():
    p = os.path.join(REPO_ROOT, "target", "release", "aetherc")
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
        self.install_btn = ttk.Button(btns, text="1. Install missing packages",
                                      command=self.install_packages)
        self.install_btn.pack(side="left", padx=4)
        self.build_btn = ttk.Button(btns, text="2. Build compiler",
                                    command=self.build_compiler)
        self.build_btn.pack(side="left", padx=4)
        self.shortcut_btn = ttk.Button(btns, text="3. Add menu shortcut",
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

    def refresh_checks(self):
        for w in self.checks_frame.winfo_children():
            w.destroy()
        rows = [
            ("git", bool(which("git"))),
            ("clang or gcc", bool(which("clang") or which("gcc"))),
            ("linker (ld.lld or ld)", bool(which("ld.lld") or which("ld"))),
            ("cargo (Rust)", bool(which("cargo"))),
            ("aetherc built", bool(aetherc_path())),
        ]
        for name, present in rows:
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

    def install_packages(self):
        def work():
            missing_apt = [p for p in APT_PACKAGES]
            if which("pkexec"):
                rc = self._stream(["pkexec", "apt-get", "install", "-y"] + missing_apt)
            else:
                cmd = "sudo apt-get install -y " + " ".join(missing_apt)
                self.log_line("No graphical sudo found. Run this in a terminal:", "bad")
                self.log_line("  " + cmd)
                rc = 1
            if not which("cargo"):
                self.log_line("Installing Rust via rustup...")
                rc2 = self._stream(["bash", "-c",
                                    "curl https://sh.rustup.rs -sSf | sh -s -- -y"])
                if rc2 == 0:
                    self.log_line("Rust installed.", "ok")
            if rc == 0:
                self.log_line("Packages installed.", "ok")
            self.root.after(0, self.refresh_checks)
        self._run_thread(work)

    def build_compiler(self):
        if not which("cargo"):
            messagebox.showerror("Missing cargo",
                                 "Rust/cargo is not installed. Run step 1 first.")
            return
        def work():
            self.log_line("Building aetherc (release)... this can take a few minutes.")
            env = dict(os.environ)
            rc = self._stream([which("cargo"), "build", "--workspace", "--release"],
                              cwd=REPO_ROOT, env=env)
            if rc == 0:
                self.log_line("Compiler built: target/release/aetherc", "ok")
            else:
                self.log_line("Build failed — see log above.", "bad")
            self.root.after(0, self.refresh_checks)
        self._run_thread(work)

    def install_shortcut(self):
        python = sys.executable or "python3"
        editor = os.path.join(REPO_ROOT, "tools", "aether_editor.py")
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
