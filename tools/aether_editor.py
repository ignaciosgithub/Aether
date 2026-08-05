#!/usr/bin/env python3
"""Multi-tab Tkinter editor for Aether (.ae) source files.

Features:
- Multiple tabs with new/open/save/save-as/close
- Aether syntax highlighting (keywords, types, builtins, strings, numbers, comments)
- Compile (Linux/Windows x86_64) via aetherc with error messages shown in the output panel
- Build & Run on Linux (assemble, link, execute) with program output/exit code

Usage: python3 tools/aether_editor.py [file.ae ...]
"""

import os
import re
import subprocess
import sys
import tempfile
import tkinter as tk
from tkinter import filedialog, font, messagebox, ttk

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

KEYWORDS = [
    "func", "pub", "return", "let", "if", "else", "while", "break",
    "continue", "struct", "static", "import", "try", "except", "throw",
    "true", "false",
]
TYPES = [
    "i32", "i64", "f32", "f64", "void", "any", "list", "vec", "Vec",
    "hlist", "HList", "string", "String",
]
BUILTINS = [
    "println", "readln", "to_int", "concat", "len", "spawn", "join",
    "destroy", "vec_new", "vec_push", "vec_pop", "vec_len", "vec_free",
    "hlist_new", "hlist_push", "hlist_len", "hlist_free", "str_len",
    "abs_i64", "abs_i32", "abs_f64", "abs_f32", "min_i64", "min_i32",
    "min_f64", "min_f32", "max_i64", "max_i32", "max_f64", "max_f32",
    "sqrt_f64", "sqrt_f32",
]

HIGHLIGHT_RULES = [
    ("comment", re.compile(r"//[^\n]*|#[^\n]*|/\*.*?\*/", re.S)),
    ("string", re.compile(r'"(?:\\.|[^"\\])*"')),
    ("keyword", re.compile(r"\b(?:%s)\b" % "|".join(KEYWORDS))),
    ("type", re.compile(r"\b(?:%s)\b" % "|".join(TYPES))),
    ("builtin", re.compile(r"\b(?:%s)\b" % "|".join(BUILTINS))),
    ("number", re.compile(r"\b\d+(?:\.\d+)?\b|-\d+(?:\.\d+)?\b")),
]

TAG_COLORS = {
    "keyword": "#c586c0",
    "type": "#4ec9b0",
    "builtin": "#dcdcaa",
    "string": "#ce9178",
    "number": "#b5cea8",
    "comment": "#6a9955",
    "error_line": "#5a1d1d",
}

EDITOR_BG = "#1e1e1e"
EDITOR_FG = "#d4d4d4"
EDITOR_CARET = "#ffffff"


def find_aetherc():
    for sub in ("target/release/aetherc", "target/debug/aetherc"):
        p = os.path.join(REPO_ROOT, sub)
        if os.path.isfile(p) and os.access(p, os.X_OK):
            return [p]
    return ["cargo", "run", "-q", "-p", "aetherc", "--"]


class EditorTab(ttk.Frame):
    def __init__(self, master, app, path=None):
        super().__init__(master)
        self.app = app
        self.path = path
        self.modified = False

        mono = font.nametofont("TkFixedFont").copy()
        mono.configure(size=11)

        self.linenums = tk.Text(
            self, width=4, padx=4, takefocus=0, bd=0, state="disabled",
            background="#252526", foreground="#858585", font=mono,
        )
        self.linenums.pack(side="left", fill="y")

        self.text = tk.Text(
            self, wrap="none", undo=True, font=mono,
            background=EDITOR_BG, foreground=EDITOR_FG,
            insertbackground=EDITOR_CARET, bd=0, padx=6,
        )
        yscroll = ttk.Scrollbar(self, orient="vertical", command=self._on_scroll)
        self.text.configure(yscrollcommand=self._on_yscroll)
        yscroll.pack(side="right", fill="y")
        self.text.pack(side="left", fill="both", expand=True)
        self._yscroll = yscroll

        for tag, color in TAG_COLORS.items():
            if tag == "error_line":
                self.text.tag_configure(tag, background=color)
            else:
                self.text.tag_configure(tag, foreground=color)

        self.text.bind("<<Modified>>", self._on_modified)
        self.text.bind("<KeyRelease>", lambda e: self._schedule_highlight())
        self._highlight_job = None

        if path and os.path.isfile(path):
            with open(path, "r", encoding="utf-8") as f:
                self.text.insert("1.0", f.read())
            self.text.edit_reset()
            self.text.edit_modified(False)
            self.modified = False
        self.highlight()
        self._update_linenums()

    def _on_scroll(self, *args):
        self.text.yview(*args)
        self.linenums.yview(*args)

    def _on_yscroll(self, first, last):
        self._yscroll.set(first, last)
        self.linenums.yview_moveto(first)

    def _on_modified(self, _event=None):
        if self.text.edit_modified():
            self.modified = True
            self.text.edit_modified(False)
            self.app.refresh_tab_title(self)
            self._update_linenums()

    def _schedule_highlight(self):
        if self._highlight_job is not None:
            self.after_cancel(self._highlight_job)
        self._highlight_job = self.after(150, self.highlight)

    def highlight(self):
        self._highlight_job = None
        src = self.text.get("1.0", "end-1c")
        for tag in TAG_COLORS:
            if tag != "error_line":
                self.text.tag_remove(tag, "1.0", "end")
        taken = []
        for tag, rx in HIGHLIGHT_RULES:
            for m in rx.finditer(src):
                if any(m.start() < e and m.end() > s for s, e in taken):
                    continue
                if tag in ("comment", "string"):
                    taken.append((m.start(), m.end()))
                self.text.tag_add(tag, f"1.0+{m.start()}c", f"1.0+{m.end()}c")

    def _update_linenums(self):
        lines = int(self.text.index("end-1c").split(".")[0])
        self.linenums.configure(state="normal")
        self.linenums.delete("1.0", "end")
        self.linenums.insert("1.0", "\n".join(str(i) for i in range(1, lines + 1)))
        self.linenums.configure(state="disabled")

    def mark_error_line(self, line):
        self.text.tag_remove("error_line", "1.0", "end")
        if line is not None:
            self.text.tag_add("error_line", f"{line}.0", f"{line}.end+1c")
            self.text.see(f"{line}.0")

    def clear_error_line(self):
        self.text.tag_remove("error_line", "1.0", "end")

    def title(self):
        name = os.path.basename(self.path) if self.path else "untitled.ae"
        return ("*" if self.modified else "") + name

    def get_source(self):
        return self.text.get("1.0", "end-1c")


class AetherEditor(tk.Tk):
    def __init__(self, paths):
        super().__init__()
        self.title("Aether Editor — More with less")
        self.geometry("1000x720")

        self._build_toolbar()

        self.notebook = ttk.Notebook(self)
        self.notebook.pack(fill="both", expand=True)

        out_frame = ttk.Frame(self)
        out_frame.pack(fill="x")
        ttk.Label(out_frame, text="Output").pack(anchor="w", padx=4)
        self.output = tk.Text(
            out_frame, height=9, state="disabled", wrap="word",
            background="#111111", foreground="#cccccc",
            font=font.nametofont("TkFixedFont"),
        )
        self.output.tag_configure("error", foreground="#f48771")
        self.output.tag_configure("ok", foreground="#89d185")
        self.output.pack(fill="x")

        if paths:
            for p in paths:
                self.add_tab(p)
        else:
            self.add_tab(None)

        self._bind_shortcuts()
        self.protocol("WM_DELETE_WINDOW", self.on_quit)

    def _build_toolbar(self):
        bar = ttk.Frame(self)
        bar.pack(fill="x")
        buttons = [
            ("New", self.new_tab),
            ("Open", self.open_file),
            ("Save", self.save_file),
            ("Save As", self.save_file_as),
            ("Close Tab", self.close_tab),
            ("Compile (Linux)", lambda: self.compile_current("linux")),
            ("Compile (Windows)", lambda: self.compile_current("windows")),
            ("Build && Run (Linux)", self.run_current),
        ]
        for label, cmd in buttons:
            ttk.Button(bar, text=label, command=cmd).pack(side="left", padx=2, pady=2)

    def _bind_shortcuts(self):
        self.bind("<Control-n>", lambda e: self.new_tab())
        self.bind("<Control-o>", lambda e: self.open_file())
        self.bind("<Control-s>", lambda e: self.save_file())
        self.bind("<Control-w>", lambda e: self.close_tab())
        self.bind("<F5>", lambda e: self.run_current())

    def current_tab(self):
        widget = self.notebook.select()
        if not widget:
            return None
        return self.nametowidget(widget)

    def add_tab(self, path):
        tab = EditorTab(self.notebook, self, path)
        self.notebook.add(tab, text=tab.title())
        self.notebook.select(tab)
        return tab

    def refresh_tab_title(self, tab):
        try:
            self.notebook.tab(tab, text=tab.title())
        except tk.TclError:
            pass

    def new_tab(self):
        self.add_tab(None)

    def open_file(self):
        path = filedialog.askopenfilename(
            initialdir=os.path.join(REPO_ROOT, "examples"),
            filetypes=[("Aether files", "*.ae"), ("All files", "*")],
        )
        if path:
            self.add_tab(path)

    def save_file(self):
        tab = self.current_tab()
        if tab is None:
            return None
        if tab.path is None:
            return self.save_file_as()
        with open(tab.path, "w", encoding="utf-8") as f:
            f.write(tab.get_source())
        tab.modified = False
        self.refresh_tab_title(tab)
        return tab.path

    def save_file_as(self):
        tab = self.current_tab()
        if tab is None:
            return None
        path = filedialog.asksaveasfilename(
            defaultextension=".ae",
            filetypes=[("Aether files", "*.ae"), ("All files", "*")],
        )
        if not path:
            return None
        tab.path = path
        return self.save_file()

    def close_tab(self):
        tab = self.current_tab()
        if tab is None:
            return
        if tab.modified and not messagebox.askyesno(
            "Unsaved changes", "Discard unsaved changes in %s?" % tab.title().lstrip("*")
        ):
            return
        self.notebook.forget(tab)
        if not self.notebook.tabs():
            self.add_tab(None)

    def on_quit(self):
        for widget in self.notebook.tabs():
            tab = self.nametowidget(widget)
            if tab.modified:
                if not messagebox.askyesno("Unsaved changes", "Quit and discard unsaved changes?"):
                    return
                break
        self.destroy()

    def log(self, text, tag=None):
        self.output.configure(state="normal")
        self.output.insert("end", text + "\n", tag or ())
        self.output.see("end")
        self.output.configure(state="disabled")

    def clear_log(self):
        self.output.configure(state="normal")
        self.output.delete("1.0", "end")
        self.output.configure(state="disabled")

    def _source_path_for_compile(self, tab):
        if tab.path and not tab.modified:
            return tab.path, None
        if tab.path:
            saved = self.save_file()
            return (saved, None) if saved else (None, None)
        tmp = tempfile.NamedTemporaryFile(
            mode="w", suffix=".ae", delete=False, encoding="utf-8"
        )
        tmp.write(tab.get_source())
        tmp.close()
        return tmp.name, tmp.name

    def _report_errors(self, tab, stderr_text):
        shown = False
        for line in stderr_text.splitlines():
            if line.strip():
                self.log(line, "error")
                shown = True
        m = re.search(r"line\s+(\d+)", stderr_text)
        tab.mark_error_line(int(m.group(1)) if m else None)
        if not shown:
            self.log("compile failed (no error message)", "error")

    def compile_current(self, target_os):
        tab = self.current_tab()
        if tab is None:
            return None
        self.clear_log()
        tab.clear_error_line()
        src, tmp = self._source_path_for_compile(tab)
        if src is None:
            return None
        base = os.path.splitext(os.path.basename(src))[0]
        out_dir = os.path.join(REPO_ROOT, "out", target_os)
        os.makedirs(out_dir, exist_ok=True)
        asm_path = os.path.join(out_dir, base + ".s")
        cmd = find_aetherc() + [src, "--arch", "x86_64", "--os", target_os, "-o", asm_path]
        self.log("$ " + " ".join(cmd))
        try:
            proc = subprocess.run(
                cmd, cwd=REPO_ROOT, capture_output=True, text=True, timeout=120
            )
        finally:
            if tmp:
                os.unlink(tmp)
        if proc.returncode != 0:
            self._report_errors(tab, proc.stderr or proc.stdout)
            return None
        self.log("compiled OK -> %s" % os.path.relpath(asm_path, REPO_ROOT), "ok")
        return asm_path

    def run_current(self):
        asm_path = self.compile_current("linux")
        if asm_path is None:
            return
        bin_path = os.path.splitext(asm_path)[0]
        link = os.path.join(REPO_ROOT, "scripts", "assemble_link.sh")
        cmd = ["bash", link, "x86_64-linux", asm_path, bin_path]
        self.log("$ " + " ".join(os.path.relpath(c, REPO_ROOT) if os.path.isabs(c) else c for c in cmd))
        proc = subprocess.run(cmd, cwd=REPO_ROOT, capture_output=True, text=True, timeout=120)
        if proc.returncode != 0:
            self._report_errors(self.current_tab(), proc.stderr or proc.stdout)
            return
        self.log("linked OK, running...", "ok")
        try:
            run = subprocess.run(
                [bin_path], cwd=REPO_ROOT, capture_output=True, text=True, timeout=30
            )
        except subprocess.TimeoutExpired:
            self.log("program timed out after 30s", "error")
            return
        if run.stdout:
            self.log(run.stdout.rstrip("\n"))
        if run.stderr:
            self.log(run.stderr.rstrip("\n"), "error")
        self.log("exit code: %d" % run.returncode, "ok" if run.returncode == 0 else "error")


def main():
    app = AetherEditor(sys.argv[1:])
    app.mainloop()


if __name__ == "__main__":
    main()
