use anyhow::{anyhow, Result};
use std::collections::{HashSet, HashMap};
use std::fs;
use std::path::{Path, PathBuf};

use crate::ast::{Module, Item};

fn parse_file(path: &Path) -> Result<Module> {
    let src = fs::read_to_string(path)?;
    let toks = crate::lexer::Lexer::tokenize(&src)?;
    crate::parser::Parser::parse(&toks)
}

fn merge_modules(mods: Vec<Module>) -> Result<Module> {
    let mut items = Vec::new();
    let mut seen_funcs: HashSet<String> = HashSet::new();
    for m in mods {
        for it in m.items {
            match &it {
                Item::Function(f) => {
                    if !seen_funcs.insert(f.name.clone()) {
                        return Err(anyhow!("duplicate function name '{}'", f.name));
                    }
                    items.push(it);
                }
                Item::Struct(_) | Item::Static(_) => {
                    items.push(it);
                }
                Item::Import(_) => {}
            }
        }
    }
    Ok(Module { items })
}

fn collect_public_items(module: Module) -> Module {
    let mut out = Module { items: Vec::new() };
    for it in module.items {
        match it {
            Item::Function(f) => {
                if f.is_pub {
                    out.items.push(Item::Function(f));
                }
            }
            Item::Struct(s) => {
                out.items.push(Item::Struct(s));
            }
            Item::Static(s) => {
                out.items.push(Item::Static(s));
            }
            Item::Import(_) => {}
        }
    }
    out
}

fn resolve_recursive(path: &Path, visiting: &mut HashSet<PathBuf>, cache: &mut HashMap<PathBuf, Module>) -> Result<Module> {
    let canon = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
    if visiting.contains(&canon) {
        return Err(anyhow!("circular import detected at {}", canon.display()));
    }
    if let Some(m) = cache.get(&canon) {
        return Ok(m.clone());
    }
    visiting.insert(canon.clone());
    let mut module = parse_file(path)?;
    let base_dir = path.parent().map(|p| p.to_path_buf()).unwrap_or_else(|| PathBuf::from("."));
    let mut resolved_children: Vec<Module> = Vec::new();

    let mut retained_items: Vec<Item> = Vec::new();
    for it in module.items.into_iter() {
        match it {
            Item::Import(s) => {
                let child_path = {
                    let p = Path::new(&s);
                    if p.is_absolute() {
                        p.to_path_buf()
                    } else {
                        base_dir.join(p)
                    }
                };
                let child_mod = resolve_recursive(&child_path, visiting, cache)?;
                let public_child = collect_public_items(child_mod);
                resolved_children.push(public_child);
            }
            other => retained_items.push(other),
        }
    }
    module = Module { items: retained_items };

    let mut merged = vec![merge_modules(resolved_children)?, module];
    let final_mod = merge_modules(merged)?;
    visiting.remove(&canon);
    cache.insert(canon.clone(), final_mod.clone());
    Ok(final_mod)
}

pub fn resolve_from_file(path: &Path) -> Result<Module> {
    let mut visiting: HashSet<PathBuf> = HashSet::new();
    let mut cache: HashMap<PathBuf, Module> = HashMap::new();
    resolve_recursive(path, &mut visiting, &mut cache)
}
