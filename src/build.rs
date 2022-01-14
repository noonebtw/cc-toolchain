#![allow(dead_code)]
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

use crate::llvm::OutputNameAffixes;
use crate::traits::{Archiver, Compiler, Linker};
use crate::{compiledb::CompileDB, traits::Toolchain};

use crate::dependency_map::{DependencyMap, DependencyMapExt};

#[derive(Debug, Clone, Copy)]
pub enum Type {
    Executable,
    SharedLibrary,
    StaticLibrary,
}

pub struct Build<T>
where
    T: Toolchain,
{
    /// toolchain used for compiling and linking the program
    toolchain: Arc<T>,
    /// List of all source files included in the program. These paths have to be
    /// either absolute or relative to [build_directory](Self::build_directory).
    source_files: Vec<PathBuf>,
    include_directories: Vec<PathBuf>,
    /// Top most directory of the project (in an alloy project this would the
    /// the same directory as the Alloy.toml file).
    build_directory: PathBuf,
    /// directory that the build files are written to (object files, dependency
    /// map, executable, etc.)
    output_directory: PathBuf,
    defines: Vec<String>,
    libraries: Vec<PathBuf>,
    cpp_standard: Option<String>,
    cc_flags: Vec<String>,
    ld_flags: Vec<String>,
    output_type: Type,
    /// clang-tools compile data base (compile_commands.json)
    compiledb: CompileDB,
    dependency_map: DependencyMap,
}

#[derive(Debug)]
pub struct BuildBuilder<T>
where
    T: Toolchain,
{
    toolchain: Option<Arc<T>>,
    source_files: Vec<PathBuf>,
    include_directories: Vec<PathBuf>,
    build_directory: Option<PathBuf>,
    output_directory: Option<PathBuf>,
    defines: Vec<String>,
    libraries: Vec<PathBuf>,
    cpp_standard: Option<String>,
    cc_flags: Vec<String>,
    ld_flags: Vec<String>,
    output_type: Type,
    compiledb_path: Option<PathBuf>,
    dependency_map_path: Option<PathBuf>,
    compiledb: Option<CompileDB>,
    dependency_map: Option<DependencyMap>,
}

impl<T> Default for BuildBuilder<T>
where
    T: Toolchain,
{
    fn default() -> Self {
        Self {
            toolchain: None,
            source_files: Default::default(),
            include_directories: Default::default(),
            build_directory: Default::default(),
            output_directory: Default::default(),
            defines: Default::default(),
            libraries: Default::default(),
            cc_flags: Default::default(),
            ld_flags: Default::default(),
            compiledb_path: None,
            dependency_map_path: None,
            compiledb: None,
            dependency_map: None,
            cpp_standard: None,
            output_type: Type::Executable,
        }
    }
}

impl<T> BuildBuilder<T>
where
    T: Toolchain,
{
    pub fn new(toolchain: Arc<T>) -> Self {
        Self {
            toolchain: Some(toolchain),
            ..Default::default()
        }
    }

    pub fn with_cpp_standard<S>(mut self, standard: S) -> Self
    where
        S: Into<String>,
    {
        self.cpp_standard = Some(standard.into());
        self
    }

    pub fn with_type(mut self, output_type: Type) -> Self {
        self.output_type = output_type;
        self
    }

    pub fn with_cc_flags<I>(mut self, cc_flags: I) -> Self
    where
        I: IntoIterator<Item = String>,
    {
        self.cc_flags.extend(cc_flags.into_iter());
        self
    }

    pub fn with_ld_flags<I>(mut self, ld_flags: I) -> Self
    where
        I: IntoIterator<Item = String>,
    {
        self.ld_flags.extend(ld_flags.into_iter());
        self
    }

    pub fn with_include_directories<I, P>(mut self, include_directories: I) -> Self
    where
        I: IntoIterator<Item = P>,
        P: Into<PathBuf>,
    {
        self.include_directories
            .extend(include_directories.into_iter().map(Into::into));
        self
    }

    pub fn with_libraries<I, P>(mut self, libraries: I) -> Self
    where
        I: IntoIterator<Item = P>,
        P: Into<PathBuf>,
    {
        self.libraries.extend(libraries.into_iter().map(Into::into));
        self
    }

    pub fn with_defines<I>(mut self, defines: I) -> Self
    where
        I: IntoIterator<Item = String>,
    {
        self.defines.extend(defines.into_iter());
        self
    }

    pub fn with_sources<I, P>(mut self, sources: I) -> Self
    where
        I: IntoIterator<Item = P>,
        P: Into<PathBuf>,
    {
        self.source_files
            .extend(sources.into_iter().map(Into::into));
        self
    }

    pub fn with_compiledb(self, compiledb: CompileDB) -> Self {
        Self {
            compiledb: Some(compiledb),
            ..self
        }
    }

    pub fn with_dependency_map(self, dependency_map: DependencyMap) -> Self {
        Self {
            dependency_map: Some(dependency_map),
            ..self
        }
    }

    pub fn with_dependency_map_path<P>(self, dependency_map_path: P) -> Self
    where
        P: Into<PathBuf>,
    {
        Self {
            dependency_map_path: Some(dependency_map_path.into()),
            ..self
        }
    }

    pub fn with_compiledb_path<P>(self, compiledb_path: P) -> Self
    where
        P: Into<PathBuf>,
    {
        Self {
            compiledb_path: Some(compiledb_path.into()),
            ..self
        }
    }

    pub fn with_output_directory<P>(self, output_directory: P) -> Self
    where
        P: Into<PathBuf>,
    {
        Self {
            output_directory: Some(output_directory.into()),
            ..self
        }
    }

    pub fn with_build_directory<P>(self, build_directory: P) -> Self
    where
        P: Into<PathBuf>,
    {
        Self {
            build_directory: Some(build_directory.into()),
            ..self
        }
    }

    pub fn finish(self) -> anyhow::Result<Build<T>> {
        let build_directory = self.build_directory.expect("build directory");
        let output_directory = self
            .output_directory
            .unwrap_or_else(|| build_directory.clone());

        let compiledb = match self.compiledb {
            Some(compiledb) => compiledb,
            None => match self.compiledb_path.as_ref() {
                Some(path) => CompileDB::from_path(path)?,
                None => CompileDB::default(),
            },
        };

        let dependency_map = match self.dependency_map {
            Some(dependency_map) => dependency_map,
            None => match self.dependency_map_path.as_ref() {
                Some(path) => DependencyMap::from_path(output_directory.join(path))?,
                None => DependencyMap::default(),
            },
        };

        anyhow::Ok(Build {
            toolchain: self.toolchain.expect("no toolchain specified"),
            source_files: self.source_files,
            include_directories: self.include_directories,
            build_directory,
            defines: self.defines,
            libraries: self.libraries,
            cpp_standard: self.cpp_standard,
            cc_flags: self.cc_flags,
            ld_flags: self.ld_flags,
            output_type: self.output_type,
            compiledb,
            dependency_map,
            output_directory,
        })
    }
}

impl<T> Build<T>
where
    T: Toolchain,
{
    async fn should_compile_file<P, C>(&self, file: P, cc: &C) -> bool
    where
        P: AsRef<std::path::Path> + Sync + Send,
        C: Compiler + Sync + Send,
    {
        if let Ok(dependencies) = cc.dependencies(file.as_ref()).await {
            if let Some(entry) = self.dependency_map.get(file.as_ref()) {
                return entry == &dependencies;
            }
        }

        return false;
    }

    pub fn map_output_name<S>(name: S, output_kind: Type, affixes: OutputNameAffixes) -> String
    where
        S: Into<String>,
    {
        let prefix = match output_kind {
            Type::Executable => None,
            Type::SharedLibrary => affixes.shared_prefix,
            Type::StaticLibrary => affixes.static_prefix,
        };
        let suffix = match output_kind {
            Type::Executable => affixes.binary_suffix,
            Type::SharedLibrary => affixes.shared_suffix,
            Type::StaticLibrary => affixes.static_suffix,
        };

        format!(
            "{}{}{}",
            prefix.unwrap_or_default(),
            name.into(),
            suffix.unwrap_or_default()
        )
    }

    pub async fn build<S>(&mut self, output: S) -> anyhow::Result<PathBuf>
    where
        S: Into<String>,
    {
        let cc = self
            .toolchain
            .compiler()
            .with_include_dirs(&self.include_directories)
            .with_defines(&self.defines)
            .with_args(&self.cc_flags);

        let triple = cc.target_triple();

        use futures::stream::StreamExt;

        let compiledb = Arc::new(Mutex::new(&mut self.compiledb));
        let dependency_map = Arc::new(Mutex::new(&mut self.dependency_map));

        let objects = futures::stream::iter(self.source_files.iter().cloned())
            .then(|source| async {
                let rel = relative_path::RelativePath::from_path(&source)
                    .map(|rel| rel.as_str())
                    .unwrap_or(&source.to_str().expect("path"));
                let digest = md5::compute(rel);
                let output = self.output_directory.join(format!("{:x}.o", digest));

                let dependencies = cc.dependencies(&source);

                dependencies.await.map(move |deps| (source, output, deps))
            })
            .then(|result| async {
                match result {
                    Ok((input, output, deps)) => {
                        let should_compile =
                            dependency_map.lock().unwrap().contains_eq_entry(&deps);

                        if !output.exists() || should_compile {
                            let compiledb_entry = cc.compile(input, &output).await?;

                            compiledb.lock().unwrap().0.replace(compiledb_entry);
                            dependency_map.lock().unwrap().replace(deps);
                        }

                        Ok(output)
                    }
                    Err(e) => Err(e),
                }
            })
            .collect::<Vec<_>>()
            .await
            .into_iter()
            .collect::<Result<Vec<_>, _>>()?;

        let affixes = triple
            .await
            .map(|a| OutputNameAffixes::from(a.binary_format))?;

        let output_path =
            self.output_directory
                .join(Self::map_output_name(output, self.output_type, affixes));

        match self.output_type {
            Type::Executable => {
                let ld = self
                    .toolchain
                    .linker()
                    .with_args(&self.ld_flags)
                    .with_libs_from_path(&self.libraries);

                ld.link(objects, &output_path).await?;
            }
            Type::SharedLibrary => {
                let ld = self
                    .toolchain
                    .linker()
                    .with_shared()
                    .with_args(&self.ld_flags)
                    .with_libs_from_path(&self.libraries);

                ld.link(objects, &output_path).await?;
            }
            Type::StaticLibrary => {
                let libtool = self.toolchain.libtool().with_args(&self.ld_flags);

                libtool.archive(objects, &output_path).await?;
            }
        }

        Ok(output_path)
    }
}
