pub mod build;

pub mod error {
    use std::path::PathBuf;

    #[derive(Debug, thiserror::Error)]
    pub enum ToolchainError {
        #[error("The source file ({0:?}) does not have a valid include dependency graph\n{1:?}")]
        /// contains optional diagnostics
        InvalidDependencyGraph(PathBuf, Option<String>),
        #[error("Failed to compile the source file ({0:?})")]
        CompilationError(PathBuf),
        #[error("Failed to link program:\n{0}")]
        /// contains diagnostics
        LinkError(String),
        #[error("Failed to create static library:\n{0}")]
        /// contains diagnostics
        ArchiveError(String),
        #[error("Failed to compile the source file ({file:?}):\n{diagnostics}")]
        CompilationWithDiagnosticsError { file: PathBuf, diagnostics: String },
        #[error("Invalid target triple: {0:?}")]
        InvalidTarget(Option<String>),
    }
}

pub mod traits {
    use crate::{
        compiledb::CompileDBEntry, dependency_map::DependencyMapEntry, error::ToolchainError,
    };
    use std::{
        ffi::OsStr,
        path::{Path, PathBuf},
    };

    #[async_trait::async_trait]
    pub trait Compiler: Send + Sync {
        fn with_working_directory<P: Into<PathBuf>>(self, working_directory: P) -> Self;

        fn with_arg<S>(self, arg: S) -> Self
        where
            S: AsRef<OsStr>;

        fn with_args<I, S>(self, args: I) -> Self
        where
            I: IntoIterator<Item = S>,
            S: AsRef<OsStr>;

        fn with_define<S>(self, arg: S) -> Self
        where
            S: AsRef<OsStr>;
        fn with_defines<I, S>(self, args: I) -> Self
        where
            I: IntoIterator<Item = S>,
            S: AsRef<OsStr>;

        fn with_include_dir<S>(self, arg: S) -> Self
        where
            S: AsRef<OsStr>;
        fn with_include_dirs<I, S>(self, args: I) -> Self
        where
            I: IntoIterator<Item = S>,
            S: AsRef<OsStr>;

        fn with_cpp_version<CppStandard>(self, cpp: CppStandard) -> Self
        where
            CppStandard: Into<String>;

        async fn compile<I, O>(
            &self,
            input: I,
            output: O,
        ) -> Result<CompileDBEntry, ToolchainError>
        where
            I: AsRef<Path> + Send + Sync,
            O: AsRef<Path> + Send + Sync;

        async fn target_triple(&self) -> Result<target_lexicon::Triple, ToolchainError>;

        async fn dependencies<P>(&self, input: P) -> Result<DependencyMapEntry, ToolchainError>
        where
            P: AsRef<Path> + Send + Sync;
    }

    #[async_trait::async_trait]
    pub trait Linker: Send + Sync {
        fn with_working_directory<P: Into<PathBuf>>(self, working_directory: P) -> Self;

        fn with_arg<S>(self, arg: S) -> Self
        where
            S: AsRef<OsStr>;
        fn with_args<I, S>(self, args: I) -> Self
        where
            I: IntoIterator<Item = S>,
            S: AsRef<OsStr>;

        fn with_lib<S>(self, arg: S) -> Self
        where
            S: AsRef<str>;
        fn with_libs<I, S>(self, args: I) -> Self
        where
            I: IntoIterator<Item = S>,
            S: AsRef<str>;

        fn with_lib_from_path<P>(self, arg: P) -> Self
        where
            P: AsRef<Path>;
        fn with_libs_from_path<I, P>(self, args: I) -> Self
        where
            I: IntoIterator<Item = P>,
            P: AsRef<Path>;

        fn with_stdlib(self) -> Self;
        fn with_shared(self) -> Self;
        fn with_output_file<P>(self, filename: P) -> Self
        where
            P: AsRef<Path>;

        async fn link<I, O, P>(&self, object_files: I, output: P) -> Result<(), ToolchainError>
        where
            P: AsRef<OsStr> + Send + Sync,
            I: IntoIterator<Item = O> + Send + Sync,
            O: AsRef<OsStr> + Send + Sync;
    }

    #[async_trait::async_trait]
    pub trait Archiver: Send + Sync {
        fn with_working_directory<P: Into<PathBuf>>(self, working_directory: P) -> Self;

        fn with_arg<S>(self, arg: S) -> Self
        where
            S: AsRef<OsStr>;
        fn with_args<I, S>(self, args: I) -> Self
        where
            I: IntoIterator<Item = S>,
            S: AsRef<OsStr>;

        async fn archive<I, P, O>(&self, input: I, output: P) -> Result<(), ToolchainError>
        where
            P: AsRef<OsStr> + Send + Sync,
            I: IntoIterator<Item = O> + Send + Sync,
            O: AsRef<OsStr> + Send + Sync;
    }

    pub trait Toolchain {
        type Compiler: Compiler + Send + Sync;
        type Linker: Linker + Send + Sync;
        type Archiver: Archiver + Send + Sync;

        fn compiler(&self) -> Self::Compiler;
        fn linker(&self) -> Self::Linker;
        fn libtool(&self) -> Self::Archiver;
    }
}

pub mod general {
    #![allow(dead_code)]

    #[derive(Debug)]
    pub enum CompilerFamily {
        Clang,
        GCC,
        MSVC,
        MSVCClang,
    }

    #[derive(Debug)]
    pub enum LinkerFamily {
        Lld,
        Ld,
        MSLink,
    }

    #[derive(Debug)]
    pub enum LibtoolFamily {
        Ar,
        DarwinLibtool,
        MsvcLib,
    }

    pub struct Compiler {}
    pub struct Linker {}
    pub struct Libtool {}
}

pub mod llvm {
    #![allow(dead_code)]

    use async_process::Command;
    use futures::TryFutureExt;
    use rayon::iter::IntoParallelIterator;
    use rayon::iter::ParallelIterator;
    use relative_path::RelativePathBuf;
    use serde::{Deserialize, Serialize};
    use target_lexicon::Triple;

    use crate::dependency_map::DependencyMapEntry;
    use crate::{compiledb::CompileDBEntry, error::ToolchainError, traits::*};
    use std::str::FromStr;
    use std::{
        collections::BTreeMap,
        ffi::{OsStr, OsString},
        path::{Path, PathBuf},
    };

    #[derive(Debug, Default)]
    pub struct OutputNameAffixes {
        pub binary_suffix: Option<String>,
        pub shared_prefix: Option<String>,
        pub shared_suffix: Option<String>,
        pub static_prefix: Option<String>,
        pub static_suffix: Option<String>,
    }

    impl From<target_lexicon::BinaryFormat> for OutputNameAffixes {
        fn from(format: target_lexicon::BinaryFormat) -> Self {
            match format {
                target_lexicon::BinaryFormat::Elf => Self::unix(),
                target_lexicon::BinaryFormat::Coff => Self::windows(),
                target_lexicon::BinaryFormat::Macho => Self::darwin(),
                _ => Self::default(),
            }
        }
    }

    impl OutputNameAffixes {
        pub fn windows() -> Self {
            Self {
                binary_suffix: Some(".exe".to_string()),
                shared_prefix: None,
                shared_suffix: Some(".dll".to_string()),
                static_prefix: None,
                static_suffix: Some(".lib".to_string()),
            }
        }

        pub fn unix() -> Self {
            Self {
                binary_suffix: None,
                shared_prefix: Some("lib".to_string()),
                shared_suffix: Some(".so".to_string()),
                static_prefix: Some("lib".to_string()),
                static_suffix: Some(".a".to_string()),
            }
        }

        pub fn darwin() -> Self {
            Self {
                binary_suffix: None,
                shared_prefix: Some("lib".to_string()),
                shared_suffix: Some(".dylib".to_string()),
                static_prefix: Some("lib".to_string()),
                static_suffix: Some(".a".to_string()),
            }
        }
    }

    #[derive(Debug, Deserialize, Serialize, Clone)]
    pub struct LLVM {
        compiler: PathBuf,
        #[serde(default)]
        default_cc_flags: Vec<OsString>,
        linker: PathBuf,
        #[serde(default)]
        default_ld_flags: Vec<OsString>,
        archiver: PathBuf,
        #[serde(default)]
        default_ar_flags: Vec<OsString>,
    }

    impl Default for LLVM {
        fn default() -> Self {
            let clang = which::which("clang").expect("clang");
            let mut ld_flags = Vec::new();
            ld_flags.push(OsString::from("-fuse-ld=lld"));

            Self {
                compiler: clang.clone(),
                default_cc_flags: Default::default(),
                linker: clang,
                default_ld_flags: ld_flags,
                #[cfg(not(target_os = "macos"))]
                archiver: which::which("llvm-ar").expect("llvm-ar"),
                #[cfg(target_os = "macos")]
                archiver: which::which("llvm-libtool-darwin").expect("llvm-libtool-darwin"),
                default_ar_flags: Default::default(),
            }
        }
    }

    pub struct Clang {
        working_directory: Option<PathBuf>,
        cc: PathBuf,
        flags: Vec<OsString>,
    }

    impl Clang {
        fn add_arg<S>(&mut self, arg: S)
        where
            S: AsRef<OsStr>,
        {
            self.flags.push(arg.as_ref().to_os_string());
        }

        fn working_directory(&self) -> PathBuf {
            self.working_directory
                .clone()
                .unwrap_or_else(|| std::env::current_dir().expect("cwd"))
        }

        pub async fn target_triple(&self) -> Result<target_lexicon::Triple, ToolchainError> {
            let mut cc = Command::new(&self.cc);
            let output = cc
                .args(&self.flags)
                .arg("--version")
                .output()
                .map_err(|e| {
                    log::error!("invoking compiler failed: {}", e);
                    ToolchainError::InvalidTarget(Some(e.to_string()))
                })
                .map_ok(|output| {
                    if output.status.success() {
                        Ok(String::from_utf8_lossy(&output.stdout).to_string())
                    } else {
                        Err(ToolchainError::InvalidTarget(Some(
                            String::from_utf8_lossy(&output.stderr).to_string(),
                        )))
                    }
                })
                .await?
                .map(|output| {
                    output
                        .lines()
                        .find_map(|line| line.strip_prefix("Target: "))
                        .map(|triple| Triple::from_str(triple))
                })?;

            Ok(output
                .ok_or(ToolchainError::InvalidTarget(None))?
                .map_err(|_| ToolchainError::InvalidTarget(None))?)
        }
    }

    #[derive(Debug)]
    pub enum LibtoolFamily {
        Ar,
        DarwinLibtool,
        MsvcLib,
    }

    impl Default for LibtoolFamily {
        fn default() -> Self {
            Self::Ar
        }
    }

    #[derive(Debug, Default)]
    pub struct Libtool {
        family: LibtoolFamily,
        working_directory: Option<PathBuf>,
        ar: PathBuf,
        flags: Vec<OsString>,
    }

    impl Libtool {
        pub fn new<P: AsRef<Path>>(path: P) -> Self {
            let family = if let Some(file_name) = path
                .as_ref()
                .file_name()
                .and_then(|file_name| file_name.to_str())
            {
                if file_name.contains("libtool") {
                    LibtoolFamily::DarwinLibtool
                } else if file_name.contains("lib") {
                    LibtoolFamily::MsvcLib
                } else if file_name.contains("ar") {
                    LibtoolFamily::Ar
                } else {
                    panic!("lib tool was none of \"libtool\", \"lib.exe\" or \"ar\".");
                }
            } else {
                LibtoolFamily::Ar
            };

            Self {
                family,
                ar: path.as_ref().to_owned(),
                ..Default::default()
            }
        }

        fn working_directory(&self) -> PathBuf {
            self.working_directory
                .clone()
                .unwrap_or_else(|| std::env::current_dir().expect("cwd"))
        }

        pub fn with_default_flags(self, flags: Vec<OsString>) -> Self {
            Self { flags, ..self }
        }

        pub fn with_cwd<P: AsRef<Path>>(self, path: P) -> Self {
            Self {
                working_directory: Some(path.as_ref().to_owned()),
                ..self
            }
        }

        fn add_arg<S>(&mut self, arg: S)
        where
            S: AsRef<OsStr>,
        {
            self.flags.push(arg.as_ref().to_os_string());
        }
    }

    pub struct Lld {
        working_directory: Option<PathBuf>,
        lld: PathBuf,
        flags: Vec<OsString>,
    }

    impl Lld {
        pub fn add_arg<S>(&mut self, arg: S)
        where
            S: AsRef<OsStr>,
        {
            self.flags.push(arg.as_ref().to_owned());
        }

        fn working_directory(&self) -> PathBuf {
            self.working_directory
                .clone()
                .unwrap_or_else(|| std::env::current_dir().expect("cwd"))
        }
    }

    impl Toolchain for LLVM {
        type Compiler = Clang;
        type Linker = Lld;
        type Archiver = Libtool;

        fn compiler(&self) -> Self::Compiler {
            Clang {
                working_directory: None,
                cc: self.compiler.clone(),
                flags: self.default_cc_flags.clone(),
            }
        }

        fn linker(&self) -> Self::Linker {
            Lld {
                working_directory: None,
                lld: self.compiler.clone(),
                flags: self.default_ld_flags.clone(),
            }
        }

        fn libtool(&self) -> Self::Archiver {
            Libtool::new(&self.archiver).with_default_flags(self.default_ar_flags.clone())
        }
    }

    #[async_trait::async_trait]
    impl Compiler for Clang {
        fn with_arg<S>(mut self, arg: S) -> Self
        where
            S: AsRef<OsStr>,
        {
            self.add_arg(arg);
            self
        }

        fn with_args<I, S>(mut self, args: I) -> Self
        where
            I: IntoIterator<Item = S>,
            S: AsRef<OsStr>,
        {
            for arg in args {
                self.add_arg(arg);
            }
            self
        }

        fn with_define<S>(mut self, arg: S) -> Self
        where
            S: AsRef<OsStr>,
        {
            self.add_arg("-D");
            self.add_arg(arg.as_ref().to_owned());
            self
        }

        fn with_defines<I, S>(mut self, args: I) -> Self
        where
            I: IntoIterator<Item = S>,
            S: AsRef<OsStr>,
        {
            for arg in args {
                self.add_arg("-D".to_string());
                self.add_arg(arg);
            }
            self
        }

        fn with_include_dir<S>(mut self, arg: S) -> Self
        where
            S: AsRef<OsStr>,
        {
            self.add_arg("-I".to_string());
            self.add_arg(arg);
            self
        }

        fn with_include_dirs<I, S>(mut self, args: I) -> Self
        where
            I: IntoIterator<Item = S>,
            S: AsRef<OsStr>,
        {
            for arg in args {
                self.add_arg("-I".to_string());
                self.add_arg(arg);
            }
            self
        }

        async fn compile<I, O>(&self, input: I, output: O) -> Result<CompileDBEntry, ToolchainError>
        where
            I: AsRef<Path> + Send + Sync,
            O: AsRef<Path> + Send + Sync,
        {
            log::debug!(
                "cc: {:?} -> {:?} {:?}",
                input.as_ref(),
                output.as_ref(),
                self.flags
            );

            let mut cc = Command::new(&self.cc);

            cc.args(&self.flags)
                .current_dir(self.working_directory())
                .arg("-o")
                .arg(output.as_ref())
                .arg("-c")
                .arg(input.as_ref());

            let input_file = input.as_ref().to_owned();

            cc.output()
                .map_err(|_| ToolchainError::CompilationError(input.as_ref().to_owned()))
                .and_then(|output| async move {
                    match output.status.success() {
                        true => Ok(()),
                        false => Err(ToolchainError::CompilationWithDiagnosticsError {
                            file: input_file,
                            diagnostics: String::from_utf8_lossy(&output.stderr).to_string(),
                        }),
                    }
                })
                .await
                .map(|_| {
                    let input_string = input.as_ref().to_string_lossy().to_string();

                    CompileDBEntry {
                        directory: self
                            .working_directory
                            .as_ref()
                            .map(|cwd| cwd.to_string_lossy().to_string()),
                        file: input_string.clone(),
                        arguments: None,
                        command: Some(format!(
                            "{} {} -o {} -c {}",
                            self.cc.to_string_lossy().to_string(),
                            self.flags
                                .iter()
                                .map(|s| s.to_string_lossy().to_string())
                                .collect::<Vec<_>>()
                                .join(" "),
                            output.as_ref().to_string_lossy().to_string(),
                            input_string,
                        )),
                        output: Some(output.as_ref().to_string_lossy().to_string()),
                    }
                })
        }

        async fn dependencies<P>(&self, input: P) -> Result<DependencyMapEntry, ToolchainError>
        where
            P: AsRef<Path> + Send + Sync,
        {
            let input_path = input.as_ref().to_owned();

            let mut cc = Command::new(&self.cc);

            cc.args(&self.flags)
                .current_dir(self.working_directory())
                .args(["-MM", "-MF", "-"])
                .arg(input.as_ref())
                .output()
                .map_err(|_| {
                    ToolchainError::InvalidDependencyGraph(
                        input.as_ref().to_owned(),
                        Some("clang failed to start".to_owned()),
                    )
                })
                .and_then(|output| async move {
                    match output.status.success() {
                        true => Ok(String::from_utf8_lossy(&output.stdout).to_string()),
                        false => Err(ToolchainError::InvalidDependencyGraph(
                            input_path,
                            Some(String::from_utf8_lossy(&output.stderr).to_string()),
                        )),
                    }
                })
                .await
                .map(|stdout| {
                    stdout
                        .replace("\n", " ")
                        .replace("\r", " ")
                        .replace("\r\n", " ")
                })
                .map(|output| {
                    let mut components = output.split_whitespace().skip(1).peekable();

                    let mut out = Vec::new();
                    while components.peek().is_some() {
                        out.push({
                            let mut out = String::new();
                            while let Some(comp) = components.next() {
                                if !comp.ends_with("\\") {
                                    out.push_str(comp);
                                    break;
                                } else {
                                    out.push_str(
                                        &comp
                                            .chars()
                                            .take(comp.chars().count() - 1)
                                            .chain(std::iter::once(' '))
                                            .collect::<String>(),
                                    );
                                }
                            }
                            out
                        });
                    }
                    out.into_iter()
                        .map(|line| PathBuf::from(&line))
                        .chain(std::iter::once(input.as_ref().to_owned()))
                        .collect::<Vec<_>>()
                })
                .and_then(|lines| {
                    lines
                        .into_par_iter()
                        .map(|path| {
                            let path = if path.is_absolute() {
                                path
                            } else {
                                RelativePathBuf::from_path(&path)
                                    .unwrap()
                                    .to_logical_path(self.working_directory())
                            };

                            path.metadata()
                                .and_then(|meta| meta.modified())
                                .map(|modified| (path, modified))
                        })
                        .collect::<std::io::Result<BTreeMap<_, _>>>()
                        .map_err(|_| {
                            ToolchainError::InvalidDependencyGraph(
                                input.as_ref().to_owned(),
                                Some(
                                    "failed to get last modified times for all dependencies"
                                        .to_owned(),
                                ),
                            )
                        })
                })
                .map(|dependencies| DependencyMapEntry::new(input.as_ref(), dependencies))
        }

        fn with_cpp_version<CppStandard>(mut self, cpp: CppStandard) -> Self
        where
            CppStandard: Into<String>,
        {
            self.add_arg(format!("-std={}", cpp.into()));
            self
        }

        fn with_working_directory<P: Into<PathBuf>>(self, working_directory: P) -> Self {
            Self {
                working_directory: Some(working_directory.into()),
                ..self
            }
        }

        async fn target_triple(&self) -> Result<target_lexicon::Triple, ToolchainError> {
            Self::target_triple(self).await
        }
    }

    #[async_trait::async_trait]
    impl Linker for Lld {
        fn with_working_directory<P: Into<PathBuf>>(self, working_directory: P) -> Self {
            Self {
                working_directory: Some(working_directory.into()),
                ..self
            }
        }

        fn with_arg<S>(mut self, arg: S) -> Self
        where
            S: AsRef<OsStr>,
        {
            self.add_arg(arg);
            self
        }

        fn with_args<I, S>(mut self, args: I) -> Self
        where
            I: IntoIterator<Item = S>,
            S: AsRef<OsStr>,
        {
            for arg in args {
                self.add_arg(arg);
            }
            self
        }

        fn with_lib<S>(mut self, arg: S) -> Self
        where
            S: AsRef<str>,
        {
            let mut lib = OsString::from("-l");
            lib.push(arg.as_ref());
            self.add_arg(&lib);
            self
        }

        fn with_libs<I, S>(mut self, args: I) -> Self
        where
            I: IntoIterator<Item = S>,
            S: AsRef<str>,
        {
            for arg in args {
                let mut lib = OsString::from("-l");
                lib.push(arg.as_ref());
                self.add_arg(&lib);
            }
            self
        }

        fn with_lib_from_path<P>(mut self, arg: P) -> Self
        where
            P: AsRef<Path>,
        {
            self.add_arg(arg.as_ref().parent().expect("library path"));
            let mut lib = OsString::from("-l");
            lib.push(arg.as_ref().file_name().expect("library name"));
            self.add_arg(&lib);
            self
        }
        fn with_libs_from_path<I, P>(mut self, args: I) -> Self
        where
            I: IntoIterator<Item = P>,
            P: AsRef<Path>,
        {
            for arg in args {
                self.add_arg("-L");
                self.add_arg(arg.as_ref().parent().expect("library path"));
                let mut lib = OsString::from("-l");
                lib.push(arg.as_ref().file_name().expect("library name"));
                self.add_arg(&lib);
            }
            self
        }

        fn with_stdlib(mut self) -> Self {
            self.add_arg("-lstdc++");
            self
        }

        fn with_shared(mut self) -> Self {
            self.add_arg("-shared");
            self
        }

        fn with_output_file<P>(mut self, filename: P) -> Self
        where
            P: AsRef<Path>,
        {
            self.add_arg("-o");
            self.add_arg(filename.as_ref().as_os_str());
            self
        }

        async fn link<I, O, P>(&self, object_files: I, output: P) -> Result<(), ToolchainError>
        where
            P: AsRef<OsStr> + Send + Sync,
            I: IntoIterator<Item = O> + Send + Sync,
            O: AsRef<OsStr> + Send + Sync,
        {
            log::debug!("ld: {:?} {:?}", self.lld, self.flags);

            let mut lld = Command::new(&self.lld);
            lld.current_dir(self.working_directory())
                .args(&self.flags)
                .arg("-o")
                .arg(output)
                .args(object_files);

            lld.output()
                .map_err(|_| ToolchainError::LinkError("failed to invoke linker".to_owned()))
                .and_then(|output| async move {
                    match output.status.success() {
                        true => Ok(()),
                        false => Err(ToolchainError::LinkError(
                            String::from_utf8_lossy(&output.stderr).to_string(),
                        )),
                    }
                })
                .await
        }
    }

    #[async_trait::async_trait]
    impl Archiver for Libtool {
        fn with_working_directory<P: Into<PathBuf>>(self, working_directory: P) -> Self {
            Self {
                working_directory: Some(working_directory.into()),
                ..self
            }
        }

        fn with_arg<S>(mut self, arg: S) -> Self
        where
            S: AsRef<OsStr>,
        {
            self.add_arg(arg);
            self
        }

        fn with_args<I, S>(mut self, args: I) -> Self
        where
            I: IntoIterator<Item = S>,
            S: AsRef<OsStr>,
        {
            for arg in args {
                self.add_arg(arg);
            }
            self
        }

        async fn archive<I, P, O>(&self, inputs: I, output: P) -> Result<(), ToolchainError>
        where
            P: AsRef<OsStr> + Send + Sync,
            I: IntoIterator<Item = O> + Send + Sync,
            O: AsRef<OsStr> + Send + Sync,
        {
            log::debug!("libtool: {:?}", self.flags);

            let mut libtool = Command::new(&self.ar);
            libtool.current_dir(self.working_directory());

            match self.family {
                LibtoolFamily::Ar => {
                    libtool.arg("rcsT").arg(&output).args(inputs);
                }
                LibtoolFamily::MsvcLib => {
                    let mut out_name = OsString::from("/NAME");
                    out_name.push(&output);

                    libtool.arg("/NOLOGO").arg(&out_name).args(inputs);
                }
                LibtoolFamily::DarwinLibtool => {
                    libtool.arg("-static").arg("-o").arg(output).args(inputs);
                }
            }

            libtool
                .output()
                .map_err(|_| {
                    ToolchainError::ArchiveError(format!(
                        "failed to invoke archiver {:?}",
                        &self.ar
                    ))
                })
                .and_then(|output| async move {
                    match output.status.success() {
                        true => Ok(()),
                        false => Err(ToolchainError::ArchiveError(
                            String::from_utf8_lossy(&output.stderr).to_string(),
                        )),
                    }
                })
                .await
        }
    }
}

pub mod dependency_map {
    use std::{
        borrow::Borrow,
        collections::{BTreeMap, HashSet},
        hash::Hash,
        io::{Read, Write},
        path::{Path, PathBuf},
        time::SystemTime,
    };

    use serde::{Deserialize, Serialize};

    pub type DependencyMap = HashSet<DependencyMapEntry>;

    pub trait DependencyMapExt: Sized {
        fn from_path<P>(path: P) -> anyhow::Result<Self>
        where
            P: AsRef<Path>;
        fn to_path<P>(self, path: P) -> anyhow::Result<()>
        where
            P: AsRef<Path>;

        fn contains_eq_entry(&self, entry: &DependencyMapEntry) -> bool;
    }

    impl DependencyMapExt for DependencyMap {
        fn from_path<P>(path: P) -> anyhow::Result<Self>
        where
            P: AsRef<Path>,
        {
            let mut file = std::fs::File::open(&path)?;

            let mut contents = Vec::new();
            file.read_to_end(&mut contents)?;
            Ok(bincode::deserialize::<Self>(&contents)?)
        }

        fn to_path<P>(self, path: P) -> anyhow::Result<()>
        where
            P: AsRef<Path>,
        {
            let mut file = std::fs::File::create(&path)?;
            file.write_all(&bincode::serialize(&self)?)?;
            Ok(())
        }

        fn contains_eq_entry(&self, entry: &DependencyMapEntry) -> bool {
            self.get(entry).map(|ele| ele == entry).unwrap_or(false)
        }
    }

    #[derive(Debug, Deserialize, Serialize, PartialEq, Eq, Clone)]
    pub struct DependencyMapEntry {
        file: PathBuf,
        dependencies: BTreeMap<PathBuf, SystemTime>,
    }

    impl Borrow<Path> for DependencyMapEntry {
        fn borrow(&self) -> &Path {
            self.file.as_path()
        }
    }

    impl Hash for DependencyMapEntry {
        fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
            self.file.hash(state);
        }
    }

    impl DependencyMapEntry {
        pub fn new<P>(file: P, dependencies: BTreeMap<PathBuf, SystemTime>) -> Self
        where
            P: Into<PathBuf>,
        {
            Self {
                file: file.into(),
                dependencies,
            }
        }

        pub fn new_from_files<P, I, PI>(for_file: P, with_dependencies: I) -> anyhow::Result<Self>
        where
            I: IntoIterator<Item = PI>,
            P: AsRef<Path>,
            PI: AsRef<Path>,
        {
            let dependencies = with_dependencies
                .into_iter()
                .map(|p| {
                    p.as_ref()
                        .metadata()
                        .and_then(|meta| meta.modified())
                        .map(|time| (p.as_ref().to_owned(), time))
                })
                .collect::<std::io::Result<BTreeMap<PathBuf, SystemTime>>>()?;

            Ok(Self {
                file: for_file.as_ref().to_path_buf(),
                dependencies,
            })
        }
    }
}

pub mod include_dependencies {
    use anyhow::Result;
    use std::{
        collections::BTreeMap,
        path::{Path, PathBuf},
        time::SystemTime,
    };

    use serde::{Deserialize, Serialize};

    #[derive(Debug, PartialEq, Eq, Deserialize, Serialize, Clone)]
    pub struct IncludeDependencies {
        pub path: PathBuf,
        pub dependencies: BTreeMap<PathBuf, SystemTime>,
    }

    impl IncludeDependencies {
        pub fn new<P, I, PI>(for_file: P, with_dependencies: I) -> Result<Self>
        where
            I: IntoIterator<Item = PI>,
            P: AsRef<Path>,
            PI: AsRef<Path>,
        {
            let dependencies = with_dependencies
                .into_iter()
                .map(|p| {
                    p.as_ref()
                        .metadata()
                        .and_then(|meta| meta.modified())
                        .map(|time| (p.as_ref().to_owned(), time))
                })
                .collect::<std::io::Result<BTreeMap<PathBuf, SystemTime>>>()?;

            Ok(Self {
                path: for_file.as_ref().to_path_buf(),
                dependencies,
            })
        }
    }
}

pub mod compiledb {
    #![allow(dead_code)]

    use std::{
        collections::HashSet,
        hash::Hash,
        io::{Read, Write},
        path::Path,
    };

    use serde::{Deserialize, Serialize};

    use anyhow::Result;

    #[derive(Default, Debug, Deserialize, Serialize, Clone)]
    pub struct CompileDB(pub HashSet<CompileDBEntry>);

    impl FromIterator<CompileDBEntry> for CompileDB {
        fn from_iter<T: IntoIterator<Item = CompileDBEntry>>(iter: T) -> Self {
            Self(iter.into_iter().collect::<HashSet<_>>())
        }
    }

    impl CompileDB {
        pub fn from(hashset: HashSet<CompileDBEntry>) -> Self {
            Self(hashset)
        }

        pub fn to_path<P>(&self, path: P) -> Result<()>
        where
            P: AsRef<Path>,
        {
            let mut file = std::fs::File::create(&path)?;

            file.write_all(serde_json::to_string_pretty(self)?.as_bytes())?;

            Ok(())
        }

        pub fn from_path<P>(path: P) -> Result<Self>
        where
            P: AsRef<Path>,
        {
            let mut file = std::fs::File::open(&path)?;

            let mut content = String::new();

            file.read_to_string(&mut content)?;

            Ok(serde_json::from_str::<Self>(&content)?)
        }
    }

    /// An entry into the [Compilation Database](CompileDB)
    #[derive(Debug, Deserialize, Serialize, Clone)]
    pub struct CompileDBEntry {
        /// The main translation unit source processed by this compilation step.
        /// This is used by tools as the key into the compilation database. There
        /// can be multiple command objects for the same file, for example if the
        /// same source file is compiled with different configurations.
        pub file: String,

        /// The working directory of the compilation. All paths specified in the
        /// [command](CompileDBEntry::command) or [file](CompileDBEntry::file)
        /// fields must be either absolute or relative to this directory.
        pub directory: Option<String>,

        /// The compile command executed. After JSON unescaping, this must be a
        /// valid command to rerun the exact compilation step for the translation
        /// unit in the environment the build system uses. Parameters use shell
        /// quoting and shell escaping of quotes, with ‘"’ and ‘\’ being the only
        /// special characters. Shell expansion is not supported.
        #[serde(skip_serializing_if = "Option::is_none")]
        pub command: Option<String>,

        /// The compile command executed as list of strings. Either
        /// [arguments](CompileDBEntry::arguments) or
        /// [command](CompileDBEntry::command) is required.
        #[serde(skip_serializing_if = "Option::is_none")]
        pub arguments: Option<String>,

        /// The name of the output created by this compilation step. This field is
        /// optional. It can be used to distinguish different processing modes of
        /// the same input file.
        #[serde(skip_serializing_if = "Option::is_none")]
        pub output: Option<String>,
    }

    impl PartialEq for CompileDBEntry {
        fn eq(&self, other: &Self) -> bool {
            self.file == other.file
        }
    }

    impl Eq for CompileDBEntry {}

    impl Hash for CompileDBEntry {
        fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
            self.file.hash(state);
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn serialize() {
            let mut db = CompileDB::default();
            db.0.insert(CompileDBEntry {
                file: "main.cc".to_owned(),
                directory: Some("/home/user/project".to_owned()),
                command: Some("cc -std=c++20 -c main.cc  -o main.o".to_owned()),
                arguments: None,
                output: Some("main.o".to_owned()),
            });

            let string = serde_json::to_string(&db).expect("serialize compiledb");
            log::debug!("{}", &string);
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{
        io::Write,
        path::{Path, PathBuf},
        process::Command,
        sync::{Arc, Once},
    };

    use target_lexicon::{Architecture, OperatingSystem, Triple};
    use tempfile::TempDir;

    use crate::{
        build::BuildBuilder,
        compiledb::CompileDB,
        dependency_map::DependencyMap,
        llvm::LLVM,
        traits::{Archiver, Compiler, Linker, Toolchain},
    };

    const LIB_CC: &str = r#"
#include <iostream>
#include <string_view>

void greet(std::string_view name) {
std::cout << "Hello, " << name << "!\n";
}
"#;

    const CONSUMER_CC: &str = r#"
#include <iostream>
#include <string_view>

void greet(std::string_view name);

int main() {
greet("Rust");
return 0;
}
"#;

    const MAIN_CC: &str = r#"
#include <iostream>

int main() {
std::cout << "Hello, World!\n";
return 0;
}
"#;

    static LOGGER: Once = Once::new();

    fn init_logger() {
        LOGGER.call_once(|| {
            env_logger::init();
        })
    }

    fn temp_dir() -> anyhow::Result<TempDir> {
        Ok(tempfile::tempdir()?)
    }

    fn temp_cc(content: &str, name: &str, temp_path: &Path) -> anyhow::Result<PathBuf> {
        let path = temp_path.join(name);

        let mut main_cc = std::fs::File::create(&path)?;
        main_cc.write_all(content.as_bytes())?;

        Ok(path)
    }

    fn temp_main_cc(temp_path: &Path) -> anyhow::Result<PathBuf> {
        temp_cc(MAIN_CC, "main.cc", temp_path)
    }

    #[tokio::test]
    async fn build() {
        init_logger();

        let temp_dir = temp_dir().unwrap();
        let main_cc = temp_main_cc(temp_dir.path()).unwrap();

        let dependency_map = DependencyMap::default();
        let compiledb = CompileDB::default();

        let llvm = Arc::new(LLVM::default());
        let mut build = BuildBuilder::new(llvm)
            .with_sources(vec![main_cc])
            .with_compiledb(compiledb)
            .with_dependency_map(dependency_map)
            .with_cc_flags(vec!["-std=c++17".to_string()])
            .with_build_directory(temp_dir.path())
            .finish()
            .expect("builder");

        build.build("test").await.expect("build");

        for entry in walkdir::WalkDir::new(temp_dir.path())
            .into_iter()
            .filter_map(|e| e.ok())
        {
            log::debug!("{:?}", entry);
        }

        assert!(Command::new(temp_dir.path().join("test.exe"))
            .status()
            .unwrap()
            .success());
    }

    #[tokio::test]
    async fn compile() {
        init_logger();

        let temp_dir = temp_dir().unwrap();
        let main_cc = temp_main_cc(temp_dir.path()).unwrap();
        let main_o = main_cc.with_extension("o");

        let llvm = LLVM::default();

        let cc = llvm
            .compiler()
            .with_working_directory(temp_dir.path())
            .with_cpp_version("c++17");
        let _compiledb = cc.compile(main_cc, main_o).await.expect("compile");
    }

    #[tokio::test]
    async fn compile_and_archive() {
        init_logger();

        let temp_dir = temp_dir().unwrap();
        let main_cc = temp_main_cc(temp_dir.path()).unwrap();
        let main_o = main_cc.with_extension("o");
        let main_lib = main_cc.with_extension("lib");

        let llvm = LLVM::default();

        let cc = llvm
            .compiler()
            .with_working_directory(temp_dir.path())
            .with_cpp_version("c++17")
            .with_arg("-flto");
        let _compiledb = cc.compile(&main_cc, &main_o).await.expect("compile");

        llvm.libtool()
            .archive(&[&main_o], &main_lib)
            .await
            .expect("archive");
    }

    #[tokio::test]
    async fn target_triple() {
        init_logger();

        let llvm = LLVM::default();
        let cc_linux = llvm
            .compiler()
            .with_arg("--target=x86_64-linux-gnu")
            .with_cpp_version("c++17")
            .with_arg("-flto");

        let triple = cc_linux.target_triple().await.expect("triple");

        assert_eq!(triple.architecture, Architecture::X86_64);
        assert_eq!(triple.operating_system, OperatingSystem::Linux);

        let cc_host = llvm.compiler().with_cpp_version("c++17").with_arg("-flto");

        let triple = cc_host.target_triple().await.expect("triple");

        assert_eq!(triple, Triple::host());
    }

    #[tokio::test]
    async fn libtool_and_link() {
        init_logger();

        let temp_dir = temp_dir().unwrap();
        let lib_cc = temp_cc(LIB_CC, "lib.cc", temp_dir.path()).unwrap();
        let consumer_cc = temp_cc(CONSUMER_CC, "consumer.cc", temp_dir.path()).unwrap();

        let dependency_map = DependencyMap::default();
        let compiledb = CompileDB::default();

        let llvm = Arc::new(LLVM::default());

        let mut lib_build = BuildBuilder::new(llvm.clone())
            .with_sources(vec![lib_cc])
            .with_type(crate::build::Type::StaticLibrary)
            .with_compiledb(compiledb.clone())
            .with_dependency_map(dependency_map.clone())
            .with_cc_flags(vec!["-std=c++17".to_string()])
            .with_build_directory(temp_dir.path())
            .finish()
            .expect("builder");

        let lib = lib_build.build("greet").await.expect("build");

        let mut consumer_build = BuildBuilder::new(llvm.clone())
            .with_sources(vec![consumer_cc])
            .with_compiledb(compiledb)
            .with_dependency_map(dependency_map)
            .with_libraries(Some(lib))
            .with_cc_flags(vec!["-std=c++17".to_string()])
            .with_build_directory(temp_dir.path())
            .finish()
            .expect("builder");

        let bin = consumer_build.build("bin").await.expect("build");

        for entry in walkdir::WalkDir::new(temp_dir.path())
            .into_iter()
            .filter_map(|e| e.ok())
        {
            log::debug!("{:?}", entry);
        }

        assert!(Command::new(temp_dir.path().join(bin))
            .status()
            .unwrap()
            .success());
    }

    #[tokio::test]
    async fn compile_and_link() {
        init_logger();

        let temp_dir = temp_dir().unwrap();
        let main_cc = temp_main_cc(temp_dir.path()).unwrap();
        let main_o = main_cc.with_extension("o");
        let main_out = main_cc.with_extension("out");

        let llvm = LLVM::default();

        let cc = llvm
            .compiler()
            .with_working_directory(temp_dir.path())
            .with_cpp_version("c++17")
            .with_arg("-flto");
        let _compiledb = cc.compile(&main_cc, &main_o).await.expect("compile");

        llvm.linker()
            .with_stdlib()
            .with_arg("-flto")
            .link(&[&main_o], &main_out)
            .await
            .expect("link");

        assert!(Command::new(main_out).status().unwrap().success());
    }
}
