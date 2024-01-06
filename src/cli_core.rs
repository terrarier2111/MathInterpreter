use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::marker::PhantomData;
use std::ops::Range;
use std::sync::Arc;

pub struct CLICore<C> {
    cmds: Arc<HashMap<String, Command<C>>>,
}

impl<C> CLICore<C> {

    pub fn new(commands: Vec<CommandBuilder<C>>) -> Self {
        let mut cmds = HashMap::new();

        for cmd in commands.into_iter() {
            cmds.insert(cmd.name.clone(), cmd.build());
        }
        Self {
            cmds: Arc::new(cmds),
        }
    }

    pub fn process(&self, ctx: &C, input: &str) -> Result<(), InputError> {
        let mut parts = input.split(" ").collect::<Vec<_>>();
        if parts.is_empty() {
            return Err(InputError::InputEmpty);
        }
        let raw_cmd = parts.remove(0).to_lowercase();
        match self.cmds.get(&raw_cmd) {
            None => Err(InputError::CommandNotFound { name: raw_cmd }),
            Some(cmd) => {
                let expected = cmd.params.as_ref().map(|all| all.inner.req.len()).unwrap_or(0);
                if expected > parts.len() {
                    return Err(InputError::ArgumentCnt {
                        name: raw_cmd,
                        expected,
                        got: parts.len(),
                    });
                }
                match cmd.cmd_impl.execute(ctx, &parts) {
                    Ok(_) => Ok(()),
                    Err(error) => Err(InputError::ExecError { name: raw_cmd, error })
                }
            }
        }
    }

    #[inline(always)]
    pub fn cmds(&self) -> &Arc<HashMap<String, Command<C>>> {
        &self.cmds
    }

}

pub enum InputError {
    ArgumentCnt {
        name: String,
        expected: usize,
        got: usize,
    },
    CommandNotFound {
        name: String,
    },
    InputEmpty,
    ExecError {
        name: String,
        error: anyhow::Error,
    },
}

impl Debug for InputError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            InputError::ArgumentCnt { name, expected, got } => f.write_str(format!("The command \"{}\" expects an argument count of {} but only {} arguments were found", name.as_str(), *expected, *got).as_str()),
            InputError::CommandNotFound { name } => f.write_str(format!("The command \"{}\" does not exist", name).as_str()),
            InputError::InputEmpty => f.write_str("The given input was found to be empty"),
            InputError::ExecError { name, error } => f.write_str(format!("There was an error executing the command \"{}\": \"{}\"", name, error).as_str()),
        }
    }
}

impl Display for InputError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}

impl Error for InputError {}

pub struct Command<C> {
    name: String,
    desc: Option<String>,
    params: Option<UsageBuilder<BuilderImmutable>>,
    aliases: Vec<String>,
    cmd_impl: Box<dyn CommandImpl<CTX=C>>,
}

impl<C> Command<C> {
    #[inline(always)]
    pub fn name(&self) -> &String {
        &self.name
    }

    #[inline(always)]
    pub fn desc(&self) -> &Option<String> {
        &self.desc
    }

    #[inline(always)]
    pub fn params(&self) -> &Option<UsageBuilder<BuilderImmutable>> {
        &self.params
    }
}

pub trait CommandImpl: Send + Sync {
    type CTX;

    fn execute(&self, ctx: &Self::CTX, input: &[&str]) -> anyhow::Result<()>;
}

pub struct CommandBuilder<C> {
    name: String,
    desc: Option<String>,
    params: Option<UsageBuilder<BuilderImmutable>>,
    aliases: Vec<String>,
    cmd_impl: Box<dyn CommandImpl<CTX=C>>,
}

impl<C> CommandBuilder<C> {
    pub fn new(name: &str, cmd_impl: impl CommandImpl<CTX=C> + 'static) -> Self {
        Self {
            name: name.to_lowercase(),
            desc: None,
            params: None,
            aliases: vec![],
            cmd_impl: Box::new(cmd_impl),
        }
    }

    pub fn desc<S: Into<String>>(mut self, desc: S) -> Self {
        self.desc = Some(desc.into());
        self
    }

    pub fn params(mut self, params: UsageBuilder<BuilderMutable>) -> Self {
        self.params = Some(params.finish());
        self
    }

    pub fn alias(mut self, alias: &str) -> Self {
        self.aliases.push(alias.to_lowercase());
        self
    }

    pub fn aliases(mut self, aliases: &[&str]) -> Self {
        let mut aliases = aliases
            .iter()
            .map(|alias| alias.to_lowercase())
            .collect::<Vec<_>>();
        self.aliases.append(&mut aliases);
        self
    }

    fn build(self) -> Command<C> {
        Command {
            name: self.name,
            desc: self.desc,
            params: self.params,
            aliases: self.aliases,
            cmd_impl: self.cmd_impl,
        }
    }
}

pub struct CommandParam {
    pub name: String,
    pub ty: CommandParamTy,
}

impl CommandParam {

    fn to_string(&self, indent: usize) -> String {
        format!("{}({})", self.name.as_str(), self.ty.to_string(indent))
    }

}

pub enum CommandParamTy {
    Int(CmdParamNumConstraints<usize>),
    Float(CmdParamDecimalConstraints<f64>),
    String(CmdParamStrConstraints),
    Enum(Vec<(&'static str, EnumVal)>),
}

impl CommandParamTy {
    pub fn to_string(&self, indents: usize) -> String {
        match self {
            CommandParamTy::Int(constraints) => match constraints {
                CmdParamNumConstraints::Range(range) => format!("int({} to {})", range.start, range.end),
                CmdParamNumConstraints::Variants(variants) => {
                    let mut finished = String::from("int(");
                    let mut variants = variants.iter();
                    if let Some(variant) = variants.next() {
                        finished.push_str(format!("{}", variant).as_str());
                        for variant in variants {
                            finished.push_str(format!(", {}", variant).as_str());
                        }
                    }
                    finished.push(')');
                    finished
                }
                CmdParamNumConstraints::None => String::from("int"),
            },
            CommandParamTy::Float(constraints) => match constraints {
                CmdParamDecimalConstraints::Range(range) => format!("decimal({} to {})", range.start, range.end),
                CmdParamDecimalConstraints::None => String::from("decimal"),
            },
            CommandParamTy::String(constraints) => match constraints {
                CmdParamStrConstraints::Range(range) => format!("string(length {} to {})", range.start, range.end),
                CmdParamStrConstraints::None => String::from("string"),
            },
            CommandParamTy::Enum(variants) => {
                let mut finished = String::from("variants:\r\n");
                for variant in variants.iter() {
                    finished.push_str(" ".repeat(indents).as_str());
                    finished.push_str("- \"");
                    finished.push_str(variant.0);
                    match &variant.1 {
                        EnumVal::Simple(ty) => {
                            finished.push_str("\"(");
                            finished.push_str(ty.to_string(indents + 1).as_str());
                            finished.push(')');
                        }
                        EnumVal::Complex(params) => {
                            finished.push_str("\": ");
                            finished.push_str(params.to_string(indents + 1).as_str());
                        }
                        EnumVal::None => {
                            finished.push('\"');
                        }
                    }
                    finished.push_str("\r\n");
                }
                if !variants.is_empty() {
                    finished.push_str(" ".repeat(indents).as_str());
                }
                finished
            },
        }
    }
}

pub enum EnumVal {
    Simple(CommandParamTy),
    Complex(UsageSubBuilder<BuilderMutable>),
    None,
}

pub enum CmdParamNumConstraints<T> {
    Range(Range<T>),
    Variants(Box<[T]>),
    None,
}

pub enum CmdParamDecimalConstraints<T> {
    Range(Range<T>),
    None,
}

pub enum CmdParamStrConstraints {
    Range(Range<usize>),
    None,
}

pub struct BuilderMutable;
pub struct BuilderImmutable;

struct InnerBuilder {
    prefix: Option<String>,
    req: Vec<CommandParam>,
    opt: Vec<CommandParam>,
    opt_prefixed: Vec<CommandParam>,
}

pub struct UsageBuilder<M = BuilderMutable> {
    inner: InnerBuilder,
    mutability: PhantomData<M>,
}

impl<'a> UsageBuilder<BuilderMutable> {
    pub fn new() -> Self {
        Self {
            inner: InnerBuilder {
                prefix: None,
                req: vec![],
                opt: vec![],
                opt_prefixed: vec![],
            },
            mutability: Default::default(),
        }
    }

    pub fn optional_prefixed_prefix(mut self, prefix: String) -> Self {
        self.inner.prefix = Some(prefix);
        self
    }

    pub fn required(mut self, param: CommandParam) -> Self {
        if !self.inner.opt.is_empty() {
            panic!("you can only append required parameters before any optional parameters get appended");
        }
        self.inner.req.push(param);
        self
    }

    pub fn optional(mut self, param: CommandParam) -> Self {
        self.inner.opt.push(param);
        self
    }

    pub fn optional_prefixed(mut self, param: CommandParam) -> Self {
        if self.inner.prefix.is_none() {
            panic!("a prefix has to be specified in order to add optional prefixed parameters");
        }
        self.inner.opt_prefixed.push(param);
        self
    }

    fn finish(self) -> UsageBuilder<BuilderImmutable> {
        UsageBuilder {
            inner: self.inner,
            mutability: Default::default(),
        }
    }
}

impl UsageBuilder<BuilderImmutable> {
    #[inline(always)]
    pub fn optional_prefixed_prefix(&self) -> &Option<String> {
        &self.inner.prefix
    }

    #[inline(always)]
    pub fn required(&self) -> &Vec<CommandParam> {
        &self.inner.req
    }

    #[inline(always)]
    pub fn optional(&self) -> &Vec<CommandParam> {
        &self.inner.opt
    }

    #[inline(always)]
    pub fn optional_prefixed(&self) -> &Vec<CommandParam> {
        &self.inner.opt_prefixed
    }
}

struct InnerSubBuilder {
    prefix: Option<String>,
    req: Vec<CommandParam>,
    opt_prefixed: Vec<CommandParam>,
}

pub struct UsageSubBuilder<M = BuilderMutable> {
    inner: InnerSubBuilder,
    mutability: PhantomData<M>,
}

impl<'a> UsageSubBuilder<BuilderMutable> {
    pub fn new() -> Self {
        Self {
            inner: InnerSubBuilder {
                prefix: None,
                req: vec![],
                opt_prefixed: vec![],
            },
            mutability: Default::default(),
        }
    }

    pub fn optional_prefixed_prefix(mut self, prefix: String) -> Self {
        self.inner.prefix = Some(prefix);
        self
    }

    pub fn required(mut self, param: CommandParam) -> Self {
        self.inner.req.push(param);
        self
    }

    pub fn optional_prefixed(mut self, param: CommandParam) -> Self {
        if self.inner.prefix.is_none() {
            panic!("a prefix has to be specified in order to add optional prefixed parameters");
        }
        self.inner.opt_prefixed.push(param);
        self
    }

    fn finish(self) -> UsageSubBuilder<BuilderImmutable> {
        UsageSubBuilder {
            inner: self.inner,
            mutability: Default::default(),
        }
    }
}

impl UsageSubBuilder<BuilderImmutable> {
    #[inline(always)]
    pub fn optional_prefixed_prefix(&self) -> &Option<String> {
        &self.inner.prefix
    }

    #[inline(always)]
    pub fn required(&self) -> &Vec<CommandParam> {
        &self.inner.req
    }

    #[inline(always)]
    pub fn optional_prefixed(&self) -> &Vec<CommandParam> {
        &self.inner.opt_prefixed
    }
}

impl<M> UsageSubBuilder<M> {

    fn to_string(&self, indents: usize) -> String {
        let mut finished = String::new();
        let mut req = self.inner.req.iter();
        if let Some(req_first) = req.next() {
            finished.push_str(req_first.to_string(indents).as_str());
            for req in req {
                finished.push(' ');
                finished.push_str(req.to_string(indents).as_str());
            }
        }
        let mut opt = self.inner.opt_prefixed.iter();
        if let Some(opt_first) = opt.next() {
            finished.push_str(self.inner.prefix.as_ref().unwrap().as_str());
            finished.push_str(opt_first.to_string(indents).as_str());
            for opt in opt {
                finished.push(' ');
                finished.push_str(self.inner.prefix.as_ref().unwrap().as_str());
                finished.push_str(opt.to_string(indents).as_str());
            }
        }
        finished
    }

}