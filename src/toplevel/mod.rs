pub mod eval;
pub mod command;

#[derive(Debug)]
pub enum TopLevelErr {
    BadVariableName,
    UnknownCommand,
    ExpectedAssignment,
}