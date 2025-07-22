use std::fs;
use std::io::{self, Read, Write};

use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use prost::Message;

use crate::{OutputOptions, Visibility, format, format_with_options, parse};

#[derive(Parser)]
#[command(name = "substrait-explain")]
#[command(about = "A CLI for parsing and formatting Substrait query plans")]
#[command(version)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Commands,
}

impl Cli {
    pub fn run(self) -> Result<()> {
        match &self.command {
            Commands::Convert {
                input,
                output,
                from,
                to,
                show_literal_types,
                show_expression_types,
                verbose,
            } => {
                let reader = get_reader(input)
                    .with_context(|| format!("Failed to open input file: {input}"))?;
                let writer = get_writer(output)
                    .with_context(|| format!("Failed to create output file: {output}"))?;
                let options =
                    self.create_output_options(*show_literal_types, *show_expression_types);
                self.run_convert_with_io(reader, writer, from, to, &options, *verbose)
            }

            Commands::Validate {
                input,
                output,
                verbose,
            } => {
                let reader = get_reader(input)
                    .with_context(|| format!("Failed to open input file: {input}"))?;
                let writer = get_writer(output)
                    .with_context(|| format!("Failed to create output file: {output}"))?;
                self.run_validate_with_io(reader, writer, *verbose)
            }
        }
    }

    /// Run CLI with provided readers and writers for testing
    pub fn run_with_io<R: Read, W: Write>(&self, reader: R, writer: W) -> Result<()> {
        match &self.command {
            Commands::Convert {
                from,
                to,
                show_literal_types,
                show_expression_types,
                verbose,
                ..
            } => {
                let options =
                    self.create_output_options(*show_literal_types, *show_expression_types);
                self.run_convert_with_io(reader, writer, from, to, &options, *verbose)
            }

            Commands::Validate { verbose, .. } => {
                self.run_validate_with_io(reader, writer, *verbose)
            }
        }
    }

    fn create_output_options(
        &self,
        show_literal_types: bool,
        show_expression_types: bool,
    ) -> OutputOptions {
        let mut options = OutputOptions::default();

        if show_literal_types {
            options.literal_types = Visibility::Always;
        }

        if show_expression_types {
            options.fn_types = true;
        }

        options
    }

    fn run_convert_with_io<R: Read, W: Write>(
        &self,
        reader: R,
        writer: W,
        from: &Format,
        to: &Format,
        options: &OutputOptions,
        verbose: bool,
    ) -> Result<()> {
        // Read input based on format
        let plan = from.read_plan(reader).with_context(|| {
            format!(
                "Failed to parse input as {} format",
                format!("{from:?}").to_lowercase()
            )
        })?;

        // Write output based on format
        to.write_plan(writer, &plan, options, verbose)
            .with_context(|| {
                format!(
                    "Failed to write output as {} format",
                    format!("{to:?}").to_lowercase()
                )
            })?;

        if verbose {
            eprintln!("Successfully converted from {from:?} to {to:?}");
        }

        Ok(())
    }

    fn run_validate_with_io<R: Read, W: Write>(
        &self,
        reader: R,
        writer: W,
        verbose: bool,
    ) -> Result<()> {
        let input_text = read_text_input(reader)?;

        // Parse text to protobuf
        let plan =
            parse(&input_text).with_context(|| "Failed to parse input as Substrait text format")?;

        // Format back to text
        let (output_text, errors) = format(&plan);

        if verbose && !errors.is_empty() {
            eprintln!("Formatting warnings:");
            for error in errors {
                eprintln!("  {error}");
            }
        }

        write_text_output(writer, &output_text)?;

        if verbose {
            eprintln!("Successfully validated plan");
        }

        Ok(())
    }
}

#[derive(Subcommand)]
pub enum Commands {
    /// Convert between different Substrait plan formats
    ///
    /// Plan formats:
    ///   text     - Human-readable Substrait text format
    ///   json     - JSON serialized protobuf
    ///   yaml     - YAML serialized protobuf
    ///   protobuf - Binary protobuf format
    Convert {
        /// Input file (use - for stdin)
        #[arg(short, long, default_value = "-")]
        input: String,
        /// Output file (use - for stdout)
        #[arg(short, long, default_value = "-")]
        output: String,
        /// Input format: text, json, yaml, protobuf/proto/pb
        #[arg(short = 'f', long, default_value = "text")]
        from: Format,
        /// Output format: text, json, yaml, protobuf/proto/pb
        #[arg(short = 't', long, default_value = "text")]
        to: Format,
        /// Show literal types (text output only)
        #[arg(long)]
        show_literal_types: bool,
        /// Show expression types (text output only)
        #[arg(long)]
        show_expression_types: bool,
        /// Verbose output
        #[arg(short, long)]
        verbose: bool,
    },
    /// Validate text format by parsing and formatting (roundtrip test)
    Validate {
        /// Input file (use - for stdin)
        #[arg(short, long, default_value = "-")]
        input: String,
        /// Output file (use - for stdout)
        #[arg(short, long, default_value = "-")]
        output: String,
        /// Verbose output
        #[arg(short, long)]
        verbose: bool,
    },
}

#[derive(Clone, Debug)]
pub enum Format {
    Text,
    Json,
    Yaml,
    Protobuf,
}

impl std::str::FromStr for Format {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "text" => Ok(Format::Text),
            "json" => Ok(Format::Json),
            "yaml" => Ok(Format::Yaml),
            "protobuf" | "proto" | "pb" => Ok(Format::Protobuf),
            _ => Err(format!(
                "Invalid format: '{s}'. Supported formats: text, json, yaml, protobuf/proto/pb"
            )),
        }
    }
}

impl Format {
    pub fn read_plan<R: Read>(&self, reader: R) -> Result<substrait::proto::Plan> {
        match self {
            Format::Text => {
                let input_text = read_text_input(reader)?;
                Ok(parse(&input_text)?)
            }
            Format::Json => {
                #[cfg(feature = "serde")]
                {
                    let input_text = read_text_input(reader)?;
                    Ok(serde_json::from_str(&input_text)?)
                }
                #[cfg(not(feature = "serde"))]
                {
                    Err("JSON support requires the 'serde' feature. Install with: cargo install substrait-explain --features cli,serde".into())
                }
            }
            Format::Yaml => {
                #[cfg(feature = "serde")]
                {
                    let input_text = read_text_input(reader)?;
                    Ok(serde_yaml::from_str(&input_text)?)
                }
                #[cfg(not(feature = "serde"))]
                {
                    Err("YAML support requires the 'serde' feature. Install with: cargo install substrait-explain --features cli,serde".into())
                }
            }
            Format::Protobuf => {
                let input_bytes = read_binary_input(reader)?;
                Ok(substrait::proto::Plan::decode(&input_bytes[..])?)
            }
        }
    }

    pub fn write_plan<W: Write>(
        &self,
        writer: W,
        plan: &substrait::proto::Plan,
        options: &OutputOptions,
        verbose: bool,
    ) -> Result<()> {
        match self {
            Format::Text => {
                let (text, errors) = format_with_options(plan, options);

                if verbose && !errors.is_empty() {
                    eprintln!("Formatting warnings:");
                    for error in errors {
                        eprintln!("  {error}");
                    }
                }

                write_text_output(writer, &text)?;
            }
            Format::Json => {
                #[cfg(feature = "serde")]
                {
                    let json = serde_json::to_string_pretty(plan)?;
                    write_text_output(writer, &json)?;
                }
                #[cfg(not(feature = "serde"))]
                {
                    return Err("JSON support requires the 'serde' feature. Install with: cargo install substrait-explain --features cli,serde".into());
                }
            }
            Format::Yaml => {
                #[cfg(feature = "serde")]
                {
                    let yaml = serde_yaml::to_string(plan)?;
                    write_text_output(writer, &yaml)?;
                }
                #[cfg(not(feature = "serde"))]
                {
                    return Err("YAML support requires the 'serde' feature. Install with: cargo install substrait-explain --features cli,serde".into());
                }
            }
            Format::Protobuf => {
                let bytes = plan.encode_to_vec();
                write_binary_output(writer, &bytes)?;
            }
        }
        Ok(())
    }
}

/// Read text input from reader
fn read_text_input<R: Read>(mut reader: R) -> Result<String> {
    let mut buffer = String::new();
    reader.read_to_string(&mut buffer)?;
    Ok(buffer)
}

/// Read binary input from reader
fn read_binary_input<R: Read>(mut reader: R) -> Result<Vec<u8>> {
    let mut buffer = Vec::new();
    reader.read_to_end(&mut buffer)?;
    Ok(buffer)
}

/// Write text output to writer
fn write_text_output<W: Write>(mut writer: W, content: &str) -> Result<()> {
    writer.write_all(content.as_bytes())?;
    Ok(())
}

/// Write binary output to writer
fn write_binary_output<W: Write>(mut writer: W, content: &[u8]) -> Result<()> {
    writer.write_all(content)?;
    Ok(())
}

/// Helper function to get reader from file path (or stdin if "-")
fn get_reader(path: &str) -> Result<Box<dyn Read>> {
    if path == "-" {
        Ok(Box::new(io::stdin()))
    } else {
        Ok(Box::new(fs::File::open(path)?))
    }
}

/// Helper function to get writer from file path (or stdout if "-")
fn get_writer(path: &str) -> Result<Box<dyn Write>> {
    if path == "-" {
        Ok(Box::new(io::stdout()))
    } else {
        Ok(Box::new(fs::File::create(path)?))
    }
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use super::*;

    const BASIC_PLAN: &str = r#"=== Plan
Root[result]
  Project[$0, $1]
    Read[data => a:i64, b:string]
"#;

    const PLAN_WITH_EXTENSIONS: &str = r#"=== Extensions
URIs:
  @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_arithmetic.yaml
Functions:
  # 10 @  1: gt

=== Plan
Root[result]
  Filter[gt($2, 100) => $0, $1, $2]
    Project[$0, $1, $2]
      Read[data => a:i64, b:string, c:i32]
"#;

    #[test]
    fn test_convert_text_to_text() {
        let input = Cursor::new(BASIC_PLAN);
        let mut output = Vec::new();

        let cli = Cli {
            command: Commands::Convert {
                input: String::new(),  // Not used in run_with_io
                output: String::new(), // Not used in run_with_io
                from: Format::Text,
                to: Format::Text,
                show_literal_types: false,
                show_expression_types: false,
                verbose: false,
            },
        };

        cli.run_with_io(input, &mut output).unwrap();

        let output_content = String::from_utf8(output).unwrap();
        assert!(output_content.contains("=== Plan"));
        assert!(output_content.contains("Root[result]"));
        assert!(output_content.contains("Project[$0, $1]"));
        assert!(output_content.contains("Read[data => a:i64, b:string]"));
    }

    #[test]
    fn test_convert_text_to_json() {
        let input = Cursor::new(BASIC_PLAN);
        let mut output = Vec::new();

        let cli = Cli {
            command: Commands::Convert {
                input: String::new(),
                output: String::new(),
                from: Format::Text,
                to: Format::Json,
                show_literal_types: false,
                show_expression_types: false,
                verbose: false,
            },
        };

        cli.run_with_io(input, &mut output).unwrap();

        let output_content = String::from_utf8(output).unwrap();
        assert!(output_content.contains("\"relations\""));
        assert!(output_content.contains("\"root\""));
        assert!(output_content.contains("\"project\""));
        assert!(output_content.contains("\"read\""));
    }

    #[test]
    fn test_convert_json_to_text() {
        // First convert text to JSON
        let input = Cursor::new(BASIC_PLAN);
        let mut json_output = Vec::new();

        let cli_to_json = Cli {
            command: Commands::Convert {
                input: String::new(),
                output: String::new(),
                from: Format::Text,
                to: Format::Json,
                show_literal_types: false,
                show_expression_types: false,
                verbose: false,
            },
        };

        cli_to_json.run_with_io(input, &mut json_output).unwrap();

        // Now convert JSON back to text
        let json_input = Cursor::new(json_output);
        let mut text_output = Vec::new();

        let cli_to_text = Cli {
            command: Commands::Convert {
                input: String::new(),
                output: String::new(),
                from: Format::Json,
                to: Format::Text,
                show_literal_types: false,
                show_expression_types: false,
                verbose: false,
            },
        };

        cli_to_text
            .run_with_io(json_input, &mut text_output)
            .unwrap();

        let output_content = String::from_utf8(text_output).unwrap();
        assert!(output_content.contains("=== Plan"));
        assert!(output_content.contains("Root[result]"));
    }

    #[test]
    fn test_convert_with_protobuf_output() {
        let input = Cursor::new(BASIC_PLAN);
        let mut output = Vec::new();

        let cli = Cli {
            command: Commands::Convert {
                input: String::new(),
                output: String::new(),
                from: Format::Text,
                to: Format::Protobuf,
                show_literal_types: false,
                show_expression_types: false,
                verbose: false,
            },
        };

        cli.run_with_io(input, &mut output).unwrap();

        // Protobuf output should be binary, so we just check that it's not empty
        assert!(!output.is_empty());

        // Should not contain readable text
        let output_string = String::from_utf8_lossy(&output);
        assert!(!output_string.contains("=== Plan"));
    }

    #[test]
    fn test_validate_command() {
        let input = Cursor::new(BASIC_PLAN);
        let mut output = Vec::new();

        let cli = Cli {
            command: Commands::Validate {
                input: String::new(),
                output: String::new(),
                verbose: false,
            },
        };

        cli.run_with_io(input, &mut output).unwrap();

        let output_content = String::from_utf8(output).unwrap();
        assert!(output_content.contains("=== Plan"));
        assert!(output_content.contains("Root[result]"));
        assert!(output_content.contains("Project[$0, $1]"));
        assert!(output_content.contains("Read[data => a:i64, b:string]"));
    }

    #[test]
    fn test_validate_with_extensions() {
        let input = Cursor::new(PLAN_WITH_EXTENSIONS);
        let mut output = Vec::new();

        let cli = Cli {
            command: Commands::Validate {
                input: String::new(),
                output: String::new(),
                verbose: false,
            },
        };

        cli.run_with_io(input, &mut output).unwrap();

        let output_content = String::from_utf8(output).unwrap();
        assert!(output_content.contains("=== Extensions"));
        assert!(output_content.contains("=== Plan"));
        assert!(output_content.contains("Root[result]"));
        assert!(output_content.contains("Filter[gt($2, 100)"));
    }

    #[test]
    fn test_convert_with_formatting_options() {
        let input = Cursor::new(BASIC_PLAN);
        let mut output = Vec::new();

        let cli = Cli {
            command: Commands::Convert {
                input: String::new(),
                output: String::new(),
                from: Format::Text,
                to: Format::Text,
                show_literal_types: true,
                show_expression_types: true,
                verbose: false,
            },
        };

        cli.run_with_io(input, &mut output).unwrap();

        let output_content = String::from_utf8(output).unwrap();
        assert!(output_content.contains("=== Plan"));
        assert!(output_content.contains("Root[result]"));
    }

    #[test]
    fn test_protobuf_roundtrip() {
        // Convert text to protobuf
        let input = Cursor::new(BASIC_PLAN);
        let mut protobuf_output = Vec::new();

        let cli_to_protobuf = Cli {
            command: Commands::Convert {
                input: String::new(),
                output: String::new(),
                from: Format::Text,
                to: Format::Protobuf,
                show_literal_types: false,
                show_expression_types: false,
                verbose: false,
            },
        };

        cli_to_protobuf
            .run_with_io(input, &mut protobuf_output)
            .unwrap();

        // Convert protobuf back to text
        let protobuf_input = Cursor::new(protobuf_output);
        let mut text_output = Vec::new();

        let cli_to_text = Cli {
            command: Commands::Convert {
                input: String::new(),
                output: String::new(),
                from: Format::Protobuf,
                to: Format::Text,
                show_literal_types: false,
                show_expression_types: false,
                verbose: false,
            },
        };

        cli_to_text
            .run_with_io(protobuf_input, &mut text_output)
            .unwrap();

        let output_content = String::from_utf8(text_output).unwrap();
        assert!(output_content.contains("=== Plan"));
        assert!(output_content.contains("Root[result]"));
        assert!(output_content.contains("Read[data => a:i64, b:string]"));
    }
}
