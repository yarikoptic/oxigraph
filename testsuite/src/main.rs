use anyhow::Result;
use clap::Parser;
use oxigraph_testsuite::evaluator::TestEvaluator;
use oxigraph_testsuite::files::read_file;
use oxigraph_testsuite::manifest::TestManifest;
use oxigraph_testsuite::parser_evaluator::register_parser_tests;
use oxigraph_testsuite::report::build_report;
use oxigraph_testsuite::sparql_evaluator::register_sparql_tests;
use rio_api::parser::TriplesParser;
use std::fs;

#[derive(Parser)]
/// Oxigraph testsuite runner
struct Args {
    /// URI of the testsuite manifest(s) to run
    manifest: Vec<String>,
}

fn main() -> Result<()> {
    /*let matches = Args::parse();

    let mut evaluator = TestEvaluator::default();
    register_parser_tests(&mut evaluator);
    register_sparql_tests(&mut evaluator);
    let manifest = TestManifest::new(matches.manifest);
    let results = evaluator.evaluate(manifest)?;
    print!("{}", build_report(results));
    Ok(())*/

    let data = fs::read_to_string("testsuite/benches/dbpedia.ttl")?;
    /*for _ in 0..5 {
        let mut parser = oxttl::TurtleParser::new().parse();
        parser.extend_from_slice(data.as_bytes());
        parser.end();
        while let Some(result) = parser.read_next() {
            result?;
        }
    }*/
    for _ in 0..5 {
        rio_turtle::TurtleParser::new(data.as_bytes(), None)
            .parse_all(&mut |_| Ok(()) as Result<(), rio_turtle::TurtleError>)?;
    }
    Ok(())
}
