use anyhow::Result;
use oxigraph_testsuite::check_testsuite;

#[test]
fn oxigraph_parser_testsuite() -> Result<()> {
    check_testsuite(
        "https://github.com/oxigraph/oxigraph/tests/parser/manifest.ttl",
        &[],
    )
}

#[test]
fn oxigraph_sparql_testsuite() -> Result<()> {
    check_testsuite(
        "https://github.com/oxigraph/oxigraph/tests/sparql/manifest.ttl",
        &[],
    )
}

#[test]
fn oxigraph_sparql_results_testsuite() -> Result<()> {
    check_testsuite(
        "https://github.com/oxigraph/oxigraph/tests/sparql-results/manifest.ttl",
        &[],
    )
}
