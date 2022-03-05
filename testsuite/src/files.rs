use anyhow::{anyhow, Result};
use oxigraph::io::{DatasetFormat, DatasetParser, GraphFormat, GraphParser};
use oxigraph::model::{Dataset, Graph, GraphNameRef};
use oxigraph::store::Store;
use oxttl::N3Parser;
use std::fs::File;
use std::io::{BufRead, BufReader, Read};
use std::path::PathBuf;

pub fn read_file(url: &str) -> Result<impl BufRead> {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push(if url.starts_with("http://w3c.github.io/rdf-tests/") {
        Ok(url.replace("http://w3c.github.io/rdf-tests/", "rdf-tests/"))
    } else if url.starts_with("http://www.w3.org/2013/RDFXMLTests/") {
        Ok(url.replace("http://www.w3.org/2013/RDFXMLTests/", "rdf-tests/rdf-xml/"))
    } else if url.starts_with("http://www.w3.org/2001/sw/DataAccess/tests/data-r2/") {
        Ok(url.replace(
            "http://www.w3.org/2001/sw/DataAccess/tests/",
            "rdf-tests/sparql11/",
        ))
    } else if url.starts_with("http://www.w3.org/2009/sparql/docs/tests/data-sparql11/") {
        Ok(url.replace(
            "http://www.w3.org/2009/sparql/docs/tests/",
            "rdf-tests/sparql11/",
        ))
    } else if url.starts_with("https://w3c.github.io/") {
        Ok(url.replace("https://w3c.github.io/", ""))
    } else if url.starts_with("https://github.com/oxigraph/oxigraph/tests/") {
        Ok(url.replace(
            "https://github.com/oxigraph/oxigraph/tests/",
            "oxigraph-tests/",
        ))
    } else {
        Err(anyhow!("Not supported url for file: {}", url))
    }?);
    Ok(BufReader::new(File::open(&path)?))
}

pub fn read_file_to_string(url: &str) -> Result<String> {
    let mut buf = String::new();
    read_file(url)?.read_to_string(&mut buf)?;
    Ok(buf)
}

pub fn load_to_store<'a>(
    url: &str,
    store: &Store,
    to_graph_name: impl Into<GraphNameRef<'a>>,
) -> Result<()> {
    if url.ends_with(".nt") {
        store.load_graph(
            read_file(url)?,
            GraphFormat::NTriples,
            to_graph_name,
            Some(url),
        )?
    } else if url.ends_with(".ttl") {
        store.load_graph(
            read_file(url)?,
            GraphFormat::Turtle,
            to_graph_name,
            Some(url),
        )?
    } else if url.ends_with(".rdf") {
        store.load_graph(
            read_file(url)?,
            GraphFormat::RdfXml,
            to_graph_name,
            Some(url),
        )?
    } else if url.ends_with(".nq") {
        store.load_dataset(read_file(url)?, DatasetFormat::NQuads, Some(url))?
    } else if url.ends_with(".trig") {
        store.load_dataset(read_file(url)?, DatasetFormat::TriG, Some(url))?
    } else {
        return Err(anyhow!("Serialization type not found for {}", url));
    }
    Ok(())
}

pub fn load_to_graph(url: &str, graph: &mut Graph, format: Option<GraphFormat>) -> Result<()> {
    let format = if let Some(format) = format {
        format
    } else {
        url.rsplit_once('.')
            .and_then(|(_, extension)| GraphFormat::from_extension(extension))
            .ok_or_else(|| anyhow!("Serialization type not found for {}", url))?
    };
    let parser = GraphParser::from_format(format).with_base_iri(url)?;
    for t in parser.read_triples(read_file(url)?)? {
        graph.insert(&t?);
    }
    Ok(())
}

pub fn load_graph(url: &str, format: Option<GraphFormat>) -> Result<Graph> {
    let mut graph = Graph::new();
    load_to_graph(url, &mut graph, format)?;
    Ok(graph)
}

pub fn load_to_dataset(
    url: &str,
    dataset: &mut Dataset,
    format: Option<DatasetFormat>,
) -> Result<()> {
    let format = if let Some(format) = format {
        format
    } else {
        url.rsplit_once('.')
            .and_then(|(_, extension)| DatasetFormat::from_extension(extension))
            .or_else(|| {
                url.rsplit_once('.')
                    .and_then(|(_, extension)| GraphFormat::from_extension(extension))
                    .and_then(|format| format.try_into().ok())
            })
            .ok_or_else(|| anyhow!("Serialization type not found for {}", url))?
    };
    let parser = DatasetParser::from_format(format).with_base_iri(url)?;
    for q in parser.read_quads(read_file(url)?)? {
        dataset.insert(&q?);
    }
    Ok(())
}

pub fn load_dataset(url: &str, format: Option<DatasetFormat>) -> Result<Dataset> {
    let mut dataset = Dataset::new();
    load_to_dataset(url, &mut dataset, format)?;
    Ok(dataset)
}

pub fn load_n3(url: &str) -> Result<()> {
    for q in N3Parser::builder()
        .with_base_iri(url)?
        .with_prefix("", url)?
        .with_read(read_file(url)?)
    {
        q?;
    }
    Ok(())
}
