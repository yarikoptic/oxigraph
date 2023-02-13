//! Shared parser implementation for Turtle and TriG.

use crate::lexer::{N3Lexer, N3LexerMode, N3LexerOptions, N3Token};
use crate::toolkit::{Lexer, Parser, RuleRecognizer, RuleRecognizerError};
use crate::{MAX_BUFFER_SIZE, MIN_BUFFER_SIZE};
use oxiri::Iri;
#[cfg(feature = "default")]
use oxrdf::Triple;
use oxrdf::{
    vocab::{rdf, xsd},
    BlankNode, GraphName, Literal, NamedNode, NamedOrBlankNode, Quad, Subject, Term,
};
use std::borrow::Cow;
use std::collections::HashMap;

#[derive(Eq, PartialEq, Debug, Clone, Hash)]
pub struct Prefix {
    pub name: String,
    pub iri: NamedNode,
}

#[derive(Eq, PartialEq, Debug, Clone, Hash)]
pub struct Base {
    pub iri: NamedNode,
}

pub struct TriGRecognizer {
    stack: Vec<TriGState>,
    with_graph_name: bool,
    #[cfg(feature = "rdf-star")]
    with_quoted_triples: bool,
    lexer_options: N3LexerOptions,
    prefixes: HashMap<String, Iri<String>>,
    cur_subject: Vec<Option<Subject>>,
    cur_predicate: Vec<Option<NamedNode>>,
    cur_object: Vec<Option<Term>>,
    cur_graph: GraphName,
}

impl RuleRecognizer for TriGRecognizer {
    type TokenRecognizer = N3Lexer;
    type Output = Quad;

    fn error_recovery_state(mut self) -> Self {
        self.stack.clear();
        self.cur_subject.clear();
        self.cur_predicate.clear();
        self.cur_object.clear();
        self
    }

    fn recognize_next(
        mut self,
        token: N3Token,
        results: &mut Vec<Quad>,
        errors: &mut Vec<RuleRecognizerError>,
    ) -> Self {
        if let Some(rule) = self.stack.pop() {
            match rule {
                // [1g] 	trigDoc 	::= 	(directive | block)*
                // [2g] 	block 	::= 	triplesOrGraph | wrappedGraph | triples2 | "GRAPH" labelOrSubject wrappedGraph
                // [3] 	directive 	::= 	prefixID | base | sparqlPrefix | sparqlBase
                // [4] 	prefixID 	::= 	'@prefix' PNAME_NS IRIREF '.'
                // [5] 	base 	::= 	'@base' IRIREF '.'
                // [5s] 	sparqlPrefix 	::= 	"PREFIX" PNAME_NS IRIREF
                // [6s] 	sparqlBase 	::= 	"BASE" IRIREF
                TriGState::TriGDoc => {
                    self.cur_graph = GraphName::DefaultGraph;
                    self.stack.push(TriGState::TriGDoc);
                    match token {
                        N3Token::PlainKeyword("BASE") => {
                            self.stack.push(TriGState::BaseExpectIri);
                            self
                        }
                        N3Token::PlainKeyword("PREFIX") => {
                            self.stack.push(TriGState::PrefixExpectPrefix);
                            self
                        }
                        N3Token::AtKeyword("prefix") => {
                            self.stack.push(TriGState::ExpectDot);
                            self.stack.push(TriGState::PrefixExpectPrefix);
                            self
                        }
                        N3Token::AtKeyword("base") => {
                            self.stack.push(TriGState::ExpectDot);
                            self.stack.push(TriGState::BaseExpectIri);
                            self
                        }
                        N3Token::PlainKeyword("GRAPH") if self.with_graph_name => {
                            self.stack.push(TriGState::WrappedGraph);
                            self.stack.push(TriGState::GraphName);
                            self
                        }
                        token @ N3Token::Punctuation("{") if self.with_graph_name => {
                            self.stack.push(TriGState::WrappedGraph);
                            self.recognize_next(token, results, errors)
                        }
                        token => {
                            self.stack.push(TriGState::TriplesOrGraph);
                            self.recognize_next(token, results, errors)
                        }
                    }
                },
                TriGState::ExpectDot => {
                    if token == N3Token::Punctuation(".") {
                        self
                    } else  {
                        errors.push("A dot is expected at the end of TriG statements".into());
                        self.recognize_next(token, results, errors)
                    }
                },
                TriGState::BaseExpectIri => match token {
                    N3Token::IriRef(iri) => {
                        self.lexer_options.base_iri = Some(iri);
                        self
                    }
                    _ => self.error(errors, "The BASE keyword should be followed by an IRI"),
                },
                TriGState::PrefixExpectPrefix => match token {
                    N3Token::PrefixedName(prefix, local) if local.is_empty() => {
                        self.stack.push(TriGState::PrefixExpectIri { name: prefix.to_string() });
                        self
                    }
                    _ => {
                        self.error(errors, "The PREFIX keyword should be followed by a prefix like 'ex:'")
                    }
                },
                TriGState::PrefixExpectIri { name } => match token {
                    N3Token::IriRef(iri) => {
                        self.prefixes.insert(name, iri);
                        self
                    }
                    _ => self.error(errors, "The PREFIX declaration should be followed by a prefix and its value as an IRI"),
                },
                // [3g] 	triplesOrGraph 	::= 	labelOrSubject ( wrappedGraph | predicateObjectList '.' ) | quotedTriple predicateObjectList '.'
                // [4g] 	triples2 	::= 	blankNodePropertyList predicateObjectList? '.' | collection predicateObjectList '.'
                TriGState::TriplesOrGraph => match token {
                    N3Token::IriRef(iri) => {
                        self.stack.push(TriGState::WrappedGraphOrPredicateObjectList {
                            term: NamedNode::new_unchecked(iri.into_inner()).into()
                        });
                        self
                    }
                    N3Token::PrefixedName(prefix, local) => {
                        if let Some(start) = self.prefixes.get(prefix) {
                            self.stack.push(TriGState::WrappedGraphOrPredicateObjectList {
                                term: Self::concatenate_iri(start, local).into()
                            });
                            self
                        } else {
                            self.error(errors, format!("The prefix {prefix}: has not been declared"))
                        }
                    }
                    N3Token::BlankNodeLabel(label) => {
                        self.stack.push(TriGState::WrappedGraphOrPredicateObjectList {
                            term: BlankNode::new_unchecked(label).into()
                        });
                        self
                    }
                    N3Token::Punctuation("[") => {
                        self.stack.push(TriGState::WrappedGraphBlankNodePropertyListCurrent);
                        self
                    }
                    N3Token::Punctuation("(") => {
                        self.stack.push(TriGState::PredicateObjectListClosingDot);
                        self.stack.push(TriGState::PredicateObjectList);
                        self.stack.push(TriGState::SubjectCollectionBeginning);
                        self
                    }
                    #[cfg(feature = "rdf-star")]
                    N3Token::Punctuation("<<") if self.with_quoted_triples => {
                        self.stack.push(TriGState::PredicateObjectListClosingDot);
                        self.stack.push(TriGState::PredicateObjectList);
                        self.stack.push(TriGState::SubjectQuotedTripleEnd);
                        self.stack.push(TriGState::QuotedObject);
                        self.stack.push(TriGState::Verb);
                        self.stack.push(TriGState::QuotedSubject);
                        self
                    }
                    token => {
                        self.error(errors, format!("The token {token:?} is not a valid subject or graph anme"))
                    }
                }
                TriGState::WrappedGraphOrPredicateObjectList { term } => {
                    if token == N3Token::Punctuation("{") && self.with_graph_name {
                        self.cur_graph = term.into();
                        self.stack.push(TriGState::WrappedGraph);
                    } else {
                        self.cur_subject.push(Some(term.into()));
                        self.stack.push(TriGState::PredicateObjectListClosingDot);
                        self.stack.push(TriGState::PredicateObjectList);
                    }
                    self.recognize_next(token, results, errors)
                }
                TriGState::WrappedGraphBlankNodePropertyListCurrent => if token == N3Token::Punctuation("]") {
                    self.stack.push(TriGState::WrappedGraphOrPredicateObjectList {
                        term: BlankNode::default().into()
                    });
                    self
                } else {
                    self.cur_subject.push(Some(BlankNode::default().into()));
                    self.stack.push(TriGState::PredicateObjectListClosingDot);
                    self.stack.push(TriGState::SubjectBlankNodePropertyListEnd);
                    self.stack.push(TriGState::PredicateObjectList);
                    self.recognize_next(token, results, errors)
                }
                TriGState::SubjectBlankNodePropertyListEnd => if token == N3Token::Punctuation("]") {
                    self.stack.push(TriGState::SubjectBlankNodePropertyListAfter);
                    self
                } else {
                    errors.push("blank node property lists should end with a ']'".into());
                    self.stack.push(TriGState::SubjectBlankNodePropertyListAfter);
                    self.recognize_next(token, results, errors)
                }
                TriGState::SubjectBlankNodePropertyListAfter => if matches!(token, N3Token::Punctuation(".") | N3Token::Punctuation("}")) {
                    self.recognize_next(token, results, errors)
                } else {
                    self.stack.push(TriGState::PredicateObjectList);
                    self.recognize_next(token, results, errors)
                }
                TriGState::SubjectCollectionBeginning => {
                    match token {
                        N3Token::Punctuation(")") => {
                            self.cur_subject.push(Some(rdf::NIL.into()));
                            self
                        }
                        token => {
                            let root = BlankNode::default();
                            self.cur_subject.push(Some(root.clone().into()));
                            self.cur_subject.push(Some(root.into()));
                            self.cur_predicate.push(Some(rdf::FIRST.into()));
                            self.stack.push(TriGState::SubjectCollectionPossibleEnd);
                            self.stack.push(TriGState::Object);
                            self.recognize_next(token, results, errors)
                        }
                    }
                },
                TriGState::SubjectCollectionPossibleEnd => {
                    let object = self.cur_object.pop().unwrap();
                    self.emit_quad(
                        results,
                        self.cur_subject.last().unwrap().clone(),
                        self.cur_predicate.last().unwrap().clone(),
                        object,
                    );
                    let old = self.cur_subject.pop().unwrap();
                    match token {
                        N3Token::Punctuation(")") => {
                            self.cur_predicate.pop().unwrap();
                            self.emit_quad(
                                results,
                                old,
                                Some(rdf::REST),
                                Some(rdf::NIL)
                            );
                            self
                        }
                        token => {
                            let new = BlankNode::default();
                            self.emit_quad(
                                results,
                                old,
                                Some(rdf::REST),
                                Some(new.clone())
                            );
                            self.cur_subject.push(Some(new.into()));
                            self.stack.push(TriGState::ObjectCollectionPossibleEnd);
                            self.stack.push(TriGState::Object);
                            self.recognize_next(token, results, errors)
                        }
                    }
                }
                // [5g] 	wrappedGraph 	::= 	'{' triplesBlock? '}'
                // [6g] 	triplesBlock 	::= 	triples ('.' triplesBlock?)?
                TriGState::WrappedGraph => if token == N3Token::Punctuation("{") {
                    self.stack.push(TriGState::WrappedGraphContent);
                    self
                } else {
                    self.error(errors, "The GRAPH keyword should be followed by a graph name and a value in '{'")
                },
                TriGState::WrappedGraphPossibleEnd => {
                    match token {
                        N3Token::Punctuation("}") => {
                            self.cur_subject.pop().unwrap();
                            self
                        }
                        N3Token::Punctuation(".") => {
                            self.cur_subject.pop().unwrap();
                            self.stack.push(TriGState::WrappedGraphContent);
                            self
                        }
                        token => {
                            errors.push("A '}' or a '.' is expected at the end of a graph block".into());
                            self.recognize_next(token, results, errors)
                        }
                    }
                }
                TriGState::WrappedGraphContent => if token == N3Token::Punctuation("}") {
                    self
                } else {
                    self.stack.push(TriGState::WrappedGraphPossibleEnd);
                    self.stack.push(TriGState::Triples);
                    self.recognize_next(token, results, errors)
                },
                // [6] 	triples 	::= 	subject predicateObjectList | blankNodePropertyList predicateObjectList?
                // [10] 	subject 	::= 	iri | BlankNode | collection | quotedTriple
                TriGState::Triples => match token {
                    N3Token::Punctuation("[") => {
                        self.cur_subject.push(Some(BlankNode::default().into()));
                        self.stack.push(TriGState::TriplesBlankNodePropertyListCurrent);
                        self
                    }
                    N3Token::IriRef(iri) => {
                        self.cur_subject.push(Some(NamedNode::new_unchecked(iri.into_inner()).into()));
                        self.stack.push(TriGState::PredicateObjectList);
                        self
                    }
                    N3Token::PrefixedName(prefix, local) => {
                        if let Some(start) = self.prefixes.get(prefix) {
                            self.cur_subject.push(Some(Self::concatenate_iri(start, local).into()));
                            self.stack.push(TriGState::PredicateObjectList);
                            self
                        } else {
                            self.error(errors, format!("The prefix {prefix}: has not been declared"))
                        }
                    }
                    N3Token::BlankNodeLabel(label) => {
                        self.cur_subject.push(Some(BlankNode::new_unchecked(label).into()));
                        self.stack.push(TriGState::PredicateObjectList);
                        self
                    }
                    N3Token::Punctuation("(") => {
                        self.stack.push(TriGState::PredicateObjectList);
                        self.stack.push(TriGState::SubjectCollectionBeginning);
                        self
                    }
                    #[cfg(feature = "rdf-star")]
                    N3Token::Punctuation("<<") if self.with_quoted_triples => {
                        self.stack.push(TriGState::PredicateObjectList);
                        self.stack.push(TriGState::SubjectQuotedTripleEnd);
                        self.stack.push(TriGState::QuotedObject);
                        self.stack.push(TriGState::Verb);
                        self.stack.push(TriGState::QuotedSubject);
                        self
                    }
                    token => {
                        self.error(errors, format!("The token {token:?} is not a valid RDF subject"))
                    }
                },
                TriGState::TriplesBlankNodePropertyListCurrent => if token == N3Token::Punctuation("]") {
                    self.stack.push(TriGState::PredicateObjectList);
                    self
                } else {
                    self.stack.push(TriGState::SubjectBlankNodePropertyListEnd);
                    self.stack.push(TriGState::PredicateObjectList);
                    self.recognize_next(token, results, errors)
                }
                // [7g] 	labelOrSubject 	::= 	iri | BlankNode
                TriGState::GraphName => match token {
                    N3Token::IriRef(iri) => {
                        self.cur_graph = NamedNode::new_unchecked(iri.into_inner()).into();
                        self
                    }
                    N3Token::PrefixedName(prefix, local) => {
                        if let Some(start) = self.prefixes.get(prefix) {
                            self.cur_graph = Self::concatenate_iri(start, local).into();
                            self
                        } else {
                            self.error(errors, format!("The prefix {prefix}: has not been declared"))
                        }
                    }
                    N3Token::BlankNodeLabel(label) => {
                        self.cur_graph = BlankNode::new_unchecked(label).into();
                        self
                    }
                    N3Token::Punctuation("[") => {
                        self.stack.push(TriGState::GraphNameAnonEnd);
                        self
                    }
                    token => {
                        self.error(errors, format!("The token {token:?} is not a valid graph name"))
                    }
                }
                TriGState::GraphNameAnonEnd => if token == N3Token::Punctuation("]") {
                    self.cur_graph = BlankNode::default().into();
                    self
                } else {
                    self.error(errors, "Anonymous blank node with a property list are not allowed as graph name")
                }
                // [7] 	predicateObjectList 	::= 	verb objectList (';' (verb objectList)?)*
                TriGState::PredicateObjectList => {
                    self.stack.push(TriGState::PredicateObjectListEnd);
                    self.stack.push(TriGState::ObjectsList);
                    self.stack.push(TriGState::Verb);
                    self.recognize_next(token, results, errors)
                },
                TriGState::PredicateObjectListEnd => {
                    let predicate = self.cur_predicate.pop().unwrap();
                    let object = self.cur_object.pop().unwrap();
                    self.emit_quad(
                        results,
                        self.cur_subject.last().unwrap().clone(),
                        predicate,
                        object,
                    );
                    match token {
                        N3Token::Punctuation(";") => {
                            self.stack.push(TriGState::PredicateObjectListPossibleContinuation);
                            self
                        }
                        N3Token::Punctuation(".") | N3Token::Punctuation("}") => {
                            self.recognize_next(token, results, errors)
                        },
                        token => {
                            self.recognize_next(token, results, errors)
                        }
                    }
                },
                TriGState::PredicateObjectListPossibleContinuation => match token {
                    N3Token::Punctuation(";")  =>{
                        self.stack.push(TriGState::PredicateObjectListPossibleContinuation);
                        self
                    },
                    N3Token::Punctuation(".") | N3Token::Punctuation("}") => {
                        self.recognize_next(token, results, errors)
                    },
                    N3Token::Punctuation("]") => {
                        self.recognize_next(token, results, errors)
                    },
                    token => {
                        self.stack.push(TriGState::PredicateObjectListEnd);
                        self.stack.push(TriGState::ObjectsList);
                        self.stack.push(TriGState::Verb);
                        self.recognize_next(token, results, errors)
                    }},
                TriGState::PredicateObjectListClosingDot => {
                    self.cur_subject.pop().unwrap();
                    if token == N3Token::Punctuation(".") {
                        self
                    } else  {
                        errors.push("A dot is expected at the end of TriG statements".into());
                        self.recognize_next(token, results, errors)
                    }
                },

                // [8] 	objectList 	::= 	object annotation? ( ',' object annotation? )*
                // [30t] 	annotation 	::= 	'{|' predicateObjectList '|}'
                TriGState::ObjectsList => {
                    self.stack.push(TriGState::ObjectsListEnd);
                    self.stack.push(TriGState::Object);
                    self.recognize_next(token, results, errors)
                }
                TriGState::ObjectsListEnd => {
                    match token {
                        N3Token::Punctuation(",") => {
                            let object = self.cur_object.pop().unwrap();
                            self.emit_quad(
                                results,
                                self.cur_subject.last().unwrap().clone(),
                                self.cur_predicate.last().unwrap().clone(),
                                object,
                            );
                            self.stack.push(TriGState::ObjectsListEnd);
                            self.stack.push(TriGState::Object);
                            self
                        },
                        #[cfg(feature = "rdf-star")]
                        N3Token::Punctuation("{|") => {
                            self.cur_subject.push(Self::new_triple(
                                self.cur_subject.last().unwrap().clone(),
                                self.cur_predicate.last().unwrap().clone(),
                                self.cur_object.last().unwrap().clone(),
                            ).map(Into::into));
                            self.stack.push(TriGState::AnnotationEnd);
                            self.stack.push(TriGState::PredicateObjectList);
                            self
                        }
                        token => {
                            self.recognize_next(token, results, errors)
                        }
                    }
                },
                #[cfg(feature = "rdf-star")]
                TriGState::AnnotationEnd => {
                    self.cur_subject.pop().unwrap();
                    self.stack.push(TriGState::ObjectsListAfterAnnotation);
                    if token == N3Token::Punctuation("|}") {
                        self
                    } else {
                        errors.push("Annotations should end with '|}'".into());
                        self.recognize_next(token, results, errors)
                    }
                },
                #[cfg(feature = "rdf-star")]
                TriGState::ObjectsListAfterAnnotation => if token == N3Token::Punctuation(",") {
                    let object = self.cur_object.pop().unwrap();
                    self.emit_quad(
                        results,
                        self.cur_subject.last().unwrap().clone(),
                        self.cur_predicate.last().unwrap().clone(),
                        object,
                    );
                    self.stack.push(TriGState::ObjectsListEnd);
                    self.stack.push(TriGState::Object);
                    self
                } else {
                    self.recognize_next(token, results, errors)
                },
                // [9] 	verb 	::= 	predicate | 'a'
                // [11] 	predicate 	::= 	iri
                TriGState::Verb => match token {
                    N3Token::PlainKeyword("a") => {
                        self.cur_predicate.push(Some(rdf::TYPE.into()));
                        self
                    }
                    N3Token::IriRef(iri) => {
                        self.cur_predicate.push(Some(NamedNode::new_unchecked(iri.into_inner())));
                        self
                    }
                    N3Token::PrefixedName(prefix, local) => {
                        if let Some(start) = self.prefixes.get(prefix) {
                            self.cur_predicate.push(Some(Self::concatenate_iri(start, local)));
                            self
                        } else {
                            self.error(errors, format!("The prefix {prefix}: has not been declared"))
                        }
                    }
                    token => {
                        self.error(errors, format!("The token {token:?} is not a valid predicate"))
                    }
                }
                // [12] 	object 	::= 	iri | BlankNode | collection | blankNodePropertyList | literal | quotedTriple
                // [13] 	literal 	::= 	RDFLiteral | NumericLiteral | BooleanLiteral
                // [14] 	blank 	::= 	BlankNode | collection
                // [15] 	blankNodePropertyList 	::= 	'[' predicateObjectList ']'
                // [16] 	collection 	::= 	'(' object* ')'
                // [17] 	NumericLiteral 	::= 	INTEGER | DECIMAL | DOUBLE
                // [128s] 	RDFLiteral 	::= 	String (LANGTAG | '^^' iri)?
                // [133s] 	BooleanLiteral 	::= 	'true' | 'false'
                // [18] 	String 	::= 	STRING_LITERAL_QUOTE | STRING_LITERAL_SINGLE_QUOTE | STRING_LITERAL_LONG_SINGLE_QUOTE | STRING_LITERAL_LONG_QUOTE
                // [135s] 	iri 	::= 	IRIREF | PrefixedName
                // [136s] 	PrefixedName 	::= 	PNAME_LN | PNAME_NS
                // [137s] 	BlankNode 	::= 	BLANK_NODE_LABEL | ANON
                TriGState::Object => {
                    match token {
                        N3Token::IriRef(iri) => {
                            self.cur_object.push(Some(NamedNode::new_unchecked(iri.into_inner()).into()));
                            self
                        }
                        N3Token::PrefixedName(prefix, local) => {
                            if let Some(start) = self.prefixes.get(prefix) {
                                self.cur_object.push(Some(Self::concatenate_iri(start, local).into()));
                                self
                            } else {
                                self.error(errors, format!("The prefix {prefix}: has not been declared"))
                            }
                        }
                        N3Token::BlankNodeLabel(label) => {
                            self.cur_object.push(Some(BlankNode::new_unchecked(label).into()));
                            self
                        }
                        N3Token::Punctuation("[") => {
                            self.stack.push(TriGState::ObjectBlankNodePropertyListCurrent);
                            self
                        }
                        N3Token::Punctuation("(") => {
                            self.stack.push(TriGState::ObjectCollectionBeginning);
                            self
                        }
                        N3Token::String(value) => {
                            self.stack.push(TriGState::LiteralPossibleSuffix { value });
                            self
                        }
                        N3Token::Integer(v) => {
                            self.cur_object.push(Some(Literal::new_typed_literal(v, xsd::INTEGER).into()));
                            self
                        }
                        N3Token::Decimal(v) => {
                            self.cur_object.push(Some(Literal::new_typed_literal(v, xsd::DECIMAL).into()));
                            self
                        }
                        N3Token::Double(v) => {
                            self.cur_object.push(Some(Literal::new_typed_literal(v, xsd::DOUBLE).into()));
                            self
                        }
                        N3Token::Boolean(v) => {
                            self.cur_object.push(Some(Literal::new_typed_literal(v, xsd::BOOLEAN).into()));
                            self
                        }
                        #[cfg(feature = "rdf-star")]
                        N3Token::Punctuation("<<") if self.with_quoted_triples => {
                            self.stack.push(TriGState::ObjectQuotedTripleEnd);
                            self.stack.push(TriGState::QuotedObject);
                            self.stack.push(TriGState::Verb);
                            self.stack.push(TriGState::QuotedSubject);
                            self
                        }
                        token => {
                            self.error(errors, format!("This is not a valid RDF object: {token:?}"))
                        }
                    }
                }
                TriGState::ObjectBlankNodePropertyListCurrent => if token == N3Token::Punctuation("]") {
                    self.cur_object.push(Some(BlankNode::default().into()));
                    self
                } else {
                    self.cur_subject.push(Some(BlankNode::default().into()));
                    self.stack.push(TriGState::ObjectBlankNodePropertyListEnd);
                    self.stack.push(TriGState::PredicateObjectList);
                    self.recognize_next(token, results, errors)
                }
                TriGState::ObjectBlankNodePropertyListEnd => {
                    self.cur_object.push(self.cur_subject.pop().unwrap().map(Into::into));
                    if token == N3Token::Punctuation("]") {
                        self
                    } else {
                        errors.push("blank node property lists should end with a ']'".into());
                        self.recognize_next(token, results, errors)
                    }
                }
                TriGState::ObjectCollectionBeginning => match token {
                    N3Token::Punctuation(")") => {
                        self.cur_object.push(Some(rdf::NIL.into()));
                        self
                    }
                    token => {
                        let root = BlankNode::default();
                        self.cur_object.push(Some(root.clone().into()));
                        self.cur_subject.push(Some(root.into()));
                        self.cur_predicate.push(Some(rdf::FIRST.into()));
                        self.stack.push(TriGState::ObjectCollectionPossibleEnd);
                        self.stack.push(TriGState::Object);
                        self.recognize_next(token, results, errors)
                    }
                },
                TriGState::ObjectCollectionPossibleEnd => {
                    let object = self.cur_object.pop().unwrap();
                    self.emit_quad(
                        results,
                        self.cur_subject.last().unwrap().clone(),
                        self.cur_predicate.last().unwrap().clone(),
                        object,
                    );
                    let old = self.cur_subject.pop().unwrap();
                    match token {
                        N3Token::Punctuation(")") => {
                            self.cur_predicate.pop().unwrap();
                            self.emit_quad(
                                results,
                                old,
                                Some(rdf::REST),
                                Some(rdf::NIL),
                            );
                            self
                        }
                        token => {
                            let new = BlankNode::default();
                            self.emit_quad(
                                results,
                                old,
                                Some(rdf::REST),
                                Some(new.clone())
                            );
                            self.cur_subject.push(Some(new.into()));
                            self.stack.push(TriGState::ObjectCollectionPossibleEnd);
                            self.stack.push(TriGState::Object);
                            self.recognize_next(token, results, errors)
                        }
                    }
                }
                TriGState::LiteralPossibleSuffix { value } => {
                    match token {
                        N3Token::LangTag(lang) => {
                            self.cur_object.push(Some(Literal::new_language_tagged_literal_unchecked(value, lang.into_inner()).into()));
                            self
                        },
                        N3Token::Punctuation("^^") => {
                            self.stack.push(TriGState::LiteralExpectDatatype { value });
                            self
                        }
                        token => {
                            self.cur_object.push(Some(Literal::new_simple_literal(value).into()));
                            self.recognize_next(token, results, errors)
                        }
                    }
                }
                TriGState::LiteralExpectDatatype { value } => {
                    match token {
                        N3Token::IriRef(datatype) => {
                            self.cur_object.push(Some(Literal::new_typed_literal(value, NamedNode::new_unchecked(datatype.into_inner())).into()));
                            self
                        },
                        N3Token::PrefixedName(prefix, local) => {
                            if let Some(start) = self.prefixes.get(prefix) {
                                self.cur_object.push(Some(Literal::new_typed_literal(value, Self::concatenate_iri(start, local)).into()));
                                self
                            } else {
                                self.error(errors, format!("The prefix {prefix}: has not been declared"))
                            }
                        }
                        token => {
                            self.error(errors, format!("Expecting a datatype IRI after '^^, found {token:?}")).recognize_next(token, results, errors)
                        }
                    }
                }
                // [27t] 	quotedTriple 	::= 	'<<' qtSubject verb qtObject '>>'
                #[cfg(feature = "rdf-star")]
                TriGState::SubjectQuotedTripleEnd => {
                    let triple = Self::new_triple(
                        self.cur_subject.pop().unwrap(),
                        self.cur_predicate.pop().unwrap(),
                        self.cur_object.pop().unwrap()
                    ).map(Into::into);
                    self.cur_subject.push(triple);
                    if token == N3Token::Punctuation(">>") {
                        self
                    } else {
                        errors.push(format!("Expecting '>>' to close a quoted triple, found {token:?}").into());
                        self.recognize_next(token, results, errors)
                    }
                }
                #[cfg(feature = "rdf-star")]
                TriGState::ObjectQuotedTripleEnd => {
                    let triple = Self::new_triple(
                        self.cur_subject.pop().unwrap(),
                        self.cur_predicate.pop().unwrap(),
                        self.cur_object.pop().unwrap()
                    ).map(Into::into);
                    self.cur_object.push(triple);
                    if token == N3Token::Punctuation(">>") {
                        self
                    } else {
                        errors.push(format!("Expecting '>>' to close a quoted triple, found {token:?}").into());
                        self.recognize_next(token, results, errors)
                    }
                }
                // [28t] 	qtSubject 	::= 	iri | BlankNode | quotedTriple
                #[cfg(feature = "rdf-star")]
                TriGState::QuotedSubject => match token {
                    N3Token::Punctuation("[") => {
                        self.cur_subject.push(Some(BlankNode::default().into()));
                        self.stack.push(TriGState::QuotedAnonEnd);
                        self
                    }
                    N3Token::IriRef(iri) => {
                        self.cur_subject.push(Some(NamedNode::new_unchecked(iri.into_inner()).into()));
                        self
                    }
                    N3Token::PrefixedName(prefix, local) => {
                        if let Some(start) = self.prefixes.get(prefix) {
                            self.cur_subject.push(Some(Self::concatenate_iri(start, local).into()));
                            self
                        } else {
                            self.error(errors, format!("The prefix {prefix}: has not been declared"))
                        }
                    }
                    N3Token::BlankNodeLabel(label) => {
                        self.cur_subject.push(Some(BlankNode::new_unchecked(label).into()));
                        self
                    }
                    N3Token::Punctuation("<<") => {
                        self.stack.push(TriGState::SubjectQuotedTripleEnd);
                        self.stack.push(TriGState::QuotedObject);
                        self.stack.push(TriGState::Verb);
                        self.stack.push(TriGState::QuotedSubject);
                        self
                    }
                    token => self.error(errors, format!("This is not a valid RDF quoted triple subject: {token:?}"))
                }
                // [29t] 	qtObject 	::= 	iri | BlankNode | literal | quotedTriple
                #[cfg(feature = "rdf-star")]
                TriGState::QuotedObject => match token {
                    N3Token::Punctuation("[") => {
                        self.cur_object.push(Some(BlankNode::default().into()));
                        self.stack.push(TriGState::QuotedAnonEnd);
                        self
                    }
                    N3Token::IriRef(iri) => {
                        self.cur_object.push(Some(NamedNode::new_unchecked(iri.into_inner()).into()));
                        self
                    }
                    N3Token::PrefixedName(prefix, local) => {
                        if let Some(start) = self.prefixes.get(prefix) {
                            self.cur_object.push(Some(Self::concatenate_iri(start, local).into()));
                            self
                        } else {
                            self.error(errors, format!("The prefix {prefix}: has not been declared"))
                        }
                    }
                    N3Token::BlankNodeLabel(label) => {
                        self.cur_object.push(Some(BlankNode::new_unchecked(label).into()));
                        self
                    }
                    N3Token::String(value) => {
                        self.stack.push(TriGState::LiteralPossibleSuffix { value });
                        self
                    }
                    N3Token::Integer(v) => {
                        self.cur_object.push(Some(Literal::new_typed_literal(v, xsd::INTEGER).into()));
                        self
                    }
                    N3Token::Decimal(v) => {
                        self.cur_object.push(Some(Literal::new_typed_literal(v, xsd::DECIMAL).into()));
                        self
                    }
                    N3Token::Double(v) => {
                        self.cur_object.push(Some(Literal::new_typed_literal(v, xsd::DOUBLE).into()));
                        self
                    }
                    N3Token::Boolean(v) => {
                        self.cur_object.push(Some(Literal::new_typed_literal(v, xsd::BOOLEAN).into()));
                        self
                    }
                    N3Token::Punctuation("<<") => {
                        self.stack.push(TriGState::ObjectQuotedTripleEnd);
                        self.stack.push(TriGState::QuotedObject);
                        self.stack.push(TriGState::Verb);
                        self.stack.push(TriGState::QuotedSubject);
                        self
                    }
                    token => self.error(errors, format!("This is not a valid RDF quoted triple object: {token:?}"))
                }
                #[cfg(feature = "rdf-star")]
                TriGState::QuotedAnonEnd => if token == N3Token::Punctuation("]") {
                    self
                } else {
                    self.error(errors, "Anonymous blank node with a property list are not allowed in quoted triples")
                }
            }
        } else if token == N3Token::Punctuation(".") || token == N3Token::Punctuation("}") {
            //TODO: be smarter depending if we are in '{' or not
            self.stack.push(TriGState::TriGDoc);
            self
        } else {
            self
        }
    }

    fn recognize_end(
        &self,
        _results: &mut Vec<Self::Output>,
        errors: &mut Vec<RuleRecognizerError>,
    ) {
        match &self.stack[..] {
            [] | [TriGState::TriGDoc] => {
                debug_assert!(self.cur_subject.is_empty());
                debug_assert!(self.cur_predicate.is_empty());
                debug_assert!(self.cur_object.is_empty());
            }
            _ => errors.push("Unexpected end".into()), //TODO
        }
    }

    fn lexer_options(&self) -> &N3LexerOptions {
        &self.lexer_options
    }
}

impl TriGRecognizer {
    pub fn new_parser(
        with_graph_name: bool,
        #[cfg(feature = "rdf-star")] with_quoted_triples: bool,
        base_iri: Option<Iri<String>>,
        prefixes: HashMap<String, Iri<String>>,
    ) -> Parser<Self> {
        Parser::new(
            Lexer::new(
                N3Lexer::new(N3LexerMode::Turtle),
                MIN_BUFFER_SIZE,
                MAX_BUFFER_SIZE,
                true,
                Some(b"#"),
            ),
            TriGRecognizer {
                stack: vec![TriGState::TriGDoc],
                with_graph_name,
                #[cfg(feature = "rdf-star")]
                with_quoted_triples,
                lexer_options: N3LexerOptions { base_iri },
                prefixes,
                cur_subject: Vec::new(),
                cur_predicate: Vec::new(),
                cur_object: Vec::new(),
                cur_graph: GraphName::DefaultGraph,
            },
        )
    }

    #[must_use]
    fn error(
        mut self,
        errors: &mut Vec<RuleRecognizerError>,
        msg: impl Into<RuleRecognizerError>,
    ) -> Self {
        errors.push(msg.into());
        self.stack.clear();
        self
    }

    fn concatenate_iri(prefix: &str, local: Cow<'_, str>) -> NamedNode {
        let mut iri = String::with_capacity(prefix.len() + local.len());
        iri.push_str(prefix);
        iri.push_str(&local);
        NamedNode::new_unchecked(iri)
    }

    fn emit_quad(
        &self,
        results: &mut Vec<Quad>,
        subject: Option<impl Into<Subject>>,
        predicate: Option<impl Into<NamedNode>>,
        object: Option<impl Into<Term>>,
    ) {
        if let (Some(subject), Some(predicate), Some(object)) = (subject, predicate, object) {
            results.push(Quad::new(
                subject,
                predicate,
                object,
                self.cur_graph.clone(),
            ))
        }
    }

    fn new_triple(
        subject: Option<Subject>,
        predicate: Option<NamedNode>,
        object: Option<Term>,
    ) -> Option<Triple> {
        Some(Triple::new(subject?, predicate?, object?).into())
    }
}

#[derive(Debug)]
enum TriGState {
    TriGDoc,
    ExpectDot,
    BaseExpectIri,
    PrefixExpectPrefix,
    PrefixExpectIri {
        name: String,
    },
    TriplesOrGraph,
    WrappedGraphBlankNodePropertyListCurrent,
    SubjectBlankNodePropertyListEnd,
    SubjectBlankNodePropertyListAfter,
    SubjectCollectionBeginning,
    SubjectCollectionPossibleEnd,
    WrappedGraphOrPredicateObjectList {
        term: NamedOrBlankNode,
    },
    WrappedGraph,
    WrappedGraphPossibleEnd,
    GraphName,
    GraphNameAnonEnd,
    WrappedGraphContent,
    Triples,
    TriplesBlankNodePropertyListCurrent,
    PredicateObjectList,
    PredicateObjectListEnd,
    PredicateObjectListPossibleContinuation,
    PredicateObjectListClosingDot,
    ObjectsList,
    ObjectsListEnd,
    #[cfg(feature = "rdf-star")]
    AnnotationEnd,
    #[cfg(feature = "rdf-star")]
    ObjectsListAfterAnnotation,
    Verb,
    Object,
    ObjectBlankNodePropertyListCurrent,
    ObjectBlankNodePropertyListEnd,
    ObjectCollectionBeginning,
    ObjectCollectionPossibleEnd,
    LiteralPossibleSuffix {
        value: String,
    },
    LiteralExpectDatatype {
        value: String,
    },
    #[cfg(feature = "rdf-star")]
    SubjectQuotedTripleEnd,
    #[cfg(feature = "rdf-star")]
    ObjectQuotedTripleEnd,
    #[cfg(feature = "rdf-star")]
    QuotedSubject,
    #[cfg(feature = "rdf-star")]
    QuotedObject,
    #[cfg(feature = "rdf-star")]
    QuotedAnonEnd,
}
