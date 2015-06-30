package controllers

import java.io.ByteArrayOutputStream
import scala.collection.JavaConversions._
import scala.concurrent.Future
import org.apache.avro.io.DecoderFactory
import org.apache.avro.io.EncoderFactory
import org.apache.avro.specific.SpecificDatumReader
import org.apache.avro.specific.SpecificDatumWriter
import org.ga4gh.models._
import models._
import play.api.Play.current
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.JsValue
import play.api.libs.ws.WS
import play.api.libs.ws.WSRequestHolder
import play.api.mvc.Action
import play.api.mvc.Controller
import org.apache.avro.specific.SpecificRecordBase
import play.api.Logger
import play.api.mvc.BodyParsers
import org.ga4gh.methods.SearchFeaturesRequest
import org.ga4gh.methods.SearchFeaturesResponse

object Application extends Controller {
  val serverUrl = "http://rosie.crbs.ucsd.edu:9000/scigraph/"
  //val serverUrl = "http://duckworth.crbs.ucsd.edu:9000/scigraph/"
  val serviceUrl = "dynamic/"

  // GET /dynamic/features/6469/phenotypes
  val phenotypesWithFeatureUrl = s"${serverUrl}${serviceUrl}features/"
  val phenotypesWithFeatureSuffixUrl = "/phenotypes.json?evidence=true"

  // GET /dynamic/phenotypes/{phenotype_id}/features
  val featuresWithPhenotypeUrl = s"${serverUrl}${serviceUrl}phenotypes/"
  val featuresWithPhenotypeSuffixUrl = "/features.json?evidence=true"

  // Static names
  val geneString = "gene"
  val sequenceFeatureString = "sequence feature"
  val phenotypeString = "Phenotype"
  val diseaseString = "disease"
  val hasObjectString = "hasObject"
  val hasSubjectString = "hasSubject"
  val evidenceString = "evidence"
  val intrinsicGenotypeString = "intrinsic genotype"

  def index = Action {
    Ok("Welcome to the G2P server.")
  }

  def g2pSearch(geneId: String) = Action.async /**(BodyParsers.parse.json)*/ {
    implicit request =>
      //val geneId = "NCBIGene:6469"
      val responseFut = requestToSciGraph(geneId, phenotypesWithFeatureUrl, phenotypesWithFeatureSuffixUrl)

      responseFut.map(_ match {
        case (nodes, edges) => {
          val searchGenotypePhenotypeResponse = toSearchGenotypePhenotypeResponse(nodes, edges)
          Ok(serialize(searchGenotypePhenotypeResponse))
        }
      })
  }

  def p2gSearch(phenotypeId: String) = Action.async /**((BodyParsers.parse.json)*/ {
    implicit request =>
      //val phenotypeId = "HP_0000528"
      Logger.info(s"Sending request to SciGraph...")
      val responseFut = requestToSciGraph(phenotypeId, featuresWithPhenotypeUrl, featuresWithPhenotypeSuffixUrl)

      responseFut.map(_ match {
        case (nodes, edges) => {
          Logger.info(s"Transforming data...")
          val searchGenotypePhenotypeResponse = toSearchPhenotypeGenotypeResponse(nodes, edges)
          Ok(serialize(searchGenotypePhenotypeResponse))
        }
      })
  }
 
  /**
   * @param input
   * @param urlPrefix
   * @param urlSuffix
   * @return nodes and edges tuple
   */
  def requestToSciGraph(input: String, urlPrefix: String, urlSuffix: String): Future[(List[SciGraphNode], List[SciGraphEdge])] = {
    val holder: WSRequestHolder = WS.url(urlPrefix + input + urlSuffix)
    val complexHolder: WSRequestHolder = holder.withHeaders("Accept" -> "application/json")
    Logger.info(s"SciGraph url: ${complexHolder.url}")
    complexHolder.get().map {
      response =>
        val json = response.json
        val nodes = (json \ "nodes").as[List[JsValue]].map(n => {
          val meta = SciGraphCategory((n \ "meta" \ "category").asOpt[List[String]].getOrElse(List.empty))
          SciGraphNode((n \ "id").as[String], (n \ "lbl").asOpt[String].getOrElse(""), meta)
        })
        val edges = (json \ "edges").as[List[JsValue]].map(e => {
          SciGraphEdge((e \ "sub").as[String], (e \ "obj").as[String], (e \ "pred").as[String])
        })
        (nodes, edges)
    }

  }

  def toSearchPhenotypeGenotypeResponse(nodes: List[SciGraphNode], edges: List[SciGraphEdge]): SearchFeaturesResponse = {
    val basePhenotype = nodes.filter { n => n.meta.category.contains(phenotypeString) }.apply(0) // TODO make sure that this the queried phenotype
    val genes = nodes.filter { n => n.meta.category.contains(geneString) || n.meta.category.contains(sequenceFeatureString) }
    val evidences = nodes.filter { n => n.meta.category.contains(evidenceString) }

    val basePhenotypeontologyTerm = new OntologyTerm()
    basePhenotypeontologyTerm.setId(basePhenotype.id)
    basePhenotypeontologyTerm.setName(basePhenotype.lbl)
    basePhenotypeontologyTerm.setOntologySource(basePhenotype.id.split(":").apply(0))

    val ph = new PhenotypeInstance()
    ph.setType(basePhenotypeontologyTerm)

    val featurePhenotypeAssociations = genes.map(gene => {
      val (evidenceIdOpt, evidenceLblOpt, associationId) = {
        val annotation = edges.find { edge => edge.obj == gene.id && edge.pred == hasSubjectString }
        if (annotation.isDefined) {
          val evidence = edges.find { edge => edge.sub == annotation.get.sub && edge.pred == evidenceString }
          if (evidence.isDefined) {
            val evidenceLbl = evidences.find(e => e.id == evidence.get.obj)
            (Some(evidence.get.obj), Some(evidenceLbl.get.lbl), Some(evidence.get.sub))
          } else {
            (None, None, None)
          }
        } else {
          (None, None, None)
        }
      }
      val evidenceOntologyTerm = new OntologyTerm()
      evidenceOntologyTerm.setId(evidenceIdOpt.getOrElse(""))
      evidenceOntologyTerm.setName(evidenceLblOpt.getOrElse(""))
      val evidenceOntologySource = {
        if (evidenceOntologyTerm.getId != "") {
          evidenceOntologyTerm.getId.toString.split(":").apply(0)
        } else { "" }
      }
      evidenceOntologyTerm.setOntologySource(evidenceOntologySource)

      val evidence = new Evidence()
      evidence.setEvidenceType(evidenceOntologyTerm)

      val ontologyTerm = new OntologyTerm()
      ontologyTerm.setId(gene.id)
      ontologyTerm.setName(gene.lbl)
      ontologyTerm.setOntologySource(gene.id.split(":").apply(0))

      val region = {
        val position = new Position() // TODO
        position.setPosition(0) // TODO
        val r = new Region()
        r.setLength(0)
        r.setStart(position)
        r
      }
      
      val path = new Path()
      path.setSegments(List.empty[Segment]) // TODO

      val attributes = new Attributes()
      attributes.setVals(mapAsJavaMap(Map.empty))

      val featureType = new OntologyTerm()
      featureType.setId("SO:0000704")
      featureType.setName("gene")
      featureType.setOntologySource("SO")

      val feature = new Feature()
      feature.setId(gene.id)
      feature.setParentIds(List.empty[String])
      feature.setFeatureSetId("") // TODO
      feature.setFeatureType(featureType)
      feature.setPath(path)
      feature.setAttributes(attributes)

      val featurePhenotypeAssociation = new FeaturePhenotypeAssociation()
      featurePhenotypeAssociation.setId(associationId.getOrElse(""))
      featurePhenotypeAssociation.setFeatures(List(feature))
      featurePhenotypeAssociation.setEvidence(List(evidence))
      featurePhenotypeAssociation.setPhenotype(ph)
      featurePhenotypeAssociation.setEnvironmentalContexts(List.empty[EnvironmentalContext])

      featurePhenotypeAssociation
    })

    val searchPhenotypeGenotypeResponse = new SearchFeaturesResponse()
    searchPhenotypeGenotypeResponse.setAssociations(featurePhenotypeAssociations)
    searchPhenotypeGenotypeResponse
  }

  def toSearchGenotypePhenotypeResponse(nodes: List[SciGraphNode], edges: List[SciGraphEdge]): SearchFeaturesResponse = {
    val baseGene = nodes.filter { n => n.meta.category.contains(geneString) }.apply(0) // TODO make sure that this the queried gene
    val phenotypes = nodes.filter { n => n.meta.category.contains(phenotypeString) || n.meta.category.contains(diseaseString) }
    val evidences = nodes.filter { n => n.meta.category.contains(evidenceString) }
    val variantPhenotypeAssociations = phenotypes.map(phenotype => {
      val ontologyTerm = new OntologyTerm()
      ontologyTerm.setId(phenotype.id)
      ontologyTerm.setName(phenotype.lbl)
      ontologyTerm.setOntologySource(phenotype.id.split(":").apply(0))

      val ph = new PhenotypeInstance()
      ph.setType(ontologyTerm)

      val (evidenceIdOpt, evidenceLblOpt, associationId) = {
        val annotation = edges.find { edge => edge.obj == phenotype.id && edge.pred == hasObjectString }
        if (annotation.isDefined) {
          val evidence = edges.find { edge => edge.sub == annotation.get.sub && edge.pred == evidenceString }
          if (evidence.isDefined) {
            val evidenceLbl = evidences.find(e => e.id == evidence.get.obj)
            (Some(evidence.get.obj), Some(evidenceLbl.get.lbl), Some(evidence.get.sub))
          } else {
            (None, None, None)
          }
        } else {
          (None, None, None)
        }
      }
      val evidenceOntologyTerm = new OntologyTerm()
      evidenceOntologyTerm.setId(evidenceIdOpt.getOrElse(""))
      evidenceOntologyTerm.setName(evidenceLblOpt.getOrElse(""))
      val evidenceOntologySource = {
        if (evidenceOntologyTerm.getId != "") {
          evidenceOntologyTerm.getId.toString.split(":").apply(0)
        } else { "" }
      }
      evidenceOntologyTerm.setOntologySource(evidenceOntologySource)

      val evidence = new Evidence()
      evidence.setEvidenceType(evidenceOntologyTerm)

      val path = new Path()
      path.setSegments(List.empty[Segment]) // TODO
      
      val region = {
        val position = new Position() // TODO
        position.setPosition(0) // TODO
        val r = new Region()
        r.setLength(0)
        r.setStart(position)
        r
      }

      val attributes = new Attributes()
      attributes.setVals(mapAsJavaMap(Map.empty))

      val baseGeneOntologyTerm = new OntologyTerm()
      baseGeneOntologyTerm.setId(baseGene.id)
      baseGeneOntologyTerm.setName(baseGene.lbl)
      baseGeneOntologyTerm.setOntologySource(baseGene.id.split(":").apply(0))

      val featureType = new OntologyTerm()
      featureType.setId("SO:0000704")
      featureType.setName("gene")
      featureType.setOntologySource("SO")

      val feature = new Feature()
      feature.setId(baseGene.id)
      feature.setParentIds(List.empty[String])
      feature.setFeatureSetId("") // TODO
      feature.setFeatureType(featureType)
      feature.setPath(path)
      feature.setAttributes(attributes)

      val featurePhenotypeAssociation = new FeaturePhenotypeAssociation()
      featurePhenotypeAssociation.setId(associationId.getOrElse(""))
      featurePhenotypeAssociation.setPhenotype(ph)
      featurePhenotypeAssociation.setEvidence(List(evidence))
      featurePhenotypeAssociation.setFeatures(List(feature))
      featurePhenotypeAssociation.setEnvironmentalContexts(List.empty[EnvironmentalContext])

      featurePhenotypeAssociation
    })

    val searchGenotypePhenotypeResponse = new SearchFeaturesResponse()
    searchGenotypePhenotypeResponse.setAssociations(variantPhenotypeAssociations)
    searchGenotypePhenotypeResponse
  }

  /**
   * Example of a query """{"pageSize": {"int":5}, "variant":null, "evidence":null, "phenotype":null , "pageToken": null }"""
   *
   * @param input
   * @return SearchFeaturesRequest
   */
  def deserializeSearchGenotypePhenotypeRequest(input: String): SearchFeaturesRequest = {
    val datumReader = new SpecificDatumReader[SearchFeaturesRequest](SearchFeaturesRequest.getClassSchema)
    val decoder = DecoderFactory.get.jsonDecoder(SearchFeaturesRequest.getClassSchema, input)
    datumReader.read(null, decoder)
  }

  def serialize[T <: SpecificRecordBase](item: T): String = {
    val out = new ByteArrayOutputStream()
    val datumWriter = new SpecificDatumWriter[T](item.getSchema)
    val encoder = EncoderFactory.get.jsonEncoder(item.getSchema, out, true)
    datumWriter.write(item, encoder)
    encoder.flush()
    out.close()
    out.toString
  }

}