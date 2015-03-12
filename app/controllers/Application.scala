package controllers

import java.io.ByteArrayOutputStream

import scala.collection.JavaConversions.seqAsJavaList
import scala.concurrent.Future

import org.apache.avro.io.DecoderFactory
import org.apache.avro.io.EncoderFactory
import org.apache.avro.specific.SpecificDatumReader
import org.apache.avro.specific.SpecificDatumWriter
import org.ga4gh.EnvironmentalContext
import org.ga4gh.Evidence
import org.ga4gh.Genotype
import org.ga4gh.Phenotype
import org.ga4gh.SearchGenotypePhenotypeRequest
import org.ga4gh.SearchGenotypePhenotypeResponse
import org.ga4gh.SearchPhenotypeGenotypeResponse
import org.ga4gh.VariantGenotypeAssociation
import org.ga4gh.VariantPhenotypeAssociation
import org.ga4gh.models.OntologyTerm

import models.SciGraphCategory
import models.SciGraphEdge
import models.SciGraphNode
import play.api.Play.current
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.JsValue
import play.api.libs.ws.WS
import play.api.libs.ws.WSRequestHolder
import play.api.mvc.Action
import play.api.mvc.Controller

object Application extends Controller {
  val serverUrl = "http://rosie.crbs.ucsd.edu:9000/scigraph/"
  val serviceUrl = "dynamic/"

  /// GET /dynamic/genes/6469/phenotypes
  val phenotypesWithGeneUrl = s"${serverUrl}${serviceUrl}genes/"
  val phenotypesWithGeneSuffixUrl = "/phenotypes.json/"

  // GET /dynamic/phenotypes/{phenotype_id}/genes
  val genesWithPhenotypeUrl = s"${serverUrl}${serviceUrl}phenotypes/"
  val genesWithPhenotypeSuffixUrl = "/genes.json/"

  def index = Action {
    //Ok(views.html.index("Your new application is ready."))
    Ok("")
  }

  def g2pSearch(geneId: String) = Action.async /**(BodyParsers.parse.json)*/ {
    implicit request =>
      //val requestObj = deserializeSearchGenotypePhenotypeRequest(request.body.toString)
      //requestObj.getGene
      //val geneId = (request.body \ "geneId").as[String]
      //val geneId = "6469"
      val responseFut = requestToSciGraph(geneId, phenotypesWithGeneUrl, phenotypesWithGeneSuffixUrl)

      responseFut.map(_ match {
        case (nodes, edges) => {
          val searchGenotypePhenotypeResponse = toSearchGenotypePhenotypeResponse(nodes, edges)
          Ok(serializeSearchGenotypePhenotypeRequest(searchGenotypePhenotypeResponse))
        }
      })
  }

  def p2gSearch(phenotypeId: String) = Action.async {
    implicit request =>
      //val phenotypeId = "HP_0000528"
      val responseFut = requestToSciGraph(phenotypeId, genesWithPhenotypeUrl, genesWithPhenotypeSuffixUrl)

      responseFut.map(_ match {
        case (nodes, edges) => {
          val searchGenotypePhenotypeResponse = toSearchPhenotypeGenotypeResponse(nodes, edges)
          Ok(serializeSearchPhenotypeGenotypeRequest(searchGenotypePhenotypeResponse))
        }
      })
  }

  /**
   * Example of a query """{"pageSize": {"int":5}, "variant":null, "evidence":null, "phenotype":null , "pageToken": null }"""
   *
   * @param input
   * @return SearchGenotypePhenotypeRequest
   */
  def deserializeSearchGenotypePhenotypeRequest(input: String): SearchGenotypePhenotypeRequest = {
    val datumReader = new SpecificDatumReader[SearchGenotypePhenotypeRequest](SearchGenotypePhenotypeRequest.getClassSchema)
    val decoder = DecoderFactory.get.jsonDecoder(SearchGenotypePhenotypeRequest.getClassSchema, input)
    datumReader.read(null, decoder)
  }

  def serializeSearchGenotypePhenotypeRequest(input: SearchGenotypePhenotypeResponse): String = {
    val out = new ByteArrayOutputStream()
    val datumWriter = new SpecificDatumWriter[SearchGenotypePhenotypeResponse](SearchGenotypePhenotypeResponse.getClassSchema)
    val encoder = EncoderFactory.get.jsonEncoder(SearchGenotypePhenotypeResponse.getClassSchema, out, true)
    datumWriter.write(input, encoder)
    encoder.flush()
    out.close()
    out.toString
  }

  def serializeSearchPhenotypeGenotypeRequest(input: SearchPhenotypeGenotypeResponse): String = {
    val out = new ByteArrayOutputStream()
    val datumWriter = new SpecificDatumWriter[SearchPhenotypeGenotypeResponse](SearchPhenotypeGenotypeResponse.getClassSchema)
    val encoder = EncoderFactory.get.jsonEncoder(SearchPhenotypeGenotypeResponse.getClassSchema, out, true)
    datumWriter.write(input, encoder)
    encoder.flush()
    out.close()
    out.toString
  }

  /**
   * http://rosie.crbs.ucsd.edu:9000/scigraph/dynamic/phenotypes_with_gene?gene_id=OMIM%3A136850
   * @param geneId
   * @return
   */
  def requestToSciGraph(input: String, urlPrefix: String, urlSuffix: String): Future[(List[SciGraphNode], List[SciGraphEdge])] = {
    val holder: WSRequestHolder = WS.url(urlPrefix + input + urlSuffix)
    val complexHolder: WSRequestHolder = holder.withHeaders("Accept" -> "application/json")
    complexHolder.get().map {
      response =>
        val json = response.json
        val nodes = (json \ "nodes").as[List[JsValue]].map(n => {
          val meta = SciGraphCategory((n \ "meta" \ "category").as[List[String]])
          SciGraphNode((n \ "id").as[String], (n \ "lbl").asOpt[String].getOrElse(""), meta)
        })
        val edges = (json \ "edges").as[List[JsValue]].map(e => {
          SciGraphEdge((e \ "sub").as[String], (e \ "obj").as[String], (e \ "pred").as[String])
        })
        (nodes, edges)
    }

  }

  def toSearchPhenotypeGenotypeResponse(nodes: List[SciGraphNode], edges: List[SciGraphEdge]): SearchPhenotypeGenotypeResponse = {
    val geneString = "gene"
    val genes = nodes.filter { n => n.meta.category.contains(geneString) }

    val variantGenotypeAssociation = genes.map(gene => {
      val ontologyTerm = new OntologyTerm()
      ontologyTerm.setId(gene.id)
      ontologyTerm.setName(gene.lbl)
      ontologyTerm.setOntologySource("") // TODO

      val genotype = new Genotype()
      genotype.setId(gene.id)
      genotype.setGenotype(ontologyTerm)

      val variantGenotypeAssociation = new VariantGenotypeAssociation()
      variantGenotypeAssociation.setId("") // TODO
      variantGenotypeAssociation.setGenotype(genotype)
      variantGenotypeAssociation
    })

    val searchPhenotypeGenotypeResponse = new SearchPhenotypeGenotypeResponse()
    searchPhenotypeGenotypeResponse.setAssociations(variantGenotypeAssociation)
    searchPhenotypeGenotypeResponse
  }

  def toSearchGenotypePhenotypeResponse(nodes: List[SciGraphNode], edges: List[SciGraphEdge]): SearchGenotypePhenotypeResponse = {
    val phenotypeString = "Phenotype"
    val hasObjectString = "hasObject"
    val evidenceString = "evidence"
    val phenotypes = nodes.filter { n => n.meta.category.contains(phenotypeString) }
    val variantPhenotypeAssociations = phenotypes.map(phenotype => {
      val ontologyTerm = new OntologyTerm()
      ontologyTerm.setId(phenotype.id)
      ontologyTerm.setName(phenotype.lbl)
      ontologyTerm.setOntologySource("") // TODO

      val ph = new Phenotype()
      ph.setId(phenotype.id)
      ph.setPhenotype(ontologyTerm)
      ph.setEnvironmentalContexts(List.empty[EnvironmentalContext])

      val (evidenceIdOpt, evidenceId) = {
        val annotation = edges.find { edge => edge.obj == phenotype.id && edge.pred == hasObjectString }
        if (annotation.isDefined) {
          val evidence = edges.find { edge => edge.sub == annotation.get.sub && edge.pred == evidenceString }
          if (evidence.isDefined) {
            (Some(evidence.get.obj), Some(evidence.get.sub))
          } else {
            (None, None)
          }
        } else {
          (None, None)
        }
      }
      val evidenceOntologyTerm = new OntologyTerm()
      evidenceOntologyTerm.setId(evidenceIdOpt.getOrElse(""))
      evidenceOntologyTerm.setOntologySource("")

      val evidence = new Evidence()
      evidence.setEvidenceType(evidenceOntologyTerm)

      val variantPhenotypeAssociation = new VariantPhenotypeAssociation()
      variantPhenotypeAssociation.setId(evidenceId.getOrElse(""))
      variantPhenotypeAssociation.setPhenotype(ph)
      variantPhenotypeAssociation.setEvidence(evidence)

      variantPhenotypeAssociation
    })

    val searchGenotypePhenotypeResponse = new SearchGenotypePhenotypeResponse()
    searchGenotypePhenotypeResponse.setAssociations(variantPhenotypeAssociations)
    searchGenotypePhenotypeResponse
  }

}