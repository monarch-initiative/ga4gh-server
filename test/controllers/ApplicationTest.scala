package controllers

import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.apache.avro.specific.SpecificDatumReader
import org.ga4gh.methods.SearchFeaturesRequest
import org.apache.avro.io.DecoderFactory
import org.apache.avro.specific.SpecificDatumWriter
import org.apache.avro.io.EncoderFactory
import java.io.OutputStream
import java.io.FileOutputStream
import org.apache.avro.file.DataFileWriter
import java.io.File
import java.io.ByteArrayOutputStream

@RunWith(classOf[JUnitRunner])
case class ApplicationTest() extends Specification {

  "Request" should {
    "be deserializable" in {

      //      val toSer = new JeremyTest(5)
      //      val userDatumWriter = new SpecificDatumWriter[JeremyTest](classOf[JeremyTest])
      //      val dataFileWriter = new DataFileWriter[JeremyTest](userDatumWriter)
      //      dataFileWriter.create(toSer.getSchema(), new File("/home/jnguyenxuan/output.avro"))
      //      dataFileWriter.append(toSer)
      //      dataFileWriter.close()

      //      val out = new FileOutputStream("/home/jnguyenxuan/output.json")
      //      val toSer = new JeremyTest()
      //      toSer.setNumb(4)
      //      val datumWriter = new SpecificDatumWriter[JeremyTest](JeremyTest.getClassSchema)
      //      val encoder = EncoderFactory.get.jsonEncoder(JeremyTest.getClassSchema, out, true)
      //      datumWriter.write(toSer, encoder)
      //      encoder.flush()
      //      out.close()
      //
      //      val input = """{"numb":{"int":4}, "s":null, "variant": null}"""
      //      val datumReader = new SpecificDatumReader[JeremyTest](JeremyTest.getClassSchema)
      //      val decoder = DecoderFactory.get.jsonDecoder(JeremyTest.getClassSchema, input)
      //      val obj = datumReader.read(null, decoder)
      //      println(obj)

      //      //val out = new FileOutputStream("/home/jnguyenxuan/output.json")
      //      val out = new ByteArrayOutputStream()
      //      val toSer = new SearchGenotypePhenotypeRequest()
      //      toSer.setPageSize(5)
      //      val datumWriter = new SpecificDatumWriter[SearchGenotypePhenotypeRequest](SearchGenotypePhenotypeRequest.getClassSchema)
      //      val encoder = EncoderFactory.get.jsonEncoder(SearchGenotypePhenotypeRequest.getClassSchema, out, true)
      //      datumWriter.write(toSer, encoder)
      //      encoder.flush()
      //      out.close()
      //      println(out.toString)

      val toSer = new SearchFeaturesRequest()
      toSer.setPageSize(5)
      println(G2P.serialize(toSer))

      val input = """{"pageSize": {"int":5},"feature":null, "evidence":null, "phenotype":null , "pageToken": null }"""
      val obj = G2P.deserializeSearchGenotypePhenotypeRequest(input)
      println("myobj")
      println(obj)
      true
    }
  }
}
