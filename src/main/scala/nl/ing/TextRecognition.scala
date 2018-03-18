package nl.ing

import com.google.cloud.vision.v1.{AnnotateImageRequest, Feature, Image, ImageAnnotatorClient}
import java.io.FileInputStream
import java.io.IOException
import java.io.PrintStream
import java.util

import com.google.protobuf.ByteString


object TextRecognition {

  @throws[Exception]
  @throws[IOException]
  def detectDocumentText(imageBytes: Seq[Byte]): Unit = {
    val requests: util.ArrayList[AnnotateImageRequest] = new util.ArrayList[AnnotateImageRequest]

    val imgBytes: ByteString = ByteString.copyFrom(imageBytes.toArray)
    val img: Image = Image.newBuilder.setContent(imgBytes).build

    val feat = Feature.newBuilder.setType(Feature.Type.DOCUMENT_TEXT_DETECTION).build
    val request = AnnotateImageRequest.newBuilder.addFeatures(feat).setImage(img).build
    requests.add(request)
    try {
      val client = ImageAnnotatorClient.create
      try {
        val response = client.batchAnnotateImages(requests)
        val responses = response.getResponsesList
        client.close
        import scala.collection.JavaConversions._
        for (res <- responses) {
          if (res.hasError) {
            println("Error: %s\n", res.getError.getMessage)
            return
          }
          // For full list of available annotations, see http://g.co/cloud/vision/docs
          val annotation = res.getFullTextAnnotation
          import scala.collection.JavaConversions._
          for (page <- annotation.getPagesList) {
            var pageText = ""
            import scala.collection.JavaConversions._
            for (block <- page.getBlocksList) {
              var blockText = ""
              import scala.collection.JavaConversions._
              for (para <- block.getParagraphsList) {
                var paraText = ""
                import scala.collection.JavaConversions._
                for (word <- para.getWordsList) {
                  var wordText = ""
                  import scala.collection.JavaConversions._
                  for (symbol <- word.getSymbolsList) {
                    wordText = wordText + symbol.getText
                  }
                  paraText = paraText + wordText
                }
                // Output Example using Paragraph:
                println("Paragraph: \n" + paraText)
                println("Bounds: \n" + para.getBoundingBox + "\n")
                blockText = blockText + paraText
              }
              pageText = pageText + blockText
            }
          }
          println(annotation.getText)
        }
      } finally if (client != null) client.close()
    }
  }
}
